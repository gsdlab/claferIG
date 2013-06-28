{-# LANGUAGE NamedFieldPuns #-}

{-
 Copyright (C) 2012-2013 Jimmy Liang <http://gsd.uwaterloo.ca>

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
-}

module Language.Clafer.IG.CommandLine (claferIGVersion, runCommandLine, printError, findNecessaryBitwidth) where

import Language.ClaferT
import Language.Clafer.IG.ClaferIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.CommandLineParser
import Language.Clafer.IG.Constraints
import Language.Clafer.IG.JSONGenerator
import qualified Language.Clafer.IG.AlloyIGInterface as AlloyIG
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Console.Haskeline
import System.IO

data AutoComplete = Auto_Command | Auto_Clafer | Auto_ClaferInstance | Auto_UnsatCoreMinimization | Auto_Space | Auto_Digit | No_Auto deriving Show

data AutoCompleteContext = AutoCompleteContext {clafers::IORef [String], claferInstances::IORef [String]}

data Context = Context {currentAlloyInstance::Maybe String, saved::[ClaferModel], unsaved::[ClaferModel], autoCompleteContext::AutoCompleteContext}


runCommandLine :: ClaferIGT IO ()
runCommandLine =
    do
        solve
        bitwidth' <- getBitwidth
        when (bitwidth' > 9) $ liftIO $ putStrLn $ "Warning! Bitwidth has been set to " ++ show bitwidth' ++ ". This is a very large bitwidth, alloy may be using a large amount of memory. This may cause slow down."
        
        clafers <- getClafers

        clafersRef <- liftIO $ newIORef clafers
        claferInstancesRef <- liftIO $ newIORef []

        let autoCompleteContext = AutoCompleteContext clafersRef claferInstancesRef
        runInputT Settings {
            complete = completeFunc autoCompleteContext,
            historyFile = Nothing,
            autoAddHistory = True
        } $ loop Next (Context Nothing [] [] autoCompleteContext)
    where 

    getScopeinfo :: Integer -> Integer -> String -> (Integer, String)    
    getScopeinfo bwcapacity requestedScope name = 
        let a = min requestedScope bwcapacity
            b =  if (requestedScope > bwcapacity) then "Requested scope for " ++ name ++ " is larger than maximum allowed by bitwidth (" ++ (show bwcapacity) ++ ")\n" else ""
        in (a,b)

    loop :: Command -> Context -> InputT (ClaferIGT IO) ()
    
    loop Quit _ = return ()
    
    loop Next context =
        do
            solution <- lift next
            claferIGArgs' <- lift getClaferIGArgs 
            info <- lift getInfo
            case solution of
                Instance claferModel xml -> do
                    liftIO $ writeIORef (claferInstances $ autoCompleteContext context) $ map c_name (traverse claferModel)
                    
                    outputStrLn $ if json claferIGArgs' 
                        then generateJSON info claferModel
                        else show claferModel
                    nextLoop context{unsaved=claferModel:(unsaved context), currentAlloyInstance=Just xml}
                UnsatCore core counterexample -> do
                    liftIO $ hPutStrLn stderr "No more instances found. Try increasing scope to get more instances."
                    liftIO $ hPutStrLn stderr "The following set of constraints cannot be satisfied in the current scope."
                    liftIO $ hPutStrLn stderr "(Hint: use the setUnsatCoreMinimization command to minimize the set of constraints below)"
                    printConstraints core
                    case counterexample of
                        Just (Counterexample removed claferModel xml) -> do
                            liftIO $ hPutStrLn stderr "Altering the following constraints produced the following near-miss example:"
                            printTransformations removed
                            outputStrLn $ show claferModel
                        Nothing -> return ()
                    nextLoop context
                NoInstance -> do
                    liftIO $ hPutStrLn stderr "No more instances found. Try increasing scope to get more instances."
                    nextLoop context
            where
            printConstraint UserConstraint{constraintInfo = info} = show info
            printConstraint constraint = show $ claferInfo constraint
            
            printConstraints = printConstraints' 1
            printConstraints' _ [] = return ()
            printConstraints' i (c:cs) =
                do
                    liftIO $ hPutStrLn stderr $ "  " ++ show i ++ ") " ++ printConstraint c
                    printConstraints' (i + 1) cs
            
            printTransformation :: Constraint -> [Constraint] -> (String, [Constraint])        
            printTransformation UserConstraint{constraintInfo = info} rest = ("removed " ++ syntax info, rest)
            printTransformation ExactCardinalityConstraint{claferInfo = info} rest =
                (show info ++ " changed to " ++ show (setUpper (setLower info 0) Nothing), rest)
            printTransformation LowerCardinalityConstraint{claferInfo = info} rest =
                case deleteUpper (uniqueId info) rest of
                    -- Deleted the lower and upper constraint
                    Just rest' -> (show info ++ " changed to " ++ show (setUpper (setLower info 0) Nothing), rest')
                    -- Only deleted the lower constraint
                    Nothing    -> (show info ++ " changed to " ++ show (setLower info 0), rest)
            printTransformation UpperCardinalityConstraint{claferInfo = info} rest =
                case deleteLower (uniqueId info) rest of
                    -- Deleted the lower and upper constraint
                    Just rest' -> (show info ++ " changed to " ++ show (setUpper (setLower info 0) Nothing), rest')
                    -- Only deleted the upper constraint
                    Nothing    -> (show info ++ " changed to " ++ show (setUpper info Nothing), rest)



            printTransformations cs = printTransformations' 1 cs
            printTransformations' _ [] = return ()
            printTransformations' i (c:cs) =
                do
                    let (print, rest) = printTransformation c cs
                    liftIO $ hPutStrLn stderr $ "  " ++ show i ++ ") " ++ print
                    printTransformations' (i + 1) rest
                    
            setLower info@ClaferInfo{cardinality = c} lower = info{cardinality = c{lower = lower}}

            setUpper info@ClaferInfo{cardinality = c} upper = info{cardinality = c{upper = upper}}

            deleteLower :: String -> [Constraint] -> Maybe [Constraint]                    
            deleteLower id ys =
                findAndDelete id (filter isLowerCardinalityConstraint ys)
            
            deleteUpper :: String -> [Constraint] -> Maybe [Constraint]
            deleteUpper id ys =
                findAndDelete id (filter isUpperCardinalityConstraint ys)
            
            findAndDelete :: String -> [Constraint] -> Maybe [Constraint]
            findAndDelete _ [] = Nothing
            findAndDelete id (c:cs)
                | id == uniqueId (claferInfo c) = Just cs
                | otherwise                     = (c :) `fmap` findAndDelete id cs
                
                    
    loop Help context =
        do
            outputStrLn (
                "---------------------------\n" ++
                "| " ++ claferIGVersion ++ " |\n" ++
                "---------------------------\n\n" ++
                "You can invoke the following commands as indicated by single quotes:\n" ++
                "[tab]           - print the available commands\n" ++ 
                "                - auto-complete command name, a clafer name, or clafer instance name in a given context\n" ++
                "'n'ext, [enter] - to produce the next instance if available or to output a message that no more \n" ++
                "                  instances exist within the given scope\n" ++
                "'i'ncrease      - to increase the maximum number of instances of a given clafer or all clafers (scope)\n" ++
                "'s'ave          - to save all instances displayed so far or a counterexample to files named \n" ++
                "                  <model file name>.cfr.<instance number>.data, one instance per file\n" ++
                "'q'uit          - to quit the interactive session\n" ++
                "'r'eload        - to reload your clafer model\n" ++
                "'h'elp          - to display this menu options summary\n" ++
                "'scope'         - to print out the values of the global scope and individual Clafer scopes\n" ++
                "'setUnsatCoreMinimization' - to choose UnSAT core minimization strategy [fastest | medium | best]. Default: fastest\n" ++ 
                "'claferModel'   - to print out the original Clafer model verbatim\n" ++
                "'alloyModel'    - to print out the output of Clafer translator verbatim\n" ++
                "'alloyInstance' - to print out the Alloy xml document of the most recent solution\n" ++
                "'f'ind          - to print a Clafer with given name found in the most recent solution\n\n" ++
                "Parameterized command usage:\n" ++
                "'i [enter]'         - to increase for all clafers by 1\n" ++
                "'i <name> [enter]'  - to increase for the clafer <name> by 1\n" ++
                "'i <name> <number>' - to increase for the clafer <name> by <number>\n" ++
                "'f <name>'          - to display a clafer <name>\n" ++
                "'setUnsatCoreMinimization fastest' - fastest but the worst\n" ++ 
                "'setUnsatCoreMinimization medium'\n" ++ 
                "'setUnsatCoreMinimization best' - best but slowest even for modest size cores" 
                )
                
            nextLoop context
            
    loop Save context@Context{saved=saved, unsaved=unsaved} =
        do
            lift $ save unsaved (toInteger $ length saved)
            nextLoop context{saved=unsaved ++ saved, unsaved=[]}

    loop Reload context =
        do
            oldScopes <- lift getScopes
            let oldScopeNames = map nameOfScope oldScopes
            oldScopeVals <- mapM (lift . valueOfScope) oldScopes
            runErrorT $ ErrorT (lift reload) `catchError` (liftIO . mapM_ (hPutStrLn stderr) . printError)
            oldBw <- lift getBitwidth
            args <- lift getClaferIGArgs
            cModel <- liftIO $ strictReadFile $ claferModelFile args
            lift $ setBitwidth $ findNecessaryBitwidth cModel oldBw
            newScopes <- lift getScopes
            lift $ setScopes (zip oldScopeNames oldScopeVals) newScopes
            lift $ solve
            nextLoop context
            where
                setScopes _ [] = return()
                setScopes old new = let oldval = lookup (nameOfScope $ head new) old
                                    in if (oldval == Nothing) then (setScopes old $ tail new) else do 
                                        newVal <- valueOfScope $ head new
                                        setScope (max newVal $ fromJust oldval) $ head new
                                        setScopes old $ tail new 
                                            


    loop (IncreaseGlobalScope i) context =
        do
            globalScope <- lift getGlobalScope
            bitwidth' <- lift getBitwidth
            let bwcapacity = ((2 ^ (bitwidth' - 1)) - 1)
            let (globalScope',errMsg) = getScopeinfo bwcapacity (globalScope+i) "Global Scope"
            lift $ setGlobalScope globalScope'
            
            scopes <- lift getScopes
            forM scopes (\x -> do
                value <- lift $ valueOfScope x
                if ((value+i) > bwcapacity) then outputStrLn $ "Requested scope for " ++ (nameOfScope x) ++ " is larger than maximum allowed by bitwidth (" ++ (show bwcapacity) ++ ")" else return ())
            lift $ mapM (increaseScope i) scopes
            lift solve
            
            outputStrLn (errMsg ++ "Global scope increased to " ++ show globalScope')
            nextLoop context
            
    loop (IncreaseScope name i) context =
        do
            try $ do
                scope <- ErrorT $ lift $ getScope name
                scopeValue <- lift $ lift $ valueOfScope scope 
                bitwidth' <- lift $ lift getBitwidth
                let (scopeValue', errorMsg) = getScopeinfo ((2 ^ (bitwidth' - 1)) - 1) (scopeValue+i) name
                ErrorT $ lift $ setScope scopeValue' scope
                
                lift $ lift $ solve
                lift $ outputStrLn (errorMsg ++ "Scope of " ++ name ++ " increased to " ++ show scopeValue')
                
            nextLoop context
            
    loop ShowScope context =
        do
            globalScope <- lift getGlobalScope
            outputStrLn $ "Global scope = " ++ show globalScope
            scopes <- lift getScopes
            mapM_ printScope scopes
            nextLoop context
            
            where
            printScope scope =
                do
                    let name = nameOfScope scope
                    value <- lift $ valueOfScope scope
                    outputStrLn $ "  " ++ name ++ " scope = " ++ show value
                    

    loop (Find name) context =
        do
            case (unsaved context ++ saved context) of
                model:_ ->
                    case find ((name ==) . c_name) $ traverse model of
                        Just clafer -> outputStrLn $ show clafer
                        Nothing -> outputStrLn $ "\"" ++ name ++ "\" not found in the model."
                []  -> outputStrLn $ "No instance"
            nextLoop context

    loop ShowClaferModel context =
        do
            claferModel <- lift getClaferModel
            outputStrLn claferModel
            nextLoop context
            
    loop ShowAlloyModel context =
        do
            alloyModel <- lift getAlloyModel
            outputStrLn alloyModel
            nextLoop context            

    loop ShowAlloyInstance context =
        do
            case currentAlloyInstance context of
                Just alloyInstance -> outputStrLn alloyInstance
                Nothing -> outputStrLn $ "No instance"
            nextLoop context
            
    loop (SetUnsatCoreMinimization level) context =
        do
            let level' = 
                    case level of
                        Fastest -> 2
                        Medium  -> 1
                        Best    -> 0
            lift $ setUnsatCoreMinimization level' >> solve
            
            nextLoop context

    nextLoop context =
        do
            minput <- getInputLine "claferIG> "
            case minput of
                Nothing -> loop Quit context
                Just input ->
                    case parseCommandLine input of
                        Left error    -> outputStrLn (show error) >> nextLoop context
                        Right command -> loop command context
    
    save :: MonadIO m => [ClaferModel] -> Integer -> ClaferIGT m ()
    save [] _ = return ()
    save (c:cs) counter = do
        claferIGArgs' <- getClaferIGArgs
        let 
            claferModelFile' = claferModelFile claferIGArgs'
            saveName = claferModelFile' ++ "." ++ (show counter) ++ ".data"
        liftIO $ writeFile saveName (show c)
        liftIO $ putStrLn $ "Saved to " ++ saveName
        save cs (counter + 1)

try :: MonadIO m => ErrorT String (InputT m) a -> InputT m ()
try e = either outputStrLn (void . return) =<< runErrorT e
        

-- i Ali|ce
--     If the cursor is at | then not open. The "ce" prevents autocomplete.
-- i Ali| ce
--     Is open, and autocomplete will kick in.
isOpen prev [] = True
isOpen prev (x:_) = isSpace x


completeFunc :: MonadIO m => AutoCompleteContext -> CompletionFunc m
completeFunc context (prev, next) = 
    if isOpen prev next then
        liftIO $ evalComplete context prev next
    else
        return (prev, [])


completePrefix prefix choices = map simpleCompletion $ filter (prefix `isPrefixOf`) choices


evalComplete :: AutoCompleteContext -> String -> String -> IO (String, [Completion])
evalComplete context prev next = 
    do
        completion <- autoComplete context word auto
        return (reverseRest, completion)
    where
    input = reverse prev
    (reverseWord, reverseRest) = break isSpace prev
    word = reverse reverseWord
    auto = autoCompleteDetect $ parseCommandLineAutoComplete input
        

autoComplete :: AutoCompleteContext -> String -> AutoComplete -> IO [Completion]
autoComplete context word Auto_Command = return $ completePrefix word commandStrings
autoComplete context word Auto_Clafer =
    do
        c <- readIORef $ clafers context
        return $ completePrefix word c
autoComplete context word Auto_ClaferInstance =
    do
        ci <- readIORef $ claferInstances context
        return $ completePrefix word ci
autoComplete context word Auto_UnsatCoreMinimization = return $ completePrefix word ["fastest", "medium", "best"]
autoComplete context word Auto_Digit = return [] -- Don't auto complete numbers.
autoComplete context word Auto_Space = return [simpleCompletion $ word]
autoComplete context word No_Auto = return []


autoCompleteDetect error
    -- An unexpected message means that parsing failed before eof
    | any (not . null) unexpectedMessages = No_Auto
    | any (== "command") expectedMessages = Auto_Command
    | any (== "clafer") expectedMessages = Auto_Clafer
    | any (== "claferInstance") expectedMessages = Auto_ClaferInstance
    | any (== "Unsat core minimization level") expectedMessages = Auto_UnsatCoreMinimization
    | any (== "digit") expectedMessages = Auto_Digit
    | any (== "space") expectedMessages = Auto_Space
    | otherwise = No_Auto
    where
    messages = errorMessages error
    unexpectedMessages = mapMaybe unexpectedMessage messages
    expectedMessages   = mapMaybe expectedMessage messages
    

printError :: ClaferErrs -> [String]    
printError (ClaferErrs errs) =
    map printError errs
    where
    printError (ParseErr ErrPos{modelPos = Pos l c} msg) =
        "Parse error at line " ++ show l ++ ", column " ++ show c ++ ":\n    " ++ msg
    printError (SemanticErr ErrPos{modelPos = Pos l c} msg) =
        "Error at line " ++ show l ++ ", column " ++ show c ++ ":\n    " ++ msg
    printError (ClaferErr msg) =
        "Error:\n    " ++ msg

findNecessaryBitwidth :: String -> Integer -> Integer
findNecessaryBitwidth model oldBw = if (newBw < oldBw) then oldBw else newBw
    where
        newBw = ceiling $ logBase 2 $ 1 + 2 * maxInModel model []
        digitToFloat = toEnum . digitToInt
        maxInModel [] [] = 0
        maxInModel [] acc = maximum acc
        maxInModel (x:xs) acc = if (isNumber x) then (findFullNum xs (digitToFloat x) acc) else (maxInModel xs acc)
        findFullNum [] n acc = maximum $ n:acc
        findFullNum (x:xs) n acc = if (isNumber x) then (findFullNum xs (n * 10 + (digitToFloat x)) acc) else maxInModel xs (n:acc)
