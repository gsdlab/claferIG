{-
 Copyright (C) 2012 Jimmy Liang <http://gsd.uwaterloo.ca>

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

module CommandLine (claferIGVersion, runCommandLine) where

import ClaferIG
import ClaferModel
import CommandLineParser
import Constraints
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Console.Haskeline


data AutoComplete = Auto_Command | Auto_Clafer | Auto_ClaferInstance | Auto_Space | Auto_Digit | No_Auto deriving Show

data AutoCompleteContext = AutoCompleteContext {clafers::IORef [String], claferInstances::IORef [String]}

data Context = Context {currentAlloyInstance::Maybe String, saved::[ClaferModel], unsaved::[ClaferModel], autoCompleteContext::AutoCompleteContext}



runCommandLine :: ClaferIG -> IO ()
runCommandLine claferIG =
    do
        solve claferIG
        
        clafers <- getClafers claferIG

        clafersRef <- newIORef clafers
        claferInstancesRef <- newIORef []

        let autoCompleteContext = AutoCompleteContext clafersRef claferInstancesRef
        runInputT Settings {
            complete = completeFunc autoCompleteContext,
            historyFile = Nothing,
            autoAddHistory = True
        } $ loop Next (Context Nothing [] [] autoCompleteContext)
    where 
    loop :: Command -> Context -> InputT IO ()
    
    loop Quit _ = return ()
    
    loop Next context =
        do
            solution <- lift $ next claferIG
            case solution of
                Instance claferModel xml -> do
                    lift $ writeIORef (claferInstances $ autoCompleteContext context) $ map c_name (traverse claferModel)
                    
                    outputStrLn $ show claferModel
                    nextLoop context{unsaved=claferModel:(unsaved context), currentAlloyInstance=Just xml}
                UnsatCore core counterexample -> do
                    outputStrLn "No more instances found. Try increasing scope to get more instances."
                    outputStrLn "The following set of constraints cannot be satisfied in the current scope."
                    printConstraints core
                    case counterexample of
                        Just (Counterexample removed claferModel xml) -> do
                            outputStrLn "Altering the following constraints produced a counterexample."
                            printTransformations removed
                            outputStrLn $ show claferModel
                        Nothing -> return ()
                    nextLoop context
                NoInstance -> do
                    outputStrLn "No more instances found. Try increasing scope to get more instances."
                    nextLoop context
            where
            printConstraint UserConstraint{constraintInfo = info} = show info
            printConstraint constraint = show $ claferInfo constraint
            
            printConstraints = printConstraints' 1
            printConstraints' _ [] = return ()
            printConstraints' i (c:cs) =
                do
                    outputStrLn $ "  " ++ show i ++ ") " ++ printConstraint c
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
                    outputStrLn $ "  " ++ show i ++ ") " ++ print
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
                "'h'elp          - to display this menu options summary\n" ++
                "'scope'         - to print out the values of the global scope and individual Clafer scopes\n" ++
                "'claferModel'   - to print out the original Clafer model verbatim\n" ++
                "'alloyModel'    - to print out the output of Clafer translator verbatim\n" ++
                "'alloyInstance' - to print out the Alloy xml document of the most recent solution\n" ++
                "'f'ind          - to print a Clafer with given name found in the most recent solution\n\n" ++
                "Parameterized command usage:\n" ++
                "'i' [enter]         - to increase for all clafers by 1\n" ++
                "'i' <name> [enter]  - to increase for the clafer <name> by 1\n" ++
                "'i' <name> <number> - to increase for the clafer <name> by <number>\n" ++
                "'f' <name>          - to display a clafer <name>\n")
                
            nextLoop context
            
    loop Save context@Context{saved=saved, unsaved=unsaved} =
        do
            lift $ save unsaved (toInteger $ length saved)
            nextLoop context{saved=unsaved ++ saved, unsaved=[]}

    loop Reload context =
        do
            lift $ reload claferIG
            lift $ solve claferIG
            nextLoop context

    loop (IncreaseGlobalScope i) context =
        do
            globalScope <- lift $ getGlobalScope claferIG
            let globalScope' = globalScope + i
            lift $ setGlobalScope globalScope' claferIG
            
            scopes <- lift $ getScopes claferIG
            lift $ mapM (increaseScope i) scopes
            lift $ solve claferIG
            
            outputStrLn ("Global scope increased to " ++ show globalScope')
            nextLoop context
            
    loop (IncreaseScope name i) context =
        do
            scope <- lift $ getScope name claferIG
            case scope of
                Just scope' ->
                    do
                        error <- lift $ increaseScope i scope'
                        case error of
                            Just subset ->
                                outputStrLn $ "Cannot increase scope of the reference Clafer \"" ++ name ++ "\". Try increasing the scope of its refered Clafer \"" ++ subset ++ "\"."
                            Nothing ->
                                do
                                    scopeValue <- lift $ valueOfScope scope'
                                    lift $ solve claferIG
                                    outputStrLn ("Scope of " ++ name ++ " increased to " ++ show scopeValue)
                Nothing -> outputStrLn ("Unknown clafer " ++ name)
            nextLoop context
            
    loop ShowScope context =
        do
            globalScope <- lift $ getGlobalScope claferIG
            outputStrLn $ "Global scope = " ++ show globalScope
            scopes <- lift $ getScopes claferIG
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
            claferModel <- lift $ getClaferModel claferIG
            outputStrLn claferModel
            nextLoop context
            
            
    loop ShowAlloyModel context =
        do
            alloyModel <- lift $ getAlloyModel claferIG
            outputStrLn alloyModel
            nextLoop context

    loop ShowAlloyInstance context =
        do
            case currentAlloyInstance context of
                Just alloyInstance -> outputStrLn alloyInstance
                Nothing -> outputStrLn $ "No instance"
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
    
    save :: [ClaferModel] -> Integer -> IO ()
    save [] _ = return ()
    save (c:cs) counter =
        do
            writeFile saveName (show c)
            putStrLn $ "Saved to " ++ saveName
            save cs (counter + 1)
        where saveName = (claferFile claferIG) ++ "." ++ (show counter) ++ ".data"
    


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
autoComplete context word Auto_Digit = return [] -- Don't auto complete numbers.
autoComplete context word Auto_Space = return [simpleCompletion $ word]
autoComplete context word No_Auto = return []


autoCompleteDetect error
    -- An unexpected message means that parsing failed before eof
    | any (not . null) unexpectedMessages = No_Auto
    | any (== "command") expectedMessages = Auto_Command
    | any (== "clafer") expectedMessages = Auto_Clafer
    | any (== "claferInstance") expectedMessages = Auto_ClaferInstance
    | any (== "digit") expectedMessages = Auto_Digit
    | any (== "space") expectedMessages = Auto_Space
    | otherwise = No_Auto
    where
    messages = errorMessages error
    unexpectedMessages = mapMaybe unexpectedMessage messages
    expectedMessages   = mapMaybe expectedMessage messages
