{-# LANGUAGE NamedFieldPuns #-}

{-
 Copyright (C) 2012-2014 Jimmy Liang, Michal Antkiewicz <http://gsd.uwaterloo.ca>

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

module Language.Clafer.IG.CommandLine (runCommandLine, printError, findNecessaryBitwidth, intToFloat, pickLargerScope, allowedMaxInt, requiredBitwidth) where

import Language.Clafer
import Language.ClaferT
import Language.Clafer.Intermediate.Intclafer
import Language.Clafer.IG.ClaferIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.CommandLineParser
import Language.Clafer.IG.Constraints
import Language.Clafer.IG.JSONGenerator
import Language.Clafer.Comments
import Language.Clafer.JSONMetaData
import Language.Clafer.QNameUID
import qualified Language.Clafer.IG.AlloyIGInterface as AlloyIG
import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.IORef
import Data.List
import Data.Monoid
import Data.Foldable (foldMap)
import qualified Data.StringMap as SMap
import qualified Data.Map as Map
import Data.Maybe
import System.Console.Haskeline
import System.IO
import GHC.Float
import Prelude hiding (id)
import qualified Text.Parsec.Error as E

data AutoComplete = Auto_Command | Auto_Clafer | Auto_ClaferInstance | Auto_UnsatCoreMinimization | Auto_Space | Auto_Digit | No_Auto deriving Show

data AutoCompleteContext = AutoCompleteContext {clafers::IORef [String], claferInstances::IORef [String]}

data Context = Context {currentAlloyInstance::Maybe String, saved::[ClaferModel], unsaved::[ClaferModel], autoCompleteContext::AutoCompleteContext}

-- | Interactive session interface
runCommandLine :: ClaferIGT IO ()
runCommandLine =
    do
        solve
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
                        else "=== Instance " ++ (show $ 1 + (length $ unsaved context)) ++ " ===\n\n" ++ (show claferModel)
                    nextLoop context{unsaved=claferModel:(unsaved context), currentAlloyInstance=Just xml}
                UnsatCore core counterexample -> do
                    liftIO $ hPutStrLn stderr "No more instances found. Try increasing scope to get more instances."
                    liftIO $ hPutStrLn stderr "The following set of constraints cannot be satisfied in the current scope."
                    liftIO $ hPutStrLn stderr "(Hint: use the setUnsatCoreMinimization command to minimize the set of constraints below)"
                    printConstraints core
                    case counterexample of
                        Just (Counterexample removed claferModel _) -> do
                            liftIO $ hPutStrLn stderr "Altering the following constraints produced the following near-miss example:"
                            printTransformations removed
                            outputStrLn $ show claferModel
                        Nothing -> return ()
                    nextLoop context
                NoInstance -> do
                    liftIO $ hPutStrLn stderr "No more instances found. Try increasing scope to get more instances."
                    nextLoop context
            where           
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

            printConstraint UserConstraint{constraintInfo = info} = show info
            printConstraint constraint = show $ claferInfo constraint

            printConstraints = printConstraints' (1::Integer)
            printConstraints' _ [] = return ()
            printConstraints' i (c:cs) =
                do
                    liftIO $ hPutStrLn stderr $ "  " ++ show i ++ ") " ++ printConstraint c
                    printConstraints' (i + 1) cs

            printTransformations cs = printTransformations' (1::Integer) cs
            printTransformations' _ [] = return ()
            printTransformations' i (c:cs) =
                do
                    let (prnt, rest) = printTransformation c cs
                    liftIO $ hPutStrLn stderr $ "  " ++ show i ++ ") " ++ prnt
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
                "--------------------\n" ++
                "| " ++ claferIGVersion ++ " |\n" ++
                "--------------------\n\n" ++
                "You can invoke the following commands as indicated by single quotes:\n" ++
                "[tab]              - print the available commands\n" ++ 
                "                   - auto-complete command name, a clafer name, or clafer instance name in a given context\n" ++
                "'n'ext, [enter]    - to produce the next instance if available or to output a message that no more \n" ++
                "                     instances exist within the given scope\n" ++
                "'i'ncrease         - to increase the maximum number of instances of a given clafer or all clafers (scope)\n" ++
                "'s'et              - to set the maximum number of instances of a given clafer or all clafers (scope)\n" ++                
                "'m'axint, 'maxint' - to set the bitwidth by providing the largest integer\n" ++                
                "sa'v'e             - to save all instances displayed so far or a counterexample to files named \n" ++
                "                     <model file name>.cfr.<instance number>.data, one instance per file\n" ++
                "'q'uit             - to quit the interactive session\n" ++
                "'r'eload           - to reload your clafer model\n" ++
                "'h'elp             - to display this menu options summary\n" ++
                "'scope'            - to print out the values of the global scope and individual Clafer scopes\n" ++
                "'saveScopes'       - to generate a '<model>.cfr-scope' file with the current scopes\n" ++
                "'loadScopes'       - to load scopes from a '<model>.cfr-scope' file\n" ++                
                "'setUnsatCoreMinimization' - to choose UnSAT core minimization strategy [fastest | medium | best]. Default: fastest\n" ++ 
                "'c', 'claferModel' - to print out the original Clafer model verbatim\n" ++
                "'a', 'alloyModel'  - to print out the output of Clafer translator verbatim\n" ++
                "'alloyInstance'    - to print out the Alloy xml document of the most recent solution\n" ++
                "'f'ind             - to print a Clafer with given name found in the most recent solution\n\n" ++
                "Parameterized command usage:\n" ++
                "'i [enter]'         - to increase for all clafers by 1\n" ++
                "'i <name> [enter]'  - to increase for the clafer <name> by 1\n" ++
                "'i <name> <number>' - to increase for the clafer <name> by <number>\n" ++
                "'s <number> [enter]'- to set for the clafers to <number>\n" ++
                "'s <name> <number>' - to set for the clafer <name> to <number>\n" ++
                "'f <name>'          - to display a clafer <name>\n" ++
                "'setUnsatCoreMinimization fastest' - fastest but the worst\n" ++ 
                "'setUnsatCoreMinimization medium'\n" ++ 
                "'setUnsatCoreMinimization best' - best but slowest even for modest size cores" 
                )
                
            nextLoop context
            
    loop Save context@Context{saved=saved, unsaved=unsaved} =
        do
            save unsaved (toInteger $ length saved)
            nextLoop context{saved=unsaved ++ saved, unsaved=[]}

    loop Reload context =
        do
            oldScopes <- lift getScopes
            oldBw <- lift getBitwidth

            runErrorT $ ErrorT (lift reload) `catchError` (liftIO . mapM_ (hPutStrLn stderr) . printError)
            
            env <- lift getClaferEnv
            let ir = fst3 $ fromJust $ cIr env
            tempScopes <- lift getScopes
            lift $ mergeScopes oldScopes tempScopes

            newScopes <- lift getScopes
            lift $ setBitwidth $ findNecessaryBitwidth ir oldBw $ snd $ unzip newScopes
            newBw <- lift getBitwidth
            printBitwidthWarning newBw
            lift $ solve
            nextLoop context

    loop (IncreaseGlobalScope inc') context =
        do
            globalScope <- lift getGlobalScope
            bitwidth' <- lift getBitwidth
            let newGlobalScope = max 1 $ globalScope+inc'
            lift $ setGlobalScope newGlobalScope
            when (newGlobalScope > allowedMaxInt bitwidth') $ do
                outputStrLn $ "Warning! Requested global scope is larger than maximum allowed by bitwidth ... increasing bitwidth"
                lift $ setBitwidth $ requiredBitwidth newGlobalScope
                newBw <- lift getBitwidth
                printBitwidthWarning newBw

            oldScopes <- lift getScopes
            mapM ( \(sigName', val') -> setAlloyScopeAndBitwidth bitwidth' (max 1 $ val'+inc') (sigToClaferName sigName') sigName') oldScopes
            lift solve    
            outputStrLn ("Global scope changed to " ++ show newGlobalScope)
            nextLoop context
    loop (SetGlobalScope newGlobalScope') context =
        do
            let newGlobalScope = max 1 newGlobalScope'
            bitwidth' <- lift getBitwidth
            lift $ setGlobalScope newGlobalScope
            when (newGlobalScope > allowedMaxInt bitwidth') $ do
                outputStrLn $ "Warning! Requested global scope is larger than maximum allowed by bitwidth ... increasing bitwidth"
                lift $ setBitwidth $ requiredBitwidth newGlobalScope
                newBw <- lift getBitwidth
                printBitwidthWarning newBw
            lift solve    
            outputStrLn ("Global scope set to " ++ show newGlobalScope)
            nextLoop context

    loop (IncreaseScope fqName inc') context =
        do
            try $ do
                qNameMaps' <- lift $ lift $ getQNameMaps
                let uids = getUIDs qNameMaps' fqName
                let sigs = map ("this/" ++) uids
                bitwidth' <- lift $ lift getBitwidth

                lift $ mapM_ (incAlloyScopeAndBitwidth bitwidth' inc' fqName) sigs
                
                lift $ lift $ solve
            nextLoop context

    loop (SetScope qName val') context =
        do
            try $ do
                qNameMaps' <- lift $ lift $ getQNameMaps
                let uids = getUIDs qNameMaps' qName
                let sigs = map ((++) "this/") uids
                bitwidth' <- lift $ lift getBitwidth

                lift $ mapM_ (setAlloyScopeAndBitwidth bitwidth' val' qName) sigs
                
                lift $ lift $ solve
            nextLoop context

    loop (SetBitwidth newBitwidth) context =
        do 
            when (newBitwidth >=4) $ do
                lift $ setBitwidth newBitwidth
                printBitwidthWarning newBitwidth
                lift solve    
                printBitwidthIntRange newBitwidth
            nextLoop context

    loop (SetMaxInt newMaxInt) context =
        do 
            when (newMaxInt >= 7) $ do
                let newBitwidth = requiredBitwidth newMaxInt
                lift $ setBitwidth newBitwidth
                printBitwidthWarning newBitwidth
                lift solve    
                printBitwidthIntRange newBitwidth
            nextLoop context

    loop ShowScopes context =
        do
            globalScope <- lift getGlobalScope
            outputStrLn $ "Global scope = " ++ show globalScope
            originalScopes <- lift getScopes
            -- remove the "this/" prefix
            mapM_ printScope originalScopes
            nextLoop context
            
            where
            printScope (sigName, value)  =
                outputStrLn $ "  " ++ (drop 5 sigName) ++ " scope = " ++ show value
                    
    loop SaveScopes context =
        do
            globalScope <- lift getGlobalScope
            originalScopes <- lift getScopes            
            claferIGArgs' <- lift getClaferIGArgs
            qNameMaps' <- lift getQNameMaps
            let
                globalScopeTuple = ("", globalScope)
                -- remove the "this/" prefix
                uidScopes = globalScopeTuple : (map (\(sigName, value) -> (drop 5 sigName, value)) originalScopes)
                json = generateJSONScopes qNameMaps' uidScopes
                claferModelFile' = claferModelFile claferIGArgs'
                modelName = take (length claferModelFile' - 4) claferModelFile'
                saveName = modelName ++ ".cfr-scope"
            
            liftIO $ writeFile saveName json
            nextLoop context
            
    loop LoadScopes context =
        do
            outputStrLn "Not implemented yet."
            nextLoop context

    loop (Find name) context =
        do
            case (unsaved context ++ saved context) of
                model:_ ->
                    case find ((name ==) . c_name) $ traverse model of
                        Just clafer' -> outputStrLn $ show clafer'
                        Nothing -> outputStrLn $ "\"" ++ name ++ "\" not found in the model."
                []  -> outputStrLn $ "No instance"
            nextLoop context

    loop ShowClaferModel context =
        do
            bitwidth' <- lift getBitwidth
            printBitwidthIntRange bitwidth'
            originalScopes <- lift getScopes
            -- remove the "this/" prefix
            let 
                scopes = map ( \ (uid', val') -> (drop 5 uid', val') ) originalScopes
                scopesMap = Map.fromList scopes

            globalScope' <- lift getGlobalScope
            outputStrLn $ "Global scope  = " ++ show globalScope' ++ "\n"

            env <- lift getClaferEnv 
            constraints' <- lift getConstraints
            AlloyIG.UnsatCore core <- lift $ ClaferIGT $ lift AlloyIG.sendUnsatCoreCommand
            let unSATs = map getConstraintInfo $ catMaybes $ findRemovable env core constraints'

            claferModel <- lift getClaferModel
            let commentLines = getCommentLines claferModel
            lineNumMap <- lift getlineNumMap


            outputStrLn $ editModel claferModel commentLines unSATs lineNumMap scopesMap
            nextLoop context
            where
                editModel :: String -> [Integer] -> [String] -> (Map.Map Integer String) -> (Map.Map String Integer) -> String
                editModel model cLines unSATs lineNumMap scopesMap' = 
                    let 
                        claferLines = lines $ removeCommentsAndUnify model
                        maxLineLength = maximum $ map length claferLines
                    in unlines $ editLines cLines unSATs (numberOfDigits $ length claferLines) maxLineLength scopesMap' lineNumMap (zip [1..] claferLines)

                editLines :: [Integer] -> [String] -> Int -> Int -> (Map.Map String Integer) -> (Map.Map Integer String) -> [(Integer, String)] -> [String]
                editLines _ _ _ _ _ _ [] = []
                editLines cLines unSATs m1 m2 scopesMap' lineNumMap ((num, l):rest) = 
                    if (num `elem` cLines && isEmptyLine l) 
                        then editLines cLines unSATs m1 m2 scopesMap' lineNumMap rest 
                        else (show num ++ "." ++ (replicate (1 + m1 - (numberOfDigits $ fromIntegral num)) ' ') ++ (if (isUnSAT unSATs l num) then "> " else "| ") ++ l ++ (replicate (3 + m2 - (length l)) ' ') ++ (if (isUnSAT unSATs l num) then "<UnSAT " else "|      ") ++ (addScopeVal scopesMap' (Map.lookup num lineNumMap))) 
                        : editLines cLines unSATs m1 m2 scopesMap' lineNumMap rest

                isUnSAT :: [String] -> String -> Integer -> Bool
                isUnSAT us l ln = getAny $ foldMap (\u -> Any (((safehead $ words u) == (safehead $ words l) && (safehead $ reverse $ words u) == (safehead $ reverse $ words l)) || (u `isInfixOf` l) || ("column" `isInfixOf` u && "line" `isInfixOf` u && (init $ head $ tail $ tail $ reverse $ words u) == show ln))) us
                safehead [] = []
                safehead x = head x

                addScopeVal :: (Map.Map String Integer) -> (Maybe String) ->String
                addScopeVal _       Nothing     = ""
                addScopeVal scopesMap' (Just name) = "scope = " ++ (fromJustShow $ Map.lookup name scopesMap') 

                getCommentLines :: String -> [Integer]
                getCommentLines = foldr (\(s, _) acc -> case s of
                    (Span (Pos l1 _) (Pos l2 _)) -> [l1..l2] ++ acc
                    (PosSpan _ (Pos l1 _) (Pos l2 _)) -> [l1..l2] ++ acc
                    (Span (PosPos _ l1 _) (Pos l2 _)) -> [l1..l2] ++ acc --This one and bellow should not happen
                    (Span (Pos l1 _) (PosPos _ l2 _)) -> [l1..l2] ++ acc
                    (Span (PosPos _ l1 _) (PosPos _ l2 _)) -> [l1..l2] ++ acc
                    (PosSpan _ (PosPos _ l1 _) (Pos l2 _)) -> [l1..l2] ++ acc
                    (PosSpan _ (Pos l1 _) (PosPos _ l2 _)) ->  [l1..l2] ++ acc
                    (PosSpan _ (PosPos _ l1 _) (PosPos _ l2 _)) -> [l1..l2] ++ acc) [] . getComments

                getConstraintInfo :: Constraint -> String
                getConstraintInfo (UserConstraint _ info) = show info
                getConstraintInfo x = show $ claferInfo x

    loop ShowAlloyModel context =
        do
            globalScope' <- lift getGlobalScope
            alloyModel <- lift getAlloyModel
            scopes <- lift getScopes

            outputStrLn $ editAlloyModel alloyModel scopes globalScope'
            nextLoop context    
            where
                editAlloyModel :: String -> [(String, Integer)] -> Integer      -> String
                editAlloyModel    model     s                      globalScope' = 
                    let alloyLines = lines $ removeCommentsAndUnify model
                        splitNum = 1 + (length $ takeWhile (not . isEmptyLine) alloyLines)
                        (alloyInfo, alloyLines') = splitAt splitNum $ alloyLines
                    in unlines $ (("global scope = " ++ show globalScope' ++ "\n") :) $ alloyInfo ++ map (\(num, line) -> show num ++ ('.' : (replicate (1 + (numberOfDigits $ length alloyLines) - (numberOfDigits num)) ' ') ++ ('|' : ' ' : line))) (zip [(1 + splitNum)..] (addScopeVals alloyLines' s (maximum $ map length alloyLines')))
                addScopeVals :: [String] -> [(String, Integer)] -> Int -> [String]
                addScopeVals [] _ _ = []
                addScopeVals (l:ls) ss m = 
                    let val = Data.List.lookup (takeWhile (`notElem` [' ','\t','\n']) $ tail $ dropWhile (/='_') l) ss 
                    in if ("sig" `notElem` words l) then ((l ++ (replicate (3 + m - (length l)) ' ') ++ " |") : addScopeVals ls ss m)
                        else (l ++ (replicate (3 + m - (length l)) ' ') ++ " | scope = " ++ (fromJustShow val)) : addScopeVals ls ss m  
   
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
                        Left msg    -> outputStrLn (show msg) >> nextLoop context
                        Right command -> loop command context
    
    save :: MonadIO m => [ClaferModel] -> Integer -> InputT (ClaferIGT m) ()
    save [] _ = return ()
    save (c:cs) counter = do
        claferIGArgs' <- lift $ getClaferIGArgs
        let 
            claferModelFile' = claferModelFile claferIGArgs'
            saveName = claferModelFile' ++ "." ++ (show counter) ++ ".data"
        liftIO $ writeFile saveName (show c)
        outputStrLn $ "Saved to " ++ saveName
        save cs (counter + 1)

try :: MonadIO m => ErrorT String (InputT m) a -> InputT m ()
try e = either outputStrLn (void . return) =<< runErrorT e

incAlloyScopeAndBitwidth :: MonadIO m => Integer -> Integer -> String -> UID -> InputT (ClaferIGT m) ()
incAlloyScopeAndBitwidth                 bitwidth'  inc'       fqName'   sigName' = do
    scopeValue <- lift $ valueOfScope sigName' 
    setAlloyScopeAndBitwidth bitwidth' (max 1 $ scopeValue+inc') fqName' sigName'

setAlloyScopeAndBitwidth :: MonadIO m => Integer -> Integer -> String -> UID -> InputT (ClaferIGT m) ()
setAlloyScopeAndBitwidth                 bitwidth'  newValue'   fqName'   sigName' = do
    when (newValue' > allowedMaxInt bitwidth') $ do
        lift $ setBitwidth $ requiredBitwidth $ newValue'
        newBw <- lift $ getBitwidth
        outputStrLn $ "Warning! Requested scope for " ++ fqName' ++ " is larger than maximum allowed by bitwidth ... increasing bitwidth"
        printBitwidthWarning newBw
    lift $ setAlloyScope newValue' sigName'
    outputStrLn $ "Scope of " ++ fqName' ++ " (" ++ (drop 5 sigName') ++ ") changed to " ++ show newValue'    

mergeScopes :: MonadIO m => [(UID, Integer)] -> [(UID, Integer)] -> ClaferIGT m ()
mergeScopes _ [] = return()
mergeScopes oldScopes newScopes = do
    let mergedScopes = map (pickLargerScope oldScopes) newScopes
    mapM_ (\(uid, val) -> setAlloyScope val uid) mergedScopes

pickLargerScope :: [(String, Integer)] -> (String, Integer) -> (String, Integer)
pickLargerScope    oldScopes              (uid, val)        =
    let 
        oldScopesMap = SMap.fromList oldScopes
        oldVal = SMap.findWithDefault val uid oldScopesMap
    in (uid, max val oldVal)

-- | bitwidth required to store the given integer
requiredBitwidth :: Integer -> Integer
requiredBitwidth    n    = ceiling $ logBase 2 $ (+1) $ (*2) $ intToFloat n

-- | the largest integer allowed by the given bitwidth
allowedMaxInt :: Integer -> Integer
allowedMaxInt    bw       = (2 ^ (bw - 1)) - 1

-- | the smallest integer allowed by the given bitwidth
allowedMinInt :: Integer -> Integer
allowedMinInt    bw       = -((allowedMaxInt bw) + 1)

printBitwidthIntRange :: MonadIO m => Integer -> InputT m ()
printBitwidthIntRange    bw       = outputStrLn $ concat [
    "Integer range = ",
    show $ allowedMinInt bw,
    "..",
    show $ allowedMaxInt bw,
    " (",
    "bitwidth = ", 
    show bw,
    ")" ]

printBitwidthWarning :: MonadIO m => Integer -> InputT m ()
printBitwidthWarning bw = when (bw > 9) $ outputStrLn $ "Warning! This is a very large bitwidth for Alloy, which may cause slow down."

-- i Ali|ce
--     If the cursor is at | then not open. The "ce" prevents autocomplete.
-- i Ali| ce
--     Is open, and autocomplete will kick in.
isOpen :: String -> [Char] -> Bool
isOpen _ [] = True
isOpen _ (x:_) = isSpace x


completeFunc :: MonadIO m => AutoCompleteContext -> CompletionFunc m
completeFunc context (prev, nxt) = 
    if isOpen prev nxt then
        liftIO $ evalComplete context prev
    else
        return (prev, [])


completePrefix :: String -> [String] -> [Completion]
completePrefix prefix choices = map simpleCompletion $ filter (prefix `isPrefixOf`) choices


evalComplete :: AutoCompleteContext -> String -> IO (String, [Completion])
evalComplete context prev = 
    do
        completion <- autoComplete context word auto
        return (reverseRest, completion)
    where
    input = reverse prev
    (reverseWord, reverseRest) = break isSpace prev
    word = reverse reverseWord
    auto = autoCompleteDetect $ parseCommandLineAutoComplete input
        

autoComplete :: AutoCompleteContext -> String -> AutoComplete -> IO [Completion]
autoComplete _ word Auto_Command = return $ completePrefix word commandStrings
autoComplete context word Auto_Clafer =
    do
        c <- readIORef $ clafers context
        return $ completePrefix word c
autoComplete context word Auto_ClaferInstance =
    do
        ci <- readIORef $ claferInstances context
        return $ completePrefix word ci
autoComplete _ word Auto_UnsatCoreMinimization = return $ completePrefix word ["fastest", "medium", "best"]
autoComplete _ _ Auto_Digit = return [] -- Don't auto complete numbers.
autoComplete _ word Auto_Space = return [simpleCompletion $ word]
autoComplete _ _ No_Auto = return []

autoCompleteDetect :: E.ParseError -> AutoComplete
autoCompleteDetect error'
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
    messages = errorMessages error'
    unexpectedMessages = mapMaybe unexpectedMessage messages
    expectedMessages   = mapMaybe expectedMessage messages
    

printError :: ClaferErrs -> [String]    
printError (ClaferErrs errs) =
    map printErr errs
    where
    printErr (ParseErr (ErrPos _ _ (Pos l c)) msg) =
        "Parse error at line " ++ show l ++ ", column " ++ show c ++ ":\n    " ++ msg
    printErr (SemanticErr (ErrPos _ _ (Pos l c)) msg) =
        "Error at line " ++ show l ++ ", column " ++ show c ++ ":\n    " ++ msg
    printErr (ParseErr (ErrPos _ _ (PosPos _ l c)) msg) =
        "Parse error at line " ++ show l ++ ", column " ++ show c ++ ":\n    " ++ msg   --This should never happen
    printErr (SemanticErr (ErrPos _ _ (PosPos _ l c)) msg) =
        "Error at line " ++ show l ++ ", column " ++ show c ++ ":\n    " ++ msg         --This should never happen
    printErr (ClaferErr msg) =
        "Error:\n    " ++ msg

-- only remove the contents while preserving the empty lines
removeCommentsAndUnify :: String -> String
removeCommentsAndUnify [] = []
removeCommentsAndUnify model@[_] = model
removeCommentsAndUnify ('\t':model) = ' ' : ' ' : ' ' : removeCommentsAndUnify model
removeCommentsAndUnify ('/':'/':model) = removeCommentsAndUnify $ dropWhile (/='\n') model
removeCommentsAndUnify ('/':'*':model) = removeBlock model
    where
        -- discard everything until "*/" is found or end of text while preserving new lines
        removeBlock :: String -> String
        removeBlock [] = []
        removeBlock [_] = []
        removeBlock ('\n':model') = '\n' : removeBlock model'
        removeBlock ('*':'/':model') = removeCommentsAndUnify model'
        -- discard contents of the block comment
        removeBlock (_:model') = removeBlock model'
removeCommentsAndUnify (c:model) = c : removeCommentsAndUnify model

isEmptyLine :: String -> Bool
isEmptyLine l = filter (`notElem` [' ', '\n', '\t', '\r']) l == []

numberOfDigits :: Int -> Int
numberOfDigits x = length $ show x

findNecessaryBitwidth :: IModule -> Integer -> [ Integer ] -> Integer
findNecessaryBitwidth ir oldBw scopeValues = 
    if (newBw < oldBw) then oldBw else newBw
    where
        newBw = ceiling $ logBase 2 $ (+1) $ (*2) $ maxInModel ir
        maxInModel :: IModule -> Float
        maxInModel ir' = intToFloat $ max (maximum scopeValues) $ foldIR getMax 0 ir'
        getMax :: Ir -> Integer -> Integer 
        getMax (IRIExp (IInt n)) m = max m $ abs n
        getMax (IRClafer IClafer{card = Just (_, n)}) m = max m n
        getMax _ m = m

intToFloat :: Integer -> Float
intToFloat = fromInteger . toInteger 

fromJustShow :: (Maybe Integer) -> String
fromJustShow (Just x) = show x
fromJustShow Nothing = "Nothing"