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

import AlloyIGInterface
import ClaferModel
import CommandLineParser
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Solution
import Sugarer
import System.Console.Haskeline
import Version


data AutoComplete = Auto_Command | Auto_Clafer | Auto_ClaferInstance | Auto_Space | Auto_Digit | No_Auto deriving Show

data AutoCompleteContext = AutoCompleteContext {clafers::IORef [String], claferInstances::IORef [String]}

data Context = Context {saved::[ClaferModel], unsaved::[ClaferModel], autoCompleteContext::AutoCompleteContext}



claferIGVersion =
    "ClaferIG v" ++ version


runCommandLine :: FilePath -> AlloyIG -> IO ()
runCommandLine filePath alloyIG =
    do
        clafers <- newIORef $ map sigToClaferName (sigs alloyIG)
        claferInstances <- newIORef []
        let autoCompleteContext = AutoCompleteContext clafers claferInstances
        runInputT Settings {
            complete = completeFunc autoCompleteContext,
            historyFile = Nothing,
            autoAddHistory = True
        } $ loop Next (Context [] [] autoCompleteContext)
    where 
    loop :: Command -> Context -> InputT IO ()
    
    loop Quit _ = lift $ sendQuitCommand alloyIG
    
    loop Next context =
        do
            xmlSolution <- lift $ sendNextCommand alloyIG
            case xmlSolution of
                Just xml -> do
                    let solution = parseSolution xml
                    let claferModel = buildClaferModel solution
                    let sugarModel = sugarClaferModel claferModel
                    
                    lift $ writeIORef (claferInstances $ autoCompleteContext context) $ map c_name (traverse sugarModel)
                    
                    outputStrLn $ show sugarModel
                    nextLoop context{unsaved=sugarModel:(unsaved context)}
                Nothing -> do
                    outputStrLn "No more instances found. Try increasing scope to get more instances."
                    nextLoop context
                    
    loop Help context =
        do
            outputStrLn (
                "---------\n" ++
                "| Usage |\n" ++
                "---------\n\n" ++
                "You can invoke the following commands by pressing the first letter of the command name:\n" ++
                "next     - to produce the next instance if available or to output a message that no more \n" ++
                "           instances exist within the given scope\n" ++
                "increase - to increase the maximum number of instances of a given clafer or all clafers (scope)\n" ++
                "save     - to save all instances displayed so far or a counterexample to files named \n" ++
                "           <model file name>.cfr.<instance number>.data, one instance per file\n" ++
                "quit     - to quit the interactive session\n" ++
                "help     - to display this menu options summary\n" ++
                "version  - to display the version (including build date)\n")
            nextLoop context
            
    loop Version context =
        do
            outputStrLn claferIGVersion
            nextLoop context
            
    loop Save context@Context{saved=saved, unsaved=unsaved} =
        do
            lift $ save unsaved (length saved)
            nextLoop context{saved=unsaved ++ saved, unsaved=[]}
                    
    loop (IncreaseGlobalScope i) context =
        do
            globalScope <- lift $ getGlobalScope alloyIG
            let globalScope' = globalScope + i

            lift $ sendSetGlobalScopeCommand globalScope' alloyIG
            scopes <- lift $ getScopes alloyIG
            let scopes' = map (\(x, y)->(x, y + i)) scopes
            lift $ mapM (apply alloyIG) (map (uncurry sendSetScopeCommand) scopes')
            lift $ sendResolveCommand alloyIG
            
            outputStrLn ("Global scope increased to " ++ show globalScope')
            nextLoop context
            
    loop (IncreaseScope name i) context =
        do
            case Map.lookup name claferToSigNameMap of
                Just sig ->
                    do
                        scope <- lift $ getScope sig alloyIG
                        let scope' = scope + i
                        lift $ sendSetScopeCommand sig scope' alloyIG
                        lift $ sendResolveCommand alloyIG
                        outputStrLn ("Scope of " ++ name ++ " increased to " ++ show scope')
                Nothing -> outputStrLn ("Unknown clafer " ++ name)
            nextLoop context
            
    loop ShowScope context =
        do
            globalScope <- lift $ getGlobalScope alloyIG
            outputStrLn $ "Global scope = " ++ show globalScope
            scopes <- lift $ getScopes alloyIG
            mapM_ (\(name, scope) -> outputStrLn $ "  " ++ sigToClaferName name ++ " scope = " ++ show scope) scopes
            nextLoop context

    loop (Find name) context =
        do
            case (unsaved context ++ saved context) of
                model:_ ->
                    case find ((name ==) . c_name) $ traverse model of
                        Just clafer -> outputStrLn $ show clafer
                        Nothing -> outputStrLn $ "\"" ++ name ++ "\" not found in the model."
                []  -> outputStrLn $ "No instance"
            nextLoop context
            
    loop ShowAlloyModel context = outputStrLn (alloyModel alloyIG) >> nextLoop context

    nextLoop context =
        do
            minput <- getInputLine "claferIG> "
            case minput of
                Nothing -> loop Quit context
                Just input ->
                    case parseCommandLine input of
                        Left error    -> outputStrLn (show error) >> nextLoop context
                        Right command -> loop command context
    
    save :: [ClaferModel] -> Int -> IO ()
    save [] _ = return ()
    save (c:cs) counter =
        do
            writeFile saveName (show c)
            putStrLn $ "Saved to " ++ saveName
            save cs (counter + 1)
        where saveName = filePath ++ "." ++ (show counter) ++ ".data"
    
    apply x y = y x
    
    claferToSigNameMap = Map.fromListWithKey (error . ("Duplicate clafer name " ++)) [(sigToClaferName x, x) | x <- (sigs alloyIG)]




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
