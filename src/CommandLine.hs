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
        
        clafers <- newIORef $ getClafers claferIG
        claferInstances <- newIORef []

        let autoCompleteContext = AutoCompleteContext clafers claferInstances
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
            solution <- lift $ nextWithAlloyInstance claferIG
            case solution of
                Just (xml, claferModel) -> do
                    lift $ writeIORef (claferInstances $ autoCompleteContext context) $ map c_name (traverse claferModel)
                    
                    outputStrLn $ show claferModel
                    nextLoop context{unsaved=claferModel:(unsaved context), currentAlloyInstance=Just xml}
                Nothing -> do
                    outputStrLn "No more instances found. Try increasing scope to get more instances."
                    nextLoop context
                    
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
            lift $ save unsaved (length saved)
            nextLoop context{saved=unsaved ++ saved, unsaved=[]}
                    
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
            case getScope name claferIG of
                Just scope ->
                    do
                        lift $ increaseScope i scope
                        scope' <- lift $ valueOfScope scope
                        lift $ solve claferIG
                        outputStrLn ("Scope of " ++ name ++ " increased to " ++ show scope')
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

    loop ShowClaferModel context = outputStrLn (claferModel claferIG) >> nextLoop context
            
    loop ShowAlloyModel context = outputStrLn (alloyModel claferIG) >> nextLoop context

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
    
    save :: [ClaferModel] -> Int -> IO ()
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
