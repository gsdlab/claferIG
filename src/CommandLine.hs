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

module CommandLine where

import AlloyIGInterface
import ClaferModel
import CommandLineParser
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Maybe
import Sugarer
import System.Console.Haskeline
import Version


data AutoComplete = Auto_Command | Auto_Clafer | Auto_Space | Auto_Digit | No_Auto deriving Show

data Context = Context {saved::[ClaferModel], unsaved::[ClaferModel]} deriving Show



claferIGVersion =
    "ClaferIG v" ++ version


runCommandLine :: FilePath -> AlloyIG -> IO ()
runCommandLine filePath alloyIG =
    runInputT settings $ loop Next (Context [] [])
    where 
    loop :: Command -> Context -> InputT IO ()
    
    loop Quit _ = lift $ sendQuitCommand alloyIG
    
    loop Next context =
        do
            solution <- lift $ sendNextCommand alloyIG
            case solution of
                Just s -> do
                    let claferModel = buildClaferModel s
                    let sugarModel = sugarClaferModel claferModel
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
            outputStrLn ("Global scope increased to " ++ show globalScope')
            nextLoop context
            
    loop (IncreaseScope _ i) context =
        do
            outputStrLn ("sc:" ++ show i)
            nextLoop context

    nextLoop context =
        do
            minput <- getInputLine "n, i, s, q, h>"
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
    
    settings =
        Settings {
            complete = completeFunc (map sigToClaferName $ sigs alloyIG),
            historyFile = Nothing,
            autoAddHistory = True
        }


-- i Ali|ce
--     If the cursor is at | then not open. The "ce" prevents autocomplete.
-- i Ali| ce
--     Is open, and autocomplete will kick in.
isOpen prev [] = True
isOpen prev (x:_) = isSpace x


completeFunc :: Monad m => [String] -> CompletionFunc m
completeFunc clafers (prev, next) = 
    if isOpen prev next then
        return $ evalComplete clafers prev next
    else
        return (prev, [])


completePrefix prefix choices = map simpleCompletion $ filter (prefix `isPrefixOf`) choices


evalComplete clafers prev next = 
    (reverseRest, autoComplete clafers word auto)
    where
    input = reverse prev
    (reverseWord, reverseRest) = break isSpace prev
    word = reverse reverseWord
    auto = autoCompleteDetect $ parseCommandLineAutoComplete input
        

autoComplete :: [String] -> String -> AutoComplete -> [Completion]
autoComplete clafers word Auto_Command = completePrefix word ["n", "i", "s", "q", "h", "v"]
autoComplete clafers word Auto_Clafer = completePrefix word clafers
autoComplete clafers word Auto_Digit = [] -- Don't auto complete numbers.
autoComplete clafers word Auto_Space = [simpleCompletion $ word]
autoComplete clafers word No_Auto = []


autoCompleteDetect error
    -- An unexpected message means that parsing failed before eof
    | any (not . null) unexpectedMessages = No_Auto
    | any (== "command") expectedMessages = Auto_Command
    | any (== "clafer") expectedMessages = Auto_Clafer
    | any (== "digit") expectedMessages = Auto_Digit
    | any (== "space") expectedMessages = Auto_Space
    | otherwise = No_Auto
    where
    messages = errorMessages error
    unexpectedMessages = mapMaybe unexpectedMessage messages
    expectedMessages   = mapMaybe expectedMessage messages
