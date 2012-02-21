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

module CommandLineParser (Command(..), parseCommandLine, parseCommandLineAutoComplete, commandStrings, expectedMessage, unexpectedMessage, errorMessages) where

import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error


data Command = Next | IncreaseGlobalScope Int | IncreaseScope String Int | Save | Quit | Help | Find String | ShowScope | ShowClaferModel | ShowAlloyModel | ShowAlloyInstance deriving Show



unexpectedMessage (SysUnExpect x) = Just x
unexpectedMessage (UnExpect x) = Just x
unexpectedMessage _ = Nothing


expectedMessage (Expect x) = Just x
expectedMessage _ = Nothing


parseCommandLine :: String -> Either ParseError Command
parseCommandLine input =
    if null input then Right Next
    else parse doParse "claferIG" input
    where
    doParse =
        do
            skipMany (space <?> "")
            c <- commandLine
            skipMany (space <?> "")
            eof
            return c


-- This function uses the expected/unexpected messages to understand what to autocomplete.
-- Any unexpected character means parse did not reach the end of the input, hence cannot autocomplete.
parseCommandLineAutoComplete :: String -> ParseError
parseCommandLineAutoComplete input =
    case parse doParse "claferIG autocomplete" input of
        Left e  -> e
        Right _ -> error "Failed at failing."
    where
    doParse =
        do
            skipMany (space <?> "")
            commandLine
            -- Force the parse to fail to gather expected/unexpected messages
            fail "reached end"


commandLine :: Parser Command
commandLine = 
    do
        name <- command
        case lookup name commandMap of
            Just x  -> x
            Nothing ->
                fail $ "Unknown command \"" ++ name ++ "\"" ++ hint didYouMean
                where
                quote x = '"' : x ++ "\""
                didYouMean = filter (name `isPrefixOf`) commandStrings
                hint [] = ""
                hint hints = ", did you mean " ++ intercalate " or " (map quote didYouMean) ++ "?"
            

command :: Parser String
command = many1 (letter <?> "command")


-- Used by the autocomplete algorithm
commandStrings :: [String]
commandStrings = map fst commandMap


commandMap :: [(String, Parser Command)]
commandMap =
    ("h", helpCommand):
    ("i", increaseCommand):
    ("n", nextCommand):
    ("f", findCommand):
    ("q", quitCommand):
    ("s", saveCommand):
    ("scope", scopeCommand):
    ("claferModel", claferModelCommand):
    ("alloyModel", alloyModelCommand):
    ("alloyInstance", alloyInstanceCommand):
    []


helpCommand          = return Help
increaseCommand      = increaseGlobalScope
nextCommand          = return Next
quitCommand          = return Quit
saveCommand          = return Save
findCommand          = Find `liftM` (gap >> claferInstance)
scopeCommand         = return ShowScope
claferModelCommand   = return ShowClaferModel
alloyModelCommand    = return ShowAlloyModel
alloyInstanceCommand = return ShowAlloyInstance


gap = skipMany1 space


increaseGlobalScope :: Parser Command
increaseGlobalScope =
    do
        try (gap >> explicitIncreaseGlobalScope)
        <|>
        return (IncreaseGlobalScope 1)


explicitIncreaseGlobalScope :: Parser Command
explicitIncreaseGlobalScope =
    do
        i <- many1 digit
        return $ IncreaseGlobalScope (read i)
    <|>
    increaseScope

    
increaseScope :: Parser Command
increaseScope = 
    do
        name <- clafer
        do
            try (gap >> explicitIncreaseScope name)
            <|>
            return (IncreaseScope name 1)
            
        
explicitIncreaseScope :: String -> Parser Command
explicitIncreaseScope name =
    do
        i <- many1 digit
        return $ IncreaseScope name (read i)


clafer :: Parser String        
clafer = many1 (letter <?> "clafer")


claferInstance :: Parser String
claferInstance = many1 (alphaNum <?> "claferInstance")
