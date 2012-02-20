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

module CommandLineParser (Command(..), parseCommandLine, parseCommandLineAutoComplete, expectedMessage, unexpectedMessage, errorMessages) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error


data Command = Next | IncreaseGlobalScope Int | IncreaseScope String Int | Save | Quit | Help | Version deriving Show



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
commandLine = choice [helpCommand, increaseCommand, nextCommand, quitCommand, saveCommand, versionCommand]


helpCommand     = command 'h' >> return Help
increaseCommand = command 'i' >> increaseGlobalScope
nextCommand     = command 'n' >> return Next
quitCommand     = command 'q' >> return Quit
saveCommand     = command 's' >> return Save
versionCommand  = command 'v' >> return Version


command :: Char -> Parser Char
command c = (char c) <?> "command"


increaseGlobalScope :: Parser Command
increaseGlobalScope =
    do
        try (skipMany1 space >> explicitIncreaseGlobalScope)
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
            try (skipMany1 space >> explicitIncreaseScope name)
            <|>
            return (IncreaseScope name 1)
            
        
explicitIncreaseScope :: String -> Parser Command
explicitIncreaseScope name =
    do
        i <- many1 digit
        return $ IncreaseScope name (read i)


clafer :: Parser String        
clafer =
    do
        name <- many1 (letter <?> "clafer")
        return name
