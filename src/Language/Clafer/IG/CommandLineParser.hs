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

module Language.Clafer.IG.CommandLineParser (Command(..), UnsatCoreMinimization(..), parseCommandLine, parseCommandLineAutoComplete, commandStrings, expectedMessage, unexpectedMessage, errorMessages) where

import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Prim hiding (try)
import Data.Functor.Identity



data Command = Next | 
               IncreaseGlobalScope Integer | 
               IncreaseScope String Integer | 
               SetGlobalScope Integer | 
               SetScope String Integer | 
               SetBitwidth Integer |
               SetMaxInt Integer | 
               Save | 
               Quit | 
               Reload | 
               Help | 
               Find String | 
               ShowScopes | 
               SaveScopes |
               LoadScopes |
               ShowClaferModel | 
               ShowAlloyModel | 
               ShowAlloyInstance | 
               SetUnsatCoreMinimization UnsatCoreMinimization deriving Show


data UnsatCoreMinimization = Fastest | Medium | Best deriving Show


unexpectedMessage :: Message -> Maybe String
unexpectedMessage (SysUnExpect x) = Just x
unexpectedMessage (UnExpect x) = Just x
unexpectedMessage _ = Nothing

expectedMessage :: Message -> Maybe String
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
                hint _ = ", did you mean " ++ intercalate " or " (map quote didYouMean) ++ "?"
            

command :: Parser String
command = many1 (letter <?> "command")


-- Used by the autocomplete algorithm
commandStrings :: [String]
commandStrings = map fst commandMap


commandMap :: [(String, Parser Command)]
commandMap =
    ("h", helpCommand):
    ("i", increaseCommand):
    ("s", setCommand):
    ("b", setBitwidthCommand):
    ("m", setMaxIntCommand):
    ("maxint", setMaxIntCommand):
    ("n", nextCommand):
    ("f", findCommand):
    ("q", quitCommand):
    ("v", saveCommand):
    ("r", reloadCommand):
    ("scope", showScopesCommand):
    ("saveScopes", saveScopesCommand):
    ("loadScopes", loadScopesCommand):
    ("claferModel", claferModelCommand):
    ("c", claferModelCommand):
    ("alloyModel", alloyModelCommand):
    ("a", alloyModelCommand):
    ("alloyInstance", alloyInstanceCommand):
    ("setUnsatCoreMinimization", setUnsatCoreMinimization):
    []


helpCommand :: ParsecT String () Identity Command
helpCommand              = return Help
increaseCommand :: Parser Command
increaseCommand          = increaseGlobalScope
setCommand :: Parser Command
setCommand               = setGlobalScope
setBitwidthCommand :: Parser Command
setBitwidthCommand       = setBitwidth
setMaxIntCommand :: Parser Command
setMaxIntCommand       = setMaxInt
nextCommand :: ParsecT String () Identity Command
nextCommand              = return Next
quitCommand :: ParsecT String () Identity Command
quitCommand              = return Quit
saveCommand :: ParsecT String () Identity Command
saveCommand              = return Save
reloadCommand :: ParsecT String () Identity Command
reloadCommand            = return Reload
findCommand :: ParsecT String () Identity Command
findCommand              = Find `liftM` (gap >> claferInstance)
showScopesCommand :: ParsecT String () Identity Command
showScopesCommand             = return ShowScopes
saveScopesCommand :: ParsecT String () Identity Command
saveScopesCommand        = return SaveScopes
loadScopesCommand :: ParsecT String () Identity Command
loadScopesCommand        = return LoadScopes
claferModelCommand :: ParsecT String () Identity Command
claferModelCommand       = return ShowClaferModel
alloyModelCommand :: ParsecT String () Identity Command
alloyModelCommand        = return ShowAlloyModel
alloyInstanceCommand :: ParsecT String () Identity Command
alloyInstanceCommand     = return ShowAlloyInstance
setUnsatCoreMinimization :: ParsecT String () Identity Command
setUnsatCoreMinimization = SetUnsatCoreMinimization `liftM` (gap >> unsatCoreMinimization)

gap :: ParsecT String () Identity ()
gap = skipMany1 space


increaseGlobalScope :: Parser Command
increaseGlobalScope =
    do
        try (gap >> explicitIncreaseGlobalScope)
    <|>
    return (IncreaseGlobalScope 1)

setGlobalScope :: Parser Command
setGlobalScope =
    do
        try (gap >> explicitSetGlobalScope) 
    <|>
    do
        try (gap >> explicitSetScope)

explicitIncreaseGlobalScope :: Parser Command
explicitIncreaseGlobalScope =
    fmap IncreaseGlobalScope signedNumber
    <|>
    increaseScope

explicitSetGlobalScope :: Parser Command
explicitSetGlobalScope = fmap SetGlobalScope number
    
increaseScope :: Parser Command
increaseScope = 
    do
        name <- clafer
        do
            try (gap >> explicitIncreaseScope name)
            <|>
            return (IncreaseScope name 1)
            
explicitIncreaseScope :: String -> Parser Command
explicitIncreaseScope name = fmap (IncreaseScope name) signedNumber

explicitSetScope :: Parser Command
explicitSetScope =
    do
        name <- clafer
        gap
        i <- number
        return (SetScope name i)

setBitwidth :: Parser Command
setBitwidth =
    do
        gap
        b <- number
        return $ SetBitwidth b

setMaxInt :: Parser Command
setMaxInt =
    do
        gap
        b <- number
        return $ SetMaxInt b

number :: Parser Integer
number = do 
            n <- many1 digit
            return $ read n

signedNumber :: Parser Integer
signedNumber = do
    s <- option "" $ string "-"
    n <- many1 digit
    return $ read $ s ++ n

clafer :: Parser String        
clafer = many1 ((alphaNum <|> char ':' <|> char '_') <?> "clafer")


claferInstance :: Parser String
claferInstance = many1 (alphaNum <?> "claferInstance")


unsatCoreMinimization :: Parser UnsatCoreMinimization
unsatCoreMinimization =
    do
        level <- many1 (letter <?> "Unsat core minimization level")
        case level of
            "fastest" -> return Fastest
            "medium"  -> return Medium
            "best"    -> return Best
            x         -> fail $ x ++ " is not a valid unsat core minimization level"
