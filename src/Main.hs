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

module Main where

import ClaferModel
import Control.Monad
import Data.Maybe
import Process
import Solution
import Sugarer
import System.Cmd
import System.Console.CmdArgs
import System.Environment.Executable
import System.Exit
import System.IO
import System.IO.Error

import Version

data IGArgs = IGArgs {
    claferFile :: FilePath
} deriving (Show, Data, Typeable)


data Command = Next | Increase | Save | Quit | Help


claferIGVersion =
    "ClaferIG v" ++ version

claferIG = IGArgs {
    claferFile = def &= argPos 0 &= typ "FILE"
} &= summary claferIGVersion


beginInterface :: FilePath -> Process -> IO ()
beginInterface file proc =
    topLevelInterface Next [] []
    where
        topLevelInterface :: Command -> [ClaferModel] -> [ClaferModel] -> IO ()
        topLevelInterface Next saved unsaved =
            do
                answer <- sendNextCommand proc
                case answer of
                    Just model -> do
                        putStrLn $ show model
                        nextInterface saved (model:unsaved)
                    Nothing -> do
                        putStrLn "No more instances found. Try increasing scope to get more instances."
                        nextInterface saved unsaved

        topLevelInterface Save saved unsaved =
            do
                save unsaved (length saved)
                nextInterface (unsaved ++ saved) []
                
        topLevelInterface Quit _ _ =
            do
                sendQuitCommand proc
               
        topLevelInterface Increase saved unsaved =
            do
                newScope <- sendIncreaseGlobalScopeCommand 1 proc
                putStrLn $ "Global scope increased to " ++ show newScope
                nextInterface saved unsaved
               
        topLevelInterface Help saved unsaved =
            do
                putStrLn (
                    "-----------------------------\n" ++
                    "| " ++ claferIGVersion ++ " |\n" ++
                    "-----------------------------\n\n" ++
                    "You can invoke the following commands by pressing the first letter of the command name:\n" ++
                    "next     - to produce the next instance if available or to output a message that no more \n" ++
                    "           instances exist within the given scope\n" ++
                    "increase - to increase the maximum number of instances of a given clafer or all clafers (scope)\n" ++
                    "save     - to save all instances displayed so far or a counterexample to files named \n" ++
                    "           <model file name>.cfr.<instance number>.data, one instance per file\n" ++
                    "quit     - to quit the interactive session\n" ++
                    "help     - to display this menu options summary\n")
                nextInterface saved unsaved
 
        nextInterface :: [ClaferModel] -> [ClaferModel] -> IO ()
        nextInterface saved unsaved =
            do
                next <- nextCommand
                topLevelInterface next saved unsaved
                
        save :: [ClaferModel] -> Int -> IO ()
        save [] _ = return ()
        save (c:cs) counter =
            do
                writeFile saveName (show c)
                putStrLn $ "Saved to " ++ saveName
                save cs (counter + 1)
            where saveName = file ++ "." ++ (show counter) ++ ".data"
                

-- Get a list of all the sigs from alloyIG
sendSigCommand :: Process -> IO [String]
sendSigCommand proc =
    do
        putMessage proc "s"
        numberOfSigs <- read `liftM` getMessage proc
        mapM getMessage (replicate numberOfSigs proc)


-- Get the next solution from alloyIG
sendNextCommand :: Process -> IO (Maybe ClaferModel)
sendNextCommand proc =
    do
        putMessage proc "n"
        status <- read `liftM` getMessage proc
        case status of
            True -> do
                xml <- getMessage proc
                let solution = parseSolution xml
                let claferModel = buildClaferModel solution
                let sugarModel = sugarClaferModel claferModel
                return $ Just sugarModel
            False -> return Nothing


-- Tell alloyIG to increase the scope
sendIncreaseGlobalScopeCommand :: Int -> Process -> IO Int
sendIncreaseGlobalScopeCommand increment proc =
    do
        putMessage proc "i"
        putMessage proc (show increment)
        newScope <- getMessage proc
        return $ read newScope


-- Tell alloyIG to quit
sendQuitCommand :: Process -> IO ()
sendQuitCommand proc = putMessage proc "q"


-- Retrieve the next user input command
nextCommand :: IO Command
nextCommand =
    do
        putStr "n, i, s, q, h>"
        hFlush stdout
        op <- try getLine
        case op of
            -- User submitted eof. Quit process.
            Left e -> if isEOFError e then return Quit else ioError e
            Right "" -> return Next
            Right "n" -> return Next
            Right "i" -> return Increase
            Right "s" -> return Save
            Right "q" -> return Quit
            Right "h" -> return Help

            _ -> putStrLn "Unknown command" >> nextCommand


main =
    do
        (execDir, _) <- splitExecutablePath
        args <- cmdArgs claferIG

        
        claferProc <- pipeProcess (execDir ++ "clafer") ["-o", "-s", claferFile args]
        claferOutput <- getContentsVerbatim claferProc
        claferExit <- waitFor claferProc
        when (claferExit /= ExitSuccess) (print "clafer unexpectedly terminated" >> exitWith claferExit)
        
        
        alloyIG <- pipeProcess "java" ["-jar", execDir ++ "alloyIG.jar"]
        putMessage alloyIG claferOutput


        beginInterface (claferFile args) alloyIG
        
