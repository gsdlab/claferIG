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
import Solution
import System.Cmd
import System.Console.CmdArgs
import System.Environment.Executable
import System.Exit
import System.IO
import System.IO.Error
import System.Process


data Process = Process{stdIn::Handle, stdOut::Handle, procHandle::ProcessHandle}


data IGArgs = IGArgs {
    claferFile :: FilePath
} deriving (Show, Data, Typeable)


data Command = Next | Save | Quit


isNext Next = True
isNext _ = False

isSave Save = True
isSave _ = False

isQuit Quit = True
isQuit _ = False


claferIG = IGArgs {
    claferFile = def &= argPos 0 &= typ "FILE"
} &= summary "ClaferIG v0.0.1"


-- Read the length, then the string
getMessage :: Process -> IO String
getMessage process =
    do
        length <- read `liftM` hGetLine (stdOut process)
        mapM hGetChar $ replicate length (stdOut process)
        

-- Put the length, then the string
putMessage :: Process -> String -> IO ()
putMessage process message =
    do
        hPutStrLn (stdIn process) (show $ length message)
        hPutStr (stdIn process) message
        hFlush (stdIn process)


-- Start another process and return the piped std_in, std_out stream
pipeCommand :: FilePath -> [String] -> IO Process
pipeCommand exec args =
    do
        (stdIn, stdOut, _, procHandle) <- createProcess process
        return $ Process (fromJust stdIn) (fromJust stdOut) procHandle -- Pipe always has a handle according to docs
    where process = (proc exec args) { std_in = CreatePipe, std_out = CreatePipe }
    

beginInterface :: FilePath -> Process -> IO ()
beginInterface file proc =
    topLevelInterface Next [] []
    where
        topLevelInterface :: Command -> [ClaferModel] -> [ClaferModel] -> IO ()
        topLevelInterface Next saved unsaved =
            do
                answer <- communicateCommand Next proc
                case answer of
                    Just model -> do
                        putStrLn $ show model
                        nextInterface saved (model:unsaved)
                    Nothing -> do
                        putStrLn "No more instances found"
                        nextInterface saved unsaved
        topLevelInterface Save saved unsaved =
            do
                communicateCommand Save proc
                save unsaved (length saved)
                nextInterface (unsaved ++ saved) []
        topLevelInterface Quit _ _ =
            do
                communicateCommand Quit proc
                return ()
                
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
            where saveName = file ++ (show counter) ++ ".data"
                
        
-- Sends the command to the alloyIG subprocess
communicateCommand :: Command -> Process -> IO (Maybe ClaferModel)
communicateCommand Next proc =
    do
        putMessage proc "n"
        status <- read `liftM` getMessage proc
        case status of
            True -> do
                xml <- getMessage proc
                let solution = parseSolution xml
                let claferModel = buildClaferModel solution
                return $ Just claferModel
            False -> return Nothing
communicateCommand Quit proc =
    do
        putMessage proc "q"
        return Nothing
communicateCommand Save _ = return Nothing


-- Retrieve the next user input command
nextCommand :: IO Command
nextCommand =
    do
        putStr "n,q,s>"
        hFlush stdout
        op <- try getLine
        case op of
            -- User submitted eof. Quit process.
            Left e -> if isEOFError e then return Quit else ioError e
            Right "n" -> return Next
            Right "q" -> return Quit
            Right "s" -> return Save
            _ -> putStrLn "Unknown command" >> nextCommand


main =
    do
        (execDir, _) <- splitExecutablePath
        args <- cmdArgs claferIG

        
        claferProc <- pipeCommand (execDir ++ "clafer") ["-o", "-s", claferFile args]
        claferOutput <- hGetContents $ stdOut claferProc
        claferExit <- waitForProcess (procHandle claferProc)
        when (claferExit /= ExitSuccess) (print "clafer unexpectedly terminated" >> exitWith claferExit)
        
        
        alloyIG <- pipeCommand "java" ["-jar", execDir ++ "alloyIG.jar"]
        putMessage alloyIG claferOutput


        beginInterface (claferFile args) alloyIG
        
