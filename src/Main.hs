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

import Control.Monad
import Data.Maybe
import System.Cmd
import System.Console.CmdArgs
import System.Environment.Executable
import System.Exit
import System.IO
import System.Process


data Process = Process{stdIn::Handle, stdOut::Handle, procHandle::ProcessHandle}


data IGArgs = IGArgs {
    claferFile :: FilePath
} deriving (Show, Data, Typeable)


data Command = Next | Save [String] | Quit


claferIG = IGArgs {
    claferFile = def &= argPos 0 &= typ "FILE"
} &= summary "ClaferIG v0.0.1"


-- Read the length, then the string
hGetMessage :: Handle -> IO String
hGetMessage handle =
    do
        length <- read `liftM` hGetLine handle
        foldr (liftM2 (:)) (return []) (replicate length $ hGetChar handle)
        

-- Put the length, then the string
hPutMessage :: Handle -> String -> IO ()
hPutMessage handle message =
    do
        hPutStrLn handle (show $ length message)
        hPutStr handle message
        hFlush handle


-- Start another process and return the piped std_in, std_out stream
pipeCommand :: FilePath -> [String] -> IO Process
pipeCommand exec args =
    do
        (stdIn, stdOut, _, procHandle) <- createProcess process
        return $ Process (fromJust stdIn) (fromJust stdOut) procHandle -- Pipe always has a handle according to docs
    where process = (proc exec args) { std_in = CreatePipe, std_out = CreatePipe }
    

collect :: Command -> String -> Command
collect (Save collection) item = Save $ item:collection
collect x _ = x


beginInterface :: FilePath -> Process -> IO ()
beginInterface file process =
    do
        answers <- interface Next process
        topLevelInterface file process 1 answers
    where
        -- Responsible for performing saves
        topLevelInterface :: FilePath -> Process -> Int -> Command -> IO ()
        topLevelInterface file process counter (Save answers) =
            do
                save answers file counter
                nextInterace process >>= topLevelInterface file process (counter + (length answers))
        topLevelInterface _ _ _ _ = return ()
        
        save :: [String] -> FilePath -> Int -> IO ()
        save [] _ _ = return ()
        save (x:xs) file counter =
            do
                writeFile saveName x
                putStrLn $ "Saved to " ++ saveName
                save xs file (counter + 1)
            where saveName = file ++ (show counter) ++ ".data"
                
        
interface :: Command -> Process -> IO Command
interface Next proc@(Process stdIn stdOut _) =
    do
        hPutMessage stdIn "n"
        status <- read `liftM` hGetMessage stdOut
        case status of
            True -> do
                xml <- hGetMessage stdOut
                putStrLn xml
                answers <- nextInterace proc
                return $ collect answers xml
            False -> do
                putStrLn "No more instances found"
                nextInterace proc
interface Quit proc =
    do
        hPutMessage (stdIn proc) "q"
        return Quit
interface x _ = return x

        
nextInterace :: Process -> IO Command
nextInterace process =
    do
        putStr "n,q,s>"
        hFlush stdout
        op <- getLine
        case op of
            "n" -> interface Next process
            "q" -> interface Quit process
            "s" -> interface (Save []) process
            _ -> putStrLn "Unknown command" >> nextInterace process


main =
    do
        (execDir, _) <- splitExecutablePath
        args <- cmdArgs claferIG

        
        claferProc <- pipeCommand (execDir ++ "clafer") ["-o", "-s", claferFile args]
        claferOutput <- hGetContents $ stdOut claferProc
        claferExit <- waitForProcess (procHandle claferProc)
        when (claferExit /= ExitSuccess) (print "clafer unexpectedly terminated" >> exitWith claferExit)
        
        
        alloyIG <- pipeCommand "java" ["-jar", execDir ++ "alloyIG.jar"]
        hPutMessage (stdIn alloyIG) claferOutput


        beginInterface (claferFile args) alloyIG
