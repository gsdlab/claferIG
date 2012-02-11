module Main where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Cmd
import System.Console.CmdArgs
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


-- Read a string of a certain length from a handle.
hGetString :: Handle -> Int -> IO String
hGetString handle size = foldr (liftM2 (:)) (return []) (replicate size $ hGetChar handle)


-- Read the length, then the string
hGetMessage :: Handle -> IO String
hGetMessage handle =
    do
        length <- read `liftM` hGetLine handle
        hGetString handle length
        

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
                nextInterace process >>= (topLevelInterface file process (counter + (length answers)))
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
interface Next process@(Process stdIn stdOut _) =
    do
        hPutStrLn stdIn "n"
        hFlush stdIn
        status <- read `liftM` hGetLine stdOut
        case status of
            True -> do
                xml <- hGetMessage stdOut
                putStrLn xml
                answers <- nextInterace process
                return $ collect answers xml
            False -> do
                putStrLn "No more instances found"
                nextInterace process
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
        args <- cmdArgs claferIG

        
        claferProc <- pipeCommand "./clafer" ["-o", "-s", claferFile args]
        claferOutput <- hGetContents $ stdOut claferProc
        claferExit <- waitForProcess (procHandle claferProc)
        when (claferExit /= ExitSuccess) (print "clafer unexpectedly terminated" >> exitWith claferExit)
        
        
        alloyIG <- pipeCommand "java" ["-jar", "alloyIG.jar"]
        hPutMessage (stdIn alloyIG) claferOutput


        beginInterface (claferFile args) alloyIG
