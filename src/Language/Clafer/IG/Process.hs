{-
 Copyright (C) 2012-2013 Jimmy Liang <http://gsd.uwaterloo.ca>

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

module Language.Clafer.IG.Process (Process, executableDirectory, waitFor, getContentsVerbatim, getMessage, readMessage, putMessage, pipeProcess) where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment.Executable
import System.IO
import System.Process

data Process = Process{stdIn::Handle, stdOut::Handle, procHandle::ProcessHandle}



executableDirectory :: IO FilePath
executableDirectory = fst `liftM` splitExecutablePath


-- Start another process and return the piped std_in, std_out stream
pipeProcess :: FilePath -> [String] -> IO Process
pipeProcess exec args =
    do
        let process = (proc exec args) { std_in = CreatePipe, std_out = CreatePipe }
        (Just stdIn, Just stdOut, _, procHandle) <- createProcess process
        hSetNewlineMode stdIn noNewlineTranslation
        return $ Process stdIn stdOut procHandle -- Pipe always has a handle according to docs
    
    
-- Wait until the process terminates
waitFor proc = waitForProcess (procHandle proc)


-- Reads the entire output verbatim
getContentsVerbatim proc =
    do
        contents <- hGetContents $ stdOut proc
        -- hGetContents is lazy. Force it to evaluate by mapping over everything doing nothing
        mapM_ return contents
        return contents

    
-- Read the message
getMessage :: MonadIO m => Process -> m String
getMessage proc =
    liftIO $ do
        length <- read `liftM` hGetLine (stdOut proc)
        mapM hGetChar $ replicate length (stdOut proc)
       
readMessage proc = read `liftM` getMessage proc

-- Put the message
putMessage :: MonadIO m => Process -> String -> m ()
putMessage proc message =
    liftIO $ do
        hPutStrLn (stdIn proc) (show $ length message)
        hPutStr (stdIn proc) message
        hFlush (stdIn proc)
       
