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

import ClaferIG
import ClaferModel
import CommandLine
import Control.Monad
import Data.Maybe
import Data.IORef
import Prelude hiding (all)
import System.Console.CmdArgs
import System.Directory
import System.FilePath


data IGArgs = IGArgs {
    all :: Maybe Int,
    saveDir :: Maybe FilePath,  
    claferModelFile :: FilePath
} deriving (Show, Data, Typeable)



claferIG = IGArgs {
    all             = def &= help "Saves all instances up to the provided scope or a counterexample.",
    saveDir         = def &= help "Specify the directory for storing saved files." &= typ "FILE",
    claferModelFile = def &= argPos 0 &= typ "FILE"
} &= summary claferIGVersion


main =
    do
        args <- cmdArgs claferIG
        claferIG <- initClaferIG $ claferModelFile args
        
        case all args of
            Just scope ->
                do
                    setGlobalScope scope claferIG
                    solve claferIG
                    
                    let file = claferModelFile args
                    counterRef <- newIORef 1
                    
                    let saveDirectory = fromMaybe return $ underDirectory `liftM` saveDir args
                    saveAll (savePath file counterRef >>= saveDirectory) claferIG
                    return ()
            Nothing    -> runCommandLine claferIG
            
        quit claferIG


savePath :: FilePath -> IORef Int -> IO FilePath
savePath file counterRef =
    do
        counter <- readIORef counterRef
        writeIORef counterRef (counter + 1)
        return $ file ++ "." ++ (show counter) ++ ".data"
        

underDirectory :: FilePath -> FilePath -> IO FilePath
underDirectory dir file =
    do
        createDirectoryIfMissing True dir
        return $ joinPath [dir, file]


saveAll :: IO FilePath -> ClaferIG -> IO ()
saveAll nextFile claferIG =
    do
        file <- nextFile
        solution <- next claferIG
        case solution of
            Instance{modelInstance = modelInstance} -> do
                writeFile file (show modelInstance)
                saveAll nextFile claferIG
            _ -> return ()
