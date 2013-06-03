{-# LANGUAGE DeriveDataTypeable #-}

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

module Main where

import Language.Clafer.IG.ClaferIG 
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.CommandLine
import Language.Clafer.IG.Solution
import Language.Clafer.IG.Sugarer
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.IORef
import Prelude hiding (all)
import System.Console.CmdArgs
import System.Directory
import System.FilePath

claferIGArgsDef = IGArgs {
    all             = def &= help "Saves all instances up to the provided scope or a counterexample.",
    saveDir         = def &= help "Specify the directory for storing saved files." &= typ "FILE",
    bitwidth        = 4 &= help "Set the bitwidth for integers." &= typ "INTEGER", -- Default bitwidth is 4.
    alloySolution   = False &= help "Convert Alloy solution to a Clafer solution.",
    claferModelFile = def &= argPos 0 &= typ "FILE",
    useUids         = False &= help "Use unique clafer names in the Clafer solution.",
    addTypes        = False &= help "Add colon/reference types to the Clafer solution.",
    json            = False &= help "Render solution as JSON (forces 'addUids')."
} &= summary claferIGVersion


main =
    do
        args <- cmdArgs claferIGArgsDef
        if (alloySolution args)
            then
                runAlloySolution args
            else if (json args)
                then 
                    tryClaferIG (args { useUids = True })
                else
                    tryClaferIG args
    where
    tryClaferIG args =
        do
            try <- runClaferIG args
            case try of
                Right r -> return r
                Left l  -> do
                    mapM putStrLn $ printError l
                    putStrLn "Press enter to retry."
                    void getLine
                    tryClaferIG args
        
runClaferIG args =
    runClaferIGT args $ do
        case all args of
            Just scope ->
                do
                    setGlobalScope scope
                    solve
                    
                    let file = claferModelFile args
                    counterRef <- liftIO $ newIORef 1
                    
                    let saveDirectory = fromMaybe return $ underDirectory `liftM` saveDir args
                    saveAll (savePath file counterRef >>= saveDirectory)
                    return ()
            Nothing    -> runCommandLine
            
        quit
        
runAlloySolution args =
    do
        content <- readFile $ claferModelFile args -- It's an Alloy XML file in this case
        let sMap = Map.empty
        putStrLn $ show $ (sugarClaferModel (useUids args) (addTypes args) Nothing $ buildClaferModel $ parseSolution content) $ sMap

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


saveAll :: IO FilePath -> ClaferIGT IO ()
saveAll nextFile =
    do
        file <- liftIO nextFile
        liftIO $ createDirectoryIfMissing True $ takeDirectory file
        solution <- next
        case solution of
            Instance{modelInstance = modelInstance} -> do
                liftIO $ writeFile file (show modelInstance)
                saveAll nextFile
            _ -> return ()  
