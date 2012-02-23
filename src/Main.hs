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
import CommandLine
import Prelude hiding (all)
import System.Console.CmdArgs


data IGArgs = IGArgs {
    all :: Maybe Int,
    claferModelFile :: FilePath
} deriving (Show, Data, Typeable)



claferIG = IGArgs {
    all             = def &= help "Saves all instances up to the provided scope or a counterexample." &= name "all",
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
                    
                    let saveNames = map (saveName $ claferModelFile args) [1,2..]
                    saveAll saveNames claferIG
                    return ()
            Nothing    -> runCommandLine claferIG
            
        quit claferIG
        
        
saveName :: FilePath -> Int -> FilePath
saveName file counter = file ++ "." ++ (show counter) ++ ".data"


saveAll :: [FilePath] -> ClaferIG -> IO [FilePath]
saveAll [] _ = fail "No more filepaths"
saveAll (f:fs) claferIG =
    do
        solution <- next claferIG
        case solution of
            Just nextSolution ->
                do
                    writeFile f (show nextSolution)
                    saved <- saveAll fs claferIG
                    return $ f:saved
            Nothing           -> return []
