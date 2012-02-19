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

module AlloyIGInterface where

import Control.Monad
import Data.IORef
import Process
import Solution


data AlloyIG = AlloyIG{proc::Process, sigs::[String], globalScope::IORef Int}



initAlloyIG :: String -> Process -> IO AlloyIG
initAlloyIG alloyModel proc =
    do
        putMessage proc alloyModel
        numberOfSigs <- read `liftM` getMessage proc
        sigs <- mapM getMessage (replicate numberOfSigs proc)
        globalScope <- read `liftM` getMessage proc
        
        scopeRef <- newIORef globalScope
        
        return $ AlloyIG proc sigs scopeRef


-- Get the next solution from alloyIG
sendNextCommand :: AlloyIG -> IO (Maybe Solution)
sendNextCommand AlloyIG{proc=proc} =
    do
        putMessage proc "next"
        status <- read `liftM` getMessage proc
        case status of
            True -> do
                xml <- getMessage proc
                let solution = parseSolution xml
                return $ Just solution
            False -> return Nothing


getGlobalScope :: AlloyIG -> IO Int
getGlobalScope alloyIG = readIORef $ globalScope alloyIG


-- Tell alloyIG to change the scope
sendSetGlobalScopeCommand :: Int -> AlloyIG -> IO ()
sendSetGlobalScopeCommand scope AlloyIG{proc=proc, globalScope=globalScope} =
    do
        putMessage proc "setGlobalScope"
        putMessage proc (show scope)
        writeIORef globalScope scope


-- Tell alloyIG to quit
sendQuitCommand :: AlloyIG -> IO ()
sendQuitCommand AlloyIG{proc=proc} = putMessage proc "quit"
