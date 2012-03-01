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
import Data.Map as Map hiding (null)
import Process


data AlloyIG = AlloyIG{proc::Process, alloyModel::String, sigMap::Map String Sig, scopes::IORef (Map String Int), globalScope::IORef Int}
data Sig = Sig{s_name::String, s_multiplicity::Multiplicity, s_subset::Maybe String}
data Multiplicity = One | Lone | Some | Any deriving (Eq, Read, Show)

data UnsatCore = UnsatCore{core::[String], subcore::[String]} deriving Show



withinRange :: Int -> Multiplicity -> Bool
withinRange scope One = scope == 1
withinRange scope Lone = scope == 0 || scope == 1
withinRange scope Some = scope >= 1
withinRange scope Any = True


initAlloyIG :: String -> IO AlloyIG
initAlloyIG alloyModel =
    do
        execPath <- executableDirectory
        alloyIGProc <- pipeProcess "java" ["-Djava.library.path=" ++ execPath ++ "lib" , "-jar", execPath ++ "alloyIG.jar"]
        putMessage alloyIGProc alloyModel
        numberOfSigs <- read `liftM` getMessage alloyIGProc
        sigs <- mapM readSig (replicate numberOfSigs alloyIGProc)
        globalScope <- read `liftM` getMessage alloyIGProc

        scopesRef <- newIORef Map.empty        
        globalScopeRef <- newIORef globalScope
        
        return $ AlloyIG alloyIGProc alloyModel (fromList [(s_name sig, sig) | sig <- sigs]) scopesRef globalScopeRef
    where
    readSig :: Process -> IO Sig
    readSig proc =
        do
            sig <- getMessage proc 
            multiplicity <- read `liftM` getMessage proc
            subset <- getMessage proc
            return $ Sig sig multiplicity (if null subset then Nothing else Just subset)
            

sigs :: AlloyIG -> [String]
sigs alloyIG = keys (sigMap alloyIG)


-- Get the next solution from alloyIG
sendNextCommand :: AlloyIG -> IO (Maybe String)
sendNextCommand AlloyIG{proc=proc} =
    do
        putMessage proc "next"
        status <- read `liftM` getMessage proc
        case status of
            True -> Just `liftM` getMessage proc
            False -> return Nothing


getScope :: String -> AlloyIG -> IO Int
getScope sig alloyIG =
    do
        rscopes <- readIORef (scopes alloyIG)
        case Map.lookup sig rscopes of
            Just scope -> return scope
            Nothing  -> readIORef (globalScope alloyIG)
        

getScopes :: AlloyIG -> IO [(String, Int)]
getScopes alloyIG =
    do
        rscopes <- readIORef (scopes alloyIG)
        return $ toList rscopes
            

-- Tell alloyIG to change the scope of a sig
sendSetScopeCommand :: String -> Int -> AlloyIG -> IO (Maybe String)
sendSetScopeCommand sig scope AlloyIG{proc=proc, scopes=scopes, sigMap=sigMap} =
    do
        -- Alloy has a fit when trying to set a scope outside its multiplicity
        -- Don't send command if outside its multiplicity but continue the illusion that
        -- the scope was set
        case subset of
            Nothing ->
                do
                    when (withinRange scope multiplicity) $
                        do
                            putMessage proc "setScope"
                            putMessage proc sig
                            putMessage proc (show scope)
                    rscopes <- readIORef scopes
                    writeIORef scopes (Map.insert sig scope rscopes)
                    return $ Nothing
            Just sub ->
                return $ Just sub
    where
    Sig{s_multiplicity = multiplicity, s_subset = subset} = sigMap ! sig
        

getGlobalScope :: AlloyIG -> IO Int
getGlobalScope alloyIG = readIORef $ globalScope alloyIG


-- Tell alloyIG to change the global scope
sendSetGlobalScopeCommand :: Int -> AlloyIG -> IO ()
sendSetGlobalScopeCommand scope AlloyIG{proc=proc, globalScope=globalScope} =
    do
        putMessage proc "setGlobalScope"
        putMessage proc (show scope)
        writeIORef globalScope scope


-- Tell alloyIG to recalculate the solution
sendResolveCommand :: AlloyIG -> IO ()
sendResolveCommand AlloyIG{proc = proc} = putMessage proc "resolve"


-- Tell alloyIG to return the unsat core
sendUnsatCoreCommand :: AlloyIG -> IO UnsatCore
sendUnsatCoreCommand AlloyIG{proc = proc} =
    do
        putMessage proc "unsatCore"
        coreLength <- read `liftM` getMessage proc
        cores <- mapM getMessage (replicate coreLength proc)
        subcoreLength <- read `liftM` getMessage proc
        subcores <- mapM getMessage (replicate subcoreLength proc)
        return $ UnsatCore cores subcores
        
        
-- Get the counterexample of the unsatisfiable solution from alloyIG
sendCounterexampleCommand :: AlloyIG -> IO (Maybe String)
sendCounterexampleCommand AlloyIG{proc=proc} =
    do
        putMessage proc "counterexample"
        status <- read `liftM` getMessage proc
        case status of
            True ->
                do
                    removedConstraints <- getMessage proc
                    putStrLn $ "Alloy unsat (temporary for debugging) = " ++ removedConstraints
                    Just `liftM` getMessage proc
            False -> return Nothing        


-- Tell alloyIG to quit
sendQuitCommand :: AlloyIG -> IO ()
sendQuitCommand AlloyIG{proc=proc} = putMessage proc "quit"

