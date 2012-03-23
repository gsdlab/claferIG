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



data AlloyIG = AlloyIG{proc::Process, alloyModel::IORef String, sigMap::IORef (Map String Sig), scopes::IORef (Map String Integer), globalScope::IORef Integer}


data Sig = Sig{s_name::String, s_multiplicity::Multiplicity, s_subset::Maybe String, s_startingScope::Maybe Integer}


data Multiplicity = One | Lone | Some | Any deriving (Eq, Read, Show)


data UnsatCore = UnsatCore{core::[Constraint]} deriving Show


data Constraint = Constraint {from::Position, to::Position} deriving (Show, Eq)


data Position = Position {line::Integer, column::Integer} deriving (Show, Eq, Ord)



withinRange :: Integer -> Multiplicity -> Bool
withinRange scope One = scope == 1
withinRange scope Lone = scope == 0 || scope == 1
withinRange scope Some = scope >= 1
withinRange scope Any = True


initAlloyIG :: String -> IO AlloyIG
initAlloyIG alloyModel =
    do
        execPath <- executableDirectory
        alloyIGProc <- pipeProcess "java" ["-Djava.library.path=" ++ execPath ++ "lib" , "-jar", execPath ++ "alloyIG.jar"]
        
        alloyModelRef <- newIORef ""
        sigMapRef <- newIORef Map.empty
        scopesRef <- newIORef Map.empty
        globalScopeRef <- newIORef 0

        let alloyIG = AlloyIG alloyIGProc alloyModelRef sigMapRef scopesRef globalScopeRef

        return alloyIG
            

getAlloyModel :: AlloyIG -> IO String
getAlloyModel AlloyIG{alloyModel = alloyModel} = readIORef alloyModel


getSigs :: AlloyIG -> IO [String]
getSigs alloyIG = keys `fmap` readIORef (sigMap alloyIG)


-- Call load before any other commands.
sendLoadCommand :: String -> AlloyIG -> IO ()
sendLoadCommand model alloyIG@(AlloyIG proc alloyModel sigMap scopes globalScope) =
    do
        putMessage proc "load"
        putMessage proc model
        numberOfSigs <- read `liftM` getMessage proc
        sigs <- replicateM numberOfSigs $ readSig proc

        let sigMap' = fromList [(s_name sig, sig) | sig <- sigs]
        let scopes' = Map.empty
        globalScope' <- read `liftM` getMessage proc
        
        writeIORef alloyModel model
        writeIORef sigMap sigMap'
        writeIORef scopes scopes'
        writeIORef globalScope globalScope'
        
        mapM_ resetScope sigs
        
    where
    readSig :: Process -> IO Sig
    readSig proc =
        do
            sig <- getMessage proc 
            multiplicity <- read `liftM` getMessage proc
            subset <- getMessage proc
            hasStartingScope <- read `liftM` getMessage proc
            startingScope <-
                if hasStartingScope then (Just . read) `liftM` getMessage proc else return Nothing
            return $ Sig sig multiplicity (if null subset then Nothing else Just subset) startingScope
            
    resetScope :: Sig -> IO ()
    resetScope Sig{s_name = name, s_startingScope = startingScope} =
        case startingScope of
            Just scope -> sendSetScopeCommand name scope alloyIG >> return ()
            Nothing    -> return ()


-- Get the next solution from alloyIG
sendNextCommand :: AlloyIG -> IO (Maybe String)
sendNextCommand AlloyIG{proc=proc} =
    do
        putMessage proc "next"
        status <- read `liftM` getMessage proc
        case status of
            True -> Just `liftM` getMessage proc
            False -> return Nothing


getScope :: String -> AlloyIG -> IO Integer
getScope sig alloyIG =
    do
        rscopes <- readIORef (scopes alloyIG)
        case Map.lookup sig rscopes of
            Just scope -> return scope
            Nothing  -> readIORef (globalScope alloyIG)
        

getScopes :: AlloyIG -> IO [(String, Integer)]
getScopes alloyIG =
    do
        rscopes <- readIORef (scopes alloyIG)
        return $ toList rscopes
            

-- Tell alloyIG to change the scope of a sig
sendSetScopeCommand :: String -> Integer -> AlloyIG -> IO (Maybe String)
sendSetScopeCommand sig scope AlloyIG{proc=proc, scopes=scopes, sigMap=sigMap} =
    do
        sigMap' <- readIORef sigMap
        let Sig{s_multiplicity = multiplicity, s_subset = subset} = sigMap' ! sig
        
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
        

getGlobalScope :: AlloyIG -> IO Integer
getGlobalScope alloyIG = readIORef $ globalScope alloyIG


-- Tell alloyIG to change the global scope
sendSetGlobalScopeCommand :: Integer -> AlloyIG -> IO ()
sendSetGlobalScopeCommand scope AlloyIG{proc=proc, globalScope=globalScope} =
    do
        putMessage proc "setGlobalScope"
        putMessage proc (show scope)
        writeIORef globalScope scope


-- Tell alloyIG to recalculate the solution
sendResolveCommand :: AlloyIG -> IO ()
sendResolveCommand AlloyIG{proc = proc} = putMessage proc "resolve"


-- Tell alloyIG to save the current state
sendSaveStateCommand :: AlloyIG -> IO ()
sendSaveStateCommand AlloyIG{proc = proc} = putMessage proc "saveState"


-- Tell alloyIG to restore the state
sendRestoreStateCommand :: AlloyIG -> IO ()
sendRestoreStateCommand AlloyIG{proc = proc} = putMessage proc "restoreState"


-- Tell alloyIG to remove the constraint
sendRemoveConstraintCommand :: Constraint -> AlloyIG -> IO ()
sendRemoveConstraintCommand (Constraint from to) AlloyIG{proc = proc} =
    do
        putMessage proc "removeConstraint"
        sendPosition from >> sendPosition to
    where
    sendPosition (Position line column) =
        putMessage proc (show line) >> putMessage proc (show column)
        

-- Tell alloyIG to return the unsat core of the previous operation        
sendUnsatCoreCommand :: AlloyIG -> IO UnsatCore
sendUnsatCoreCommand AlloyIG{proc = proc} =
    do
        putMessage proc "unsatCore"
        coreLength         <- read `liftM` getMessage proc
        core               <- replicateM coreLength readConstraint
        return $ UnsatCore core
    where
    readPosition =
        do
            line   <- getMessage proc
            column <- getMessage proc
            return $ Position (read line) (read column)
    readConstraint =
        do
            from <- readPosition
            to   <- readPosition
            return $ Constraint from to
            
            
-- Tell alloyIG to change the unsat core minimization level.
-- 0 -> Fastest
-- 1 -> Medium
-- 2 -> Best
sendSetUnsatCoreMinimizationCommand :: Integer -> AlloyIG -> IO ()
sendSetUnsatCoreMinimizationCommand level AlloyIG{proc = proc} =
    do
        putMessage proc "unsatCoreMinimization"
        putMessage proc (show level)
        

-- Tell alloyIG to change the bitwidth        
sendSetBitwidthCommand :: Integer -> AlloyIG -> IO ()
sendSetBitwidthCommand bitwidth AlloyIG{proc = proc} =
    do
        when (bitwidth < 0) $ fail (show bitwidth ++ " is not a valid bitwidth.")
        putMessage proc "setBitwidth"
        putMessage proc (show bitwidth)


-- Tell alloyIG to quit
sendQuitCommand :: AlloyIG -> IO ()
sendQuitCommand AlloyIG{proc=proc} = putMessage proc "quit"

