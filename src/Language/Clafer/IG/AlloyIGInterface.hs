{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

{-
 Copyright (C) 2012-2017 Jimmy Liang <http://gsd.uwaterloo.ca>

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

module Language.Clafer.IG.AlloyIGInterface where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Data.Map as Map hiding (null)
import Data.Maybe
import Language.Clafer.Front.AbsClafer (Span(..), Pos(..))
import Language.Clafer.IG.Process
import Prelude

-- | An interface to the Alloy Analyzer

newtype AlloyIGT m a = AlloyIGT (StateT (Maybe AlloyIGEnv) (ReaderT Process m) a) deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans AlloyIGT where
    lift = AlloyIGT . lift . lift

fetch :: Monad m => AlloyIGT m AlloyIGEnv
fetch = fromMaybe (error "AlloyIG not loaded.") `liftM` AlloyIGT get

fetches :: Monad m => (AlloyIGEnv -> a) -> AlloyIGT m a
fetches = (`liftM` fetch)

set :: Monad m => AlloyIGEnv -> AlloyIGT m ()
set = AlloyIGT . put . Just

proc :: Monad m => AlloyIGT m Process
proc = AlloyIGT ask

-- | Instance generator's environment
data AlloyIGEnv = AlloyIGEnv {alloyModel::String, sigMap::Map String Sig, scopes::Map String Integer, globalScope::Integer}


data Sig = Sig{s_name::String, s_multiplicity::Multiplicity, s_subset::Maybe String, s_startingScope::Maybe Integer}


data Multiplicity = One | Lone | Some | Any deriving (Eq, Read, Show)


newtype UnsatCore = UnsatCore{core::[Span]} deriving Show



withinRange :: Integer -> Multiplicity -> Bool
withinRange scope One = scope == 1
withinRange scope Lone = scope == 0 || scope == 1
withinRange scope Some = scope >= 1
withinRange _ Any = True


runAlloyIGT :: MonadIO m => AlloyIGT m a -> m a
runAlloyIGT run =
    do
        execPath <- liftIO $ executableDirectory
        proce     <- liftIO $ pipeProcess "java" ["-Djava.library.path=" ++ execPath ++ "lib" , "-jar", execPath ++ "alloyIG.jar"]

        runReaderT (evalStateT (unwrap run) Nothing) proce
    where
    unwrap (AlloyIGT a) = a


getAlloyModel :: MonadIO m => AlloyIGT m String
getAlloyModel = fetches alloyModel


getSigs :: MonadIO m => AlloyIGT m [String]
getSigs = keys `liftM` fetches sigMap

-- | Call load before any other commands.
load :: Process -> String -> IO AlloyIGEnv
load proce alloyModel' =
    do
        putMessage proce "load"
        putMessage proce alloyModel'
        numberOfSigs <- readMessage proce
        sigs <- replicateM numberOfSigs readSig

        let sigMap' = fromList [(s_name sig, sig) | sig <- sigs]
        let scopes' = Map.empty
        globalScope' <- readMessage proce

        return $ AlloyIGEnv alloyModel' sigMap' scopes' globalScope'
    where
    readSig =
        do
            sig <- getMessage proce
            multiplicity <- readMessage proce
            subset <- getMessage proce
            hasStartingScope <- readMessage proce
            startingScope <-
                if hasStartingScope then Just <$> readMessage proce else return Nothing
            return $ Sig sig multiplicity (if null subset then Nothing else Just subset) startingScope


sendLoadCommand :: MonadIO m => String -> AlloyIGT m ()
sendLoadCommand alloyModel' =
    do
        proc' <- proc
        env <- liftIO $ load proc' alloyModel'
        set env

        sigs <- elems `liftM` fetches sigMap
        mapM_ resetScope sigs
    where
    resetScope Sig{s_name = name, s_startingScope = startingScope} =
        case startingScope of
            Just scope -> sendSetScopeCommand name scope >> return ()
            Nothing    -> sendSetScopeCommand name 1 >> return ()


-- | Get the next solution from alloyIG
sendNextCommand :: MonadIO m => AlloyIGT m (Maybe String)
sendNextCommand =
    do
        putMsg "next"
        status <- readMsg
        case status of
            True  -> Just `liftM` getMsg
            False -> return Nothing


getScope :: MonadIO m => String -> AlloyIGT m Integer
getScope sig =
    do
        rscopes <- fetches scopes
        case Map.lookup sig rscopes of
            Just scope -> return scope
            Nothing    -> getGlobalScope


getScopes :: MonadIO m => AlloyIGT m [(String, Integer)]
getScopes = toList `liftM` fetches scopes


-- | Tell alloyIG to change the scope of a sig
sendSetScopeCommand :: MonadIO m => String -> Integer -> AlloyIGT m (Maybe String)
sendSetScopeCommand sig scope =
    do
        sigMap' <- fetches sigMap
        let Sig{s_multiplicity = multiplicity, s_subset = subset} = sigMap' ! sig

        -- Alloy has a fit when trying to set a scope outside its multiplicity
        -- Don't send command if outside its multiplicity but continue the illusion that
        -- the scope was set
        case subset of
            Nothing ->
                do
                    when (withinRange scope multiplicity) $
                        do
                            putMsg "setScope"
                            putMsg sig
                            putMsg $ show scope
                    rscopes <- fetches scopes
                    env <- fetch
                    set env {scopes = Map.insert sig scope rscopes}
                    return $ Nothing
            Just sub ->
                return $ Just sub


getGlobalScope :: MonadIO m => AlloyIGT m Integer
getGlobalScope = fetches globalScope


-- | Tell alloyIG to change the global scope
sendSetGlobalScopeCommand :: MonadIO m => Integer -> AlloyIGT m ()
sendSetGlobalScopeCommand scope =
    do
        putMsg "setGlobalScope"
        putMsg $ show scope

        env <- fetch
        set env {globalScope = scope}


-- | Tell alloyIG to recalculate the solution
sendResolveCommand :: MonadIO m => AlloyIGT m ()
sendResolveCommand = putMsg "resolve"


-- | Tell alloyIG to save the current state
sendSaveStateCommand :: MonadIO m => AlloyIGT m ()
sendSaveStateCommand = putMsg "saveState"


-- | Tell alloyIG to restore the state
sendRestoreStateCommand :: MonadIO m => AlloyIGT m ()
sendRestoreStateCommand = putMsg "restoreState"


-- | Tell alloyIG to remove the constraint
sendRemoveConstraintCommand :: MonadIO m => Span -> AlloyIGT m ()
sendRemoveConstraintCommand s = case s of
    (Span from to) ->
        do
            putMsg "removeConstraint"
            sendPosition from >> sendPosition to
    where
    sendPosition (Pos line column) =
        putMsg (show line) >> putMsg (show column)

-- | Tell alloyIG to return the unsat core of the previous operation
sendUnsatCoreCommand :: MonadIO m => AlloyIGT m UnsatCore
sendUnsatCoreCommand =
    do
        putMsg "unsatCore"
        coreLength <- readMsg
        core'       <- replicateM coreLength readConstraint
        return $ UnsatCore core'
    where
    readPosition   = liftM2 Pos readMsg readMsg
    readConstraint = liftM2 Span readPosition readPosition


-- | Tell alloyIG to change the unsat core minimization level.
--  0 -> Fastest,
--  1 -> Medium,
--  2 -> Best
sendSetUnsatCoreMinimizationCommand :: MonadIO m => Integer -> AlloyIGT m ()
sendSetUnsatCoreMinimizationCommand level =
    do
        putMsg "unsatCoreMinimization"
        putMsg $ show level


-- | Tell alloyIG to change the bitwidth
sendSetBitwidthCommand :: MonadIO m => Integer -> AlloyIGT m ()
sendSetBitwidthCommand bitwidth =
    do
        when (bitwidth < 0) $ error (show bitwidth ++ " is not a valid bitwidth.")
        putMsg "setBitwidth"
        putMsg $ show bitwidth


-- | Tell alloyIG to quit
sendQuitCommand :: MonadIO m => AlloyIGT m ()
sendQuitCommand = putMsg "quit"


getMsg :: MonadIO m => AlloyIGT m String
getMsg = getMessage =<< proc

readMsg :: (MonadIO m, Read r) => AlloyIGT m r
readMsg = read `liftM` getMsg

putMsg :: MonadIO m => String -> AlloyIGT m ()
putMsg msg =
    do
        proc' <- proc
        putMessage proc' msg
