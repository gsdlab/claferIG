{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

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

module ClaferIG (ClaferIGT, Scope, Instance(..), Counterexample(..), runClaferIGT, claferIGVersion, getClaferModel, getAlloyModel, solve, getClafers, getGlobalScope, setGlobalScope, getScopes, getScope, nameOfScope, valueOfScope, increaseScope, setScope, next, setUnsatCoreMinimization, getClaferFile, getBitwidth, setBitwidth, quit, reload) where

import Debug.Trace
import Language.Clafer
import Language.ClaferT
import Language.Clafer.Front.Absclafer (Span(..))
import Language.Clafer.Generator.Xml
import AlloyIGInterface (AlloyIGT)
import qualified AlloyIGInterface as AlloyIG
import ClaferModel
import Constraints
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Map as Map hiding (map, null)
import Data.Maybe
import Process
import Solution
import Sugarer
import System.Directory
import System.FilePath
import System.Exit
import Version
import System.Console.Haskeline.MonadException


newtype ClaferIGT m a = ClaferIGT (StateT ClaferIGEnv (AlloyIGT m) a)
    deriving (Applicative, Functor, Monad, MonadIO)

deriving instance MonadException m => MonadException (ClaferIGT m)

instance MonadTrans ClaferIGT where
    lift = ClaferIGT . lift . lift

fetch :: Monad m => ClaferIGT m ClaferIGEnv
fetch = ClaferIGT get

fetches :: Monad m => (ClaferIGEnv -> a) -> ClaferIGT m a
fetches = ClaferIGT . gets

set :: Monad m => ClaferIGEnv -> ClaferIGT m ()
set = ClaferIGT . put

runClaferIGT :: MonadIO m => FilePath -> Integer -> ClaferIGT m a -> m (Either ClaferErrs a)
runClaferIGT claferFile bitwidth run =
    AlloyIG.runAlloyIGT $ runErrorT $ do
        env <- (ErrorT $ load claferFile bitwidth) `catchError` (\x -> lift AlloyIG.sendQuitCommand >> throwError x)
        lift $ evalStateT (unwrap run) env
    where
    unwrap (ClaferIGT c) = c


data ClaferIGEnv = ClaferIGEnv{claferFile::FilePath, bitwidth::Integer, constraints:: [Constraint], claferModel::String, claferToSigNameMap:: Map String String}


data Scope = Scope {name::String, sigName::String}


data Instance =
    Instance {modelInstance::ClaferModel, alloyModelInstance::String} |
    UnsatCore {unsatConstraints::[Constraint], counterexample::Maybe Counterexample} |
    NoInstance
    

data Counterexample = Counterexample {removedConstraints::[Constraint], counterexampleInstance::ClaferModel, counterexampleAlloyInstance::String}



claferIGVersion =
    "ClaferIG " ++ version
    

getClaferModel :: Monad m => ClaferIGT m String
getClaferModel = fetches claferModel


load :: MonadIO m => String -> Integer -> AlloyIGT m (Either ClaferErrs ClaferIGEnv)
load claferFile bitwidth  =
    runErrorT $ do
        claferModel <- liftIO $ strictReadFile claferFile
        
        (ir, alloyModel, mapping) <- ErrorT $ return $ callClaferTranslator claferModel
        
        let constraints = parseConstraints claferModel ir mapping
        
        lift $ AlloyIG.sendLoadCommand alloyModel
        lift $ AlloyIG.sendSetBitwidthCommand bitwidth

        sigs <- lift $ AlloyIG.getSigs
        let claferToSigNameMap = fromListWithKey (error . ("Duplicate clafer name " ++)) [(sigToClaferName x, x) | x <- sigs]
        
        return $ ClaferIGEnv claferFile bitwidth constraints claferModel claferToSigNameMap
    where
    callClaferTranslator code =
        mapLeft ClaferErrs $ runClafer args $ do
            addModuleFragment code
            parse
            compile
            result <- generate
            return (fst3 $ fromJust $ cIr $ claferEnv result, outputCode result, mappingToAlloy result)
    args = defaultClaferArgs {keep_unused = Just True, no_stats = Just True}
    mapLeft f (Left l) = Left $ f l
    mapLeft _ (Right r) = Right r
    fst3 (a, _, _) = a

                
strictReadFile :: FilePath -> IO String 
strictReadFile filePath = 
    do
        contents <- readFile filePath
        -- readFile is lazy. Force it to evaluate by mapping over everything doing nothing
        mapM_ return contents
        return contents


getAlloyModel :: MonadIO m => ClaferIGT m String
getAlloyModel = ClaferIGT $ lift AlloyIG.getAlloyModel


solve :: MonadIO m => ClaferIGT m ()
solve = ClaferIGT $ lift AlloyIG.sendResolveCommand

 
getClafers :: MonadIO m => ClaferIGT m [String]
getClafers =
    do
        sigs <- ClaferIGT $ lift AlloyIG.getSigs
        return $ map sigToClaferName sigs


getGlobalScope :: MonadIO m => ClaferIGT m Integer
getGlobalScope = ClaferIGT $ lift AlloyIG.getGlobalScope


setGlobalScope :: MonadIO m => Integer -> ClaferIGT m ()
setGlobalScope scope = ClaferIGT $ lift $ AlloyIG.sendSetGlobalScopeCommand scope


getScopes :: MonadIO m => ClaferIGT m [Scope]
getScopes = 
    do
        scopes <- ClaferIGT $ lift AlloyIG.getScopes
        return $ [Scope (sigToClaferName sig) sig | (sig, scope) <- scopes]
        
        
getScope :: MonadIO m => String -> ClaferIGT m (Either String Scope)
getScope name =
    do
        claferToSigNameMap' <- fetches claferToSigNameMap
        runErrorT $ case name `Map.lookup` claferToSigNameMap' of
            Just sigName -> return $ Scope name sigName
            Nothing      -> throwError $ "Unknown clafer " ++ name
        

nameOfScope :: Scope -> String
nameOfScope = name


valueOfScope :: MonadIO m => Scope -> ClaferIGT m Integer
valueOfScope Scope{sigName} = ClaferIGT $ lift $ AlloyIG.getScope sigName


increaseScope :: MonadIO m => Integer -> Scope -> ClaferIGT m (Either String ())
increaseScope increment scope =
    do
        value <- valueOfScope scope
        let value' = value + increment
        setScope value' scope
    

setScope :: MonadIO m => Integer -> Scope -> ClaferIGT m (Either String ())
setScope scope Scope{name, sigName} =
    do
        subset <- ClaferIGT $ lift $ AlloyIG.sendSetScopeCommand sigName scope
        runErrorT $ maybe (return ()) throwError $ sigToClaferName <$> subset


next :: MonadIO m => ClaferIGT m Instance
next =
    do
        constraints' <- fetches constraints
        nextImpl constraints'

nextImpl constraints = 
    do
        xmlSolution <- ClaferIGT $ lift AlloyIG.sendNextCommand
        case xmlSolution of
            Just xml -> return $ Instance (xmlToModel xml) xml
            Nothing  -> do
                ClaferIGT $ lift AlloyIG.sendSaveStateCommand
                -- Generating counterexample modifies the state. If we ever want to
                -- rerun the original model, we need to restore the state.
                AlloyIG.UnsatCore core <- ClaferIGT $ lift AlloyIG.sendUnsatCoreCommand
                c <- counterexample core
                ClaferIGT $ lift AlloyIG.sendRestoreStateCommand
                return c
    where
    counterexample originalCore =
        counterexample' originalCore []
        where
        counterexample' core removed =
            case msum $ findRemovable core of
                Just remove -> do
                    ClaferIGT $ lift $ AlloyIG.sendRemoveConstraintCommand $ range remove
                    ClaferIGT $ lift AlloyIG.sendResolveCommand
                    xmlSolution <- ClaferIGT $ lift AlloyIG.sendNextCommand
                    case xmlSolution of
                        Just xml -> return $
                            UnsatCore (catMaybes $ findRemovable core) (Just $ Counterexample (reverse $ remove : removed) (xmlToModel xml) xml)
                        Nothing ->
                            do
                                AlloyIG.UnsatCore core' <- ClaferIGT $ lift AlloyIG.sendUnsatCoreCommand
                                counterexample' core' $ remove : removed
                Nothing -> -- It is possible that none of the constraints are removable
                    return NoInstance

    findRemovable core = [find ((== c). range) constraints | c <- core]
                
    xmlToModel xml = sugarClaferModel $ buildClaferModel $ parseSolution xml
    
    
reload :: MonadIO m => ClaferIGT m (Either ClaferErrs ())
reload  =
    runErrorT $ do
        globalScope <- lift $ getGlobalScope
        
        claferFile' <- lift $ getClaferFile
        bitwidth'   <- lift $ getBitwidth
        env <- ErrorT $ ClaferIGT $ lift $ load claferFile' bitwidth'
        lift $ set env
      
        lift $ setGlobalScope globalScope
        
        
setUnsatCoreMinimization :: MonadIO m => Integer -> ClaferIGT m ()
setUnsatCoreMinimization level = ClaferIGT $ lift $ AlloyIG.sendSetUnsatCoreMinimizationCommand level


getClaferFile :: Monad m => ClaferIGT m FilePath
getClaferFile = fetches claferFile


getBitwidth :: Monad m => ClaferIGT m Integer
getBitwidth = fetches bitwidth


setBitwidth :: MonadIO m => Integer -> ClaferIGT m ()
setBitwidth bitwidth = ClaferIGT $ lift $ AlloyIG.sendSetBitwidthCommand bitwidth

    
quit :: MonadIO m => ClaferIGT m ()
quit = ClaferIGT $ lift AlloyIG.sendQuitCommand
    
    
sigToClaferName :: String -> String
sigToClaferName n =
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
