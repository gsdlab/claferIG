{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}

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

module Language.Clafer.IG.ClaferIG (
    IGArgs(..),
    ClaferIGEnv(..),
    getClaferEnv,
    getClaferIGArgs,
    getConstraints,
    getClaferModel,
    getInfo,
    getStrMap,
    ClaferIGT, 
    Scope(..), 
    Instance(..), 
    Counterexample(..), 
    runClaferIGT, 
    claferIGVersion, 
    getAlloyModel, 
    solve, 
    getClafers, 
    getGlobalScope, 
    getBitwidth,
    setGlobalScope, 
    getScopes, 
    getScope, 
    nameOfScope, 
    valueOfScope, 
    increaseScope, 
    setScope, 
    next, 
    setUnsatCoreMinimization,  
    setBitwidth, 
    quit, 
    reload) where

import Debug.Trace
import Language.Clafer
import Language.ClaferT
import Language.Clafer.Front.Absclafer (Span(..))
import Language.Clafer.Generator.Xml
import qualified Language.Clafer.Intermediate.Analysis as Analysis
import Language.Clafer.IG.AlloyIGInterface (AlloyIGT)
import qualified Language.Clafer.IG.AlloyIGInterface as AlloyIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.Constraints
import Language.Clafer.IG.JSONGenerator
import Language.Clafer.IG.Process
import Language.Clafer.IG.Solution
import Language.Clafer.IG.Sugarer
import Language.Clafer.IG.Version
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Map as Map hiding (map, null)
import Data.Maybe
import Data.Data
import Data.Typeable
import System.Directory
import System.FilePath
import System.Exit
import System.Console.Haskeline.MonadException

data IGArgs = IGArgs {
    all :: Maybe Integer,
    saveDir :: Maybe FilePath,  
    claferModelFile :: FilePath,
    alloySolution :: Bool,
    bitwidth :: Integer,
    useUids :: Bool,
    addTypes :: Bool,
    json :: Bool
} deriving (Show, Data, Typeable)

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

runClaferIGT :: MonadIO m => IGArgs -> ClaferIGT m a -> m (Either ClaferErrs a)
runClaferIGT                 args      run =
    AlloyIG.runAlloyIGT $ runErrorT $ do
        env <- (ErrorT $ load args) `catchError` (\x -> lift AlloyIG.sendQuitCommand >> throwError x)
        lift $ evalStateT (unwrap run) env
    where
    unwrap (ClaferIGT c) = c


data ClaferIGEnv = ClaferIGEnv{
    claferEnv'::ClaferEnv,
    claferIGArgs :: IGArgs,
    constraints:: [Constraint], 
    claferModel::String, 
    claferToSigNameMap:: Map String String,
    info :: Analysis.Info,
    strMap :: (Map Int String)
}

data Scope = Scope {name::String, sigName::String}

data Instance =
    Instance {modelInstance::ClaferModel, alloyModelInstance::String} |
    UnsatCore {unsatConstraints::[Constraint], counterexample::Maybe Counterexample} |
    NoInstance
    

data Counterexample = Counterexample {removedConstraints::[Constraint], counterexampleInstance::ClaferModel, counterexampleAlloyInstance::String}

claferIGVersion =
    "ClaferIG " ++ version
    
getClaferEnv :: Monad m => ClaferIGT m ClaferEnv
getClaferEnv = fetches claferEnv'

getClaferIGArgs :: Monad m => ClaferIGT m IGArgs
getClaferIGArgs = fetches claferIGArgs

getConstraints :: Monad m => ClaferIGT m [ Constraint ]
getConstraints = fetches constraints

getClaferModel :: Monad m => ClaferIGT m String
getClaferModel = fetches claferModel

getStrMap :: Monad m => ClaferIGT m (Map Int String)
getStrMap = fetches strMap

getInfo :: Monad m => ClaferIGT m Analysis.Info
getInfo = fetches info

load :: MonadIO m => IGArgs -> AlloyIGT m (Either ClaferErrs ClaferIGEnv)
load                 igArgs    =
    runErrorT $ do
        claferModel <- liftIO $ strictReadFile claferFile'
        
        (claferEnv', alloyModel, mapping, sMap) <- ErrorT $ return $ callClaferTranslator claferModel 

        let ir = fst3 $ fromJust $ cIr claferEnv'
        let constraints = parseConstraints claferModel ir mapping
        
        lift $ AlloyIG.sendLoadCommand alloyModel
        lift $ AlloyIG.sendSetBitwidthCommand bitwidth'

        sigs <- lift $ AlloyIG.getSigs
        let claferToSigNameMap = fromListWithKey (error . ("Duplicate clafer name " ++)) [(sigToClaferName x, x) | x <- sigs]
        
        let info = Analysis.gatherInfo ir 

        return $ ClaferIGEnv claferEnv' igArgs constraints claferModel claferToSigNameMap info sMap
    where
    callClaferTranslator code =
        mapLeft ClaferErrs $ runClafer claferArgs $ do
            addModuleFragment code
            parse
            compile
            result <- generate
            return (claferEnv result, outputCode result, mappingToAlloy result, stringMap result)
    mapLeft f (Left l) = Left $ f l
    mapLeft _ (Right r) = Right r
    claferArgs = defaultClaferArgs {keep_unused = Just True, no_stats = Just True}
    claferFile' = claferModelFile igArgs
    bitwidth' = bitwidth igArgs
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

getBitwidth :: MonadIO m => ClaferIGT m Integer
getBitwidth = 
    do
        claferIGArgs' <- getClaferIGArgs
        return $ bitwidth claferIGArgs'

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
        bitwidth' <- getBitwidth
        let value' = min (value + increment) ((2 ^ (bitwidth' - 1)) - 1)
        setScope value' scope
    

setScope :: MonadIO m => Integer -> Scope -> ClaferIGT m (Either String ())
setScope scope Scope{name, sigName} =
    do
        subset <- ClaferIGT $ lift $ AlloyIG.sendSetScopeCommand sigName scope
        runErrorT $ maybe (return ()) throwError $ sigToClaferName <$> subset


next :: MonadIO m => ClaferIGT m Instance
next = do
    claferIGArgs' <- getClaferIGArgs
    let 
        useUids' = useUids claferIGArgs'
        addTypes' = addTypes claferIGArgs'
    constraints' <- getConstraints
    info' <- getInfo
    xmlSolution <- ClaferIGT $ lift AlloyIG.sendNextCommand
    sMap <- getStrMap
    case xmlSolution of
        Just xml -> return $ Instance (xmlToModel useUids' addTypes' info' xml sMap) xml
        Nothing  -> do
            ClaferIGT $ lift AlloyIG.sendSaveStateCommand
            -- Generating counterexample modifies the state. If we ever want to
            -- rerun the original model, we need to restore the state.
            AlloyIG.UnsatCore core <- ClaferIGT $ lift AlloyIG.sendUnsatCoreCommand
            c <- counterexample core useUids' addTypes' info' constraints' sMap
            ClaferIGT $ lift AlloyIG.sendRestoreStateCommand
            return c
    where
    counterexample originalCore useUids' addTypes' info' constraints' sMap = counterexample' originalCore [] useUids' addTypes' info' constraints' sMap
        where
        counterexample' core removed useUids' addTypes' info' constraints' sMap =
            case msum $ findRemovable core constraints' of
                Just remove -> do
                    ClaferIGT $ lift $ AlloyIG.sendRemoveConstraintCommand $ range remove
                    ClaferIGT $ lift AlloyIG.sendResolveCommand
                    xmlSolution <- ClaferIGT $ lift AlloyIG.sendNextCommand
                    case xmlSolution of
                        Just xml -> return $
                            UnsatCore (catMaybes $ findRemovable core constraints') (Just $ Counterexample (reverse $ remove : removed) (xmlToModel useUids' addTypes' info' xml sMap) xml)
                        Nothing ->
                            do
                                AlloyIG.UnsatCore core' <- ClaferIGT $ lift AlloyIG.sendUnsatCoreCommand
                                counterexample' core' (remove : removed) useUids' addTypes' info' constraints' sMap
                Nothing -> -- It is possible that none of the constraints are removable
                    return NoInstance

    findRemovable core constraints' = [find ((== c). range) constraints' | c <- core]
    
    xmlToModel :: Bool -> Bool -> Analysis.Info -> String -> (Map Int String) -> ClaferModel
    xmlToModel  useUids' addTypes' info' xml sMap = (sugarClaferModel useUids' addTypes' (Just info') $ buildClaferModel $ parseSolution xml) sMap

reload :: MonadIO m => ClaferIGT m (Either ClaferErrs ())
reload  =
    runErrorT $ do
        globalScope <- lift $ getGlobalScope
        claferIGArgs' <- lift $ getClaferIGArgs
        env <- ErrorT $ ClaferIGT $ lift $ load claferIGArgs'
        lift $ set env
        lift $ setGlobalScope globalScope
        
        
setUnsatCoreMinimization :: MonadIO m => Integer -> ClaferIGT m ()
setUnsatCoreMinimization level = ClaferIGT $ lift $ AlloyIG.sendSetUnsatCoreMinimizationCommand level


setBitwidth :: MonadIO m => Integer -> ClaferIGT m ()
setBitwidth bitwidth = ClaferIGT $ lift $ AlloyIG.sendSetBitwidthCommand bitwidth

    
quit :: MonadIO m => ClaferIGT m ()
quit = ClaferIGT $ lift AlloyIG.sendQuitCommand
    
    
sigToClaferName :: String -> String
sigToClaferName n =
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
