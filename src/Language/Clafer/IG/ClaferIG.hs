{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}

{-
 Copyright (C) 2012-2014 Jimmy Liang, Michal Antkiewicz <http://gsd.uwaterloo.ca>

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
    claferIGVersion,
    IGArgs(..),
    ClaferIGEnv(..),
    getClaferEnv,
    getClaferIGArgs,
    getConstraints,
    getClaferModel,
    getStrMap,
    getUIDIClaferMap,
    ClaferIGT(..),
    Instance(..),
    Counterexample(..),
    runClaferIGT,
    getAlloyModel,
    solve,
    getClafers,
    getGlobalScope,
    getBitwidth,
    setGlobalScope,
    getScopes,
    getScope,
    getQNameMaps,
    valueOfScope,
    increaseScope,
    setScope,
    setAlloyScope,
    next,
    setUnsatCoreMinimization,
    setBitwidth,
    quit,
    reload,
    findRemovable,
    fst3,
    getlineNumMap,
    strictReadFile,
    sigToClaferName) where

import Language.Clafer
import Language.Clafer.Common
import Language.Clafer.QNameUID
import Language.ClaferT
import Language.Clafer.Intermediate.Intclafer
import Language.Clafer.IG.AlloyIGInterface (AlloyIGT)
import qualified Language.Clafer.IG.AlloyIGInterface as AlloyIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.Constraints
import Language.Clafer.IG.Solution
import Language.Clafer.IG.Sugarer
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Monoid
import Data.Tuple (swap)
import Data.Map as Map hiding (map, null)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Data
import System.Console.Haskeline.MonadException
import Paths_claferIG (version)
import Data.Version (showVersion)

claferIGVersion :: String
claferIGVersion = "ClaferIG " ++ showVersion Paths_claferIG.version

data IGArgs = IGArgs {
    all :: Maybe Integer,
    saveDir :: Maybe FilePath,
    claferModelFile :: FilePath,
    alloySolution :: FilePath,
    bitwidth :: Integer,
    maxInt :: Integer,
    useUids :: Bool,
    addTypes :: Bool,
    json :: Bool,
    flatten_inheritance_comp :: Bool,
    no_layout_comp :: Bool,
    check_duplicates_comp :: Bool,
    skip_resolver_comp :: Bool,
    scope_strategy_comp :: ScopeStrategy

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

getUIDIClaferMap :: Monad m => ClaferIGT m UIDIClaferMap
getUIDIClaferMap = fetches info

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
    claferModel:: String,
    qNameMaps :: QNameMaps,
    info :: UIDIClaferMap,
    strMap :: Map Int String,
    lineNumMap :: Map Integer String
}

data Instance =
    Instance {modelInstance::ClaferModel, alloyModelInstance::String} |
    UnsatCore {unsatConstraints::[Constraint], counterexample::Maybe Counterexample} |
    NoInstance


data Counterexample = Counterexample {removedConstraints::[Constraint], counterexampleInstance::ClaferModel, counterexampleAlloyInstance::String}

getClaferEnv :: Monad m => ClaferIGT m ClaferEnv
getClaferEnv = fetches claferEnv'

getlineNumMap :: Monad m => ClaferIGT m (Map Integer String)
getlineNumMap = fetches lineNumMap

getClaferIGArgs :: Monad m => ClaferIGT m IGArgs
getClaferIGArgs = fetches claferIGArgs

getConstraints :: Monad m => ClaferIGT m [ Constraint ]
getConstraints = fetches constraints

getClaferModel :: Monad m => ClaferIGT m String
getClaferModel = fetches claferModel

getStrMap :: Monad m => ClaferIGT m (Map Int String)
getStrMap = fetches strMap

load :: MonadIO m => IGArgs -> AlloyIGT m (Either ClaferErrs ClaferIGEnv)
load                 igArgs    =
    runErrorT $ do
        claferModel <- liftIO $ strictReadFile claferFile'

        (claferEnv', alloyModel, mapping, sMap) <- ErrorT $ return $ callClaferTranslator claferModel

        let
            (ir, genv', _) = fromJust $ cIr claferEnv'
        let constraints = parseConstraints claferModel ir mapping

        lift $ AlloyIG.sendLoadCommand alloyModel
        lift $ AlloyIG.sendSetBitwidthCommand bitwidth'

        let qNameMaps = deriveQNameMaps ir

        let uidIClaferMap' = uidClaferMap genv'
        let irTrace = editMap $ irModuleTrace claferEnv'

        return $ ClaferIGEnv claferEnv' igArgs constraints claferModel qNameMaps uidIClaferMap' sMap irTrace
    where
    editMap :: (Map.Map Span [Ir]) -> (Map.Map Integer String) -- Map Line Number to Clafer Name
    editMap =
        fromList . removeConstraints . Data.List.foldr (\(num, ir) acc -> case (getIClafer ir) of
            Just (IClafer _ _ _ _ uid' _ _ _ _ _ _) -> (num, uid') : acc
            _ -> acc) [] . tail . (Data.List.foldr (\((Span (Pos l1 _) (Pos l2 _)), irs) acc -> (zip [l1..l2] (replicate (fromIntegral $ l2 - l1 + 1) irs)) ++ acc) []) . toList
    getIClafer :: [Ir] -> Maybe IClafer
    getIClafer [] = Nothing
    getIClafer ((IRClafer c):_) = Just c
    getIClafer (_:rs) = getIClafer rs
    removeConstraints :: [(Integer, String)] -> [(Integer, String)]
    removeConstraints = map swap . reverse . toList . fromList . reverse . map swap

    callClaferTranslator code =
        mapLeft ClaferErrs $ runClafer claferArgs $ do
            addModuleFragment code
            parse
            iModule <- desugar Nothing
            compile iModule
            results <- generate
            let (Just alloyResult) = Map.lookup Alloy42 results
            return (claferEnv alloyResult, outputCode alloyResult, mappingToAlloy alloyResult, stringMap alloyResult)
    mapLeft f (Left l) = Left $ f l
    mapLeft _ (Right r) = Right r
    claferArgs = defaultClaferArgs{mode = [Alloy42], keep_unused = True, no_stats = True, skip_goals = True ,flatten_inheritance = flatten_inheritance_comp igArgs, no_layout = no_layout_comp igArgs, check_duplicates = check_duplicates_comp igArgs, skip_resolver = skip_resolver_comp igArgs, scope_strategy = scope_strategy_comp igArgs}
    claferFile' = claferModelFile igArgs
    bitwidth' = bitwidth igArgs


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


getScopes :: MonadIO m => ClaferIGT m [ (String, Integer) ]
getScopes = ClaferIGT $ lift AlloyIG.getScopes


getScope :: MonadIO m => QName -> ClaferIGT m ([String])
getScope qName = do
        qNameMaps' <- fetches qNameMaps
        return $ getUIDs qNameMaps' qName

getQNameMaps :: MonadIO m => ClaferIGT m (QNameMaps)
getQNameMaps = fetches qNameMaps

valueOfScope :: MonadIO m => String -> ClaferIGT m Integer
valueOfScope sigName = ClaferIGT $ lift $ AlloyIG.getScope sigName


increaseScope :: MonadIO m => Integer -> (String, Integer) -> ClaferIGT m (Either String ())
increaseScope increment (sigName, value) = setAlloyScope (value + increment) sigName

setScope :: MonadIO m => Integer -> (String, Integer) -> ClaferIGT m (Either String ())
setScope value (sigName, _) = setAlloyScope value sigName

setAlloyScope :: MonadIO m => Integer -> String -> ClaferIGT m (Either String ())
setAlloyScope value sigName =
    do
        subset <- ClaferIGT $ lift $ AlloyIG.sendSetScopeCommand sigName value
        runErrorT $ maybe (return ()) throwError $ sigToClaferName <$> subset


next :: MonadIO m => ClaferIGT m Instance
next = do
    env <- getClaferEnv
    claferIGArgs' <- getClaferIGArgs
    uidIClaferMap' <- getUIDIClaferMap
    let
        useUids' = useUids claferIGArgs'
        addTypes' = addTypes claferIGArgs'
    constraints' <- getConstraints
    xmlSolution <- ClaferIGT $ lift AlloyIG.sendNextCommand
    sMap <- getStrMap
    case xmlSolution of
        Just xml -> return $ Instance (xmlToModel useUids' addTypes' uidIClaferMap' xml sMap) xml
        Nothing  -> do
            ClaferIGT $ lift AlloyIG.sendSaveStateCommand
            -- Generating counterexample modifies the state. If we ever want to
            -- rerun the original model, we need to restore the state.
            AlloyIG.UnsatCore core <- ClaferIGT $ lift AlloyIG.sendUnsatCoreCommand
            c <- counterexample env core useUids' addTypes' uidIClaferMap' constraints' sMap
            ClaferIGT $ lift AlloyIG.sendRestoreStateCommand
            return c
    where
    counterexample env' originalCore useUids'' addTypes'' uidIClaferMap'' constraints' sMap = counterexample' env' originalCore [] useUids'' addTypes'' uidIClaferMap'' constraints' sMap
        where
        counterexample' env'' core removed useUids''' addTypes''' uidIClaferMap''' constraints'' sMap' =
            case msum $ findRemovable env'' core constraints'' of
                Just remove -> do
                    ClaferIGT $ lift $ AlloyIG.sendRemoveConstraintCommand $ range remove
                    ClaferIGT $ lift AlloyIG.sendResolveCommand
                    xmlSolution <- ClaferIGT $ lift AlloyIG.sendNextCommand
                    case xmlSolution of
                        Just xml -> return $
                            UnsatCore (catMaybes $ findRemovable env'' core constraints'') (Just $ Counterexample (reverse $ remove : removed) (xmlToModel useUids''' addTypes''' uidIClaferMap''' xml sMap') xml)
                        Nothing ->
                            do
                                AlloyIG.UnsatCore core' <- ClaferIGT $ lift AlloyIG.sendUnsatCoreCommand
                                counterexample' env'' core' (remove : removed) useUids''' addTypes''' uidIClaferMap''' constraints' sMap'
                Nothing -> -- It is possible that none of the constraints are removable
                    return NoInstance

    xmlToModel :: Bool -> Bool -> UIDIClaferMap -> String -> (Map Int String) -> ClaferModel
    xmlToModel  useUids' addTypes' uidIClaferMap' xml sMap = (sugarClaferModel useUids' addTypes' uidIClaferMap' $ buildClaferModel $ parseSolution xml) sMap

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
setBitwidth bitwidth' = do
    claferIGArgs' <- getClaferIGArgs
    igEnv <- fetch
    set igEnv{claferIGArgs = claferIGArgs'{bitwidth=bitwidth'}}
    ClaferIGT $ lift $ AlloyIG.sendSetBitwidthCommand bitwidth'


quit :: MonadIO m => ClaferIGT m ()
quit = ClaferIGT $ lift AlloyIG.sendQuitCommand


sigToClaferName :: String -> String
sigToClaferName n =
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x

findRemovable :: ClaferEnv -> [Span] -> [Constraint] -> [Maybe Constraint]
findRemovable env core constraints' =
    let absIDs = foldMapIR getId $ getIMod $ fromJust $ cIr env
    in  Data.List.filter (removeAbsZero absIDs ) $ map (\c -> find ((== c) . range) constraints') core
    where
        removeAbsZero :: (Seq.Seq String) -> Maybe Constraint -> Bool
        removeAbsZero absIDs (Just (UpperCardinalityConstraint _ (ClaferInfo uID (Cardinality 0 (Just 0))))) = ((Seq.elemIndexL uID absIDs)==Nothing)
        removeAbsZero _ _ = True
        getId :: Ir -> (Seq.Seq String)
        getId (IRClafer (IClafer _ True _ uID _ _ _ _ _ _ _)) = Seq.singleton uID
        getId _ = mempty

getIMod :: (IModule, GEnv, Bool) -> IModule
getIMod (imod, _, _) = imod
