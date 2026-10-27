{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

{-
 Copyright (C) 2012-2017 Jimmy Liang, Michal Antkiewicz <http://gsd.uwaterloo.ca>

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
    getModelFiles,
    getModelFileName,
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
    sigToClaferName,
    claferNameOfSig) where

import Language.Clafer
import Language.Clafer.Common
import Language.Clafer.Import (ModelFile, readModel, modelText, modelPosition)
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
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Monoid
import Data.Tuple (swap)
import Data.Map as Map hiding (map, null)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Data
import Paths_claferIG (version)
import Data.Version (showVersion)
import Prelude

claferIGVersion :: String
claferIGVersion = "ClaferIG " ++ showVersion Paths_claferIG.version

data IGArgs = IGArgs {
    all :: Maybe Integer,
    saveDir :: Maybe FilePath,
    claferModelFile :: FilePath,
    imports_comp :: [FilePath],
    import_root_comp :: [FilePath],
    public_imports_comp :: Bool,
    import_base_comp :: String,
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
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

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
runClaferIGT                 igArgs      run =
    AlloyIG.runAlloyIGT $ runExceptT $ do
        env <- ExceptT (load igArgs) `catchError` (\x -> lift AlloyIG.sendQuitCommand >> throwError x)
        lift $ evalStateT (unwrap run) env
    where
    unwrap (ClaferIGT c) = c


data ClaferIGEnv = ClaferIGEnv{
    claferEnv'::ClaferEnv,
    claferIGArgs :: IGArgs,
    constraints:: [Constraint],
    claferModel:: String,
    modelFiles :: [ModelFile],
    modelFileName :: FilePath,
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

-- | The files of the model: the imported files, then the model.
getModelFiles :: Monad m => ClaferIGT m [ModelFile]
getModelFiles = fetches modelFiles

-- | The name of the model's file, which names the saved files: for a model read from a URL,
--   the last segment of the URL.
getModelFileName :: Monad m => ClaferIGT m FilePath
getModelFileName = fetches modelFileName

getStrMap :: Monad m => ClaferIGT m (Map Int String)
getStrMap = fetches strMap

load :: MonadIO m => IGArgs -> AlloyIGT m (Either ClaferErrs ClaferIGEnv)
load                 igArgs    =
    runExceptT $ do
        when (null claferFile') $ throwError $ inFiles [] $ ClaferErrs [ClaferErr "Provide the file name or URL of a Clafer model."]
        -- the model, the files it imports, and the options of all of them
        (claferArgs', modelFiles') <- ExceptT $ liftIO $ either (Left . inFiles [] . ClaferErrs . (:[]) . ClaferErr) Right <$> readModel claferArgs
        let claferModel = modelText modelFiles'
            -- the integer range of the options (--maxint, --minint) raises the bitwidth
            bitwidth' = maximum $ bitwidth igArgs : map requiredBitwidth (catMaybes [maxint claferArgs', (\n -> -n - 1) <$> minint claferArgs'])

        (claferEnv', alloyModel, mapping, sMap) <- ExceptT $ return $ mapLeft (inFiles modelFiles') $ callClaferTranslator claferArgs' claferModel

        let
            (ir, genv', _) = fromJust $ cIr claferEnv'
        let constraints = parseConstraints modelFiles' ir mapping

        lift $ AlloyIG.sendLoadCommand alloyModel
        lift $ AlloyIG.sendSetBitwidthCommand bitwidth'

        let qNameMaps = deriveQNameMaps ir

        let uidIClaferMap' = uidClaferMap genv'
        let irTrace = editMap $ irModuleTrace claferEnv'

        return $ ClaferIGEnv claferEnv' igArgs{bitwidth = bitwidth'} constraints claferModel modelFiles' (file claferArgs') qNameMaps uidIClaferMap' sMap irTrace
    where
    editMap :: Map.Map Span [Ir] -> Map.Map Integer String -- Map Line Number to Clafer Name
    editMap =
        fromList . removeConstraints . Data.List.foldr (\(num, ir) acc -> case getIClafer ir of
            Just (IClafer _ _ _ _ uid' _ _ _ _ _ _) -> (num, uid') : acc
            _ -> acc) [] . (Data.List.foldr (\((Span (Pos l1 _) (Pos l2 _)), irs) acc -> (zip [l1..l2] (replicate (fromIntegral $ l2 - l1 + 1) irs)) ++ acc) []) . toList
    getIClafer :: [Ir] -> Maybe IClafer
    getIClafer [] = Nothing
    getIClafer ((IRClafer c):_) = Just c
    getIClafer (_:rs) = getIClafer rs
    removeConstraints :: [(Integer, String)] -> [(Integer, String)]
    removeConstraints = map swap . reverse . toList . fromList . reverse . map swap

    callClaferTranslator claferArgs' code =
        mapLeft ClaferErrs $ runClafer claferArgs'{mode = [Alloy], keep_unused = True, no_stats = True} $ do
            addModuleFragment code
            parse
            iModule <- desugar Nothing
            compile iModule
            results <- generate
            case Map.lookup Alloy results of
                Nothing            ->
                    throwErr (ClaferErr "No Alloy compilation result." :: ClaferSErr)
                -- e.g., a model with decimals, reals or doubles, which Alloy does not support
                (Just NoCompilerResult{reason = reason'}) ->
                    throwErr (ClaferErr reason' :: ClaferSErr)
                (Just alloyResult) ->
                    if extension alloyResult == "tmp.als"
                    then throwErr (ClaferErr "Instance generation currently not supported for behavioral Clafer.\nRemove temporal constructs or press <ctrl>+c to exit." :: ClaferSErr)
                    else return (claferEnv alloyResult, outputCode alloyResult, mappingToAlloy alloyResult, stringMap alloyResult)
    mapLeft f (Left l) = Left $ f l
    mapLeft _ (Right r) = Right r
    -- the options given to claferIG take precedence over the files' options
    claferArgs = defaultClaferArgs{file = claferFile', imports = imports_comp igArgs, import_root = import_root_comp igArgs, public_imports = public_imports_comp igArgs, import_base = import_base_comp igArgs, flatten_inheritance = flatten_inheritance_comp igArgs, no_layout = no_layout_comp igArgs, check_duplicates = check_duplicates_comp igArgs, skip_resolver = skip_resolver_comp igArgs, scope_strategy = scope_strategy_comp igArgs}
    claferFile' = claferModelFile igArgs
    -- the bitwidth required to store the given integer
    requiredBitwidth :: Integer -> Integer
    requiredBitwidth n = ceiling $ logBase (2 :: Double) $ fromInteger $ 2 * n + 1

-- | The errors as the compiler reports them, with the file and the position in it:
--   @Parse failed at a.cfr:3:5...@, @Compile error at a.cfr:3:5...@, or @Error...@, then the message.
inFiles :: [ModelFile] -> ClaferErrs -> ClaferErrs
inFiles files (ClaferErrs errs') = ClaferErrs $ map inFile errs'
    where
    inFile (ParseErr ErrPos{modelPos = Pos l c} msg') = ClaferErr $ "Parse failed at " ++ at l c ++ "...\n" ++ msg'
    inFile (SemanticErr ErrPos{modelPos = Pos l c} msg') = ClaferErr $ "Compile error at " ++ at l c ++ "...\n" ++ msg'
    inFile (ClaferErr msg') = ClaferErr $ "Error...\n" ++ msg'
    at l c = let (path, l') = modelPosition files l in path ++ ":" ++ show l' ++ ":" ++ show c


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
        mapM claferNameOfSig sigs


getGlobalScope :: MonadIO m => ClaferIGT m Integer
getGlobalScope = ClaferIGT $ lift AlloyIG.getGlobalScope

getBitwidth :: MonadIO m => ClaferIGT m Integer
getBitwidth =
    do
        bitwidth <$> getClaferIGArgs

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
        subsetName <- traverse claferNameOfSig subset
        runExceptT $ maybe (return ()) throwError subsetName

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

    xmlToModel :: Bool -> Bool -> UIDIClaferMap -> String -> Map Int String -> ClaferModel
    xmlToModel  useUids' addTypes' uidIClaferMap' xml sMap = (sugarClaferModel useUids' addTypes' uidIClaferMap' $ buildClaferModel $ parseSolution xml) sMap

reload :: MonadIO m => ClaferIGT m (Either ClaferErrs ())
reload  =
    runExceptT $ do
        globalScope <- lift getGlobalScope
        claferIGArgs' <- lift getClaferIGArgs
        env <- ExceptT $ ClaferIGT $ lift $ load claferIGArgs'
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


-- | The clafer's name for an Alloy signature (this/c0_name). UIDs are not always
-- cN_name: an apostrophe in a name is replaced in the UID (name' -> c0_name__prime__).
claferNameOfSig :: MonadIO m => String -> ClaferIGT m String
claferNameOfSig sig = do
    uidIClaferMap' <- getUIDIClaferMap
    let uid = fromMaybe sig $ stripPrefix "this/" sig
    return $ maybe (sigToClaferName sig) _ident $ findIClafer uidIClaferMap' uid

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
        removeAbsZero :: Seq.Seq String -> Maybe Constraint -> Bool
        removeAbsZero absIDs (Just (UpperCardinalityConstraint _ (ClaferInfo uID (Cardinality 0 (Just 0))))) = ((Seq.elemIndexL uID absIDs)==Nothing)
        removeAbsZero _ _ = True
        getId :: Ir -> Seq.Seq String
        getId (IRClafer (IClafer _ IClaferModifiers{_abstract=True} _ uID _ _ _ _ _ _ _)) = Seq.singleton uID
        getId _ = mempty

getIMod :: (IModule, GEnv, Bool) -> IModule
getIMod (imod, _, _) = imod
