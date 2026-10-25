{-# LANGUAGE TemplateHaskell #-}
{-
 Copyright (C) 2012-2017 Luke Brown <http://gsd.uwaterloo.ca>

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
-- Generates instances of the models in test/positive with Alloy. Built only with
-- the model-tests flag, in a checkout of the repository: it needs the models, and
-- alloyIG.jar and alloy6.2.0.jar next to the test executable (see `make test`).
import Language.Clafer.IG.ClaferIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.CommandLine
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Language.Clafer.IG.Constraints
import Data.Maybe
import Data.IORef
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import Prelude hiding (all)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

tg_modeltests :: TestTree
tg_modeltests = $(testGroupGenerator)

main :: IO ()
main = defaultMain $ testGroup "Model tests" [ tg_modeltests ]

claferIGArgsDef :: IGArgs
claferIGArgsDef = IGArgs {
    all             = def,
    saveDir         = def,
    claferModelFile = def,
    imports_comp    = def,
    alloySolution   = def,
    bitwidth        = 4,
    maxInt          = 7,
    useUids         = False,
    addTypes        = False,
    json            = False,
    flatten_inheritance_comp = False,
    no_layout_comp = False,
    check_duplicates_comp = False,
    skip_resolver_comp = False,
    scope_strategy_comp = def
} &= summary claferIGVersion


defaultIGArgs :: FilePath -> IGArgs
--defaultIGArgs fPath = IGArgs Nothing Nothing fPath False 4 False False False
defaultIGArgs fPath = claferIGArgsDef{claferModelFile = fPath}

--getModel :: MonadIO m => FilePath -> ClaferIGT m (Either Language.ClaferT.ClaferErrs Instance)
getModel fPath = runClaferIGT (defaultIGArgs fPath) $ do
    setGlobalScope (fromMaybe 1 $ all $ defaultIGArgs fPath)
    solve
    counterRef <- liftIO $ newIORef 1
    let saveDirectory = fromMaybe return $ underDirectory `liftM` saveDir (defaultIGArgs fPath)
    let nextFile = savePath fPath counterRef >>= saveDirectory
    file <- liftIO nextFile
    liftIO $ createDirectoryIfMissing True $ takeDirectory file
    next
    where
        savePath :: FilePath -> IORef Int -> IO FilePath
        savePath fPath' counterRef =
            do
                counter <- readIORef counterRef
                writeIORef counterRef (counter + 1)
                return $ fPath' ++ "." ++ (show counter) ++ ".data"
        underDirectory :: FilePath -> FilePath -> IO FilePath
        underDirectory dir file =
            do
                createDirectoryIfMissing True dir
                return $ joinPath [dir, file]


fromRight (Right x) = x
fromRight _         = error "fromRight received Left _. Should never happen."


case_strMapCheck :: Assertion
case_strMapCheck = do
        --let claferModel = Right $ Instance (ClaferModel [(Clafer (Id "" 0) (Just (StringValue "")) [])]) ""
        claferModel' <- getModel "test/positive/i220.cfr"
        (valueCheck $ c_value $ head $ c_topLevel $ modelInstance $ fromRight $ claferModel') @? "Mapping Int back to String Failed!"
        where
            valueCheck Nothing = False
            valueCheck (Just (AliasValue _)) = False
            valueCheck (Just (IntValue _)) = False
            valueCheck (Just (StringValue _)) = True

-- A model importing a file with --import in its //# OPTIONS directive; an error in the
-- imported file names it.
case_Imports :: Assertion
case_Imports = do
        dir <- (</> "claferIG-test-imports") <$> getTemporaryDirectory
        createDirectoryIfMissing True dir
        writeFile (dir </> "lib.cfr") "abstract Lib\n    x : integer\n    [ x = 3 ]\n"
        writeFile (dir </> "model.cfr") "//# OPTIONS --import=lib.cfr\nA : Lib\n"
        instance' <- getModel $ dir </> "model.cfr"
        ("x -> 3" `isInfixOf` show (modelInstance $ fromRight instance')) @? "The instance has the imported clafer's value"
        writeFile (dir </> "lib.cfr") "abstract Lib\n    x : integer\n    [ x = y ]\n"
        result <- getModel $ dir </> "model.cfr"
        case result of
            Left errs -> ("lib.cfr:3:11" `isInfixOf` show errs) @? ("The error names the imported file: " ++ show errs)
            Right _   -> assertFailure "The error in the imported file is not reported"

-- The constraints of an unsatisfiable core name the files that they are in.
case_ImportsUnsatCore :: Assertion
case_ImportsUnsatCore = do
        dir <- (</> "claferIG-test-imports") <$> getTemporaryDirectory
        createDirectoryIfMissing True dir
        writeFile (dir </> "lib2.cfr") "B\n    y : integer\n    [ y > 2 ]\n"
        writeFile (dir </> "unsat.cfr") "//# OPTIONS --import=lib2.cfr\n[ B.y < 2 ]\n"
        result <- getModel $ dir </> "unsat.cfr"
        case result of
            Right UnsatCore{unsatConstraints = core} -> do
                let shown = map show [ info | UserConstraint{constraintInfo = info} <- core ]
                any ("y > 2 (" `isPrefixOf`) shown && any ("lib2.cfr:3:7)" `isSuffixOf`) shown @? ("The core names lib2.cfr: " ++ show shown)
                any ("B.y < 2 (" `isPrefixOf`) shown && any ("unsat.cfr:2:3)" `isSuffixOf`) shown @? ("The core names unsat.cfr: " ++ show shown)
            _ -> assertFailure "The model has no unsatisfiable core"
