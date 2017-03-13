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
import Language.Clafer.IG.ClaferIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.CommandLine
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import Prelude hiding (all)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

tg_testsuite :: TestTree
tg_testsuite = $(testGroupGenerator)

main :: IO ()
main = defaultMain $ testGroup "Tests" [ tg_testsuite ]

claferIGArgsDef :: IGArgs
claferIGArgsDef = IGArgs {
    all             = def,
    saveDir         = def,
    claferModelFile = def,
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

case_pickLargerScope :: Assertion
case_pickLargerScope = do
    let
        oldScopes = [ ("c0_1", 1), ("c1_b", 2), ("c0_x", 5) ]
        newScopes = [ ("c0_1", 2), ("c0_b", 2), ("c1_b", 1)]
        mergedScopes = map (pickLargerScope oldScopes) newScopes

    mergedScopes @?= [ ("c0_1", 2), ("c0_b", 2), ("c1_b", 2)]


