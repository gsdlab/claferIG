{-# OPTIONS_GHC -XTemplateHaskell #-}
{-
 Copyright (C) 2012-2013 Luke Brown <http://gsd.uwaterloo.ca>

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
import Language.Clafer.IG.Solution
import Language.Clafer.IG.Sugarer
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import Prelude hiding (all)
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import System.Console.Haskeline

tg_testsuite = $(testGroupGenerator)

main = defaultMain[tg_testsuite]

claferIGArgsDef = IGArgs {
    all             = def,
    saveDir         = def,
    bitwidth        = 4,
    alloySolution   = False,
    claferModelFile = def,
    useUids         = False,
    addTypes        = False,
    json            = False 
} &= summary claferIGVersion

defaultIGArgs :: FilePath -> IGArgs
--defaultIGArgs fPath = IGArgs Nothing Nothing fPath False 4 False False False
defaultIGArgs fPath = claferIGArgsDef{claferModelFile = fPath}

--getModel :: FilePath -> ClaferIGT m (Either Language.ClaferT.ClaferErrs Instance)
getModel fPath = runClaferIGT (defaultIGArgs fPath) $ do
	setGlobalScope (fromJust $ all $ defaultIGArgs fPath)
	solve
	counterRef <- liftIO $ newIORef 1
	let saveDirectory = fromMaybe return $ underDirectory `liftM` saveDir (defaultIGArgs fPath)
	let nextFile = savePath fPath counterRef >>= saveDirectory
	file <- liftIO nextFile
	liftIO $ createDirectoryIfMissing True $ takeDirectory file
	next
	where
		savePath :: FilePath -> IORef Int -> IO FilePath
		savePath fPath counterRef =
		    do
		        counter <- readIORef counterRef
		        writeIORef counterRef (counter + 1)
		        return $ fPath ++ "." ++ (show counter) ++ ".data"
		underDirectory :: FilePath -> FilePath -> IO FilePath
		underDirectory dir file =
		    do
		        createDirectoryIfMissing True dir
		        return $ joinPath [dir, file]


fromRight (Right x) = x


case_strMapCheck :: Assertion
case_strMapCheck = do
		--let claferModel = Right $ Instance (ClaferModel [(Clafer (Id "" 0) (Just (StringValue "")) [])]) ""
		claferModel <- getModel "suite/i220.cfr"
		(valueCheck $ c_value $ head $ c_topLevel $ modelInstance $ fromRight $ claferModel) @? "Mapping Int back to String Failed!"
		where
			valueCheck Nothing = False
			valueCheck (Just (AliasValue _)) = False
			valueCheck (Just (IntValue _)) = False
			valueCheck (Just (StringValue _)) = True
			



