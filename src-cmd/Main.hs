{-# LANGUAGE DeriveDataTypeable #-}

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

module Main where

import Language.Clafer.IG.ClaferIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.CommandLine
import Language.Clafer.IG.Solution
import Language.Clafer.IG.Sugarer
import Language.Clafer.ClaferArgs
import Language.Clafer.JSONMetaData
import Language.ClaferT
import Language.Clafer.Common
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import qualified Data.Map as Map
import Data.List (partition)
import Data.Maybe
import Data.IORef
import Prelude hiding (all)
import System.Console.CmdArgs
import System.Directory
import System.FilePath

claferIGArgsDef :: IGArgs
claferIGArgsDef = IGArgs {
    all                         = def &= opt "1" &= help "Saves all instances or a counterexample. Reads scopes from a `.cfr.scope` file or uses the provided global scope." &= typ "INTEGER",
    saveDir                     = def &= help "Specify the directory for storing saved files." &= typ "FILE",
    bitwidth                    = 4 &= help "Set the bitwidth for integers." &= typ "INTEGER", -- Default bitwidth is 4.
    maxInt                      = 7 &= help "Set the bitwidth for integers based on the largest required number. Overrides --bitwidth argument." &= typ "INTEGER",
    alloySolution               = def &= help "Convert Alloy solution to a Clafer solution." &= typ "FILE",
    claferModelFile             = def &= argPos 0 &= typ "FILE",
    useUids                     = False &= help "Use unique clafer names in the Clafer solution.",
    addTypes                    = False &= help "Add colon/reference types to the Clafer solution.",
    json                        = False &= help "Render solution as JSON (forces 'addUids').",
    flatten_inheritance_comp    = def &= help "Flatten inheritance during compiling ('alloy' mode only)" &= name "i",
    no_layout_comp              = def &= help "Don't resolve off-side rule layout during compiling" &= name "l",
    check_duplicates_comp       = def &= help "Check duplicated clafer names during compiling"  &= name "c",
    skip_resolver_comp          = def &= help "Skip name resolution during compiling" &= name "f",
    scope_strategy_comp         = Simple &= help "Use scope computation strategy during compiling: none, simple (default), or full." &= name "ss"
} &= summary claferIGVersion &= program "claferig"


main :: IO ()
main =
    do
        args' <- cmdArgs claferIGArgsDef
        let
            bw = bitwidth args'
            mi = maxInt args'
            -- maxInt overrides the bitwidth setting
            args'' = if (mi > allowedMaxInt bw)
                        then args' {bitwidth = requiredBitwidth mi}
                        else args'

        if (not $ null $ alloySolution args'')
            then do
                _ <- runAlloySolution args''
                return ()
            else if (json args'')
                then
                    tryClaferIG (args'' { useUids = True })
                else
                    tryClaferIG args''
    where
    tryClaferIG args3 =
        do
            try <- runClaferIG args3
            case try of
                Right r -> return r
                Left l  -> do
                    mapM_ putStrLn $ printError l
                    putStrLn "Press enter to retry."
                    void getLine
                    tryClaferIG args3

runClaferIG :: IGArgs -> IO (Either ClaferErrs ())
runClaferIG args' =
    runClaferIGT args' $ do
        let claferModelFileName = claferModelFile args'
        cModel <- liftIO $ strictReadFile claferModelFileName
        if null cModel
        then error "Cannot instantiate an empty model."
        else liftIO $ putStrLn "Compiling the Clafer model..."

        oldBw <- getBitwidth
        env <- getClaferEnv
        let ir = fst3 $ fromJust $ cIr env
        scopes <- getScopes
        setBitwidth $ findNecessaryBitwidth ir oldBw $ map snd scopes

        solve
        case all args' of
            Just scope -> do
                -- copied from CommandLine LoadScopes command
                qNameMaps' <- getQNameMaps
                maybeUidScopes <- liftIO $ readCfrScopeFile qNameMaps' claferModelFileName
                case maybeUidScopes of
                    Nothing -> do
                        liftIO $ putStrLn "Using the provided global scope as a `.cfr-scope` file does not exist. Use the command `saveScopes` to create one."
                        setGlobalScope scope
                    Just uidScopes -> do
                        let
                            (globalScopes, normalScopes) = partition (\(uid, _) -> null uid) uidScopes
                            -- from the globalScopes, take the maximum
                            globalScopeVals = map snd globalScopes
                            globalScope = maximum globalScopeVals
                            -- add the "this/" prefix
                            normalScopesAlloy = map (\(uid, scope2) -> ("this/"++uid, scope2)) normalScopes
                        setGlobalScope globalScope
                        mapM_ (\(uid, val) -> setAlloyScope val uid) normalScopesAlloy
                -- end copied

                solve

                counterRef <- liftIO $ newIORef 1

                let saveDirectory = fromMaybe return $ underDirectory `liftM` saveDir args'
                saveAll (savePath claferModelFileName counterRef >>= saveDirectory)
                quit
            Nothing    -> do
                liftIO $ putStrLn "Type 'h' for the list of available REPL commands\n"
                runCommandLine


-- | Convert an Alloy XML file into an instance in Clafer
runAlloySolution :: IGArgs -> IO (Either ClaferErrs ())
runAlloySolution args' =
    runClaferIGT args' $ do
        let claferModelFileName = claferModelFile args'
        cModel <- liftIO $ strictReadFile claferModelFileName
        when (cModel == "") $ error $ "Cannot convert Alloy solution without the Clafer model from which the instance was created.\n"
                                   ++ "Usage: claferIG [OPTIONS] <model.cfr> --alloy-solution=<instance.xml>\n"
        alloyInstance <- liftIO $ strictReadFile $ alloySolution args' -- It's an Alloy XML file in this case
        when (null alloyInstance) $ error $ "Provide an Alloy solution Alloy file name.\n"
                                         ++ "Usage: claferIG [OPTIONS] <model.cfr> --alloy-solution=<instance.xml>\n"
        env <- getClaferEnv
        let (_, genv', _) = fromJust $ cIr env
        let
            sMap = Map.empty
            uidIClaferMap' = uidClaferMap genv'
        liftIO $ putStrLn $ show $ (sugarClaferModel (useUids args') (addTypes args') uidIClaferMap' $ buildClaferModel $ parseSolution alloyInstance) $ sMap

savePath :: FilePath -> IORef Int -> IO FilePath
savePath file' counterRef =
    do
        counter <- readIORef counterRef
        writeIORef counterRef (counter + 1)
        return $ file' ++ "." ++ (show counter) ++ ".data"


underDirectory :: FilePath -> FilePath -> IO FilePath
underDirectory dir file' =
    do
        createDirectoryIfMissing True dir
        return $ joinPath [dir, file']


saveAll :: IO FilePath -> ClaferIGT IO ()
saveAll nextFile =
    do
        file' <- liftIO nextFile
        liftIO $ createDirectoryIfMissing True $ takeDirectory file'
        solution <- next
        case solution of
            Instance{modelInstance = modelInstance'} -> do
                liftIO $ writeFile file' (show modelInstance')
                saveAll nextFile
            _ -> return ()
