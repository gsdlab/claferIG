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

module ClaferIG (ClaferIG(claferFile), Scope, Instance(..), Counterexample(..), claferIGVersion, getClaferModel, initClaferIG, getAlloyModel, solve, getClafers, getGlobalScope, setGlobalScope, getScopes, getScope, nameOfScope, valueOfScope, increaseScope, setScope, next, setUnsatCoreMinimization, setBitwidth, quit, reload) where

import qualified AlloyIGInterface as AlloyIG
import ClaferModel
import Constraints
import Control.Monad
import Data.IORef
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


data ClaferIG = ClaferIG{claferFile::FilePath, constraints::IORef [Constraint], claferModel::IORef String, claferToSigNameMap::IORef (Map String String), alloyIG::IORef AlloyIG.AlloyIG}


data Scope = Scope {name::String, sigName::String, claferIG::ClaferIG}


data Instance =
    Instance {modelInstance::ClaferModel, alloyModelInstance::String} |
    UnsatCore {unsatConstraints::[Constraint], counterexample::Maybe Counterexample} |
    NoInstance
    

data Counterexample = Counterexample {removedConstraints::[Constraint], counterexampleInstance::ClaferModel, counterexampleAlloyInstance::String}



claferIGVersion =
    "ClaferIG " ++ version
    

getClaferModel :: ClaferIG -> IO String
getClaferModel ClaferIG{claferModel = claferModel} = readIORef claferModel


initClaferIG :: FilePath -> IO ClaferIG
initClaferIG claferFile = 
    do
        (constraints, claferModel, claferToSigNameMap, alloyIG) <- load claferFile
        
        constraintsRef <- newIORef constraints
        claferModelRef <- newIORef claferModel
        claferToSigNameMapRef <- newIORef claferToSigNameMap
        alloyIGRef <- newIORef alloyIG
        
        return $ ClaferIG claferFile constraintsRef claferModelRef claferToSigNameMapRef alloyIGRef
        

load :: String -> IO ([Constraint], String, Map String String, AlloyIG.AlloyIG)
load claferFile =
    do
        alloyModel  <- callClaferTranslator ["-o", "-s", claferFile]
        ir          <- callClaferTranslator ["-o", "-s", "-m", "xml", "-a", claferFile]
        
        let mappingFile = replaceExtension claferFile "map"
        
        mapping     <- strictReadFile mappingFile
        claferModel <- strictReadFile claferFile
        
        removeFile mappingFile
        
        let constraints = parseConstraints ir mapping
        
        alloyIG <- AlloyIG.initAlloyIG alloyModel
        AlloyIG.sendLoadCommand alloyModel alloyIG
        
        sigs <- AlloyIG.getSigs alloyIG
        let claferToSigNameMap = fromListWithKey (error . ("Duplicate clafer name " ++)) [(sigToClaferName x, x) | x <- sigs]
        
        return (constraints, claferModel, claferToSigNameMap, alloyIG)


callClaferTranslator :: [String] -> IO String    
callClaferTranslator args =
    do
        execPath <- executableDirectory
        claferProc <- pipeProcess (execPath ++ "clafer") args
        claferOutput <- getContentsVerbatim claferProc
        claferExit <- waitFor claferProc
        when (claferExit /= ExitSuccess) (fail "clafer unexpectedly terminated")
        return claferOutput
                

strictReadFile :: FilePath -> IO String 
strictReadFile filePath = 
    do
        contents <- readFile filePath
        -- readFile is lazy. Force it to evaluate by mapping over everything doing nothing
        mapM_ return contents
        return contents


getAlloyModel :: ClaferIG -> IO String
getAlloyModel ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.getAlloyModel alloyIG'


solve :: ClaferIG -> IO ()
solve ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.sendResolveCommand alloyIG'

 
getClafers :: ClaferIG -> IO [String]
getClafers ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        sigs <- AlloyIG.getSigs alloyIG'
        return $ map sigToClaferName sigs


getGlobalScope :: ClaferIG -> IO Integer
getGlobalScope ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.getGlobalScope alloyIG'


setGlobalScope :: Integer -> ClaferIG -> IO ()
setGlobalScope scope ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.sendSetGlobalScopeCommand scope alloyIG'


getScopes :: ClaferIG -> IO [Scope]
getScopes claferIG@ClaferIG{alloyIG = alloyIG} = 
    do
        alloyIG' <- readIORef alloyIG
        scopes <- AlloyIG.getScopes alloyIG'
        return $ [Scope (sigToClaferName sig) sig claferIG | (sig, scope) <- scopes]
        
        
getScope :: String -> ClaferIG -> IO (Maybe Scope)
getScope name claferIG@ClaferIG{claferToSigNameMap = claferToSigNameMap} =
    do
        claferToSigNameMap' <- readIORef claferToSigNameMap
        return $
            do
                sigName <- name `Map.lookup` claferToSigNameMap'
                return $Scope name sigName claferIG
        

nameOfScope :: Scope -> String
nameOfScope = name


valueOfScope :: Scope -> IO Integer
valueOfScope Scope{sigName = sigName, claferIG = ClaferIG{alloyIG = alloyIG}} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.getScope sigName alloyIG'


increaseScope :: Integer -> Scope -> IO (Maybe String)
increaseScope increment scope =
    do
        value <- valueOfScope scope
        let value' = value + increment
        setScope value' scope
    

setScope :: Integer -> Scope -> IO (Maybe String)
setScope scope Scope{sigName = sigName, claferIG = ClaferIG{alloyIG = alloyIG}} =
    do
        alloyIG' <- readIORef alloyIG
        subset <- AlloyIG.sendSetScopeCommand sigName scope alloyIG'
        return $ sigToClaferName `liftM` subset


next :: ClaferIG -> IO Instance
next ClaferIG{alloyIG = alloyIG, constraints = constraints} =
    do
        alloyIG' <- readIORef alloyIG
        constraints' <- readIORef constraints
        nextImpl alloyIG' constraints'

nextImpl alloyIG constraints = 
    do
        xmlSolution <- AlloyIG.sendNextCommand alloyIG
        case xmlSolution of
            Just xml -> return $ Instance (xmlToModel xml) xml
            Nothing  -> do
                AlloyIG.sendSaveStateCommand alloyIG
                -- Generating counterexample modifies the state. If we ever want to
                -- rerun the original model, we need to restore the state.
                AlloyIG.UnsatCore core <- AlloyIG.sendUnsatCoreCommand alloyIG
                c <- counterexample core
                AlloyIG.sendRestoreStateCommand alloyIG 
                return c
    where
    counterexample :: [AlloyIG.Constraint] -> IO Instance
    counterexample originalCore =
        if null claferCore then return NoInstance else counterexample' originalCore []
        
        where
        
        -- Give back the original core, (but don't return non removable since that would confuse the user.
        claferCore = [claferConstraint | alloyConstraint <- originalCore, let claferConstraint = lookup alloyConstraint, removable claferConstraint]
        
        counterexample' core removed =
            case null core of
                True  ->
                    return NoInstance
                False ->
                    case findRemovable core of
                        Just remove -> do
                            AlloyIG.sendRemoveConstraintCommand remove alloyIG
                            AlloyIG.sendResolveCommand alloyIG
                            xmlSolution <- AlloyIG.sendNextCommand alloyIG
                            case xmlSolution of
                                Just xml -> return $
                                    UnsatCore claferCore (Just $ Counterexample (reverse $ lookup remove : removed) (xmlToModel xml) xml)
                                Nothing ->
                                    do
                                        AlloyIG.UnsatCore core' <- AlloyIG.sendUnsatCoreCommand alloyIG
                                        counterexample' core' $ lookup remove : removed
                        Nothing -> -- It is possible that none of the constraints are removable
                            return $ UnsatCore claferCore Nothing

    findRemovable core = find (removable . lookup) core
            
    lookup x = lookupConstraint x constraints

    -- Only certain types of constraints can be removed for counterexamples.
    removable SigConstraint{} = False
    removable SubSigConstraint{} = False
    removable ParentConstraint{} = False
    removable InConstraint{} = False
    removable ExtendsConstraint{} = False
    removable _ = True
                
    xmlToModel xml = sugarClaferModel $ buildClaferModel $ parseSolution xml
    
    
reload :: ClaferIG -> IO ()
reload claferIG@(ClaferIG claferFile constraints claferModel claferToSigNameMap alloyIG) =
    do
        globalScope <- getGlobalScope claferIG
        
        quit claferIG
        
        (constraints', claferModel', claferToSigNameMap', alloyIG') <- load claferFile
        
        writeIORef constraints constraints'
        writeIORef claferModel claferModel'
        writeIORef claferToSigNameMap claferToSigNameMap'
        writeIORef alloyIG alloyIG'
        
        setGlobalScope globalScope claferIG
        
        return ()
        
        
setUnsatCoreMinimization :: Integer -> ClaferIG -> IO ()
setUnsatCoreMinimization level ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.sendSetUnsatCoreMinimizationCommand level alloyIG'
        
        
setBitwidth :: Integer -> ClaferIG -> IO ()
setBitwidth bitwidth ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.sendSetBitwidthCommand bitwidth alloyIG'

        
    
quit :: ClaferIG -> IO ()
quit ClaferIG{alloyIG = alloyIG} =
    do
        alloyIG' <- readIORef alloyIG
        AlloyIG.sendQuitCommand alloyIG'
    
    
sigToClaferName :: String -> String
sigToClaferName n =
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
