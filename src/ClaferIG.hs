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

module ClaferIG (ClaferIG(claferModel, claferFile), Scope, Instance(..), claferIGVersion, initClaferIG, alloyModel, solve, getClafers, getGlobalScope, setGlobalScope, getScopes, getScope, nameOfScope, valueOfScope, increaseScope, setScope, next, quit) where

import qualified AlloyIGInterface as AlloyIG
import ClaferModel
import Constraints
import Control.Monad
import Data.List
import Data.Map as Map hiding (map, null)
import Data.Maybe
import Process
import Solution
import Sugarer
import System.FilePath
import System.Exit
import Version


data ClaferIG = ClaferIG{constraints::[Constraint], claferModel::String, claferFile::FilePath, claferToSigNameMap::Map String String, alloyIG::AlloyIG.AlloyIG}
data Scope = Scope {name::String, sigName::String, claferIG::ClaferIG}
data Instance =
    Instance {modelInstance::ClaferModel, alloyModelInstance::String} |
    Counterexample {unsatConstraints::[Constraint], removedConstraints::[Constraint], modelInstance::ClaferModel, alloyModelInstance::String} |
    NoInstance


claferIGVersion =
    "ClaferIG " ++ version
    
    
initClaferIG :: FilePath -> IO ClaferIG
initClaferIG claferFile = 
    do
        alloyModel  <- callClaferTranslator ["-o", "-s", claferFile]
        ir          <- callClaferTranslator ["-o", "-s", "-m", "xml", "-a", claferFile]
        mapping     <- strictReadFile $ replaceExtension claferFile "map"
        claferModel <- strictReadFile claferFile
        
        let constraints = parseConstraints ir mapping
        
        alloyIG <- AlloyIG.initAlloyIG alloyModel
        
        let claferToSigNameMap = fromListWithKey (error . ("Duplicate clafer name " ++)) [(sigToClaferName x, x) | x <- (AlloyIG.sigs alloyIG)]
        
        return $ ClaferIG constraints claferModel claferFile claferToSigNameMap alloyIG
        

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


alloyModel = AlloyIG.alloyModel . alloyIG


solve :: ClaferIG -> IO ()
solve claferIG = AlloyIG.sendResolveCommand (alloyIG claferIG)

 
getClafers :: ClaferIG -> [String]
getClafers ClaferIG{alloyIG = alloyIG} = map sigToClaferName (AlloyIG.sigs alloyIG)


getGlobalScope :: ClaferIG -> IO Int
getGlobalScope ClaferIG{alloyIG = alloyIG} = AlloyIG.getGlobalScope alloyIG


setGlobalScope :: Int -> ClaferIG -> IO ()
setGlobalScope scope ClaferIG{alloyIG = alloyIG} = AlloyIG.sendSetGlobalScopeCommand scope alloyIG


getScopes :: ClaferIG -> IO [Scope]
getScopes claferIG = 
    do
        scopes <- AlloyIG.getScopes (alloyIG claferIG)
        return $ [Scope (sigToClaferName sig) sig claferIG | (sig, scope) <- scopes]
        
        
getScope :: String -> ClaferIG -> Maybe Scope
getScope name claferIG =
    case Map.lookup name $ claferToSigNameMap claferIG of
        Just sigName -> Just $ Scope name sigName claferIG
        Nothing -> Nothing
        

nameOfScope :: Scope -> String
nameOfScope = name


valueOfScope :: Scope -> IO Int
valueOfScope Scope{sigName = sigName, claferIG = claferIG} = AlloyIG.getScope sigName (alloyIG claferIG)


increaseScope :: Int -> Scope -> IO (Maybe String)
increaseScope increment scope =
    do
        value <- valueOfScope scope
        let value' = value + increment
        setScope value' scope
    

setScope :: Int -> Scope -> IO (Maybe String)
setScope scope Scope{sigName = sigName, claferIG = claferIG} =
    do
        subset <- AlloyIG.sendSetScopeCommand sigName scope (alloyIG claferIG)
        return $ sigToClaferName `liftM` subset


next :: ClaferIG -> IO Instance
next ClaferIG{alloyIG = alloyIG, constraints = constraints} = 
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
        counterexample' originalCore []
        where
        counterexample' core removed =
            case null core of
                True  ->
                    return NoInstance
                False ->
                    do
                        let remove = findRemovable core
                        AlloyIG.sendRemoveConstraintCommand remove alloyIG
                        AlloyIG.sendResolveCommand alloyIG
                        xmlSolution <- AlloyIG.sendNextCommand alloyIG
                        case xmlSolution of
                            Just xml -> return $
                                Counterexample
                                    -- Give back the original core, (but don't return non removable since that
                                    -- would confuse the user.
                                    [claferCore | alloyCore <- originalCore, let claferCore = lookup alloyCore, removable claferCore]
                                    (reverse $ lookup remove : removed)
                                    (xmlToModel xml)
                                    xml
                            Nothing ->
                                do
                                    AlloyIG.UnsatCore core' <- AlloyIG.sendUnsatCoreCommand alloyIG
                                    counterexample' core' $ lookup remove : removed

    findRemovable core =
        fromMaybe
            -- I don't think this case is ever possible...
            (error $ "Cannot find a removable constraint in the core " ++ show core)
            (find (removable . lookup) core)
            
    lookup x = lookupConstraint x constraints

    -- Only certain types of constraints can be removed for counterexamples.
    removable SigConstraint{} = False
    removable SubSigConstraint{} = False
    removable ParentConstraint{} = False
    removable InConstraint{} = False
    removable ExtendsConstraint{} = False
    removable _ = True
                
    xmlToModel xml = sugarClaferModel $ buildClaferModel $ parseSolution xml
    
    
quit :: ClaferIG -> IO ()
quit ClaferIG{alloyIG = alloyIG} = AlloyIG.sendQuitCommand alloyIG
    
    
sigToClaferName :: String -> String
sigToClaferName n =
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
