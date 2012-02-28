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

module ClaferIG (ClaferIG(claferModel, claferFile), Scope, claferIGVersion, initClaferIG, alloyModel, solve, getClafers, getGlobalScope, setGlobalScope, getScopes, getScope, nameOfScope, valueOfScope, increaseScope, setScope, next, nextWithAlloyInstance, unsatCore, counterexample, quit) where

import qualified AlloyIGInterface as AlloyIG
import ClaferModel
import Control.Monad
import Data.Map as Map hiding (map)
import Process
import Solution
import Sugarer
import System.Exit
import Version


data ClaferIG = ClaferIG{claferModel::String, claferFile::FilePath, claferToSigNameMap::Map String String, alloyIG::AlloyIG.AlloyIG}
data Scope = Scope {name::String, sigName::String, claferIG::ClaferIG}


claferIGVersion =
    "ClaferIG " ++ version
    
    
initClaferIG :: FilePath -> IO ClaferIG
initClaferIG claferFile = 
    do
        execPath <- executableDirectory
        claferProc <- pipeProcess (execPath ++ "clafer") ["-f", "-o", "-s", claferFile]
        claferOutput <- getContentsVerbatim claferProc
        claferExit <- waitFor claferProc
        when (claferExit /= ExitSuccess) (fail "clafer unexpectedly terminated")
        claferModel <- readFile claferFile
        
        alloyIG <- AlloyIG.initAlloyIG claferOutput
        
        let claferToSigNameMap = fromListWithKey (error . ("Duplicate clafer name " ++)) [(sigToClaferName x, x) | x <- (AlloyIG.sigs alloyIG)]
        
        return $ ClaferIG claferModel claferFile claferToSigNameMap alloyIG


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


next :: ClaferIG -> IO (Maybe ClaferModel)
next claferIG = 
    do
        solution <- nextWithAlloyInstance claferIG
        return $ snd `liftM` solution 


nextWithAlloyInstance :: ClaferIG -> IO (Maybe (String, ClaferModel))
nextWithAlloyInstance ClaferIG{alloyIG = alloyIG} =
    do
        xmlSolution <- AlloyIG.sendNextCommand alloyIG
        return $
            do
                xml <- xmlSolution
                let solution = parseSolution xml
                let claferModel = buildClaferModel solution
                let sugarModel = sugarClaferModel claferModel
                return (xml, sugarModel)


unsatCore :: ClaferIG -> IO AlloyIG.UnsatCore
unsatCore ClaferIG{alloyIG = alloyIG} = AlloyIG.sendUnsatCoreCommand alloyIG


counterexample :: ClaferIG -> IO (Maybe ClaferModel)
counterexample ClaferIG{alloyIG = alloyIG} =
    do
        xmlSolution <- AlloyIG.sendCounterexampleCommand alloyIG
        return $
            do
                xml <- xmlSolution
                let solution = parseSolution xml
                let claferModel = buildClaferModel solution
                let sugarModel = sugarClaferModel claferModel
                return sugarModel


quit :: ClaferIG -> IO ()
quit ClaferIG{alloyIG = alloyIG} = AlloyIG.sendQuitCommand alloyIG
    
    
sigToClaferName :: String -> String
sigToClaferName n =
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
