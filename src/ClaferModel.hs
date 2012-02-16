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

module ClaferModel (ClaferModel, Clafer(..), buildClaferModel) where

import Data.List 
import Data.Either
import Data.Map as Map hiding (map)
import Data.Maybe
import Data.Set as Set hiding (map)
import Debug.Trace
import Solution


data ClaferModel = ClaferModel {c_topLevel::[Clafer]}
data Clafer = Clafer {c_name::String, c_aliases::[String], c_value::Maybe Value, c_children::[Clafer]}
data Value = IntClafer {v_value::Int} deriving Show

data FamilyTree = FamilyTree {roots::Set String, descendants::Map String [String]} deriving Show


instance Show ClaferModel where
    show (ClaferModel clafers) = concatMap show clafers

instance Show Clafer where
    show clafer = displayClafer "" clafer
        where
        displayClafer indent (Clafer name aliases value children) =
            indent ++ name ++ displayAlias aliases ++ displayValue value ++
            "\n" ++ concatMap (displayClafer (indent ++ "  ")) children
        displayAlias [] = ""
        displayAlias as = ',' : intercalate "," as
        displayValue Nothing = ""
        displayValue (Just (IntClafer value)) = " = " ++ show value
            

addChild :: String -> String -> FamilyTree -> FamilyTree
addChild parent child (FamilyTree roots descendants) =
    FamilyTree roots' descendants'
    where
    roots' = Set.delete child roots
    descendants' = (insertWith (++) parent [child] descendants)


getChildren :: String -> FamilyTree -> [String]
getChildren parent (FamilyTree _ descendants) = findWithDefault [] parent descendants


getRoots :: FamilyTree -> [String]
getRoots = Set.toList . roots 


buildFamilyTree :: Solution -> FamilyTree
buildFamilyTree (Solution sigs _ fields) =
    buildTuples tuples
    where
    tuples = concatMap f_tuples fields
    atoms = concatMap s_atoms sigs
    atomLabels = map a_label atoms
    buildTuples [] = FamilyTree (Set.fromList atomLabels) Map.empty
    buildTuples (t:ts) = addChild (t_from t) (t_to t) $ buildTuples ts


-- A map of label -> Sig
buildSigMap :: Solution -> Map String Sig
buildSigMap (Solution sigs _ _) = Map.fromList $ zip (map s_label sigs) sigs


-- A map of label -> ID
buildTypeMap :: Solution -> Map String Int
buildTypeMap (Solution sigs _ fields) =
    mapSigs `Map.union` mapTuples
    where
    buildSig :: Sig -> Map String Int
    buildSig sig = Map.fromList $ zip (map a_label $ s_atoms sig) (repeat $ s_id sig)
    
    buildTuples :: [Tuple] -> Map String Int
    buildTuples [] = Map.empty
    buildTuples (t:ts) =
        Map.insert (t_from t) (t_fromType t) $
        Map.insert (t_to t) (t_toType t) (buildTuples ts)
        
    mapSigs = foldr (Map.union) Map.empty (map buildSig sigs)
    mapTuples = buildTuples $ concatMap f_tuples fields
    

-- A map of aliases
buildAliasMap :: Solution -> Map String [String]
buildAliasMap (Solution _ aliases _) =
    buildAlias aliases
    where
    buildAlias :: [Alias] -> Map String [String]
    buildAlias [] = Map.empty
    buildAlias (a:as) =
        foldr (flip (Map.insertWith (++)) [l_label a]) aa bb
        where 
            aa :: Map String [String]
            aa = (buildAlias as)
            
            bb :: [String]
            bb = (l_alias a)


buildClaferModel :: Solution -> ClaferModel
buildClaferModel solution =
    trace (show $ buildAliasMap solution)
    ClaferModel $ map (left . buildClafer) (getRoots ftree)
    where
    sigMap = buildSigMap solution
    typeMap = buildTypeMap solution
    ftree = buildFamilyTree solution
    aliasMap = buildAliasMap solution
    
    intId = s_id $ findWithDefault (error "Missing Int sig") "Int" sigMap
    isInt label = (findWithDefault (error $ "Missing label " ++ label) label typeMap) == intId
    
    left (Left x) = x
    
    singleton [] = Nothing
    singleton [x] = Just x
    singleton xs = error $ "Received more than one value " ++ show xs
    
    buildClafer :: String -> Either Clafer Value
    buildClafer label = 
        if isInt label then
            Right $ IntClafer (read label)
        else
            Left $ Clafer (simpleName label) (map simpleAlias aliases) (singleton valueChildren) claferChildren
            where
            aliases = Map.findWithDefault [] label aliasMap
            children = map buildClafer (getChildren label ftree)
            (claferChildren, valueChildren) = partitionEithers children
    

-- Only keeps the substring between the '_' and '$' exclusive.
simpleName :: String -> String
simpleName n =
    fst $ break ('$' ==) $
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
        
simpleAlias :: String -> String
simpleAlias n =
    case stripPrefix "this/" n of
        Just x -> simpleName x
        Nothing -> error $ "Unexpected alias name " ++ n
