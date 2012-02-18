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

module ClaferModel (ClaferModel(..), Clafer(..), Value(..), buildClaferModel) where

import Data.List 
import Data.Either
import Data.Map as Map hiding (map, mapMaybe)
import Data.Maybe
import Data.Set as Set hiding (map)
import Solution


data ClaferModel = ClaferModel {c_topLevel::[Clafer]}
data Clafer = Clafer {c_name::String, c_value::Maybe Value, c_children::[Clafer]}
data Value = AliasValue {c_alias::String} | IntValue {v_value::Int} deriving Show

data FamilyTree = FamilyTree {roots::Set Node, descendants::Map String [Node]} deriving Show
data Node = ClaferNode  {n_name::String} | AliasNode {n_name, n_alias::String} deriving (Eq, Ord, Show)

instance Show ClaferModel where
    show (ClaferModel clafers) = concatMap show clafers

instance Show Clafer where
    show clafer = displayClafer "" clafer
        where
        displayClafer indent (Clafer name value children) =
            indent ++ name ++ (maybe "" displayValue value) ++
            "\n" ++ concatMap (displayClafer (indent ++ "  ")) children
        displayValue (AliasValue alias) = " = " ++ alias
        displayValue (IntValue value) = " = " ++ show value
            

addChild :: String -> Node -> FamilyTree -> FamilyTree
addChild parent child (FamilyTree roots descendants) =
    FamilyTree roots' descendants'
    where
    roots' = Set.delete child roots
    descendants' = (insertWith (++) parent [child] descendants)


getChildren :: String -> FamilyTree -> [Node]
getChildren parent (FamilyTree _ descendants) = findWithDefault [] parent descendants


getRoots :: FamilyTree -> [Node]
getRoots = Set.toList . roots 


getAliases :: [Sig] -> Set String
getAliases sigs =
    Set.fromList $ mapMaybe getAlias sigs
    where
    getAlias AliasSig{s_label=label} = Just $ aliasToName label
    getAlias _ = Nothing


buildFamilyTree :: Solution -> FamilyTree
buildFamilyTree (Solution sigs fields) =
    buildFields fields
    where
    asNodes Sig{s_atoms=atoms} = map ClaferNode $ map a_label atoms
    asNodes AliasSig{s_label=label, s_atoms=atoms} = map (AliasNode $ aliasToName label) $ map a_label atoms
    nodes = concatMap asNodes sigs
    aliases = getAliases sigs
    
    buildFields [] = FamilyTree (Set.fromList nodes) Map.empty
    buildFields (f:fs) =
        buildTuples 1 $ f_tuples f
        where
        label = f_label f
        buildTuples ordinal [] = buildFields fs 
        buildTuples ordinal (t:ts) = addChild (t_from t) (buildNode $ t_to t) $ buildTuples (ordinal + 1) ts
            where
            -- Aliases do not have unique labels. Hence we need to append an ordinal to make them unique.
            buildNode = if label `Set.member` aliases then AliasNode $ label ++ "$" ++ show ordinal else ClaferNode


-- A map of label -> Sig
buildSigMap :: Solution -> Map String Sig
buildSigMap (Solution sigs _) = Map.fromList $ zip (map s_label sigs) sigs


-- A map of label -> ID
buildTypeMap :: Solution -> Map String Int
buildTypeMap (Solution sigs fields) =
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
    

buildClaferModel :: Solution -> ClaferModel
buildClaferModel solution =
    ClaferModel $ lefts $ map buildClafer (getRoots ftree)
    where
    
    sigMap = buildSigMap solution
    typeMap = buildTypeMap solution
    ftree = buildFamilyTree solution
    
    intId = s_id $ findWithDefault (error "Missing Int sig") "Int" sigMap
    isInt label = (findWithDefault (error $ "Missing label " ++ label) label typeMap) == intId
    
    singleton [] = Nothing
    singleton [x] = Just x
    singleton xs = error $ "Received more than one value " ++ show xs
    
    buildClafer :: Node -> Either Clafer Value
    buildClafer (ClaferNode name) =
        if isInt name then
            Right $ IntValue (read name)
        else
            Left $ Clafer name (singleton valueChildren) claferChildren
        where
        (claferChildren, valueChildren) = partitionEithers $ map buildClafer children
        children = getChildren name ftree
    buildClafer (AliasNode name alias) =
        Left $ Clafer (simpleAlias name) (Just $ AliasValue alias) []
        

-- Alters an alias name to a normal clafer name.
-- We need to do this step to unify the alloy names.
simpleAlias :: String -> String
simpleAlias n =
    case stripPrefix "r_" n of
        Just x -> x
        Nothing -> error $ "Unexpected alias " ++ n


aliasToName :: String -> String
aliasToName n =
    case stripPrefix "this/" n of
        Just x -> "r_" ++ x
        Nothing -> error $ "Unexpected alias name " ++ n
