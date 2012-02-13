module ClaferModel (ClaferModel, Clafer(..), buildClaferModel) where

import Data.List 
import Data.Map as Map hiding (map)
import Data.Maybe
import Data.Set as Set hiding (map)
import Solution


data ClaferModel = ClaferModel {c_topLevel::[Clafer]}
data Clafer = Clafer {c_name::String, c_children::[Clafer]}

data FamilyTree = FamilyTree {roots::Set String, descendants::Map String [String]} deriving Show


instance Show ClaferModel where
    show (ClaferModel clafers) = concatMap show clafers

instance Show Clafer where
    show clafer = displayClafer "" clafer
        where
        displayClafer indent (Clafer name children) =
            indent ++ name ++ "\n" ++ concatMap (displayClafer (indent ++ "  ")) children
            

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
buildFamilyTree (Solution sigs fields) =
    buildTuples tuples
    where
    tuples = concatMap f_tuples fields
    atoms = concatMap s_atoms sigs
    atomLabels = map a_label atoms
    buildTuples [] = FamilyTree (Set.fromList atomLabels) Map.empty
    buildTuples ((Tuple from to):fs) = addChild from to $ buildTuples fs


buildClaferModel :: Solution -> ClaferModel
buildClaferModel solution =
    ClaferModel $ map buildClafer (getRoots ftree)
    where
    ftree = buildFamilyTree solution
    buildClafer name = Clafer (simpleName name) $ map buildClafer (getChildren name ftree)
    

-- Find the sig with the given label
findSig :: String -> Solution -> Sig
findSig label (Solution sigs _) = fromJust $ find ((==) label . s_label) sigs
    

-- Only keeps the substring between the '_' and '$' exclusive.
simpleName :: String -> String
simpleName n =
    case snd $ break ((==) '_') n of
        [] -> fst $ break ((==) '$') n
        x -> fst $ break ((==) '$') $ tail x
