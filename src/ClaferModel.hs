module ClaferModel (ClaferModel, Clafer(..), buildClaferModel) where

import Data.List 
import Data.Map as Map hiding (map)
import Data.Maybe
import Data.Set as Set hiding (map)
import Solution


data ClaferModel = ClaferModel {c_topLevel::[Clafer]}
data Clafer = Clafer {c_name::String, c_children::[Clafer]} | IntClafer {c_value::Int}

data FamilyTree = FamilyTree {roots::Set String, descendants::Map String [String]} deriving Show


instance Show ClaferModel where
    show (ClaferModel clafers) = concatMap show clafers

instance Show Clafer where
    show clafer = displayClafer "" clafer
        where
        displayClafer indent (Clafer name children) =
            indent ++ name ++ "\n" ++ concatMap (displayClafer (indent ++ "  ")) children
        displayClafer indent (IntClafer value) =
            indent ++ "[this = " ++ (show value) ++ "]\n"
            

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
    buildTuples (t:ts) = addChild (t_from t) (t_to t) $ buildTuples ts


-- A map of label -> Sig
buildSigMap :: Solution -> Map String Sig
buildSigMap (Solution sigs _) = Map.fromList $ zip (map s_label sigs) sigs


-- A map of label -> ID
buildTypeMap :: Solution -> Map String Int
buildTypeMap (Solution _ fields) =
    buildTuples tuples
    where
    tuples = concatMap f_tuples fields
    buildTuples :: [Tuple] -> Map String Int
    buildTuples [] = Map.empty
    buildTuples (t:ts) = Map.insert (t_from t) (t_fromType t) $ Map.insert (t_to t) (t_toType t) (buildTuples ts)


buildClaferModel :: Solution -> ClaferModel
buildClaferModel solution =
    ClaferModel $ map buildClafer (getRoots ftree)
    where
    sigMap = buildSigMap solution
    typeMap = buildTypeMap solution
    ftree = buildFamilyTree solution
    
    intId = s_id $ findWithDefault (error "Missing Int sig") "Int" sigMap
    isInt label = (findWithDefault (error $ "Missing label " ++ label) label typeMap) == intId
    
    buildClafer label = 
        if isInt label then
            IntClafer $ read label
        else
            Clafer (simpleName label) $ map buildClafer (getChildren label ftree)
    

-- Find the sig with the given label
findSig :: String -> Solution -> Sig
findSig label (Solution sigs _) = fromJust $ find ((==) label . s_label) sigs
    

-- Only keeps the substring between the '_' and '$' exclusive.
simpleName :: String -> String
simpleName n =
    case snd $ break ((==) '_') n of
        [] -> fst $ break ((==) '$') n
        x -> fst $ break ((==) '$') $ tail x
