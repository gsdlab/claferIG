{-
 Copyright (C) 2012-2015 Jimmy Liang, Michal Antkiewicz, Luke Michael Brown <http://gsd.uwaterloo.ca>

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

module Language.Clafer.IG.ClaferModel (ClaferModel(..), Clafer(..), Id(..), Value(..), c_name, buildClaferModel, traverseModel) where

import Data.List
import Data.Either
import Data.Map as Map hiding (filter, map, foldr, singleton)
import Data.Maybe
import Language.Clafer.IG.Solution
import Prelude hiding (id)

-- | Clafer model instance
data ClaferModel = ClaferModel {c_topLevel::[Clafer]}
data Clafer = Clafer {c_id::Id, c_value::Maybe Value, c_children::[Clafer]} deriving Eq
data Value = AliasValue {c_alias::Id} | IntValue {v_value::Int} | StringValue {v_str ::String} deriving (Show, Eq)

c_name :: Clafer -> String
c_name = i_name . c_id

-- | The tuple of name and ordinal must be globally unique
data Id = Id {i_name::String, i_ordinal::Int} deriving (Eq, Ord, Show)

data FamilyTree = FamilyTree (Map Id Node) (Map Id [Node]) deriving Show
data Node = ClaferNode Id Int | ValueNode Id Int deriving (Eq, Ord, Show)

roots :: FamilyTree -> (Map Id Node)
roots (FamilyTree r _) = r

n_id :: Node -> Id
n_id (ClaferNode i _) = i
n_id (ValueNode i _) = i

{-n_type :: Node -> Int
n_type (ClaferNode _ t) = t
n_type (ValueNode _ t) = t-}

instance Show ClaferModel where
    show (ClaferModel clafers) = concatMap show clafers

instance Show Clafer where
    show clafer = displayClafer "" clafer
        where
        displayClafer indent (Clafer id value children) =
            indent ++ i_name id ++ maybe "" displayValue value ++
            "\n" ++ concatMap (displayClafer $ indent ++ "  ") children
        displayValue (AliasValue alias) = " = " ++ i_name alias
        displayValue (IntValue value) = " = " ++ show value
        displayValue (StringValue value) = " = " ++ value



traverseModel :: ClaferModel -> [Clafer]
traverseModel (ClaferModel clafers) =
    traverseClafers clafers
    where
    traverseClafers :: [Clafer] -> [Clafer]
    traverseClafers clafs = concatMap traverseClafer clafs
    traverseClafer :: Clafer -> [Clafer]
    traverseClafer clafer = clafer : traverseClafers (c_children clafer)


addChild :: Id -> Node -> FamilyTree -> FamilyTree
addChild parent child (FamilyTree roots' descens) =
    FamilyTree roots'' descens'
    where
    roots'' =
        case child of
            (ClaferNode id _) -> Map.delete id roots'
            _ -> roots'
    descens' = (insertWith (++) parent [child] descens)


getChildren :: Id -> FamilyTree -> [Node]
getChildren parent (FamilyTree _ descens) = findWithDefault [] parent descens


getRoots :: FamilyTree -> [Node]
getRoots = elems . roots

{-
-- Renames the items in the solution so they are easier to work with
-- <sig label="this/c2_Age">  ->  <sig label="c2_Age">
-- <field label="r_c2_Age">   ->  <field label="c2_Age">
-- <atom label="c5_Bob$0">    ->  <atom label="c5_Bob$0">
renameSolution :: Solution -> Solution
renameSolution (Solution sigs fields) =
    Solution (map renameSig sigs) (map renameField fields)
    where
    renameSig sig = sig{s_label = renameSigLabel $ s_label sig}
    renameSigLabel label
        | label `elem` ["univ", "Int", "seq/Int", "String"] = label
        | otherwise = fromMaybe (error $ "Unexpected sig label " ++ label) $ dropDelimiter '/' label

    renameField field = field{f_label = renameFieldLabel $ f_label field}
    renameFieldLabel "ref" = "ref"
    renameFieldLabel label = fromMaybe (error $ "Unexpected field label " ++ label) $ dropDelimiter '_' label

    dropDelimiter char string =
        case snd $ break (== char) string of
            [] -> Nothing
            x  -> Just $ tail x
-}

buildFamilyTree :: Solution -> FamilyTree
buildFamilyTree (Solution sigs fields) =
    buildFields fields
    where
    asNodes :: Sig -> [Node]
    asNodes Sig{s_id = id, s_atoms = atoms} = map (flip ClaferNode id) $ map (labelAsId . a_label) atoms
    rootNodes :: [(Id, Node)]
    rootNodes = [(n_id rootNode, rootNode) | rootNode <- concatMap asNodes sigs]

    buildFields fields' = foldr buildField (FamilyTree (fromList rootNodes) Map.empty) fields'
    buildField field tree = foldr (uncurry buildTuple) tree (zip ([0,1..]::[Integer]) $ f_tuples field)
        where
        label = f_label field
        {-
         - <field label="Met" ID="9" parentID="5">
         -   <tuple> <atom label="Alice$0"/> <atom label="Bob$0"/> </tuple>
         -   <tuple> <atom label="Bob$0"/> <atom label="Alice$0"/> </tuple>
         -   <types> <type ID="5"/> <type ID="5"/> </types>
         - </field>
         -
         - If Met is an alias then add two alias nodes in the family tree.
         -     addChild (Id "Alice" 0) (AliasNode (Id "Met" 1) (Id "Bob" 0))
         -     addChild (Id "Bob" 0) (AliasNode (Id "Met" 2) (Id "Alice" 0))
         -
         - Otherwise add two Clafer nodes in the family tree.
         -     addChild (Id "Alice" 0) (ClaferNode $ Id "Bob" 0)
         -     addChild (Id "Bob" 0) (ClaferNode $ Id "Alice" 0)
         -}
        buildTuple _ Tuple{t_from = from, t_to = to, t_toType = toType} tree' =
            addChild (labelAsId $ a_label from) (buildNode (labelAsId $ a_label to) toType) tree'
            where
            buildNode = if "_ref" `isSuffixOf` label then ValueNode else ClaferNode

    labelAsId :: String -> Id
    labelAsId label =
        case e of
            [] -> Id label 0
            _  -> Id s (read $ tail e)
        where
        (s, e) = break (== '$') label


-- | A map of label -> Sig
buildSigMap :: Solution -> Map String Sig
buildSigMap (Solution sigs _) = Map.fromList $ zip (map s_label sigs) sigs


buildClaferModel :: Solution -> ClaferModel
buildClaferModel solution =
    ClaferModel $ removeDups [] $ lefts $ map buildClafer (getRoots ftree)
    where
    sigMap = buildSigMap solution
    ftree = buildFamilyTree solution

    removeDups :: [Clafer] -> [Clafer] -> [Clafer]
    removeDups acc [] = acc
    removeDups acc (m:ms) = if (m `elem` ms) then removeDups acc ms
        else removeDups (m{c_children = (removeDups [] $ c_children m)} : acc) ms

    intType = s_id $ findWithDefault (error "Missing Int sig") "Int" sigMap

    singleton [] = Nothing
    singleton [x] = Just x
    singleton xs = error $ "Received more than one value " ++ show xs

    buildClafer :: Node -> Either Clafer Value
    buildClafer (ClaferNode id _) =
        Left $ Clafer id (singleton $ nub valueChildren) claferChildren
        where
        (claferChildren, valueChildren) = partitionEithers $ map buildClafer children
        children = getChildren id ftree
    buildClafer (ValueNode value ntype') =
        if ntype' == intType then
            Right $ IntValue (read name)
        else
            Right $ AliasValue value
        where
            name = i_name value
