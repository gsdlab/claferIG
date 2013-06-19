{-# LANGUAGE NamedFieldPuns #-}

{-
 Copyright (C) 2012-2013 Jimmy Liang <http://gsd.uwaterloo.ca>

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

module Language.Clafer.IG.Constraints (Constraint(..), Cardinality(..), ClaferInfo(..), ConstraintInfo(..), isLowerCardinalityConstraint, isUpperCardinalityConstraint, lookupConstraint, parseConstraints)  where

import qualified Language.Clafer.IG.AlloyIGInterface as AlloyIG
import Data.List
import Data.Maybe
import Data.Ord
import Language.Clafer
import Language.Clafer.Front.Absclafer (Span(..), Pos(..))
import Language.Clafer.Intermediate.Intclafer
import qualified Language.Clafer.Intermediate.Intclafer as I



data Constraint =
    ExactCardinalityConstraint {range::Span, claferInfo::ClaferInfo} |
    LowerCardinalityConstraint {range::Span, claferInfo::ClaferInfo} |
    UpperCardinalityConstraint {range::Span, claferInfo::ClaferInfo} |
    UserConstraint {range::Span, constraintInfo::ConstraintInfo}
    deriving Show


data Cardinality = Cardinality {lower::Integer, upper::Maybe Integer}

instance Show Cardinality where
    show (Cardinality 0 Nothing) = "*"
    show (Cardinality 1 Nothing) = "+"
    show (Cardinality lower Nothing) = show lower ++ "..*"
    show (Cardinality 0 (Just 1)) = "?"
    show (Cardinality lower (Just upper))
        | lower == upper = show lower
        | otherwise      = show lower ++ ".." ++ show upper


data ClaferInfo = ClaferInfo {uniqueId::String, cardinality::Cardinality}

instance Show ClaferInfo where
    show (ClaferInfo uniqueId cardinality) = uniqueId ++ " " ++ show cardinality


data ConstraintInfo = ConstraintInfo {pId::String, pos::Span, syntax::String}

instance Show ConstraintInfo where
    show ConstraintInfo{pos = Span (Pos l c) _, syntax} = syntax ++ " (line " ++ show l ++ ", column " ++ show c ++ ")"
    


isLowerCardinalityConstraint :: Constraint -> Bool
isLowerCardinalityConstraint LowerCardinalityConstraint{} = True
isLowerCardinalityConstraint _ = False


isUpperCardinalityConstraint :: Constraint -> Bool
isUpperCardinalityConstraint UpperCardinalityConstraint{} = True
isUpperCardinalityConstraint _ = False


to :: Span -> Pos
to (Span _ t) = t
from :: Span -> Pos
from (Span f _) = f


lookupConstraint :: Span -> [Constraint] -> Constraint
lookupConstraint constraint constraints =
    case [x | x <- constraints, constraint == range x] of
        [] -> error $ show constraint ++ " not equal to known constraints " ++ show constraints
        cs -> minimumBy (comparing $ to . range) cs


parseConstraints :: String -> IModule -> [(Span, IrTrace)] -> [Constraint]
parseConstraints claferModel imodule mapping =
    mapMaybe (uncurry convert) mapping
    where
    clafers = mDecls imodule >>= subclafers
    pexps = (mapMaybe constraint $ mDecls imodule ++ concatMap elements clafers) >>= subexpressions
    convert span IrPExp{pUid} =
        Just $ UserConstraint span $ ConstraintInfo pUid (inPos $ findPExp pUid) $ extract $ inPos $ findPExp pUid
    convert span LowerCard{pUid, isGroup = False} =
        Just $ LowerCardinalityConstraint span $ claferInfo pUid 
    convert span UpperCard{pUid, isGroup = False} =
        Just $ UpperCardinalityConstraint span $ claferInfo pUid
    convert span ExactCard{pUid, isGroup = False} =
        Just $ ExactCardinalityConstraint span $ claferInfo pUid
    convert _ _ = Nothing
    
    findPExp pUid   = fromMaybe (error $ "Unknown constraint " ++ pUid) $ find ((== pUid) . pid) pexps
    findClafer pUid = fromMaybe (error $ "Unknown clafer " ++ pUid) $ find ((== pUid) . uid) clafers
    text = lines claferModel
    extract s@(Span (Pos l1 c1) (Pos l2 c2))
        | l1 == l2  = drop (fromInteger $ c1 - 1) $ take (fromInteger $ c2 - 1) $ text !! (fromInteger $ l1 - 1)
        | otherwise = unlines $ f1 : fs ++ [fn]
        where
        f1 = drop (fromInteger $ c1 - 1) $ text !! (fromInteger $ l1 - 1)
        fs = map (text !!) [(fromInteger $ l1) .. (fromInteger $ l2 - 2)]
        fn = take (fromInteger $ c2 - 1) $ text !! (fromInteger $ l2 - 1)
    convertCard (l, -1) = Cardinality l Nothing
    convertCard (l, h)  = Cardinality l $ Just h
    claferInfo pUid =
        ClaferInfo (ident clafer) $ convertCard $ fromJust $ card clafer
        where
        clafer = findClafer pUid


subclafers :: IElement -> [IClafer]
subclafers (IEClafer clafer) = clafer : (elements clafer >>= subclafers)
subclafers _ = []

constraint :: IElement -> Maybe PExp
constraint (IEConstraint _ pexp) = Just pexp
constraint _ = Nothing

subexpressions :: PExp -> [PExp]
subexpressions p@PExp{I.exp} =
    p : subexpressions' exp
    where
    subexpressions' IDeclPExp{oDecls, bpexp} =
        concatMap (subexpressions . body) oDecls ++ subexpressions bpexp
    subexpressions' IFunExp{exps} = concatMap subexpressions exps
    subexpressions' _ = []
