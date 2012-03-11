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

module Constraints where

import Control.Monad
import Debug.Trace
import Data.List
import Data.Maybe
import Text.XML.HaXml hiding (find)
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Posn



data Constraint =
    ParentConstraint {span::Span} |
    ExactCardinalityConstraint {span::Span, claferInfo::ClaferInfo} |
    LowerCardinalityConstraint {span::Span, claferInfo::ClaferInfo} |
    UpperCardinalityConstraint {span::Span, claferInfo::ClaferInfo} |
    UserConstraint {span::Span, constraintInfo::ConstraintInfo}
    deriving Show


data Cardinality = Cardinality {lower::Integer, upper::Maybe Integer}

instance Show Cardinality where
    show (Cardinality lower Nothing) = show lower ++ "..*"
    show (Cardinality lower (Just upper)) = show lower ++ ".." ++ show upper


data ClaferInfo = ClaferInfo {uniqueId::String, cardinality::Cardinality} deriving Show


data ConstraintInfo = ConstraintInfo {pId::String, syntax::String} deriving Show


data Position = Position {line::Integer, column::Integer} deriving Show


type Span = (Position, Position)



qType = QN (Namespace "xsi" "http://www.w3.org/2001/XMLSchema-instance") "type"


typed name = attrval (qType, AttValue [Left name])


parseConstraints :: String -> String -> [Constraint]
parseConstraints ir mapping =
    map buildConstraint mapping'
    where
    (claferInfos, constraintInfos) = parseIr ir
    mapping' = parseMapping mapping
    
    findClafer name =
        fromMaybe
            (error $ name ++ " not in " ++ show (map uniqueId claferInfos))
            (find ((== name) . uniqueId) claferInfos)
            
    findConstraint name =
        fromMaybe
            (error $ name ++ " not in " ++ show (map pId constraintInfos))
            (find ((== name) . pId) constraintInfos)

    exactCardinalityConstraint (source, span) =
        do
            claferId <- stripPrefix "Cardinality exact " source
            return $ ExactCardinalityConstraint span (findClafer claferId)

    lowerCardinalityConstraint (source, span) =
        do
            claferId <- stripPrefix "Cardinality lower " source
            return $ LowerCardinalityConstraint span (findClafer claferId)
            
    upperCardinalityConstraint (source, span) =
        do
            claferId <- stripPrefix "Cardinality upper " source
            return $ UpperCardinalityConstraint span (findClafer claferId)
    
    parentConstraint ("Parent-relationship", span) = Just $ ParentConstraint span
    parentConstraint _ = Nothing
    
    userConstraint (constraintId, span) = Just $ UserConstraint span (findConstraint constraintId)
    
    buildConstraint :: (String, Span) -> Constraint
    buildConstraint x =
        fromMaybe (error $ "Unknown constraint " ++ show x) $
            msum [exactCardinalityConstraint x, lowerCardinalityConstraint x, upperCardinalityConstraint x, parentConstraint x, userConstraint x]


-- Get all the constraint information from the IR
parseIr :: String -> ([ClaferInfo], [ConstraintInfo])
parseIr xml =
    let (Document _ _ root _) = resolveAllNames qualify $ xmlParse "ir" xml
        rootElem    = CElem (xmlUnEscape stdXmlEscaper root) noPos
        -- As far as I can tell, there is no way to resolve an attribute value that is suppose to be a QName.
        -- If the prefix is no "cl" then this does not work!
        decls = declElems rootElem
        claferElems = concatMap (keep `with` typed "cl:IClafer") decls
        constraintElems = concatMap (keep `with` typed "cl:IConstraint") decls
    in
    (map parseIClafer claferElems, concatMap parseIConstraint constraintElems)


p xml =
    let (Document _ _ root _) = resolveAllNames qualify $ xmlParse "ir" xml
        rootElem    = CElem root noPos
    in
    rootElem
    

declElems :: Content i -> [Content i]
declElems content =
    concat [decls, concatMap declElems $ decls]
    where
    decls = (keep /> tag "Declaration") content
    
    
parseIClafer :: Content i -> ClaferInfo 
parseIClafer content =
    ClaferInfo uid $ Cardinality min max
    where
    uid = verbatim $ (keep /> tag "UniqueId" /> txt) content
    min = read $ verbatim $ (keep /> (tag "Card" /> tag "Min" /> tag "IntLiteral" /> txt)) content
    maxAst = (keep /> tag "Card" /> (tag "Max" `with` typed "cl:ExIntegerAst")) content
    maxNum = (keep /> tag "Card" /> (tag "Max" `with` typed "cl:ExIntegerNum") /> tag "IntLiteral" /> txt) content
    max = 
        do
            max' <- if null maxAst then Just maxNum else Nothing
            return $ read $ verbatim max'


parseIConstraint :: Content i -> [ConstraintInfo]
parseIConstraint content =
    concatMap (snd . parsePExp) ((keep /> tag "ParentExp") content)


parsePExp :: Content i -> (String, [ConstraintInfo])
parsePExp content =
    case expType of
        "cl:IFunctionExp"          ->
            let
                operation = verbatim $ (keep /> tag "Operation" /> txt) exp
                arguments = (keep /> tag "Argument") exp
                arguments' = map parsePExp arguments
                
                subConstraints = concatMap snd arguments'
                args = map fst arguments'
                
                format :: String -> [String] -> String
                -- Handle minus as a special case
                format "-" [arg1] = formatUn "-" [arg1]
                format "-" args   = formatBin "-" args
                format op args    = (nary op) op args

                -- Minus is a special case since it can be either negate or subtraction
                nary "-"      = undefined
                nary "!"      = formatUn
                nary "#"      = formatUn
                nary "max"    = formatUn
                nary "min"    = formatUn
                nary "<=>"    = formatBin
                nary "=>"     = formatBin
                nary "||"     = formatBin
                nary "xor"    = formatBin                                
                nary "&&"     = formatBin
                nary "<"      = formatBin
                nary ">"      = formatBin
                nary "="      = formatBin
                nary "<="     = formatBin
                nary ">="     = formatBin
                nary "!="     = formatBin
                nary "in"     = formatBin
                nary "not in" = formatBin
                nary "+"      = formatBin
                nary "*"      = formatBin
                nary "/"      = formatBin
                nary "++"     = formatBin
                nary "--"     = formatBin
                nary "&"      = formatBin
                nary "<:"     = formatBin
                nary ">:"     = formatBin
                nary "."      = formatBin
                nary "=>else" = formatIfElse
                nary op       = error $ "Unknown operator " ++ op
                
                formatUn op [arg1] = op ++ parens arg1
                formatUn op args = error (op ++ show args ++ " is an invalid unary operation")
                
                formatBin op [arg1, arg2] = parens arg1 ++ op ++ parens arg2
                formatBin op args = error (op ++ show args ++ " is an invalid binary operation")
                
                formatIfElse "=>else" [arg1, arg2, arg3] = parens arg1 ++ "=>" ++ parens arg2 ++ "else" ++ parens arg3
                formatIfElse op args = error (op ++ show args ++ " is an invalid ternary operation")
                
                parens s = '(':s ++ ")"
                
                syntax = format operation args
            in
                (syntax, (ConstraintInfo pId syntax):subConstraints)
        
        "cl:IIntExp"               ->
            (verbatim $ (keep /> tag "IntLiteral" /> txt) exp, [])
            
        "cl:IDoubleExp"            ->
            (verbatim $ (keep /> tag "DoubleLiteral" /> txt) exp, [])
            
        "cl:IStringExp"            ->
            (verbatim $ (keep /> tag "StringLiteral" /> txt) exp, [])
            
        "cl:IClaferId"             ->
            (verbatim $ (keep /> tag "Id" /> txt) exp, [])
        
        "cl:IDeclarationParentExp" -> error "TODO: IDeclarationParentExp is not yet supported"
    where
    pId = verbatim $ (keep /> tag "ParentId" /> txt) content
    exp = ((keep /> tag "Exp") content) !! 0
    expType = findAttr qType exp
    
    
findOptAttr :: QName -> Content i -> Maybe String
findOptAttr name elem = show `fmap` lookup name (getAttrs elem)


findAttr :: QName -> Content i -> String
findAttr name elem = show $ fromJust $ lookup name (getAttrs elem)


getAttrs :: Content i -> [Attribute]
getAttrs (CElem (Elem _ attributes _) _) = attributes


parseMapping :: String -> [(String, Span)]
parseMapping text =
    map transform mapping'
    where
    mapping :: [(String , ((Integer, Integer), (Integer, Integer)))]
    mapping = read text
    
    mapping' = filter (not . null . fst) mapping
    
    transform (source, ((line1, column1), (line2, column2))) = (source, (Position line1 column1, Position line2 column2))
