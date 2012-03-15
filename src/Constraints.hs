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

module Constraints (Constraint(..), Cardinality(..), ClaferInfo(..), ConstraintInfo(..), isLowerCardinalityConstraint, isUpperCardinalityConstraint, lookupConstraint, parseConstraints)  where

import qualified AlloyIGInterface as AlloyIG
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List hiding (span)
import Data.Maybe
import Data.Ord
import Text.XML.HaXml hiding (find)
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Posn



data Constraint =
    SigConstraint {range::AlloyIG.Constraint} |
    SubSigConstraint {range::AlloyIG.Constraint} |
    ParentConstraint {range::AlloyIG.Constraint} |
    InConstraint {range::AlloyIG.Constraint} |
    ExtendsConstraint {range::AlloyIG.Constraint} |
    ExactCardinalityConstraint {range::AlloyIG.Constraint, claferInfo::ClaferInfo} |
    LowerCardinalityConstraint {range::AlloyIG.Constraint, claferInfo::ClaferInfo} |
    UpperCardinalityConstraint {range::AlloyIG.Constraint, claferInfo::ClaferInfo} |
    UserConstraint {range::AlloyIG.Constraint, constraintInfo::ConstraintInfo}
    deriving Show


data Cardinality = Cardinality {lower::Integer, upper::Maybe Integer}

instance Show Cardinality where
    show (Cardinality 1 Nothing) = "+"
    show (Cardinality lower Nothing) = show lower ++ "..*"
    show (Cardinality 0 (Just 1)) = "?"
    show (Cardinality lower (Just upper))
        | lower == upper = show lower
        | otherwise      = show lower ++ ".." ++ show upper


data ClaferInfo = ClaferInfo {uniqueId::String, cardinality::Cardinality}

instance Show ClaferInfo where
    show (ClaferInfo uniqueId cardinality) = uniqueId ++ " " ++ show cardinality


data ConstraintInfo = ConstraintInfo {pId::String, syntax::String}

instance Show ConstraintInfo where
    show ConstraintInfo{syntax = syntax} = syntax
    
    
type Precidence = Int



isLowerCardinalityConstraint LowerCardinalityConstraint{} = True
isLowerCardinalityConstraint _ = False


isUpperCardinalityConstraint UpperCardinalityConstraint{} = True
isUpperCardinalityConstraint _ = False


lookupConstraint :: AlloyIG.Constraint -> [Constraint] -> Constraint
lookupConstraint constraint constraints =
    case [x | x <- constraints, constraint == range x] of
        [] -> error $ show constraint ++ " not equal to known constraints " ++ show constraints
        cs -> minimumBy (comparing $ AlloyIG.to . range) cs


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

    sigConstraint (source, range) =
        if source == "Sig" then Just $ SigConstraint range
        else Nothing
        
    subSigConstraint (source, range) =
        if source == "SubSig" then Just $ SubSigConstraint range
        else Nothing

    inConstraint (source, range) =
        if source == "In" then Just $ InConstraint range
        else Nothing
        
    extendsConstraint (source, range) =
        if source == "Extends" then Just $ ExtendsConstraint range
        else Nothing

    exactCardinalityConstraint (source, range) =
        do
            claferId <- stripPrefix "Cardinality exact " source
            return $ ExactCardinalityConstraint range (findClafer $ sigToClaferName claferId)

    lowerCardinalityConstraint (source, range) =
        do
            claferId <- stripPrefix "Cardinality lower " source
            return $ LowerCardinalityConstraint range (findClafer $ sigToClaferName claferId)
            
    upperCardinalityConstraint (source, range) =
        do
            claferId <- stripPrefix "Cardinality upper " source
            return $ UpperCardinalityConstraint range (findClafer $ sigToClaferName claferId)
            
    parentConstraint ("Parent-relationship", range) = Just $ ParentConstraint range
    parentConstraint _ = Nothing
    
    userConstraint (constraintId, range) = UserConstraint range (findConstraint constraintId)
    
    buildConstraint :: (String, AlloyIG.Constraint) -> Constraint
    buildConstraint x =
        fromMaybe (userConstraint x) $
            msum [sigConstraint x, subSigConstraint x, inConstraint x, extendsConstraint x, exactCardinalityConstraint x, lowerCardinalityConstraint x, upperCardinalityConstraint x, parentConstraint x]


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
    ClaferInfo (sigToClaferName uid) $ Cardinality min max
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
parseIConstraint content = parsePExp (content `unique` tag "ParentExp")


parsePExp :: Content i -> [ConstraintInfo]
parsePExp content = execState (parsePExp' content) []
parsePExp' :: Content i -> State [ConstraintInfo] (String, Precidence)
parsePExp' content =
    case expType of
        "cl:IFunctionExp"          -> do
            let
                operation = verbatim $ exp `unique` (tag "Operation" /> txt)
                arguments = exp `many` tag "Argument"
            
            arguments' <- (mapM parsePExp' arguments)
            
            let
                format :: String -> [(String, Precidence)] -> (String, Precidence)
                -- Handle minus as a special case
                format "-" [arg1] = formatUn 6 "-" [arg1]
                format "-" args   = formatBin 6 "-" args
                format op args    = nary op args

                nary :: String -> [(String, Precidence)] -> (String, Precidence)
                nary op = nary' op op
                -- Minus is a special case since it can be either negate or subtraction
                nary' "-"      = undefined
                nary' "!"      = formatUn 9
                nary' "#"      = formatUn 9
                nary' "max"    = formatUn 8
                nary' "min"    = formatUn 8
                nary' "<=>"    = formatBin 1
                nary' "=>"     = formatBin 1
                nary' "||"     = formatBin 1
                nary' "xor"    = formatBin 1                              
                nary' "&&"     = formatBin 1
                nary' "<"      = formatBin 2
                nary' ">"      = formatBin 2
                nary' "="      = formatBin 2
                nary' "<="     = formatBin 2
                nary' ">="     = formatBin 2
                nary' "!="     = formatBin 2
                nary' "in"     = formatBin 3
                nary' "not in" = formatBin 3
                nary' "+"      = formatBin 6
                nary' "*"      = formatBin 7
                nary' "/"      = formatBin 7
                nary' "++"     = formatBin 4
                nary' "--"     = formatBin 4
                nary' "&"      = formatBin 4
                nary' "<:"     = formatBin 4
                nary' ">:"     = formatBin 4
                nary' "."      = formatBin 1
                nary' "=>else" = formatIfElse
                nary' op       = error $ "Unknown operator " ++ op
                
                formatUn precidence op [arg1] = (op ++ parens precidence arg1, precidence)
                formatUn precidence op args = error (op ++ show args ++ " is an invalid unary operation")
                
                formatBin precidence op [arg1, arg2] = (parens precidence arg1 ++ op ++ parens precidence arg2, precidence)
                formatBin precidence op args = error (op ++ show args ++ " is an invalid binary operation")
                
                formatIfElse "=>else" [arg1, arg2, arg3] = (parens  0 arg1 ++ "=>" ++ parens 0 arg2 ++ "else" ++ parens 0 arg3, 0)
                formatIfElse op args = error (op ++ show args ++ " is an invalid ternary operation")
                
                parens :: Precidence -> (String, Precidence) -> String
                parens precidence (s, precidence') = if precidence < precidence' then s else '(':s ++ ")"
                
                (syntax, precidence) = format operation arguments'
            
            -- Cons a new ConstraintInfo into the state
            modify $ (ConstraintInfo pId syntax :)
            return (syntax, precidence)
        
        "cl:IIntExp"               ->
            return (verbatim $ exp `unique` (tag "IntLiteral" /> txt), 10)
            
        "cl:IDoubleExp"            ->
            return (verbatim $ exp `unique` (tag "DoubleLiteral" /> txt), 10)
            
        "cl:IStringExp"            ->
            return (verbatim $ exp `unique` (tag "StringLiteral" /> txt), 10)
            
        "cl:IClaferId"             ->
            return (sigToClaferName $ verbatim $ exp `unique` (tag "Id" /> txt), 10)
        
        "cl:IDeclarationParentExp" ->
            let
                quantifier =
                    case findAttr qType $ exp `unique` tag "Quantifier" of
                        "cl:INo"   -> "no"
                        "cl:ILone" -> "lone"
                        "cl:IOne"  -> "one"
                        "cl:ISome" -> "some"
                        "cl:IAll"  -> "all"
                        x          -> error "Unknown quantifier " ++ x
                
                decl = exp `perhaps` tag "Declaration"
                
                parseDeclSyntax =
                    case decl of
                        Just dexp ->
                            do
                                let
                                    disj = if "true" == (verbatim $ dexp `unique` (tag "IsDisjunct" /> txt)) then "disj" else ""
                                    locIds = map verbatim $ dexp `many` (tag "Declaration" /> tag "LocalDeclaration")
                                (body, _) <- lift $ parsePExp' $ dexp `unique` tag "Body"
                                return $ disj ++ " " ++ intercalate " " locIds ++ " : " ++ body                
                        Nothing -> fail "No decl"
            in
                do
                    (bodyParent, _) <- parsePExp' $ exp `unique` tag "BodyParentExp"
                    declSyntax <- runMaybeT parseDeclSyntax
                    let syntax = case declSyntax of
                            Just declSyntax' -> quantifier ++ " " ++ declSyntax' ++ " | " ++ bodyParent
                            Nothing          -> quantifier ++ " " ++ bodyParent
                    -- Cons a new ConstraintInfo into the state
                    modify $ (ConstraintInfo pId syntax :)
                    return (syntax, 0)
    where
    pId = verbatim $ (keep /> tag "ParentId" /> txt) content
    exp = content `unique` tag "Exp"
    expType = findAttr qType exp    
    
    
findOptAttr :: QName -> Content i -> Maybe String
findOptAttr name elem = show `fmap` lookup name (getAttrs elem)


findAttr :: QName -> Content i -> String
findAttr name elem = show $ fromJust $ lookup name (getAttrs elem)


getAttrs :: Content i -> [Attribute]
getAttrs (CElem (Elem _ attributes _) _) = attributes


perhaps :: Content i -> CFilter i -> Maybe (Content i)
perhaps exp y =
    case many exp y of
        [] -> Nothing
        [x] -> Just x
        xs  -> error $ "Expected one or less but got " ++ show (length xs) ++ " elements"        


unique :: Content i -> CFilter i -> Content i
unique exp y =
    case many exp y of
        [x] -> x
        xs  -> error $ "Expected unique but got " ++ show (length xs) ++ " elements"


many :: Content i -> CFilter i -> [Content i]
many exp y = (keep /> y) exp


parseMapping :: String -> [(String, AlloyIG.Constraint)]
parseMapping text =
    map transform mapping'
    where
    mapping :: [(String , ((Integer, Integer), (Integer, Integer)))]
    mapping = read text
    
    mapping' = filter (not . null . fst) mapping
    
    transform (source, ((line1, column1), (line2, column2))) =
        (source,
            AlloyIG.Constraint
                (AlloyIG.Position line1 column1)
                (AlloyIG.Position line2 column2))
                
                
-- This is a duplicate method from ClaferIG.hs
-- TODO: only define the method once
sigToClaferName :: String -> String
sigToClaferName n =
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
