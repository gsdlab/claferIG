{-
 Copyright (C) 2012-2017 Jimmy Liang, Michal Antkiewicz <http://gsd.uwaterloo.ca>

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

module Language.Clafer.IG.Sugarer (sugarClaferModel) where

import Language.Clafer.Common
import Language.Clafer.IG.ClaferModel
import Data.Maybe (fromMaybe, fromJust)
import Data.List as List hiding (map)
import Data.Map as Map hiding (map, foldr, foldl)
import Prelude hiding (id)


-- | Sample: maps the id to the its simple name and the number of times its simple name appeared in the census before it
-- | Count: maps the simple name to the total count of the simple name
data Census = Census
    (Map Id (Int, String))  -- Sample
    (Map String Int)        -- Counts
 deriving Show


-- | Adds the full name to the census
poll :: Id -> Census -> Census
poll id (Census sample' counts') =
    Census sample'' counts''
    where
    fullName = i_name id
    name = makeSimpleName fullName
    counts'' = insertWith (+) name 1 counts'
    ordinal' = findWithDefault (error $ "Did not find " ++ name ++ " in counts.") name counts''
    sample'' = insertWith (error $ "Polled " ++ fullName ++ " twice in the census.") id (ordinal', name) sample'

-- Transforms c2_name -> name
makeSimpleName :: String -> String
makeSimpleName "" = ""
makeSimpleName "root" = "root"
makeSimpleName name' = case dropWhile (/='_') name' of
    "" ->  "" -- error "Unexpected Clafer name: " ++ name'
    x -> tail x

-- | Count the number of each clafer
claferModelCensus :: ClaferModel -> Census
claferModelCensus (ClaferModel topLevelClafers) =
    clafersCensus (Census Map.empty Map.empty) topLevelClafers
    where
    clafersCensus = foldl claferCensus
    claferCensus census Clafer{c_id=id, c_children=children} = poll id (clafersCensus census children)


-- | Rewrite the model into a human-friendlier format
sugarClaferModel:: Bool -> Bool  -> UIDIClaferMap -> ClaferModel                      -> Map Int String -> ClaferModel
sugarClaferModel   useUids addTypes uidIClaferMap'   model@(ClaferModel topLevelClafers) sMap            =
    ClaferModel $ map sugarClafer topLevelClafers
    where
    sugarClafer (Clafer id value children) =
        Clafer (sugarId useUids addTypes True id) (sugarValue (Clafer id value children)) (map sugarClafer children)

    sugarValue (Clafer _ (Just (AliasValue alias)) _) = Just $ AliasValue $ sugarId useUids addTypes False alias
    sugarValue (Clafer _ Nothing _) = Nothing
    sugarValue c  = if cType c == "string" then Just (StringValue (getString c)) else c_value c


    cType (Clafer id _ _) = cTypeSolve $ getReference iclafer
      where
        iclafer = fromMaybe (error $ "IClafer not found:" ++ i_name id) $ findIClafer uidIClaferMap' $ i_name id

    cTypeSolve ["string"]  = "string"
    cTypeSolve ["integer"] = "integer"
    cTypeSolve ["int"]     = "integer"
    cTypeSolve ["real"]    = "real"
    cTypeSolve _           = ""

    getString c = case Map.lookup strNumber sMap of
        Nothing -> "\"<text " ++ show strNumber ++ ">\""
        Just s -> s
        where strNumber = v_value  $ fromJust  $ c_value c


    Census sample' counts' = claferModelCensus model

    sugarId :: Bool -> Bool  -> Bool    -> Id -> Id
    sugarId    useUids' addTypes' addRefDecl id  =
        Id (finalName ++ ordinalDisplay ++ refDecl addTypes' addRefDecl uidIClaferMap') 0
        where
        fullName = i_name id
        ordinalDisplay = if useUids || count > 1
                         then "$" ++ show ordinal
                         else ""

        refDecl :: Bool -> Bool ->  UIDIClaferMap -> String
        refDecl    True    True    uidIClaferMap'' = retrieveSuper useUids' uidIClaferMap'' $ i_name id
        refDecl    _       _       _               = ""

        (ordinal, simpleName) = findWithDefault (error $ "Sample lookup " ++ show id ++ " failed.") id sample'
        count = findWithDefault (error $ "Count lookup " ++ simpleName ++ " failed.") simpleName counts'
        finalName = if useUids' then fullName else simpleName

retrieveSuper :: Bool -> UIDIClaferMap -> String -> String
retrieveSuper useUids' uidIClaferMap'      uid =
    sugarSuper (getSuper iclafer)
    -- ++
    -- sugarReference  (getReference iclafer)
    where
        iclafer = fromMaybe (error $ "IClafer not found: " ++ uid) $ findIClafer uidIClaferMap' uid

        sugarSuper :: [String] -> String
        sugarSuper [s] = " : " ++ if useUids' then s else makeSimpleName s
        sugarSuper _   = ""
        -- sugarReference :: [String] -> String
        -- sugarReference [s] = " -> " ++ s
        -- sugarReference _   = ""
