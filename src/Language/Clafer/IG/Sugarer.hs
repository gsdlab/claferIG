{-
 Copyright (C) 2012-2014 Jimmy Liang, Michal Antkiewicz <http://gsd.uwaterloo.ca>

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

import Language.Clafer.IG.ClaferModel
import qualified Language.Clafer.Intermediate.Analysis as Analysis
import Data.Maybe (fromJust)
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
    makeSimpleName name' = case dropWhile (/='_') name' of
        "" ->  error "Unexpected Clafer name " ++ name'
        x -> tail x

-- | Count the number of each clafer
claferModelCensus :: ClaferModel -> Census
claferModelCensus (ClaferModel topLevelClafers) =
    clafersCensus (Census Map.empty Map.empty) topLevelClafers
    where
    clafersCensus = foldl claferCensus
    claferCensus census Clafer{c_id=id, c_children=children} = poll id (clafersCensus census children) 


-- | Rewrite the model into a human-friendlier format
sugarClaferModel:: Bool -> Bool  -> Maybe Analysis.Info -> ClaferModel -> (Map Int String) -> ClaferModel
sugarClaferModel   useUids addTypes info model@(ClaferModel topLevelClafers) sMap =
    ClaferModel $ map sugarClafer topLevelClafers
    where
    sugarClafer (Clafer id value children) = 
        Clafer (sugarId useUids addTypes True id) (sugarValue (Clafer id value children)) (map sugarClafer children)

    sugarValue (Clafer _ (Just (AliasValue alias)) _) = Just $ AliasValue $ sugarId useUids addTypes False alias
    sugarValue (Clafer _ Nothing _) = Nothing
    sugarValue c  = if (cType c) == "string" then (Just ((StringValue) (getString c))) else (c_value c)

    cType (Clafer id _ _) = 
        case (fromJust (Analysis.super (Analysis.runAnalysis (Analysis.claferWithUid (i_name id)) (fromJust info)))) of
            (Analysis.Ref s) -> cTypeSolve s
            (Analysis.Colon s) -> cTypeSolve s
    
    cTypeSolve "string" = "string"
    cTypeSolve "integer" = "integer"
    cTypeSolve "int" = "integer"
    cTypeSolve "real" = "real"
    cTypeSolve x = cType (Clafer (Id x 0) Nothing []) 

    getString c = case (Map.lookup strNumber sMap) of
        Nothing -> "\"<text " ++ show strNumber ++ ">\""
        Just s -> s
        where strNumber = v_value  $ fromJust  $ c_value c
    

    Census sample' counts' = claferModelCensus model
    
    sugarId :: Bool -> Bool  -> Bool    -> Id -> Id
    sugarId    useUids' addTypes' addRefDecl id  =
        if count == 1 
            then Id (finalName ++ (refDecl addTypes' addRefDecl info)) 0
            else Id (finalName ++ "$" ++ show ordinal ++ (refDecl addTypes' addRefDecl info)) 0  
        where
        fullName = i_name id

        refDecl :: Bool -> Bool -> Maybe Analysis.Info -> String
        refDecl    True    True    (Just info')          = retrieveSuper info' $ i_name id
        refDecl    _       _       _                    = ""
        
        (ordinal, simpleName) = findWithDefault (error $ "Sample lookup " ++ show id ++ " failed.") id sample'
        count = findWithDefault (error $ "Count lookup " ++ simpleName ++ " failed.") simpleName counts'
        finalName = if useUids' then fullName else simpleName

retrieveSuper :: Analysis.Info -> String -> String
retrieveSuper info uid = 
    if (Analysis.isBase sclafer)
        then ""
        else maybe "" sugarSuper (Analysis.super sclafer)
    where
        sclafer = Analysis.runAnalysis (Analysis.claferWithUid uid) info

        sugarSuper :: Analysis.SSuper -> String
        sugarSuper (Analysis.Ref s) = " -> " ++ s
        sugarSuper (Analysis.Colon s) = " : " ++ s    
