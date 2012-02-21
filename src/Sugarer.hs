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

module Sugarer (sugarClaferModel) where

import ClaferModel
import Control.Monad
import Data.List as List hiding (map)
import Data.Map as Map hiding (map)


-- Sample: maps the full name to the its simple name and the number of times its simple name appeared in the census before it
-- Count: maps the simple name to the total count of the simple name
data Census = Census {sample::Map String (Int, String), counts::Map String Int} deriving Show


-- Adds the full name to the census
poll :: String -> Census -> Census
poll fullName (Census sample counts) =
    Census sample' counts'
    where
    name = simpleName fullName
    counts' = insertWith (+) name 1 counts
    ordinal' = findWithDefault (error $ "Did not find " ++ name ++ " in counts.") name counts'
    sample' = insertWith (error $ "Polled " ++ fullName ++ " twice in the census.") fullName (ordinal', name) sample


-- Count the number of each clafer
claferModelCensus :: ClaferModel -> Census
claferModelCensus (ClaferModel topLevel) =
    clafersCensus (Census Map.empty Map.empty) topLevel
    where
    clafersCensus = foldl claferCensus
    claferCensus census Clafer{c_id=id, c_children=children} = poll (i_name id) (clafersCensus census children) 


-- Rewrite the model into a human-friendlier format
sugarClaferModel:: ClaferModel -> ClaferModel
sugarClaferModel model@(ClaferModel topLevel) = model
    {-ClaferModel $ map sugarClafer topLevel
    where
    Census sample counts = claferModelCensus model
    
    sugarName fullName =
        if count == 1 then
            simpleName
        else
            simpleName ++ show ordinal
        where
        (ordinal, simpleName) = findWithDefault (error $ "Sample lookup " ++ fullName ++ " failed.") fullName sample
        count = findWithDefault (error $ "Count lookup " ++ simpleName ++ " failed.") simpleName counts
    
    sugarClafer (Clafer fullName value children) = Clafer (sugarName fullName) (sugarValue `liftM` value) (map sugarClafer children)

    sugarValue (AliasValue alias) = AliasValue $ sugarName alias
    sugarValue x = x-}


-- Only keeps the substring between the '_' and '$' exclusive.
simpleName :: String -> String
simpleName n =
    fst $ break ('$' ==) $
    case snd $ break ('_' ==) n of
        [] ->  n
        x -> tail x
