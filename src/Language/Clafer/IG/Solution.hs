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

module Language.Clafer.IG.Solution (Solution(..), Sig(..), Atom(..), Field(..), Tuple(..), parseSolution) where

import Control.Monad
import Data.Maybe
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Prelude hiding (id)


data Solution = Solution{s_sigs::[Sig], s_fields::[Field]} deriving (Show, Eq)

-- The univ sig does not have a parent
data Sig = Sig {s_label::String, s_id::Int, s_parentId::Maybe Int, s_atoms::[Atom]} deriving (Show, Eq)

data Atom = Atom {a_label::String} deriving (Show, Eq)

data Field = Field {f_label::String, f_id::Int, f_parentId::Int, f_tuples::[Tuple]} deriving (Show, Eq)

data Tuple = Tuple {t_from::Atom, t_fromType::Int, t_to::Atom, t_toType::Int} deriving (Show, Eq)



parseSolution :: String -> Solution
parseSolution xml =
    let (Document _ _ root _) = xmlParse "claferIG" xml
        rootElem   = CElem root noPos
        sigElems   = tag "alloy" /> tag "instance" /> tag "sig"
        fieldElems = tag "alloy" /> tag "instance" /> tag "field"
        
        sigs = map parseSig $ sigElems rootElem
        fields = map parseField $ fieldElems rootElem
    in
    Solution sigs fields


parseSig :: Content i -> Sig
parseSig content' =
    Sig label id parentId atoms
    where
    parentId = read `liftM` findOptAttr "parentID" content'
    label = findAttr "label" content'
    id = read $ findAttr "ID" content'
    atoms = map parseAtom $ (keep /> tag "atom") content'
    

parseAtom :: Content i -> Atom
parseAtom = Atom . findAttr "label"


parseField :: Content i -> Field
parseField content' =
    Field
        (findAttr "label" content')
        (read $ findAttr "ID" content')
        (read $ findAttr "parentID" content')
        (map parseTuple $ (keep /> tag "tuple") content')
    where
    
    parseType :: Content i -> (Int, Int)
    parseType content'' = 
        (toFromType !! 0, toFromType !! 1)
        where toFromType = map (read . findAttr "ID") $ (keep /> tag "types" /> tag "type") content''

    (fromType, toType) = parseType content'

    parseTuple :: Content i -> Tuple
    parseTuple content''' =
        Tuple (toFrom !! 0) fromType (toFrom !! 1) toType
        where toFrom = map parseAtom $ (keep /> tag "atom") content'''


findOptAttr :: String -> Content i -> Maybe String
findOptAttr name ele = show `liftM` lookup (N name) (getAttrs ele)


findAttr :: String -> Content i -> String
findAttr name ele = fromJust $ show `fmap` lookup (N name) (getAttrs ele)


getAttrs :: Content i -> [Attribute]
getAttrs (CElem (Elem _ attributes _) _) = attributes
getAttrs _ = error "Function getAttrs from Solution was given an invliad argument expected of type (CElem (Elem _ attributes _) _)" -- This should never happen