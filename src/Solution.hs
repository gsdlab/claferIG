module Solution (Solution(..), Sig(..), Atom(..), Field(..), Tuple(..), parseSolution) where

import Control.Monad
import Data.Graph
import Data.Maybe
import Text.XML.HaXml
import Text.XML.HaXml.Posn


data Solution = Solution{s_sigs::[Sig], s_fields::[Field]} deriving Show

-- The univ sig does not have a parent
data Sig = Sig {s_label::String, s_id::Int, s_parentId::Maybe Int, s_atoms::[Atom]} deriving Show

data Atom = Atom {a_label::String} deriving Show

data Field = Field {f_label::String, f_id::Int, f_parentId::Int, f_tuples::[Tuple]} deriving Show

data Tuple = Tuple {t_from::String, t_fromType::Int, t_to::String, t_toType::Int} deriving Show


parseSolution :: String -> Solution
parseSolution xml =
    let (Document _ _ root _) = xmlParse "claferIG" xml
        rootElem   = CElem root noPos
        sigElems   = tag "alloy" /> tag "instance" /> tag "sig"
        fieldElems = tag "alloy" /> tag "instance" /> tag "field"
    in
    Solution (map parseSig $ sigElems rootElem) (map parseField $ fieldElems rootElem)


parseSig :: Content i -> Sig
parseSig content =
    Sig
        (findAttr "label" content)
        (read $ findAttr "ID" content)
        (read `liftM` findOptAttr "parentID" content)
        (map parseAtom $ (keep /> tag "atom") content)
    where
    parseAtom :: Content i -> Atom
    parseAtom = Atom . findAttr "label"


parseField :: Content i -> Field
parseField content =
    Field
        (findAttr "label" content)
        (read $ findAttr "ID" content)
        (read $ findAttr "parentID" content)
        (map parseTuple $ (keep /> tag "tuple") content)
    where
    
    parseType :: Content i -> (Int, Int)
    parseType content = 
        (toFromType !! 0, toFromType !! 1)
        where toFromType = map (read . findAttr "ID") $ (keep /> tag "types" /> tag "type") content

    (fromType, toType) = parseType content

    parseTuple :: Content i -> Tuple
    parseTuple content =
        Tuple (toFrom !! 0) fromType (toFrom !! 1) toType
        where toFrom = map (findAttr "label") $ (keep /> tag "atom") content


findOptAttr :: String -> Content i -> Maybe String
findOptAttr name elem = show `liftM` lookup (N name) (getAttrs elem)


findAttr :: String -> Content i -> String
findAttr name elem = fromJust $ show `fmap` lookup (N name) (getAttrs elem)


getAttrs :: Content i -> [Attribute]
getAttrs (CElem (Elem _ attributes _) _) = attributes
