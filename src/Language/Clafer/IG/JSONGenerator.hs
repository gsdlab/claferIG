{-
 Copyright (C) 2013-2017 Michal Antkiewicz <http://gsd.uwaterloo.ca>

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

module Language.Clafer.IG.JSONGenerator (generateJSON) where

import Language.Clafer.Common
import Language.Clafer.Intermediate.Intclafer
import qualified Language.Clafer.IG.ClaferModel as M
import Data.Maybe (fromJust)
import Data.Json.Builder
import Data.String.Conversions
import Prelude hiding (id)

-- | Generate a representation of the instance in JSON format
generateJSON :: UIDIClaferMap -> M.ClaferModel                  -> String
generateJSON    uidIClaferMap'   (M.ClaferModel topLevelClafers) =
    convertString $ toJsonBS $ constructElements $ map (printClafer uidIClaferMap') topLevelClafers

printClafer :: UIDIClaferMap -> M.Clafer                           -> Object
printClafer    uidIClaferMap'      (M.Clafer id value children) =
    map (printClafer uidIClaferMap') children `addElements` completeClaferObject
    where
        uid' = M.i_name id
        iclafer = fromJust $ findIClafer uidIClaferMap' $ removeOrdinal uid'
        ident' = _ident iclafer

        super' = getSuper iclafer
        reference' = getReference iclafer
        (Just (cardMin, _)) = _card iclafer
        (Just (_, cardMax)) = _card iclafer
        basicClaferObject = makeBasicClaferObject ident' uid' super' reference' cardMin cardMax

        addValue :: Maybe M.Value         -> Object -> Object
        addValue    Nothing                  object = object
        addValue    (Just (M.IntValue i))    object = addIntValue i object
        addValue    (Just (M.AliasValue a))  object = addStringValue (M.i_name a) object
        addValue    (Just (M.StringValue _)) _      = error "Function addValue from JSONGenerator does not accept StringValues" -- Should never happen, string values are not generated yet

        completeClaferObject = addValue value basicClaferObject

        removeOrdinal :: String -> String
        removeOrdinal = takeWhile (/= '$')

makeBasicClaferObject :: String -> String -> [String] -> [String] -> Integer -> Integer -> Object
makeBasicClaferObject    ident'    uid'      super'      reference'   cardMin    cardMax  =
    mconcat [ row "ident" ident',
              row "uid" uid',
              superRow,
              refRow,
              row "cardMin" cardMin,
              row "cardMax" cardMax ]
    where
        superRow = case super' of
            [s] -> row "super" s
            _   -> mempty
        refRow = case reference' of
            [r] -> row "reference" r
            _   -> mempty

addIntValue :: Int -> Object      -> Object
addIntValue    value  claferObject =
    claferObject `mappend` row "value" value

addStringValue :: String -> Object      -> Object
addStringValue    value     claferObject =
    claferObject `mappend` row "value" value

addElements :: [ Object ] -> Object      -> Object
addElements    elements'      claferObject =
    claferObject `mappend` constructElements elements'

constructElements :: [ Object ] -> Object
constructElements    elements'    =
    row "elements" $ mconcat $ map element elements'
