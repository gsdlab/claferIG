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
{-# LANGUAGE OverloadedStrings #-}

module Language.Clafer.IG.JSONGenerator (generateJSON) where

import Language.Clafer.Common
import Language.Clafer.Intermediate.Intclafer
import qualified Language.Clafer.IG.ClaferModel as M
import Data.Maybe (fromJust)
import Data.Aeson ((.=))
import Data.Aeson.Encoding (Series, encodingToLazyByteString, list, pair, pairs)
import Data.String.Conversions
import Prelude hiding (id)

-- | A JSON object, whose fields keep their order
type Object = Series

-- | Generate a representation of the instance in JSON format
generateJSON :: UIDIClaferMap -> M.ClaferModel                  -> String
generateJSON    uidIClaferMap'   (M.ClaferModel topLevelClafers) =
    convertString $ encodingToLazyByteString $ pairs $ constructElements $ map (printClafer uidIClaferMap') topLevelClafers

printClafer :: UIDIClaferMap -> M.Clafer                           -> Object
printClafer    uidIClaferMap'      (M.Clafer id _ value children) =
    map (printClafer uidIClaferMap') children `addElements` completeClaferObject
    where
        uid' = M.i_name id
        iclafer = fromJust $ findIClafer uidIClaferMap' $ removeOrdinal uid'
        ident' = _ident iclafer

        super' = getSuper iclafer
        reference' = getReference iclafer
        (cardMin, cardMax) = case _card iclafer of
            Just card' -> card'
            Nothing    -> error $ "Function printClafer from JSONGenerator expects the cardinality of " ++ uid' -- Should never happen, the compiler sets every cardinality
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
    mconcat [ "ident" .= ident',
              "uid" .= uid',
              superRow,
              refRow,
              "cardMin" .= cardMin,
              "cardMax" .= cardMax ]
    where
        superRow = case super' of
            [s] -> "super" .= s
            _   -> mempty
        refRow = case reference' of
            [r] -> "reference" .= r
            _   -> mempty

addIntValue :: Int -> Object      -> Object
addIntValue    value  claferObject =
    claferObject `mappend` ("value" .= value)

addStringValue :: String -> Object      -> Object
addStringValue    value     claferObject =
    claferObject `mappend` ("value" .= value)

addElements :: [ Object ] -> Object      -> Object
addElements    elements'      claferObject =
    claferObject `mappend` constructElements elements'

constructElements :: [ Object ] -> Object
constructElements    elements'    =
    pair "elements" $ list pairs elements'
