{-
 Copyright (C) 2013 Michal Antkiewicz <http://gsd.uwaterloo.ca>

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

module JSONGenerator (generateJSON) where

import qualified ClaferModel as M
import Sugarer
import qualified Language.Clafer.Intermediate.Analysis as A
import Data.Json.Builder
import Data.String.Conversions

generateJSON :: A.Info -> M.ClaferModel                        -> String
generateJSON    info      model@(M.ClaferModel topLevelClafers) = 
	convertString $ toJsonBS $ constructElements $ map (printClafer info) topLevelClafers 

printClafer :: A.Info -> M.Clafer                           -> Object
printClafer    info      clafer@(M.Clafer id value children) = 
	(map (printClafer info) children) `addElements` completeClaferObject
	where
		uid = M.i_name id
		sclafer = A.runAnalysis (A.claferWithUid $ removeOrdinal uid) info
		ident = A.uid sclafer

		getSuper :: Maybe A.SSuper    -> (Bool, String)
		getSuper    Nothing            = (False, "")
		getSuper    (Just (A.Ref s))   = (True, s)
		getSuper    (Just (A.Colon s)) = (False, s)

		(isOverlapping, super) = getSuper $ A.super sclafer
		cardMin = A.low sclafer
		cardMax = A.high sclafer
		basicClaferObject = makeBasicClaferObject ident uid super isOverlapping cardMin cardMax

		addValue :: Maybe M.Value        -> Object -> Object
		addValue    Nothing                 object = object
		addValue 	(Just (M.IntValue i))   object = addIntValue (read i) object
		addValue 	(Just (M.AliasValue a)) object = addStringValue (M.i_name a) object

		completeClaferObject = addValue value basicClaferObject

		removeOrdinal :: String -> String 
		removeOrdinal = takeWhile (/= '$')

makeBasicClaferObject :: String -> String -> String -> Bool       -> Integer -> Integer    -> Object
makeBasicClaferObject    ident     uid       super     isOverlapping cardMin    cardMax =
	mconcat [ row "ident" ident, 
			  row "uid" uid,
			  row "super" super,
			  row "isOverlapping" isOverlapping,
			  row "cardMin" cardMin,
			  row "cardMax" cardMax ]

addIntValue :: Int -> Object      -> Object
addIntValue    value  claferObject = 
	claferObject `mappend` (row "value" value)

addStringValue :: String -> Object      -> Object
addStringValue    value     claferObject = 
	claferObject `mappend` (row "value" value)

addElements :: [ Object ] -> Object      -> Object
addElements    elements      claferObject = 
	claferObject `mappend` (constructElements elements)

constructElements :: [ Object ] -> Object
constructElements    elements    =
	row "elements" $ mconcat $ map element elements