{-
 Copyright (C) 2026 Michal Antkiewicz <http://gsd.uwaterloo.ca>

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
-- | The views of the solutions: a static model has instances, and a dynamic model has traces,
--   whose states are instances. Both are sequences of snapshots, which the full view shows
--   one by one, and the diff view shows as the changes from the previous snapshot.
module Language.Clafer.IG.SolutionView (showSolution, showSolutionDiff, solutionText, solutionJSON, lpqName) where

import Data.Char (isDigit)
import Data.List
import Language.Clafer.Common (UIDIClaferMap)
import Language.Clafer.QNameUID (QNameMaps, getLPQName)
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.JSONGenerator (generateJSON)
import Prelude hiding (id)

-- | The full view of the n-th solution: the instance, or each state of the trace
showSolution :: Int -> ClaferSolution -> String
showSolution n (ClaferInstance model) =
    "=== Instance " ++ show n ++ " Begin ===\n\n" ++
    show model ++
    "\n--- Instance " ++ show n ++ " End ---\n\n"
showSolution n (ClaferTrace states loopState) =
    "=== Trace " ++ show n ++ " Begin ===\n\n" ++
    solutionText (ClaferTrace states loopState) ++
    "--- Trace " ++ show n ++ " End ---\n\n"

-- | The text of a solution, as it is saved: the instance, or the states of the trace
solutionText :: ClaferSolution -> String
solutionText (ClaferInstance model) = show model
solutionText (ClaferTrace states loopState) =
    concat (zipWith showState [0 :: Int ..] states) ++ loopBack loopState
    where
    showState i state = stateHeader i ++ show state ++ "\n"

-- | The diff view of the n-th solution. The first state of a trace, whose clafers all appear (+),
--   is shown in full, and each next state as its changes: the clafers that appear (+), with their children, and that
--   disappear (-), and the values that change (~), each at its depth in the tree and with its
--   least-partially-qualified name, given by the UID of its clafer. The states are compared by
--   instance, which has the same name in every state. An instance is compared with the previous
--   instance, if any, by its structure, since the names of the instances of separate solutions
--   are unrelated.
showSolutionDiff :: (String -> String) -> Int -> Maybe ClaferSolution -> ClaferSolution -> String
showSolutionDiff names n (Just (ClaferInstance previous)) (ClaferInstance model) =
    "=== Instance " ++ show n ++ " Begin ===\n\n" ++
    showChanges (structureDiff names previous model) ++
    "\n--- Instance " ++ show n ++ " End ---\n\n"
showSolutionDiff _ n _ (ClaferInstance model) =
    "=== Instance " ++ show n ++ " Begin ===\n\n" ++
    appears model ++
    "\n--- Instance " ++ show n ++ " End ---\n\n"
showSolutionDiff names n _ (ClaferTrace states loopState) =
    "=== Trace " ++ show n ++ " Begin ===\n\n" ++
    concat (zipWith3 showState [0 :: Int ..] (Nothing : map Just states) states) ++
    loopBack loopState ++
    "--- Trace " ++ show n ++ " End ---\n\n"
    where
    showState i Nothing state = stateHeader i ++ appears state ++ "\n"
    showState i (Just previous) state = stateHeader i ++ showChanges (instanceDiff names previous state) ++ "\n"

-- | A snapshot without a previous one, whose clafers all appear (+), aligned with the changes
appears :: ClaferModel -> String
appears = unlines . map ("+ " ++) . lines . show

-- | The least-partially-qualified name of a clafer, with dots, e.g., normal.green, or its UID
lpqName :: QNameMaps -> String -> String
lpqName qNameMaps' uid' = maybe uid' (dropWhile (== '.') . dotted) $ getLPQName qNameMaps' uid'
    where
    dotted (':' : ':' : rest) = '.' : dotted rest
    dotted (c : rest) = c : dotted rest
    dotted [] = []

-- | The JSON of a solution: the instance, or the states of the trace and its loop state
solutionJSON :: UIDIClaferMap -> ClaferSolution -> String
solutionJSON uidIClaferMap' (ClaferInstance model) = generateJSON uidIClaferMap' model
solutionJSON uidIClaferMap' (ClaferTrace states loopState) =
    "{\"states\": [" ++ intercalate ", " (map (generateJSON uidIClaferMap') states) ++ "], \"loop\": " ++ show loopState ++ "}"

stateHeader :: Int -> String
stateHeader i = "--- State " ++ show i ++ " ---\n"

loopBack :: Int -> String
loopBack loopState = "--- Loop back to state " ++ show loopState ++ " ---\n\n"

-- | A clafer of an instance: the names on the path from a top-level clafer to it, which identify
--   it, its name to show and its value
data Entry = Entry {e_path :: [String], e_name :: String, e_value :: String} deriving (Eq, Ord)

entries :: (String -> String) -> ClaferModel -> [Entry]
entries names (ClaferModel topLevelClafers) = concatMap (entries' []) topLevelClafers
    where
    entries' path clafer@(Clafer id _ value children) =
        Entry path' (displayName clafer) (maybe "" showValue value) : concatMap (entries' path') children
        where
        path' = path ++ [i_name id]
    showValue (AliasValue alias) = " -> " ++ i_name alias
    showValue (IntValue v) = " -> " ++ show v
    showValue (StringValue v) = " -> " ++ v
    -- the instance's name with the least-partially-qualified name of its clafer: green$2 : Light
    -- -> normal.green$2 : Light
    displayName (Clafer id uid' _ _) =
        case stripPrefix (lastSegment lpq) (i_name id) of
            Just rest -> lpq ++ rest
            Nothing   -> i_name id
        where
        lpq = names uid'
        lastSegment = reverse . takeWhile (/= '.') . reverse

data Change = Added Entry | Removed Entry | Changed Entry

-- | The changes, each as in the tree of the instance, after its marker
showChanges :: [Change] -> String
showChanges [] = "(no changes)\n"
showChanges changes = concatMap showChange changes
    where
    showChange (Added e) = line '+' e
    showChange (Removed e) = line '-' e
    showChange (Changed e) = line '~' e
    line marker (Entry path name value) = marker : ' ' : replicate (2 * (length path - 1)) ' ' ++ name ++ value ++ "\n"

-- | The changes from one state to the next: an instance is the same in both if it has the same
--   path. A removed clafer is shown without its children, which disappear with it.
instanceDiff :: (String -> String) -> ClaferModel -> ClaferModel -> [Change]
instanceDiff names previous current =
    [Removed e | e <- before, e_path e `notElem` currentPaths, not $ any (`elem` removedPaths) (properPrefixes $ e_path e)] ++
    [Changed e | e <- after, Just v <- [lookup (e_path e) previousValues], v /= e_value e] ++
    [Added e | e <- after, e_path e `notElem` previousPaths]
    where
    before = entries names previous
    after = entries names current
    previousPaths = map e_path before
    currentPaths = map e_path after
    previousValues = [(e_path e, e_value e) | e <- before]
    removedPaths = filter (`notElem` currentPaths) previousPaths

-- | The changes from one instance to another of a static model, by their structure: the clafers
--   without the numbers of their instances, with their values, as multisets.
structureDiff :: (String -> String) -> ClaferModel -> ClaferModel -> [Change]
structureDiff names previous current =
    map Removed (sort $ before \\ after) ++ map Added (sort $ after \\ before)
    where
    before = map anonymous $ entries names previous
    after = map anonymous $ entries names current
    anonymous (Entry path name value) = Entry (map dropOrdinal path) (dropOrdinal name) (dropOrdinal value)
    -- name$2 : Super -> name : Super
    dropOrdinal name = case break (== '$') name of
        (prefix, '$' : rest) -> prefix ++ dropWhile isDigit rest
        _ -> name

-- | The paths of the ancestors, the outermost first
properPrefixes :: [a] -> [[a]]
properPrefixes xs = take (length xs - 1) $ drop 1 $ inits xs
