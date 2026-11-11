{-# LANGUAGE TemplateHaskell #-}
{-
 Copyright (C) 2012-2017 Luke Brown <http://gsd.uwaterloo.ca>

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
-- Generates instances of the models in test/positive with Alloy. Built only with
-- the model-tests flag, in a checkout of the repository: it needs the models, and
-- alloyIG.jar and alloy6.2.0.jar next to the test executable (see `make test`).
import Language.Clafer.IG.ClaferIG
import Language.Clafer.IG.ClaferModel
import Language.Clafer.IG.CommandLine
import Language.Clafer.IG.SolutionView
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Language.Clafer.IG.Constraints
import Language.Clafer.ClaferArgs (ScopeStrategy(..))
import qualified Data.Map as Map
import Data.Maybe
import Data.IORef
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import Prelude hiding (all)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

tg_modeltests :: TestTree
tg_modeltests = $(testGroupGenerator)

-- | The tests run in parallel, on 8 cores (the executable runs with +RTS -N8): each test has a
--   folder of its own, and the long ones are split into parts.
main :: IO ()
main = defaultMain $ testGroup "Model tests" [ tg_modeltests, lifetimes, patternScopes, finalTarget ]

-- | A new folder for the models of a test, under the temporary directory.
testDir :: String -> IO FilePath
testDir name = do
    dir <- (</> ("claferIG-test-" ++ name)) <$> getTemporaryDirectory
    createDirectoryIfMissing True dir
    return dir

claferIGArgsDef :: IGArgs
claferIGArgsDef = IGArgs {
    all             = def,
    maxInstances    = def,
    saveDir         = def,
    claferModelFile = def,
    imports_comp    = def,
    import_root_comp = def,
    public_imports_comp = False,
    import_base_comp = def,
    alloySolution   = def,
    bitwidth        = 4,
    maxInt          = 7,
    useUids         = False,
    addTypes        = False,
    json            = False,
    diff            = False,
    dot             = False,
    tl_comp         = def,
    flatten_inheritance_comp = False,
    no_layout_comp = False,
    check_duplicates_comp = False,
    skip_resolver_comp = False,
    scope_strategy_comp = def
} &= summary claferIGVersion


defaultIGArgs :: FilePath -> IGArgs
--defaultIGArgs fPath = IGArgs Nothing Nothing fPath False 4 False False False
defaultIGArgs fPath = claferIGArgsDef{claferModelFile = fPath}

--getModel :: MonadIO m => FilePath -> ClaferIGT m (Either Language.ClaferT.ClaferErrs Instance)
getModel fPath = runClaferIGT (defaultIGArgs fPath) $ do
    setGlobalScope (fromMaybe 1 $ all $ defaultIGArgs fPath)
    solve
    counterRef <- liftIO $ newIORef 1
    let saveDirectory = fromMaybe return $ underDirectory `liftM` saveDir (defaultIGArgs fPath)
    let nextFile = savePath fPath counterRef >>= saveDirectory
    file <- liftIO nextFile
    liftIO $ createDirectoryIfMissing True $ takeDirectory file
    next
    where
        savePath :: FilePath -> IORef Int -> IO FilePath
        savePath fPath' counterRef =
            do
                counter <- readIORef counterRef
                writeIORef counterRef (counter + 1)
                return $ fPath' ++ "." ++ (show counter) ++ ".data"
        underDirectory :: FilePath -> FilePath -> IO FilePath
        underDirectory dir file =
            do
                createDirectoryIfMissing True dir
                return $ joinPath [dir, file]


fromRight (Right x) = x
fromRight _         = error "fromRight received Left _. Should never happen."

-- | The instance, or the first state of the trace
firstModel :: Instance -> ClaferModel
firstModel = head . solutionModels . modelSolution

-- | A traffic light, whose scenario is a trace of a dynamic model
trafficLight :: String
trafficLight = unlines
    [ "abstract xor TrafficLight"
    , "    initial xor on -->> off"
    , "        initial xor normal"
    , "            initial green --> yellow"
    , "            yellow --> red"
    , "            red --> green"
    , "        xor flashing -->> normal"
    , "            initial yellow_on --> yellow_off"
    , "            yellow_off --> yellow_on"
    , "    off --> on"
    , "tl : TrafficLight"
    ]

-- | The solutions of a dynamic model are traces: the scenario gives the states of its trace, and
--   the diff view shows the changes from one state to the next.
case_DynamicModelTrace :: Assertion
case_DynamicModelTrace = do
        dir <- testDir "DynamicModelTrace"
        writeFile (dir </> "scenario.cfr") $ trafficLight ++ "    [ initially (green --> yellow --> red --> green --> yellow_on --> yellow_off --> off) ]\n"
        result <- getModel $ dir </> "scenario.cfr"
        case modelSolution $ fromRight result of
            ClaferTrace states _ -> do
                length states >= 7 @? ("The trace has the 7 states of the scenario: " ++ show (length states))
                let leaves = map (map c_name . filter (null . c_children) . traverseModel) states
                take 7 leaves == [["green"], ["yellow"], ["red"], ["green"], ["yellow_on"], ["yellow_off"], ["off"]]
                    @? ("The states follow the scenario: " ++ show leaves)
                let diffView = showSolutionDiff id 1 Nothing (ClaferTrace states 0)
                ("-       green\n+       yellow" `isInfixOf` diffView) @? ("The diff view shows the change of the light: " ++ diffView)
            ClaferInstance _ -> assertFailure "A dynamic model has traces"

-- | The graph of a trace has its states, their transitions and the loop; of an instance, a node.
case_SolutionGraph :: Assertion
case_SolutionGraph = do
        dir <- testDir "SolutionGraph"
        writeFile (dir </> "graph.cfr") trafficLight
        result <- runClaferIGT (defaultIGArgs $ dir </> "graph.cfr") $ solve >> next
        case fromRight result of
            Instance solution@(ClaferTrace states loopState) _ -> do
                let
                    graph = solutionDot solution
                    lastState = length states - 1
                    has line = (line `isInfixOf` graph) @? ("The graph has " ++ line ++ ": " ++ graph)
                has "start -> S0;"
                forM_ [1 .. lastState] $ \i -> has $ "S" ++ show (i - 1) ++ " -> S" ++ show i ++ ";"
                has $ "S" ++ show lastState ++ " -> S" ++ show loopState ++ " [style=dashed"
                has "xlabel=\"S0\""
                (not ("State" `isInfixOf` graph)) @? ("The states have no header: " ++ graph)
                let
                    diffGraph = solutionDiffDot id solution
                    diffHas line = (line `isInfixOf` diffGraph) @? ("The diff graph has " ++ line ++ ": " ++ diffGraph)
                diffHas "S0 [label=\"+ "
                diffHas "S1 [label=\""
                diffHas $ "S" ++ show lastState ++ " -> S" ++ show loopState ++ " [style=dashed"
                diffHas "[label=\"- "
            _ -> assertFailure "A trace"
        let
            value = Clafer (Id "c" 0) "c" (Just $ StringValue "\"a\"") []
            graph = solutionDot $ ClaferInstance $ ClaferModel [value]
        ("\\\"a\\\"" `isInfixOf` graph) @? ("The quotes are escaped: " ++ graph)

-- | Each assertion, also a nested one or one on the line of another, is checked within the scopes:
--   a failed one has a counterexample, which leaves the next solution as it is.
case_Assertions :: Assertion
case_Assertions = do
    dir <- testDir "Assertions"
    writeFile (dir </> "dynamic.cfr") $ unlines
        [ "xor s"
        , "    initial a --> b"
        , "    b --> a"
        , "    assert [ a => X b ]"
        , "assert [ never s.b ]  assert [ s.a => X s.a ]" ]
    writeFile (dir </> "static.cfr") $ unlines
        [ "abstract P"
        , "    age -> integer"
        , "    [ age >= 0 ]"
        , "    assert [ age >= 0 ]"
        , "    assert [ age >= 3 ]"
        , "x : P" ]
    let
        results file = do
            result <- runClaferIGT (defaultIGArgs $ dir </> file) $ do
                solve
                _ <- next
                assertions <- getAssertions
                checks <- forM assertions $ \(name, info) -> ((,) (syntax info) . isJust) <$> checkAssertion name
                solution <- next
                return (checks, solution)
            return $ fromRight result
    (dynamic, _) <- results "dynamic.cfr"
    dynamic @?= [("a => X b", False), ("never s.b", True), ("s.a => X s.a", True)]
    (static, solution) <- results "static.cfr"
    static @?= [("age >= 0", False), ("age >= 3", True)]
    onLine <- runClaferIGT (defaultIGArgs $ dir </> "static.cfr") $ do
        solve
        assertions <- getAssertions
        return $ map (\n -> map (syntax . snd) $ filter (isOnLine n . snd) assertions) [4, 5, 6]
    fromRight onLine @?= [["age >= 0"], ["age >= 3"], []]
    case solution of
        Instance{} -> return ()
        _ -> assertFailure "The next solution stays as it is"

-- | The assertions with whether they hold, and whether the model has a solution, with the scope
--   strategy, the global scope and the maximum trace length.
checkLifetimes :: FilePath -> String -> ScopeStrategy -> Integer -> Integer -> IO ([(String, Bool)], Bool)
checkLifetimes dir model strategy scope traceLength = do
    writeFile (dir </> "lifetimes.cfr") model
    result <- runClaferIGT (defaultIGArgs $ dir </> "lifetimes.cfr"){scope_strategy_comp = strategy} $ do
        -- as the command i does: the clafers have scopes, which the global scope does not change
        getScopes >>= mapM_ (\s -> (fmap (max scope) . valueOfScope) (fst s) >>= \n -> setScope n s)
        setGlobalScope scope
        setMaxTraceLength traceLength
        solve
        solution <- next
        assertions <- getAssertions
        holds <- forM assertions $ \(name', info') -> ((,) (syntax info') . isNothing) <$> checkAssertion name'
        return (holds, case solution of
            Instance{} -> True
            _ -> False)
    return $ fromRight result

-- | A clafer whose UID cannot be an Alloy name, e.g., sig, has a sig of its own, this/sig_1, and so
--   does r_x next to x (r_x is the relation of x); claferIG translates between the UIDs and the
--   sigs, and shows the clafers by their names.
case_AlloyNamesOfUIDs :: Assertion
case_AlloyNamesOfUIDs = do
    dir <- testDir "AlloyNamesOfUIDs"
    writeFile (dir </> "names.cfr") $ unlines [ "sig", "x", "r_x ?" ]
    result <- runClaferIGT (defaultIGArgs $ dir </> "names.cfr") $ do
        sigs <- map fst <$> getScopes
        sigOfSig <- sigOfUID "sig"
        sigOfRx <- sigOfUID "r_x"
        sigOfX <- sigOfUID "x"
        uid' <- uidOfSig "this/sig_1"
        name' <- claferNameOfSig "this/sig_1"
        return (sigs, [sigOfSig, sigOfRx, sigOfX], uid', name')
    let (sigs, mapped, uid', name') = fromRight result
    mapped @?= ["this/sig_1", "this/r_x_1", "this/x"]
    and [ m `elem` sigs | m <- mapped ] @? ("The sigs are not those of the Alloy model: " ++ show sigs)
    uid' @?= "sig"
    name' @?= "sig"

-- | An abstract clafer that no concrete clafer inherits from, and the clafers in it, have no
--   instances: their scopes stay 0 when the scopes are raised. An abstract clafer that a concrete
--   clafer inherits from through another abstract clafer keeps its scope.
case_EmptyAbstractScopes :: Assertion
case_EmptyAbstractScopes = do
    dir <- testDir "EmptyAbstractScopes"
    writeFile (dir </> "empty.cfr") $ unlines
        [ "abstract A", "    x ?", "abstract B : A", "abstract E", "    y ?", "C : B" ]
    result <- runClaferIGT (defaultIGArgs $ dir </> "empty.cfr") $ do
        initial <- getScopes
        getScopes >>= mapM_ (increaseScope 2)
        mapM_ (setAlloyScope 4) ["this/E", "this/y"]
        raised <- getScopes
        return (initial, raised)
    let (initial, raised) = fromRight result
    [ lookup ("this/" ++ s) initial | s <- ["A", "B", "E", "y"] ] @?= [Just 1, Just 1, Just 0, Just 0]
    [ lookup ("this/" ++ s) raised | s <- ["A", "B", "C", "E", "y"] ] @?= [Just 3, Just 3, Just 3, Just 0, Just 0]

-- | The obligations of a constraint last while the parent of its context instance exists, also
--   when they are negated; a scope bounds the instance identities across the trace, and an
--   instance keeps its parent. Checked with both scope strategies, at scopes 1 to 3 and up to
--   3 and 10 states.
lifetimes :: TestTree
lifetimes = testGroup "Lifetimes"
    [ testCase (show s ++ ", scope " ++ show n ++ ", " ++ show l ++ " states") $ lifetimesAt s n l
    | s <- [None, Simple], n <- [1, 2, 3], l <- [3, 10] ]

lifetimesAt :: ScopeStrategy -> Integer -> Integer -> Assertion
lifetimesAt strategy scope traceLength = do
    dir <- testDir $ "Lifetimes-" ++ show strategy ++ "-" ++ show scope ++ "-" ++ show traceLength
    let
        machine final' = unlines
            [ "go ?"
            , final' ++ "xor sm"
            , "  initial a"
            , "    [ go --> b ]"
            , "  b"
            , "  c"
            , "assert [ all m : sm | m.a && go => X (m.b || no (m ** sm)) ]"
            , "assert [ sm.a && go => X sm.b ]" ]
        negated constraints' = unlines $
            [ "machine ?"
            , "  ctx"
            , "    error ?" ] ++ map ("    " ++) constraints' ++
            [ "[ initially machine ]"
            , "[ initially (X no machine) ]" ]
        scopeIdentities = "item\n[ initially (all x : item | X no (x ** item)) ]\n"
        parentAcrossLifetimes = unlines
            [ "box 2"
            , "  item ?"
            , "[ initially (some box.item) ]"
            , "[ initially (all i : box.item | X (no (i ** box.item) && X some (i ** box.item))) ]"
            , "assert [ all b : box | all i : b.item | G (some (i ** box.item) => some (i ** b.item)) ]" ]
        persistent = unlines
            [ "final machine"
            , "  ctx"
            , "    error ?"
            , "    [ F error ]"
            , "    [ !(F error) ]" ]
        holds = map snd
        at = " (" ++ show strategy ++ ", scope " ++ show scope ++ ", " ++ show traceLength ++ " states)"
        check = checkLifetimes dir
    do
        -- a transition in its source state leads to b while the machine exists
        (final', _) <- check (machine "final ") strategy scope traceLength
        holds final' @?= [True, True]
        -- a replaced machine starts again, and the transition of the old one lapses
        (replaced, _) <- check (machine "") strategy scope traceLength
        holds replaced == [True, scope == 1] @? ("The transition lapses with a replaced machine" ++ at ++ ": " ++ show replaced)
        -- a negated obligation lapses with the parent too
        (equivalences, trace) <- check (negated
            [ "assert [ (G !error) <=> !(F error) ]"
            , "assert [ !(X error) <=> X !error ]"
            , "assert [ !(error U !error) <=> (error W (!error && error)) ]" ]) strategy scope traceLength
        holds equivalences @?= [True, True, True]
        trace @? ("The machine disappears" ++ at)
        (_, globally) <- check (negated ["[ G !error ]"]) strategy scope traceLength
        globally @? ("G !error lets the machine disappear" ++ at)
        (_, notEventually) <- check (negated ["[ !(F error) ]"]) strategy scope traceLength
        notEventually @? ("!(F error) lets the machine disappear" ++ at)
        -- while the parent persists, a formula and its negation contradict each other
        (_, contradiction) <- check persistent strategy scope traceLength
        not contradiction @? ("F error and !(F error) contradict each other while the machine exists" ++ at)
        -- another instance of item in each state needs two identities
        (_, identities) <- check scopeIdentities strategy scope traceLength
        identities == (scope > 1) @? ("A scope bounds the identities across the trace" ++ at)
        (sameParent, _) <- check parentAcrossLifetimes strategy scope traceLength
        holds sameParent @?= [True]

-- | Whether the model has a trace, with the global scope.
hasTrace :: FilePath -> Integer -> String -> IO Bool
hasTrace dir scope model = snd <$> checkLifetimes dir model Simple scope 10

-- | s must follow every p between q and r, not only a p at q; s must precede p from the first q,
--   not from a later q, so at the top level, where a pattern holds in every state, every q starts
--   the scope (kbak/clafer-semantics#127).
patternScopes :: TestTree
patternScopes = testGroup "PatternScopes" $ zipWith test [1 :: Int ..]
    -- q, then p, then r: p needs s before r
    [ (False, follow, "q && !p && !r && !s && X (!q && p && !r && !s && X (!q && !p && !r && !s && X (r && !p && !s)))")
    , (True, follow, "q && !p && !r && !s && X (!q && p && !r && !s && X (!q && !p && !r && s && X (r && !p)))")
    -- q, p, q, s: the second q does not hide the p after the first one
    , (False, precede, "q && !p && !s && X (!q && p && !s && X (q && !p && !s && X (s && !p && !q)))")
    -- q, s, p, q, p: at the top level, the second q needs an s before its p
    , (False, precede, qspqp)
    , (True, "initially (" ++ precede ++ ")", qspqp) ]
  where
    model pattern' scenario = unlines ["q ?", "p ?", "s ?", "r ?", "[ " ++ pattern' ++ " ]", "[ initially (" ++ scenario ++ ") ]"]
    test i (expected, pattern', scenario) = testCase ("[ " ++ pattern' ++ " ] " ++ show i) $ do
        dir <- testDir $ "PatternScopes-" ++ show i
        actual <- hasTrace dir 1 $ model pattern' scenario
        actual == expected @? ("[ " ++ pattern' ++ " ] on " ++ scenario ++ (if expected then " has a trace" else " has no trace"))
    follow = "s must follow p between q and r"
    precede = "s must precede p after q"
    qspqp = "q && !p && !s && X (!q && !p && s && X (!q && p && !s && X (q && !p && !s && X (!q && p && !s))))"

-- | finalref fixes the target of each instance, finaltarget the set of targets of the instances
--   in their parent, so they may swap targets, but a new instance points to an old target; final
--   fixes both (kbak/clafer-semantics#127).
finalTarget :: TestTree
finalTarget = testGroup "FinalTarget"
    [ testCase "its own reference" $ finalTargetOf "own" ("", " -> E")
    , testCase "an inherited reference" $ finalTargetOf "inherited" (" : engine", "")
    , testCase "finaltarget on the abstract clafer" finalTargetAbstract ]

finalTargetModel :: [String] -> String -> String
finalTargetModel clafer' scenario = unlines $
    ["abstract E", "e1 : E", "e2 : E", "abstract Car", "  abstract engine -> E", "final car : Car"]
    ++ map ("  " ++) clafer' ++ ["[ initially (" ++ scenario ++ ") ]"]

finalTargetSwap, finalTargetReplace :: String
finalTargetSwap = "some x : car.h | x.dref = e1 && X (some (x ** car.h) && x.dref = e2)"
finalTargetReplace = "car.h.dref = e1 && X (car.h.dref = e2)"

finalTargetOf :: String -> (String, String) -> Assertion
finalTargetOf name (super', ref) = do
    dir <- testDir $ "FinalTarget-" ++ name
    let
        swap = finalTargetSwap
        replace = finalTargetReplace
        check expected clafer' scenario = do
            actual <- hasTrace dir 2 $ finalTargetModel clafer' scenario
            actual == expected @? (unwords clafer' ++ ": " ++ scenario ++ (if expected then " has a trace" else " has no trace"))
        h modifier card = [modifier ++ " h" ++ super' ++ ref ++ card]
    do
        check True (h "finaltarget" " 2") swap
        check False (h "finalref" " 2") swap
        check False (h "final" " 2") swap
        check False (h "finaltarget" "") replace
        check True (h "finalref" "") replace
        check True (h "" "") replace
        -- both modifiers, in either order, mean both
        forM_ ["finalref finaltarget", "finaltarget finalref"] $ \both -> do
            check False (h both " 2") swap
            check False (h both "") replace

-- | finaltarget on the abstract clafer fixes the targets of all its instances in the parent.
finalTargetAbstract :: Assertion
finalTargetAbstract = do
    dir <- testDir "FinalTarget-abstract"
    let abstract' clafer' scenario = replace' "abstract engine" "abstract finaltarget engine" $ finalTargetModel clafer' scenario
        replace' old new s = unlines [ if l == "  " ++ old ++ " -> E" then "  " ++ new ++ " -> E" else l | l <- lines s ]
    hasTrace dir 2 (abstract' ["h : engine 2"] finalTargetSwap) >>= (@? "finaltarget engine: the instances of h swap their targets")
    hasTrace dir 2 (abstract' ["h : engine"] finalTargetReplace) >>= (@? "finaltarget engine: a new h points to an old target") . not

-- | A scenario that contradicts the model has an unsat core with the scenario and the transition
--   it contradicts.
case_DynamicModelUnsatCore :: Assertion
case_DynamicModelUnsatCore = do
        dir <- testDir "DynamicModelUnsatCore"
        writeFile (dir </> "contradiction.cfr") $ trafficLight ++ "    [ initially (green --> red) ]\n"
        result <- getModel $ dir </> "contradiction.cfr"
        case fromRight result of
            UnsatCore core _ -> do
                let shown = map show core
                any ("initially (green --> red)" `isInfixOf`) shown @? ("The core has the scenario: " ++ show shown)
                any ("green --> yellow" `isInfixOf`) shown @? ("The core has the transition: " ++ show shown)
            _ -> assertFailure "The scenario contradicts the model"

-- | A fork has the states of the last trace before the given state; a new configuration differs
--   in the clafers that do not change over time.
case_DynamicModelForkAndConfiguration :: Assertion
case_DynamicModelForkAndConfiguration = do
        dir <- testDir "DynamicModelForkAndConfiguration"
        writeFile (dir </> "fork.cfr") trafficLight
        forked <- runClaferIGT (defaultIGArgs $ dir </> "fork.cfr") $ do
            solve
            liftM2 (,) next (fork 2)
        case fromRight forked of
            (Instance (ClaferTrace states _) _, Instance (ClaferTrace states' _) _) ->
                map show (take 2 states) == map show (take 2 states') @? "The fork has the first 2 states of the trace"
            _ -> assertFailure "A trace and its fork"
        writeFile (dir </> "configuration.cfr") $ unlines
            [ "final Features"
            , "    final EU ?"
            , "xor Light"
            , "    initial red --> green"
            , "    green --> red"
            ]
        configurations <- runClaferIGT (defaultIGArgs $ dir </> "configuration.cfr") $ do
            solve
            sequence [next, nextConfiguration, nextConfiguration]
        case fromRight configurations of
            [Instance (ClaferTrace (state : _) _) _, Instance (ClaferTrace (state' : _) _) _, NoInstance] ->
                (("EU" `isInfixOf` show state) /= ("EU" `isInfixOf` show state')) @? "The configurations differ in EU"
            _ -> assertFailure "Two configurations"

-- | Only the first solve logs the translation of the model, to a file kodkod<n>.log in the session's
--   folder, which Kodkod's prover needs for the unsat core of a model without instances. The next
--   trace, a fork and another configuration give no core, so they log nothing. Each of them logged
--   the translation too, which for a large model took gigabytes of the folder, e.g., 11 GB in 3
--   minutes for the next traces of the TLS handshake model.
--   When the session ends, its folder is removed. Alloy removes its own files in it at the same
--   time, and the removal stopped at the first file that was gone, with a NoSuchFileException; the
--   test cannot make that happen, it only checks that the folder is gone.
case_SessionTmpdir :: Assertion
case_SessionTmpdir = do
        dir <- testDir "SessionTmpdir"
        writeFile (dir </> "light.cfr") trafficLight
        result <- runClaferIGT (defaultIGArgs $ dir </> "light.cfr") $ do
            tmpdir <- getSessionTmpdir
            setMaxTraceLength 3
            -- the first trace, of the first solve, which solve only starts
            solve
            _ <- next
            first <- liftIO $ translationLogs tmpdir
            others <- sequence [next, fork 1, nextConfiguration]
            after <- liftIO $ translationLogs tmpdir
            quit
            return (tmpdir, first, others, after)
        let (tmpdir, first, others, after) = fromRight result
        not (null first) @? "The first solve logs the translation"
        [ isInstance o | o <- others ] == [True, True, False] @? ("A next trace and a fork; the traffic light has one configuration: " ++ show [ isInstance o | o <- others ])
        null (filter (`notElem` first) after) @? ("The next trace, the fork and the configuration log no translation: " ++ show (filter (`notElem` first) after))
        removed <- gone tmpdir (100 :: Int)
        removed @? ("The session's folder is removed when the session ends: " ++ tmpdir)
    where
    isInstance Instance{} = True
    isInstance _ = False
    translationLogs tmpdir = filter (\f -> "kodkod" `isPrefixOf` f && ".log" `isSuffixOf` f) <$> listDirectory tmpdir
    -- alloyIG removes the folder when its JVM exits, after the quit command
    gone tmpdir tries = do
        exists <- doesDirectoryExist tmpdir
        if not exists then return True
        else if tries <= 0 then return False
        else threadDelay 100000 >> gone tmpdir (tries - 1)

-- | A static model gives an instance with another structure, regardless of the values.
case_StaticModelConfiguration :: Assertion
case_StaticModelConfiguration = do
        dir <- testDir "StaticModelConfiguration"
        writeFile (dir </> "static.cfr") $ unlines
            [ "EU ?"
            , "age -> integer"
            ]
        result <- runClaferIGT (defaultIGArgs $ dir </> "static.cfr") $ do
            solve
            sequence [next, nextConfiguration, nextConfiguration]
        case fromRight result of
            [Instance (ClaferInstance model) _, Instance (ClaferInstance model') _, NoInstance] ->
                (("EU" `isInfixOf` show model) /= ("EU" `isInfixOf` show model')) @? "The configurations differ in EU"
            _ -> assertFailure "Two configurations"

-- | The traces given by next are distinct, and limited by the maximum trace length.
case_DynamicModelTraces :: Assertion
case_DynamicModelTraces = do
        dir <- testDir "DynamicModelTraces"
        writeFile (dir </> "light.cfr") trafficLight
        result <- runClaferIGT (defaultIGArgs $ dir </> "light.cfr") $ do
            setMaxTraceLength 3
            solve
            replicateM 5 next
        let traces = [show $ solutionText s | Instance s _ <- fromRight result]
        -- the traces of at most 3 states: one of 2 states and two of 3 states
        length traces == 3 @? ("3 traces of at most 3 states: " ++ show (length traces))
        length traces == length (nubOrd traces) @? "The traces are distinct"
        -- the reference of a clafer that is not in a state has any value, which is not shown:
        -- traces that differ only in it are the same trace
        writeFile (dir </> "absent.cfr") $ unlines
            [ "enum R = a | b"
            , "req -> R ?"
            , "[ never req ]"
            ]
        absent <- runClaferIGT (defaultIGArgs $ dir </> "absent.cfr") $ do
            setMaxTraceLength 1
            solve
            replicateM 3 next
        let absentTraces = [show $ solutionText s | Instance s _ <- fromRight absent]
        length absentTraces == 1 @? ("1 trace of 1 state without req: " ++ show absentTraces)
    where
    nubOrd = foldr (\x seen -> if x `elem` seen then seen else x : seen) []


case_strMapCheck :: Assertion
case_strMapCheck = do
        --let claferModel = Right $ Instance (ClaferModel [(Clafer (Id "" 0) (Just (StringValue "")) [])]) ""
        claferModel' <- getModel "test/positive/i220.cfr"
        (valueCheck $ c_value $ head $ c_topLevel $ firstModel $ fromRight $ claferModel') @? "Mapping Int back to String Failed!"
        where
            valueCheck Nothing = False
            valueCheck (Just (AliasValue _)) = False
            valueCheck (Just (IntValue _)) = False
            valueCheck (Just (StringValue _)) = True

-- A model importing a file with --import in its //# OPTIONS directive; an error in the
-- imported file names it.
case_Imports :: Assertion
case_Imports = do
        dir <- testDir "Imports"
        writeFile (dir </> "lib.cfr") "abstract Lib\n    x : integer\n    [ x = 3 ]\n"
        writeFile (dir </> "model.cfr") "//# OPTIONS --import=lib.cfr\nA : Lib\n"
        instance' <- getModel $ dir </> "model.cfr"
        ("x -> 3" `isInfixOf` show (firstModel $ fromRight instance')) @? "The instance has the imported clafer's value"
        writeFile (dir </> "lib.cfr") "abstract Lib\n    x : integer\n    [ x = y ]\n"
        result <- getModel $ dir </> "model.cfr"
        case result of
            Left errs -> ("lib.cfr:3:11" `isInfixOf` show errs) @? ("The error names the imported file: " ++ show errs)
            Right _   -> assertFailure "The error in the imported file is not reported"

-- The constraints of an unsatisfiable core name the files that they are in.
case_ImportsUnsatCore :: Assertion
case_ImportsUnsatCore = do
        dir <- testDir "ImportsUnsatCore"
        writeFile (dir </> "lib2.cfr") "B\n    y : integer\n    [ y > 2 ]\n"
        writeFile (dir </> "unsat.cfr") "//# OPTIONS --import=lib2.cfr\n[ B.y < 2 ]\n"
        result <- getModel $ dir </> "unsat.cfr"
        case result of
            Right UnsatCore{unsatConstraints = core} -> do
                let shown = map show [ info | UserConstraint{constraintInfo = info} <- core ]
                any ("y > 2 (" `isPrefixOf`) shown && any ("lib2.cfr:3:7)" `isSuffixOf`) shown @? ("The core names lib2.cfr: " ++ show shown)
                any ("B.y < 2 (" `isPrefixOf`) shown && any ("unsat.cfr:2:3)" `isSuffixOf`) shown @? ("The core names unsat.cfr: " ++ show shown)
            _ -> assertFailure "The model has no unsatisfiable core"

-- The unsat core has the constraints of abstract clafers and of their children, each at its
-- position in the model (#46).
case_AbstractClaferUnsatCore :: Assertion
case_AbstractClaferUnsatCore = do
        dir <- testDir "AbstractClaferUnsatCore"
        writeFile (dir </> "engine.cfr") $ unlines
            [ "abstract Engine"
            , "    power -> integer"
            , "gas : Engine"
            , "    [ power = 2 ]"
            , "electric : Engine"
            , "    [ power = 5 ]"
            , "abstract Car"
            , "    abstract engine -> Engine"
            , "        [ power >= 4 ]"
            , "golf : Car"
            , "    motor : engine -> gas"
            ]
        writeFile (dir </> "car.cfr") $ unlines
            [ "abstract Car"
            , "    x -> integer"
            , "        [ this > 5 ]"
            , "    [ x < 3 ]"
            , "golf : Car"
            ]
        engine <- runClaferIGT (defaultIGArgs $ dir </> "engine.cfr") $ do
            solve
            fastest <- next
            setUnsatCoreMinimization 0
            solve
            best <- next
            return (fastest, best)
        case fromRight engine of
            (UnsatCore{unsatConstraints = fastest}, UnsatCore{unsatConstraints = best}) -> do
                let core = shownCore fastest
                any ("power >= 4 (" `isPrefixOf`) core && any ("engine.cfr:9:11)" `isSuffixOf`) core @? ("The core has the constraint of the abstract engine: " ++ show core)
                any ("power = 2 (" `isPrefixOf`) core @? ("The core has the power of gas: " ++ show core)
                let minimal = shownCore best
                length minimal == 2 && not (any ("power = 5" `isPrefixOf`) minimal) @? ("The minimal core leaves out electric: " ++ show minimal)
            _ -> assertFailure "The model has no unsatisfiable core"
        car <- getModel $ dir </> "car.cfr"
        case fromRight car of
            UnsatCore{unsatConstraints = core'} -> do
                let core = shownCore core'
                any ("x < 3 (" `isPrefixOf`) core && any ("this > 5 (" `isPrefixOf`) core @? ("The core has both constraints of the abstract car: " ++ show core)
            _ -> assertFailure "The model has no unsatisfiable core"
    where
    shownCore core = map show [ info | UserConstraint{constraintInfo = info} <- core ]

-- The command c shows the scope of every clafer, also of the first one: the line of each
-- clafer's declaration names the clafer.
case_LineNumMapFirstClafer :: Assertion
case_LineNumMapFirstClafer = do
        dir <- testDir "LineNumMapFirstClafer"
        writeFile (dir </> "lines.cfr") "B\n    y : integer\n    [ y > 2 ]\nC\n"
        result <- runClaferIGT (defaultIGArgs $ dir </> "lines.cfr") getlineNumMap
        case result of
            Right lineNumMap -> Map.toList lineNumMap @?= [(1, "B"), (2, "y"), (4, "C")]
            Left errs -> assertFailure $ show errs
