{-# OPTIONS_GHC -XTemplateHaskell #-}
import ClaferIG
import ClaferModel
import CommandLine
import Solution
import Sugarer
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

tg_testsuite = $(testGroupGenerator)

main = defaultMain[tg_testsuite]
