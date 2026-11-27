{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
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
-- | The version of claferIG with its build date, as @claferIG --version@ prints it:
--   @ClaferIG v0.6.0.2026-11-29@, the format of @clafer --version@ and @chocosolver --version@.
--
--   The build date is the day this module was compiled, or the day of @SOURCE_DATE_EPOCH@
--   ("Language.Clafer.BuildDate"). It is compiled in every build (@-fforce-recomp@), so the date
--   is never one of an earlier build; the other modules see no change, since 'buildDate' is not
--   inlined, and are only linked again.
module Language.Clafer.IG.Version (buildDate, claferIGVersion) where

import Data.Version (showVersion)

import Language.Clafer.BuildDate (buildDateQ)
import qualified Paths_claferIG

-- | The day claferIG was built, @YYYY-MM-DD@.
buildDate :: String
buildDate = $(buildDateQ)
{-# NOINLINE buildDate #-}

-- | @ClaferIG v0.6.0.2026-11-29@: the version and the build date.
claferIGVersion :: String
claferIGVersion = "ClaferIG v" ++ showVersion Paths_claferIG.version ++ "." ++ buildDate
