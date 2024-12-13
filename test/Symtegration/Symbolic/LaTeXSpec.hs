-- |
-- Description: Tests for Symtegration.Symbolic.LaTeX.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.LaTeXSpec (spec) where

import Symtegration.Symbolic.Arbitrary ()
import Symtegration.Symbolic.LaTeX
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ describe "toLaTeX" $ do
  -- There really should be more tests, but for now, just check that it is total.
  prop "is total" $ \e -> total (toLaTeX e)
