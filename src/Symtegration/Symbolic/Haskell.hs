-- |
-- Module: Symtegration.Symbolic.Haskell
-- Description: Converts a symbolic representation of a mathematical expression into equivalent Haskell code.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Support for converting symbolic representations of mathematical expressions
-- into equivalent Haskell code.
module Symtegration.Symbolic.Haskell
  ( toHaskell,

    -- * Support functions
    getUnaryFunctionText,
    getBinaryFunctionText,
  )
where

import Data.Text
import Symtegration.Symbolic
import TextShow (showt)

-- $setup
-- >>> import Symtegration.Symbolic

-- | Converts an 'Expression' into an equivalent Haskell expression.
--
-- >>> toHaskell $ BinaryApply Add (Number 1) (Number 3)
-- "1 + 3"
-- >>> toHaskell $ 1 + 3
-- "1 + 3"
--
-- Symbols are included without quotation.
--
-- >>> toHaskell $ ("x" + "y") * 4
-- "(x + y) * 4"
toHaskell :: Expression -> Text
toHaskell (Number n) = showt n
toHaskell (Symbol t) = t
toHaskell (UnaryApply fun x) = funcText <> " " <> asArg x
  where
    funcText = getUnaryFunctionText fun
toHaskell (LogBase' x y) = funcText <> " " <> asArg x <> " " <> asArg y
  where
    funcText = getBinaryFunctionText LogBase
toHaskell (x@(_ :*: _) :+: y@(_ :*: _)) = toHaskell x <> " + " <> toHaskell y
toHaskell (x@(_ :*: _) :+: y@(_ :+: _)) = toHaskell x <> " + " <> toHaskell y
toHaskell (x@(_ :*: _) :+: y) = toHaskell x <> " + " <> asArg y
toHaskell (x@(_ :+: _) :+: y@(_ :*: _)) = toHaskell x <> " + " <> toHaskell y
toHaskell (x :+: y@(_ :*: _)) = asArg x <> " + " <> toHaskell y
toHaskell (x@(_ :+: _) :+: y@(_ :+: _)) = toHaskell x <> " + " <> toHaskell y
toHaskell (x@(_ :+: _) :+: y) = toHaskell x <> " + " <> asArg y
toHaskell (x :+: y@(_ :+: _)) = asArg x <> " + " <> toHaskell y
toHaskell (x@(_ :*: _) :*: y@(_ :*: _)) = toHaskell x <> " * " <> toHaskell y
toHaskell (x@(_ :*: _) :*: y) = toHaskell x <> " * " <> asArg y
toHaskell (x :*: y@(_ :*: _)) = asArg x <> " * " <> toHaskell y
toHaskell (x@(_ :+: _) :-: y) = toHaskell x <> " - " <> asArg y
toHaskell (BinaryApply op x y) = asArg x <> " " <> opText <> " " <> asArg y
  where
    opText = getBinaryFunctionText op

-- | Converts an 'Expression' to Haskell code appropriate for use as an argument.
-- In other words, show numbers and symbols as is, while surrounding everything
-- else in parentheses.
asArg :: Expression -> Text
asArg x@(Number n)
  | n >= 0 = toHaskell x
  | otherwise = "(" <> toHaskell x <> ")"
asArg x@(Symbol _) = toHaskell x
asArg x = "(" <> toHaskell x <> ")"

-- | Returns the corresponding Haskell function name.
getUnaryFunctionText :: UnaryFunction -> Text
getUnaryFunctionText Negate = "negate"
getUnaryFunctionText Abs = "abs"
getUnaryFunctionText Signum = "signum"
getUnaryFunctionText Exp = "exp"
getUnaryFunctionText Log = "log"
getUnaryFunctionText Sqrt = "sqrt"
getUnaryFunctionText Sin = "sin"
getUnaryFunctionText Cos = "cos"
getUnaryFunctionText Tan = "tan"
getUnaryFunctionText Asin = "asin"
getUnaryFunctionText Acos = "acos"
getUnaryFunctionText Atan = "atan"
getUnaryFunctionText Sinh = "sinh"
getUnaryFunctionText Cosh = "cosh"
getUnaryFunctionText Tanh = "tanh"
getUnaryFunctionText Asinh = "asinh"
getUnaryFunctionText Acosh = "acosh"
getUnaryFunctionText Atanh = "atanh"

-- | Returns the corresponding Haskell function name.
--
-- For binary operators, it will be the infix form.
-- In other words, @"+"@ will be returned for 'Add', not @"(+)"@.
getBinaryFunctionText :: BinaryFunction -> Text
getBinaryFunctionText Add = "+"
getBinaryFunctionText Multiply = "*"
getBinaryFunctionText Subtract = "-"
getBinaryFunctionText Divide = "/"
getBinaryFunctionText Power = "**"
getBinaryFunctionText LogBase = "logBase"
