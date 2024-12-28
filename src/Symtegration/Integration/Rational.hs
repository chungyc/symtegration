-- |
-- Module: Symtegration.Integration.Rational
-- Description: Integration of rational functions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Integrates rational functions.
-- Rational functions are ratios of two polynomials,
-- not functions of rational numbers.
-- Only rational number coefficients are supported.
module Symtegration.Integration.Rational (integrate) where

import Data.Text (Text)
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Substitution qualified as Substitution
import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- $setup
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify.RecursiveHeuristic

-- | Integrate a ratio of two polynomials with rational number coefficients.
--
-- >>> let p = "x" ** 7 - 24 * "x" ** 4 - 4 * "x" ** 2 + 8 * "x" - 8
-- >>> let q = "x" ** 8 + 6 * "x" ** 6 + 12 * "x" ** 4 + 8 * "x" ** 2
-- >>> toHaskell . simplify <$> integrate "x" (p / q)
-- Just "(3 / ((x ** 2) + 2)) + ((8 * (x ** 2) + 4) / ((x ** 5) + 4 * (x ** 3) + 4 * x)) + (log x)"
integrate :: Text -> Expression -> Maybe Expression
integrate v e
  | (x :/: y) <- e',
    (Just n) <- fromExpression (forVariable v) x,
    (Just d) <- fromExpression (forVariable v) y =
      integrate' n d
  | otherwise = Nothing
  where
    e' = simplify v e
    integrate' n d = (+) (sum (map fromRationalFunction g)) <$> logs
      where
        (g, h) = hermiteReduce $ toRationalFunction n d
        -- For now, try to integrate by powers.
        -- We will want something more complete for this remaining portion eventually.
        logs = Substitution.integrate [Powers.integrate] v (simplify v $ fromRationalFunction h)
        fromRationalFunction (RationalFunction u w) = u' / w'
          where
            u' = toExpression v toRationalCoefficient u
            w' = toExpression v toRationalCoefficient w

-- | Represents the ratio of two polynomials with rational number coefficients.
data RationalFunction = RationalFunction IndexedPolynomial IndexedPolynomial
  deriving (Eq)

instance Num RationalFunction where
  (RationalFunction x y) + (RationalFunction u v) =
    toRationalFunction (x * v + u * y) (y * v)

  (RationalFunction x y) - (RationalFunction u v) =
    toRationalFunction (x * v - u * y) (y * v)

  (RationalFunction x y) * (RationalFunction u v) =
    toRationalFunction (x * u) (y * v)

  abs = id

  signum 0 = 0
  signum _ = 1

  fromInteger n = RationalFunction (fromInteger n) 1

-- | Form a rational function from two polynomials.
-- The polynomials will be reduced so that the numerator and denominator are coprime.
toRationalFunction ::
  -- | Numerator.
  IndexedPolynomial ->
  -- | Denominator.
  IndexedPolynomial ->
  RationalFunction
toRationalFunction x 0 = RationalFunction x 0
toRationalFunction x y = RationalFunction x' y'
  where
    g = monic $ greatestCommonDivisor x y
    (x', _) = x `divide` g
    (y', _) = y `divide` g

-- | Applies Hermite reduction to a rational function.
-- Returns a list of rational functions whose sums add up to the integral
-- and a rational function which remains to be integrated.
--
-- Specifically, for rational function \(x = \frac{A}{D}\),
-- where \(A\) and \(D\) are coprime, then for return value @(gs, h)@,
-- the sum of @gs@ is equal to \(g\) and @h@ is equal to \(h\) in the following:
--
-- \[ \frac{A}{D} = \frac{dg}{dx} + h \]
hermiteReduce :: RationalFunction -> ([RationalFunction], RationalFunction)
hermiteReduce h@(RationalFunction _ 0) = ([], h)
hermiteReduce h@(RationalFunction x y)
  | (Just z) <- reduce x [] common = z
  | otherwise = ([], h) -- Should never happen, but a fallback if it does.
  where
    common = monic $ greatestCommonDivisor y $ differentiate y
    (divisor, _) = y `divide` common
    reduce a g d
      | degree d > 0 = do
          let d' = monic $ greatestCommonDivisor d $ differentiate d
          let (d'', _) = d `divide` d'
          let (d''', _) = (divisor * differentiate d) `divide` d
          (b, c) <- diophantineEuclidean (-d''') d'' a
          let (b', _) = (differentiate b * divisor) `divide` d''
          let a' = c - b'
          let g' = toRationalFunction b d : g
          reduce a' g' d'
      | otherwise = Just (g, toRationalFunction a divisor)
