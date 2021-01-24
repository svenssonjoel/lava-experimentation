module Lava.Arithmetic where

import Lava
import Lava.Patterns

----------------------------------------------------------------
-- Basic Components

halfAdd (a, b) = (sum, carry)
  where
    sum   = xor2 (a, b)
    carry = and2 (a, b)

fullAdd (carryIn, (a, b)) = (sum, carryOut)
  where
    (sum1, carry1) = halfAdd (a, b)
    (sum, carry2)  = halfAdd (carryIn, sum1)
    carryOut       = xor2 (carry1, carry2)

bitAdder = row halfAdd

adder (carryIn, ([],   []))   = ([], carryIn)
adder (carryIn, (as,   []))   = bitAdder (carryIn, as)
adder (carryIn, ([],   bs))   = bitAdder (carryIn, bs)
adder (carryIn, (a:as, b:bs)) = (s:ss, carryOut)
  where
    (s, carry)     = fullAdd (carryIn, (a, b))
    (ss, carryOut) = adder (carry, (as, bs))

binAdder (as, bs) = sum ++ [carryOut]
  where
    (sum, carryOut) = adder (low, (as, bs))

bitMulti (a, bs) = [ and2 (a, b) | b <- bs ]

multi ([],   []) = []
multi (as,   []) = replicate (length as) low
multi ([],   bs) = replicate (length bs) low
multi (a:as, bs) = m : ms
  where
    (m:abs) = bitMulti (a, bs)
    asbs    = multi (as, bs)
    (ms,_)  = adder (low, (abs, asbs))

----------------------------------------------------------------
-- the end.


