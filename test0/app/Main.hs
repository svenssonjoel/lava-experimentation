module Main where

import Lib

import Lava
import Lava.Arithmetic
import Lava.SequentialCircuits

import Lava.Vhdl

------------------------------------------------------------
-- Lava experimentation test 0

test_seq :: IO ()
test_seq = writeVhdl "seq0" (edge . toggle)

(->-) a b = (.) b a

apa :: Vector (Signal Bool) -> Vector (Signal Bool) -> Vector (Signal Bool)
apa a b = bitwiseNeg (bitwiseAnd a b) 

bepa :: (Signal Bool, Signal Bool)
bepa = fullAdder low high low

main :: IO ()
main =
  do
    test_seq
    writeVhdl "test" and2

    putStrLn (show (apa
                     (packVec [bool True, bool False])
                     (packVec [bool False, bool True])))

    putStrLn "----------------------"

    putStrLn (show bepa)

{-
Vector 2 [VecOutput 2 0 BitOutput of
            [bitwise_neg [Vector 2 [VecOutput 2 0 BitOutput of
                                      [bitwiseAnd [Vector 2 [high,low],Vector 2[low,high]]]
                                   ,VecOutput 2 1 BitOutput of
                                      [bitwiseAnd [Vector 2 [high,low],Vector 2[low,high]]]]]]
         ,VecOutput 2 1 BitOutput of
            [bitwise_neg [Vector 2 [VecOutput 2 0 BitOutput of [bitwiseAnd [Vector 2 [high,low],Vector 2 [low,high]]]
                                   ,VecOutput 2 1 BitOutput of [bitwiseAnd [Vector 2 [high,low],Vector 2 [low,high]]]]]]]
----------------------
(TupleOutput 2 0 [BitOutput,BitOutput] of
    [fullAdder [low,high,low]]
,TupleOutput 2 1 [BitOutput,BitOutput] of
    [fullAdder[low,high,low]])


-}
