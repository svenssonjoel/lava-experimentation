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


compBitwiseInv :: Signal BitVec -> Signal BitVec
compBitwiseInv s = head $ mkComponent ("bitwise_neg_" ++ (show n)) n [s]
  where
    sigs = unpackVec s
    n = length sigs 



main :: IO ()
main =
  do
    test_seq
    writeVhdl "test" and2

