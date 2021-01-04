module Main where

import Lib

import Lava
import Arithmetic
import SequentialCircuits

import Vhdl

------------------------------------------------------------
-- Lava experimentation test 0

test_seq :: IO ()
test_seq = writeVhdl "seq0" (edge . toggle)


main :: IO ()
main =
  do
    test_seq
    writeVhdl "test" and2

