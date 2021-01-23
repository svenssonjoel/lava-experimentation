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

main :: IO ()
main =
  do
    test_seq
    writeVhdl "test" and2

