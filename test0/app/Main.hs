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

-- Probably not correct ;) 
add :: Vector (Signal Bool) -> Vector (Signal Bool) -> Vector (Signal Bool)
add a b = packVec $ snd $ foldr (\(i,j) (cin,sums) ->
                                    let (s, cout) = fullAdder i j cin
                                    in (cout, s:sums)) (low,[]) (zip (reverse a')
                                                                     (reverse b'))
  where a' = unpackVec a
        b' = unpackVec b

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

    putStrLn "----------------------"

    putStrLn (show (add
                    (packVec [bool True, bool False])
                    (packVec [bool False, bool True])))
