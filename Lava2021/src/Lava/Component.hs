-- Copyright 2021 Bo Joel Svensson

--    Licensed under the Apache License, Version 2.0 (the "License");
--    you may not use this file except in compliance with the License.
--    You may obtain a copy of the License at

--        http://www.apache.org/licenses/LICENSE-2.0

--    Unless required by applicable law or agreed to in writing, software
--    distributed under the License is distributed on an "AS IS" BASIS,
--    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--    See the License for the specific language governing permissions and
--    limitations under the License.
{-# LANGUAGE ScopedTypeVariables #-} 

module Lava.Component where

import Lava.Ref
import Lava.Error
import Lava.Generic
import Lava.Signal
import Lava.Vector

-- TODO: See what we can do without going totally crazy about types.  



----------------------------------------------------------------
-- Component


-- No. Bad approach. Not possible to reconstruct a structured
--  output type unless we maintain a lot of size info in the types.
--  Do not want to go there.
--
-- mkComponent :: forall a b . (Generic a, Generic b) => String -> a -> b
-- mkComponent name ins =
--   let si = struct ins
--       fi = flatten si
--       so = struct (undefined :: a)
--   in undefined 
      


----------------------------------------------------------------
-- Some components

bitwiseNeg :: Vector (Signal Bool) -> Vector (Signal Bool)
bitwiseNeg vec = packVec outs  
  where
    n = lengthVec vec
    output i = VecOutput n i BitOutput
  
    outs = [(wrapElt (symbol (ComponentOutput (output i) c))) :: Signal Bool| i <- [0..n-1]]
    
    -- Want just one unique symbol per component instantiation.
    -- Each output node will refer to the same component symbol.
    c = symbol (Component "bitwiseNeg" [unVec vec])
    
bitwiseAnd :: Vector (Signal Bool) -> Vector (Signal Bool) -> Vector (Signal Bool)
bitwiseAnd a b =
  if a_n == b_n
  then packVec outs
  else wrong Lava.Error.VecSizeMismatch
  where
    a_n = lengthVec a
    b_n = lengthVec b
    output i = VecOutput a_n i BitOutput
    outs = [(wrapElt (symbol (ComponentOutput (output i) c))) :: Signal Bool| i <- [0..a_n-1]]

    -- Maybe need to store away input vector sizes so appropriateVHDL
    -- can be be generated. This info is present in the vectors though
    -- so should be possible to rebuild that info from the AST. 
    c = symbol (Component "bitwiseAnd" [unVec a, unVec b]) -- two vector inputs
    

fullAdder :: Signal Bool -> Signal Bool -> Signal Bool -> (Signal Bool, Signal Bool)
fullAdder a b cin = (o, sum) 
  where
    o = lift0 (ComponentOutput (output 0) c)
    sum = lift0 (ComponentOutput (output 1) c)
    output i = TupleOutput 2 i [BitOutput, BitOutput]
    c = symbol (Component "fullAdder" (map unsignal [a,b,cin]))
