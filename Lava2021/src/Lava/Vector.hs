
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

module Lava.Vector where

import Lava.Ref
import Lava.Error
import Lava.Generic
import Lava.Signal

data Vector a = Vector Symbol

-- So we can have tuples of vectors 
instance Generic (Vector a) where
  struct    (Vector s) = Object s
  construct (Object s) = Vector s

instance Eq (Vector a) where
  Vector (Symbol r1) == Vector (Symbol r2) = r1 == r2

instance Show (Vector a) where
  showsPrec n (Vector s) =
    showsPrec n s

class Elt a where
  unwrapElt :: a -> Symbol
  wrapElt   :: Symbol -> a

instance Elt (Signal a) where
  unwrapElt (Signal s) = s
  wrapElt s = Signal s
  
instance Elt (Vector a) where
  unwrapElt (Vector s) = s
  wrapElt s = Vector s
  
mkVec :: Elt a => [a] -> Vector a
mkVec sigs = genericLift0 (Vec n (map unwrapElt sigs))
 where n = length sigs

unVec :: Elt a => Vector a -> Symbol
unVec (Vector s) = s

packVec :: Elt a => [a] -> Vector a
packVec sigs = mkVec sigs

unpackVec :: Elt a => Vector a -> [a]
unpackVec (Vector s) = case unsymbol s of
                         Vec n sigs -> [genericLift0
                                         (VecIndex i s) 
                                       | i <- [0..n-1]]
                         _ -> wrong Lava.Error.NotAVec

-- Operations

lengthVec :: Elt a => Vector a -> Int
lengthVec v = case unsymbol (unwrapElt v) of
                Vec n sigs -> n
                _ -> wrong Lava.Error.NotAVec


varVec :: String -> Int -> Vector (Signal Bool)
varVec s n = mkVec [varBool (s ++ (show i)) | i <- [0..n-1]]

-- Lifting

genericLift0 :: Elt a => S Symbol -> a
genericLift0 oper = wrapElt (symbol oper) 

liftlVec :: (Elt a, Elt c) => ([Symbol] -> S Symbol) -> [a] -> Vector c
liftlVec oper sigas = Vector (symbol (oper (map unwrapElt sigas)))
