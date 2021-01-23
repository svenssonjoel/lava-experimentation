
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
mkVec sigs = genericLift0 (Vec n (-1) (map unwrapElt sigs))
 where n = length sigs

packVec :: Elt a => [a] -> Vector a
packVec sigs = mkVec sigs

unpackVec :: Elt a => Vector a -> [a]
unpackVec (Vector s) = case unsymbol s of
                         Vec n _ sigs -> [genericLift0
                                           (Vec n i sigs) 
                                         | i <- [0..n-1]]
                         _ -> wrong Lava.Error.NotAVec

-- Operations

varVec :: String -> Int -> Vector (Signal Bool)
varVec s n = mkVec [varBool (s ++ (show i)) | i <- [0..n-1]]

-- Lifting

genericLift0 :: Elt a => S Symbol -> a
genericLift0 oper = wrapElt (symbol oper) 

liftlVec :: (Elt a, Elt c) => ([Symbol] -> S Symbol) -> [a] -> Vector c
liftlVec oper sigas = Vector (symbol (oper (map unwrapElt sigas)))
