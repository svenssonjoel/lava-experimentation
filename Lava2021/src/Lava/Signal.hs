module Lava.Signal where

import Lava.Ref
import Lava.Sequent
import Lava.Error

import Data.List
  ( transpose
  )

----------------------------------------------------------------
-- Signal, Symbol, S

newtype Signal a
  = Signal Symbol

newtype Symbol
  = Symbol (Ref (S Symbol))
    
data S s
  = Bool      Bool
  | Inv       s
  | And       [s]
  | Or        [s]
  | Xor       [s]
  | VarBool   String
  | DelayBool s s

  | If       s s s

  -- A node that collects several signals of the
  -- same type into a vector. 
  | Vec -- A vector of signals
    Int -- Number of elements
    [s] -- Values

  -- Index into a vector. s must be a Vec node!
  | VecIndex Int s 

  | Component
    String           -- name
    [s]              -- inputs

  -- Accessing an ouput from a component (indexing)
  | ComponentOutput
    CompOut          -- A specific output bit
    s                -- Component producing output

-- Identifier for component outputs
-- BitOutput has an index
-- VecOutput has a length and an index
-- TupleOutput has a length and an index
data CompOut
  = BitOutput  
  | VecOutput   Int Int CompOut
  | TupleOutput Int Int [CompOut]
  deriving (Eq, Show)
-- TupleOutput only allowed as the toplevel 

symbol :: S Symbol -> Symbol
symbol = Symbol . ref

unsymbol :: Symbol -> S Symbol
unsymbol (Symbol r) = deref r

unsignal :: Signal a -> Symbol
unsignal (Signal s) = s

instance Eq (Signal a) where
  Signal (Symbol r1) == Signal (Symbol r2) = r1 == r2

----------------------------------------------------------------
-- operations

-- on bits

bool :: Bool -> Signal Bool
bool b = lift0 (Bool b)

low, high :: Signal Bool
low  = bool False
high = bool True

inv :: Signal Bool -> Signal Bool
inv = lift1 Inv

andl, orl, xorl :: [Signal Bool] -> Signal Bool
andl = liftl And
orl  = liftl Or
xorl = liftl Xor

equalBool :: Signal Bool -> Signal Bool -> Signal Bool
equalBool x y = inv (xorl [x,y])

ifBool :: Signal Bool -> (Signal Bool, Signal Bool) -> Signal Bool
ifBool c (x,y) = orl[andl[c,x],andl[inv c,y]]

delayBool :: Signal Bool -> Signal Bool -> Signal Bool
delayBool = lift2 DelayBool

varBool :: String -> Signal Bool
varBool s = lift0 (VarBool s)

-- liftings

lift0 :: S Symbol -> Signal a
lift0 oper = Signal (symbol oper)

lift1 :: (Symbol -> S Symbol) -> Signal a -> Signal b
lift1 oper (Signal a) = Signal (symbol (oper a))

lift2 :: (Symbol -> Symbol -> S Symbol) -> Signal a -> Signal b -> Signal c
lift2 oper (Signal a) (Signal b) = Signal (symbol (oper a b))

lift3 :: (Symbol -> Symbol -> Symbol -> S Symbol)
      -> Signal a -> Signal b -> Signal c -> Signal d
lift3 oper (Signal a) (Signal b) (Signal c) = Signal (symbol (oper a b c))

liftl :: ([Symbol] -> S Symbol) -> [Signal a] -> Signal c
liftl oper sigas = Signal (symbol (oper (map (\(Signal a) -> a) sigas)))

----------------------------------------------------------------
-- evaluate

eval :: S (S a) -> S a
eval s =
  case s of
    Bool b       -> Bool b
    Inv (Bool b) -> Bool (not b)
    And xs       -> Bool . all bval $ xs
    Or xs        -> Bool . any bval $ xs
    Xor xs       -> Bool . (1 ==) . length . filter bval $ xs

    If (Bool c) x y       -> if c then x else y

    DelayBool s s' -> wrong Lava.Error.DelayEval
    VarBool   s    -> wrong Lava.Error.VarEval
 where
  bval (Bool b) = b
  
evalLazy :: S (Maybe (S a)) -> Maybe (S a)
evalLazy s =
  case s of
    -- lazy
    And xs
      | any (`bval` False) xs        -> bans False
      
    Or xs
      | any (`bval` True) xs         -> bans True
      
    Xor xs
      | number (`bval` True) xs >= 2 -> bans False
    
    -- strict
    _ -> eval `fmap` sequent s
    
 where
  bans = Just . Bool
 
  bval (Just (Bool b)) b' = b == b'
  bval _               _  = False
  
  number p = length . filter p
  
arguments :: S a -> [a]
arguments s =
  case s of
    Bool b     -> []
    Inv s      -> [s]
    And xs     -> xs
    Or xs      -> xs
    Xor xs     -> xs

    If x y z   -> [x,y,z]

    DelayBool s s' -> [s,s']
    VarBool s      -> []

zips :: S [a] -> [S a]
zips s =
  case s of
    Bool b     -> [Bool b]
    Inv s      -> map Inv s
    And xs     -> map And (transpose xs)
    Or xs      -> map Or  (transpose xs)
    Xor xs     -> map Xor (transpose xs)

    If x y z   -> zipWith3 If x y z

    DelayBool s s' -> zipWith DelayBool s s'
    VarBool s      -> [VarBool s]

----------------------------------------------------------------
-- properties of S

instance Functor S where
  fmap f s =
    case s of
      Bool b    -> Bool b
      Inv x     -> Inv (f x)
      And xs    -> And (map f xs)
      Or  xs    -> Or  (map f xs)
      Xor xs    -> Xor (map f xs)

      If x y z  -> If (f x) (f y) (f z)

      DelayBool x y -> DelayBool (f x) (f y)
      VarBool   v   -> VarBool v

instance Sequent S where
  sequent s = 
    case s of
      Bool b    -> lift0 (Bool b)
      Inv x     -> lift1 Inv x
      And xs    -> liftl And xs
      Or  xs    -> liftl Or  xs
      Xor xs    -> liftl Xor xs

      If x y z  -> lift3 If x y z

      DelayBool x y -> lift2 DelayBool x y
      VarBool  v    -> lift0 (VarBool v)
   where
    lift0 op =
      do return op

    lift1 op x =
      do x' <- x
         return (op x')

    lift2 op x y =
      do x' <- x
         y' <- y
         return (op x' y')

    lift3 op x y z =
      do x' <- x
         y' <- y
         z' <- z
         return (op x' y' z')

    liftl op xs =
      do xs' <- sequence xs
         return (op xs')

instance Show (Signal a) where
  showsPrec n (Signal s) =
    showsPrec n s

instance Show Symbol where
  showsPrec n sym =
    showsPrec n (unsymbol sym)

instance Show a => Show (S a) where
  showsPrec n s =
    case s of
      Bool True  -> showString "high"
      Bool False -> showString "low"

      Inv x      -> showString "inv"  . showList [x]
      And xs     -> showString "andl" . showList xs
      Or  xs     -> showString "orl"  . showList xs
      Xor xs     -> showString "xorl" . showList xs

      If x y z   -> showString "ifThenElse" . showList [x,y,z]

      DelayBool x y -> showString "delay" . showList [x,y]
      
      VarBool s     -> showString s

      Vec i xs    -> showString ("Vector " ++ show i) . showList xs
      VecIndex i s  -> showString ("Index " ++ show i) . showList [s]
      Component nom  xs -> showString nom . showList xs
      ComponentOutput bit s -> showString ((show bit) ++ " of ") . showList [s]      
--      _             -> showString "<<symbol>>"

----------------------------------------------------------------
-- the end.

