module Lava.Operators where

import Lava.Signal
import Lava.Generic
import Lava.Error

infix  4 <==>
infixr 3 <&>
infixr 2 <|>, ==>, <==
infixr 2 <=>, <#>
infixr 1 |->

----------------------------------------------------------------------
-- Gates

and2 (x, y) = andl [x, y]
or2  (x, y) = orl  [x, y]
xor2 (x, y) = xorl [x, y]

nand2 = inv . and2
nor2  = inv . or2
xnor2 = inv . xor2

equiv (x, y) = xnor2 (x, y)
impl  (x, y) = or2   (inv x, y)

nandl = inv . andl
norl  = inv . orl

----------------------------------------------------------------------
-- Binary Operators

x |->  y = delay  x  y
x <==> y = equal (x, y)

x <&> y = and2  (x, y)
x <|> y = or2   (x, y)
x <#> y = xor2  (x, y)
x <=> y = equiv (x, y)
x ==> y = impl  (x, y)
x <== y = impl  (y, x)


----------------------------------------------------------------------
-- the end.

