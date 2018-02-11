module Data.Graph where

import Data.Spec

{- Representation of a Graph

x = [1,2,3]
y = map (+ 1) x
z = reduce (+) 0 y

        z
        |
        v
    reduce <+>
      /   \
   <0>     y
           |
           v
        map <+ 1>
           |
           v
        x = [1,2,3]

{ x : [1,2,3]
  y : (Map (+ 1), x)
  z : (Reduce (+) 0, y)
}

Function := Standard Haskell functions
Data     := Repa Array/ hmatrix matrix etc
Tuple    := (Task Type, Function)
Map      := < Variable, Data/Tuple>


-}
