module Data.Spec where

import Data.List as L

data Task a b = Map     (a -> b)
              | Filter  (a -> Bool)
              | Reduce  (a -> b -> b) b
              | GroupBy (a -> a -> Bool)

data Result t a b = Mapped (t b)
                  | Filtered (t a)
                  | Reduced b
                  | Grouped (t (t a))

{- t a is a polymorphic container of a

1. t can be a repa array type
https://hackage.haskell.org/package/repa-3.4.1.3/docs/Data-Array-Repa.html#t:Array
2. t can be a hmatrix matrix type
https://hackage.haskell.org/package/hmatrix-0.18.2.0/docs/Numeric-LinearAlgebra-Data.html#t:Matrix
3. t can be a hmatrix vector type
https://hackage.haskell.org/package/hmatrix-0.18.2.0/docs/Numeric-LinearAlgebra-Data.html#t:Vector
4. t can be a Frames Frame type
https://hackage.haskell.org/package/Frames-0.3.0.2/docs/Frames-Frame.html#t:Frame
5. t can be an Accelerate Array type (GPU)
https://hackage.haskell.org/package/accelerate-1.1.1.0/docs/Data-Array-Accelerate.html#t:Array
6. t can be a plain Haskell container like a List
-}

class Applicable t where
  apply :: Task a b -> t a -> Result t a b

-- The design is to eventually add multiple typeclasses like Mappable, Reducible etc and expose these as the API.
-- The instances can be written in a separate project choosing whichever typeclass applies to that project.
-- This would make this project a plain scheduler which schedules functions like Map, reduce etc No implementation will be here
-- This is just a sample instance for experimentation
instance Applicable [] where
  apply (Map f)      x = Mapped   $ map f x
  apply (Filter f)   x = Filtered $ filter f x
  apply (Reduce f i) x = Reduced  $ foldr f i x
  apply (GroupBy f)  x = Grouped  $ L.groupBy f x
