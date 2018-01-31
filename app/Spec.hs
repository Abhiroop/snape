{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Spec where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.StaticPtr

-- Limiting the kind of tasks possible right now but will extend this in future to include all kinds of Haskell tasks
-- TODO: Look at LINQ operators for this function
-- (a -> b) is the serialized function to be applied here Eg: (+ 1)
data Task a b = Map     (StaticPtr (a -> b))
              | Filter  (StaticPtr (a -> Bool))
              | Reduce  (StaticPtr ((a -> b -> b) -> b))
              | GroupBy (StaticPtr (a -> a -> Bool))
              deriving (Generic, Typeable)

-- TODO: Need to serialize the task for sending the function
--instance (Binary a, Binary b) => Binary (Task a b)

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

data Result t b = Singleton b
                | Container (t b)
                | Grouped (t (t b))

-- Here everything can be polymorphic.
-- t can be any container
-- a and b are of course polymorphic

class Applicable t a b where
  apply :: Task a b -> t a -> Result t b

