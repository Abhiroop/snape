{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Spec where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.StaticPtr

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

class Applicable t a b where
  type ResultTy t a b
  apply :: Task a b -> t a -> ResultTy t a b
