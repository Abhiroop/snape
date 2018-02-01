{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StaticPointers #-}
module Test where

-- This is a test file to experiment with static pointers. Will be checked out later. Need to track this for certain tricky cases.
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import GHC.StaticPtr

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main = do
  let sptr = static fact :: StaticPtr (Int -> Int)
  print $ staticPtrInfo sptr
  print $ deRefStaticPtr sptr 10

data Bar = C StaticKey deriving Generic

instance Binary Bar

data Foo f = Map f deriving (Generic, Typeable)

foo :: Int -> Int
foo = (+1)

send :: IO ()
send = do
  let a = static (Map foo) :: StaticPtr (Foo (Int -> Int))
      t = staticKey a
  x <- unsafeLookupStaticPtr t
  return ()
