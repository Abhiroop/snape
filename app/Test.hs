{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StaticPointers #-}
module Test where

-- This is a test file to experiment with static pointers. Will be checked out later. Need to track this for certain tricky cases.
import Control.Concurrent.Chan.Unagi.Bounded
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Debug.Trace
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

data Foo a b seed = Map (a -> b)
                  | Reduce (a -> b -> b) seed

foo :: Int -> Int
foo = (+1)

bar :: Int -> Int -> Int
bar = (+)

send :: IO (Either [Int] Int)
send = do
  let list = [1,2,3]
      a = static (Map foo)      :: StaticPtr (Foo Int Int Int)
      --b = static (Reduce bar 0) :: StaticPtr (Foo Int Int Int)
      t = staticKey a
      --t = staticKey b
  -- x <- unsafeLookupStaticPtr t
  -- case x of
  --   Just sptr -> case deRefStaticPtr sptr of
  --                  Map f -> return $ Containerized $ map f list
  --                  --_     -> error "Task undefined"
  --   Nothing -> error "Wrong function serialized"
  x <- unsafeLookupStaticPtr t
  case x of
    Just sptr -> case deRefStaticPtr sptr of
                   Reduce f z -> return $ Right $ foldr f z list
                   Map f      -> return $ Left $ map f list
    Nothing -> error "Wrong function serialized"


data Test a b s = A (a -> b) | B (a -> b -> b) s

baz :: IO (Either [Int] Int)
baz = do
  let b = B bar 0
  case b of
    A f   -> return $ Left $ map f [1,2,3]
    B f s -> return $ Right $ foldr f s [1,2,3]

-----------------------------------
-- UnagiChan

test :: IO ()
test = do
  (inC, outC) <- newChan 5
  writeChan inC 6
  writeChan inC 7
  writeChan inC 8
  x <- readChan outC
  print x
  n <- estimatedLength inC
  print n
  return ()
