{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StaticPointers #-}
module Test where

import GHC.StaticPtr

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main = do
  let sptr = static fact :: StaticPtr (Int -> Int)
  print $ staticPtrInfo sptr
  print $ deRefStaticPtr sptr 10
