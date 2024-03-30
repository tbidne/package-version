{-# LANGUAGE CPP #-}

module Utils
  ( testPropertyCompat,
  )
where

import Hedgehog (Property, PropertyName)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog qualified as TH

testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat = TH.testPropertyNamed
#else
testPropertyCompat tn _ = TH.testProperty tn
#endif
