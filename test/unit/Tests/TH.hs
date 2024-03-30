{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | TH Unit tests.
--
-- @since 0.1.0.0
module Tests.TH (tests) where

import Data.Version.Package qualified as PV
import Data.Version.Package.Internal (PackageVersion (MkPackageVersion))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- @since 0.1.0.0
tests :: TestTree
tests =
  Tasty.testGroup
    "TH tests"
    [ listIntTHSuccess,
      thSuccess,
      stringTHSuccess,
      stringTHFailure,
      textTHSuccess,
      textTHFailure
    ]

listIntTHSuccess :: TestTree
listIntTHSuccess = THU.testCase "mkPackageVersionTH creates PackageVersion" $ do
  let version = $$(PV.mkPackageVersionTH [0, 1, 0, 0])
  MkPackageVersion [0, 1, 0, 0] @=? version

thSuccess :: TestTree
thSuccess = THU.testCase "packageVersionTH retrieves version" $ do
  let version = $$(PV.packageVersionTH "package-version.cabal")
  MkPackageVersion [0, 4] @=? version

stringTHSuccess :: TestTree
stringTHSuccess = THU.testCase "packageVersionStringTH retrieves version" $ do
  let version = $$(PV.packageVersionStringTH "package-version.cabal")
  "0.4" @=? version

stringTHFailure :: TestTree
stringTHFailure = THU.testCase "packageVersionStringTH retrieves UNKNOWN" $ do
  let version = $$(PV.packageVersionStringTH "not-found.cabal")
  "UNKNOWN" @=? version

textTHSuccess :: TestTree
textTHSuccess = THU.testCase "packageVersionTextTH retrieves version" $ do
  let version = $$(PV.packageVersionTextTH "package-version.cabal")
  "0.4" @=? version

textTHFailure :: TestTree
textTHFailure = THU.testCase "packageVersionTextTH retrieves UNKNOWN" $ do
  let version = $$(PV.packageVersionTextTH "not-found.cabal")
  "UNKNOWN" @=? version
