{-# LANGUAGE OverloadedLists #-}

-- | IO Unit tests.
--
-- @since 0.1.0.0
module Tests.IO (tests) where

import Control.Exception (try)
import Data.Version.Package qualified as PV
import Data.Version.Package.Internal
  ( PackageVersion (MkPackageVersion),
    ReadFileError (ReadFileErrorGeneral),
  )
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | @since 0.1.0.0
tests :: TestTree
tests =
  Tasty.testGroup
    "IO tests"
    [ throwIOSuccess,
      throwIOFailure,
      stringIOSuccess,
      stringIOFailure,
      textIOSuccess,
      textIOFailure,
      eitherIOSuccess,
      eitherIOFailure
    ]

throwIOSuccess :: TestTree
throwIOSuccess = THU.testCase "packageVersionThrowIO retrieves version" $ do
  version <- PV.packageVersionThrowIO "package-version.cabal"
  MkPackageVersion [0, 4] @=? version

throwIOFailure :: TestTree
throwIOFailure = THU.testCase "packageVersionThrowIO throws exception" $ do
  eVersion <- try $ PV.packageVersionThrowIO "not-found.cabal"
  case eVersion of
    Left (ReadFileErrorGeneral _) -> pure ()
    Left ex ->
      THU.assertFailure $
        "Received unexpected exception: " <> show ex
    Right pv ->
      THU.assertFailure $
        "Did not throw expected exception, received: " <> show pv

stringIOSuccess :: TestTree
stringIOSuccess = THU.testCase "packageVersionStringIO retrieves version" $ do
  version <- PV.packageVersionStringIO "package-version.cabal"
  "0.4" @=? version

stringIOFailure :: TestTree
stringIOFailure = THU.testCase "packageVersionStringIO retrieves UNKNOWN" $ do
  version <- PV.packageVersionStringIO "not-found.cabal"
  "UNKNOWN" @=? version

textIOSuccess :: TestTree
textIOSuccess = THU.testCase "packageVersionTextIO retrieves version" $ do
  version <- PV.packageVersionTextIO "package-version.cabal"
  "0.4" @=? version

textIOFailure :: TestTree
textIOFailure = THU.testCase "packageVersionTextIO retrieves UNKNOWN" $ do
  version <- PV.packageVersionTextIO "not-found.cabal"
  "UNKNOWN" @=? version

eitherIOSuccess :: TestTree
eitherIOSuccess = THU.testCase "packageVersionEitherIO retrieves version" $ do
  version <- PV.packageVersionEitherIO "package-version.cabal"
  Right (MkPackageVersion [0, 4]) @=? version

eitherIOFailure :: TestTree
eitherIOFailure = THU.testCase "packageVersionEitherIO retrieves error" $ do
  eVersion <- PV.packageVersionEitherIO "not-found.cabal"
  case eVersion of
    Left (ReadFileErrorGeneral _) -> pure ()
    Left ex ->
      THU.assertFailure $
        "Received unexpected error: " <> show ex
    Right pv ->
      THU.assertFailure $
        "Did not receive error, received: " <> show pv
