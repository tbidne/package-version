-- | Entrypoint for unit tests.
--
-- @since 0.1.0.0
module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Version.Package (PackageVersion (MkPackageVersion, unPackageVersion))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (testCase, (@=?))
import Tests.IO qualified
import Tests.TH qualified

-- | Runs unit tests.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup
      "Unit tests"
      [ Tests.TH.tests,
        Tests.IO.tests,
        miscTests
      ]

miscTests :: TestTree
miscTests =
  testGroup
    "Misc tests"
    [ testOverloadedRecordDot
    ]

testOverloadedRecordDot :: TestTree
testOverloadedRecordDot = testCase "Compatible with OverloadedRecordDot" $ do
  versNE @=? pv.unPackageVersion
  where
    versNE = 0 :| [1, 3]
    pv = MkPackageVersion versNE
