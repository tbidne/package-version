-- | Properties for List 'Int'.
--
-- @since 0.1.0.0
module Tests.ListInt (props) where

import Data.Either qualified as Either
import Data.Version.Package (ValidationError (..))
import Data.Version.Package qualified as PV
import Gens qualified
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Utils qualified

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "List Int properties"
    [ validListIntSucceeds,
      shortListIntFails,
      negativeListIntFails
    ]

validListIntSucceeds :: TestTree
validListIntSucceeds =
  Utils.testPropertyCompat "Valid List Int is decoded" "validListIntSucceeds" $
    H.property $ do
      vs <- H.forAll Gens.genValidListInt
      H.assert $ Either.isRight $ PV.mkPackageVersion vs

shortListIntFails :: TestTree
shortListIntFails =
  Utils.testPropertyCompat "Short List Int is not decoded" "shortListIntFails" $
    H.property $ do
      vs <- H.forAll Gens.genShortListInt
      case PV.mkPackageVersion vs of
        Left ValidationErrorEmpty -> H.success
        bad -> H.annotateShow bad *> H.failure

negativeListIntFails :: TestTree
negativeListIntFails =
  Utils.testPropertyCompat "Negative List Int is not decoded" "negativeListIntFails" $
    H.property $ do
      vs <- H.forAll Gens.genNegativeListInt
      case PV.mkPackageVersion vs of
        Left (ValidationErrorNegative _) -> H.success
        bad -> H.annotateShow bad *> H.failure
