-- | Properties for List 'Int'.
--
-- @since 0.1.0.0
module Tests.ListInt (props) where

import Data.Either qualified as Either
import Data.Version.Package (ValidationError (..))
import Data.Version.Package qualified as PV
import Gens qualified
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
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
validListIntSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "Valid List Int is decoded" "validListIntSucceeds" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genValidListInt
        H.assert $ Either.isRight $ PV.mkPackageVersion vs

shortListIntFails :: TestTree
shortListIntFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "Short List Int is not decoded" "shortListIntFails" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genShortListInt
        case PV.mkPackageVersion vs of
          Left (ValidationErrorTooShort _) -> H.success
          bad -> H.annotateShow bad *> H.failure

negativeListIntFails :: TestTree
negativeListIntFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "Negative List Int is not decoded" "negativeListIntFails" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genNegativeListInt
        case PV.mkPackageVersion vs of
          Left (ValidationErrorNegative _) -> H.success
          bad -> H.annotateShow bad *> H.failure
