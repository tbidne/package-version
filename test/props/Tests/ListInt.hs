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
import Test.Tasty.Hedgehog qualified as TH

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
  TH.testProperty "Valid List Int is decoded" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genValidListInt
        H.assert $ Either.isRight $ PV.mkPackageVersion vs

shortListIntFails :: TestTree
shortListIntFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Short List Int is not decoded" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genShortListInt
        case PV.mkPackageVersion vs of
          Left (VTooShortErr _) -> H.success
          bad -> H.annotateShow bad *> H.failure

negativeListIntFails :: TestTree
negativeListIntFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Negative List Int is not decoded" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genNegativeListInt
        case PV.mkPackageVersion vs of
          Left (VNegativeErr _) -> H.success
          bad -> H.annotateShow bad *> H.failure
