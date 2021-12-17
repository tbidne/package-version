-- | Properties for 'Version'.
--
-- @since 0.1.0.0
module Props.Version (props) where

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
    "Version properties"
    [ validVersionSucceeds,
      shortVersionFails,
      negativeVersionFails
    ]

validVersionSucceeds :: TestTree
validVersionSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Valid Version is decoded" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genValidVersion
        H.assert $ Either.isRight $ PV.fromVersion vs

shortVersionFails :: TestTree
shortVersionFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Short Version is not decoded" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genShortVersion
        case PV.fromVersion vs of
          Left (VTooShortErr _) -> H.success
          bad -> H.annotateShow bad *> H.failure

negativeVersionFails :: TestTree
negativeVersionFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Negative Version is not decoded" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genNegativeVersion
        case PV.fromVersion vs of
          Left (VNegativeErr _) -> H.success
          bad -> H.annotateShow bad *> H.failure
