-- | Properties for 'Version'.
--
-- @since 0.1.0.0
module Tests.Version (props) where

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
    "Version properties"
    [ validVersionSucceeds,
      shortVersionFails,
      negativeVersionFails
    ]

validVersionSucceeds :: TestTree
validVersionSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "Valid Version is decoded" "validVersionSucceeds" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genValidVersion
        H.assert $ Either.isRight $ PV.fromVersion vs

shortVersionFails :: TestTree
shortVersionFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "Short Version is not decoded" "shortVersionFails" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genShortVersion
        case PV.fromVersion vs of
          Left (VTooShortErr _) -> H.success
          bad -> H.annotateShow bad *> H.failure

negativeVersionFails :: TestTree
negativeVersionFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "Negative Version is not decoded" "negativeVersionFails" $
    H.withTests limit $
      H.property $ do
        vs <- H.forAll Gens.genNegativeVersion
        case PV.fromVersion vs of
          Left (VNegativeErr _) -> H.success
          bad -> H.annotateShow bad *> H.failure
