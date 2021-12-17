-- | Properties for 'String' and 'Text'.
--
-- @since 0.1.0.0
module Props.Text (props) where

import Data.Either qualified as Either
import Data.Version.Package (ReadStringError (..), ValidationError (..))
import Data.Version.Package qualified as PV
import Gens qualified
import Hedgehog ((===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "Text properties"
    [ toTextRoundTrip,
      validStrSucceeds,
      validTextSucceeds,
      shortStringFails,
      shortTextFails,
      negativeStringFails,
      negativeTextFails
    ]

toTextRoundTrip :: TestTree
toTextRoundTrip = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "PackageVersion -> Text -> PackageVersion is a round trip" $
    H.withTests limit $
      H.property $ do
        pv <- H.forAll Gens.genPackageVersion
        Right pv === roundTrip pv
  where
    roundTrip = PV.fromText . PV.toText

validStrSucceeds :: TestTree
validStrSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Valid String is decoded" $
    H.withTests limit $
      H.property $ do
        str <- H.forAll Gens.genValidString
        H.assert $ Either.isRight $ PV.fromString str

validTextSucceeds :: TestTree
validTextSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Valid Text is decoded" $
    H.withTests limit $
      H.property $ do
        txt <- H.forAll Gens.genValidText
        H.assert $ Either.isRight $ PV.fromText txt

shortStringFails :: TestTree
shortStringFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Short String is not decoded" $
    H.withTests limit $
      H.property $ do
        str <- H.forAll Gens.genShortString
        case PV.fromString str of
          Left (RsReadStrErr _) -> H.success
          Left (RsValidateErr (VTooShortErr _)) -> H.success
          bad -> H.annotateShow bad *> H.failure

shortTextFails :: TestTree
shortTextFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Short Text is not decoded" $
    H.withTests limit $
      H.property $ do
        txt <- H.forAll Gens.genShortText
        case PV.fromText txt of
          Left (RsReadStrErr _) -> H.success
          Left (RsValidateErr (VTooShortErr _)) -> H.success
          bad -> H.annotateShow bad *> H.failure

negativeStringFails :: TestTree
negativeStringFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Negative String is not decoded" $
    H.withTests limit $
      H.property $ do
        str <- H.forAll Gens.genNegativeStr
        case PV.fromString str of
          Left (RsValidateErr (VNegativeErr _)) -> H.success
          bad -> H.annotateShow bad *> H.failure

negativeTextFails :: TestTree
negativeTextFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Negative Text is not decoded" $
    H.withTests limit $
      H.property $ do
        txt <- H.forAll Gens.genNegativeText
        case PV.fromText txt of
          Left (RsValidateErr (VNegativeErr _)) -> H.success
          bad -> H.annotateShow bad *> H.failure
