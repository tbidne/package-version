-- | Properties for 'String' and 'Text'.
--
-- @since 0.1.0.0
module Tests.Text (props) where

import Data.Either qualified as Either
import Data.Version.Package (ReadStringError (..), ValidationError (..))
import Data.Version.Package qualified as PV
import Gens qualified
import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Utils qualified

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
toTextRoundTrip =
  Utils.testPropertyCompat "PackageVersion -> Text -> PackageVersion is a round trip" "toTextRoundTrip" $
    H.property $ do
      pv <- H.forAll Gens.genPackageVersion
      Right pv === roundTrip pv
  where
    roundTrip = PV.fromText . PV.toText

validStrSucceeds :: TestTree
validStrSucceeds =
  Utils.testPropertyCompat "Valid String is decoded" "validStrSucceeds" $
    H.property $ do
      str <- H.forAll Gens.genValidString
      H.assert $ Either.isRight $ PV.fromString str

validTextSucceeds :: TestTree
validTextSucceeds =
  Utils.testPropertyCompat "Valid Text is decoded" "validTextSucceeds" $
    H.property $ do
      txt <- H.forAll Gens.genValidText
      H.assert $ Either.isRight $ PV.fromText txt

shortStringFails :: TestTree
shortStringFails =
  Utils.testPropertyCompat "Short String is not decoded" "shortStringFails" $
    H.property $ do
      str <- H.forAll Gens.genShortString
      case PV.fromString str of
        Left (ReadStringErrorParse _) -> H.success
        Left (ReadStringErrorValidate ValidationErrorEmpty) -> H.success
        bad -> H.annotateShow bad *> H.failure

shortTextFails :: TestTree
shortTextFails =
  Utils.testPropertyCompat "Short Text is not decoded" "shortTextFails" $
    H.property $ do
      txt <- H.forAll Gens.genShortText
      case PV.fromText txt of
        Left (ReadStringErrorParse _) -> H.success
        Left (ReadStringErrorValidate ValidationErrorEmpty) -> H.success
        bad -> H.annotateShow bad *> H.failure

negativeStringFails :: TestTree
negativeStringFails =
  Utils.testPropertyCompat "Negative String is not decoded" "negativeStringFails" $
    H.property $ do
      str <- H.forAll Gens.genNegativeStr
      case PV.fromString str of
        Left (ReadStringErrorValidate (ValidationErrorNegative _)) -> H.success
        bad -> H.annotateShow bad *> H.failure

negativeTextFails :: TestTree
negativeTextFails =
  Utils.testPropertyCompat "Negative Text is not decoded" "negativeTextFails" $
    H.property $ do
      txt <- H.forAll Gens.genNegativeText
      case PV.fromText txt of
        Left (ReadStringErrorValidate (ValidationErrorNegative _)) -> H.success
        bad -> H.annotateShow bad *> H.failure
