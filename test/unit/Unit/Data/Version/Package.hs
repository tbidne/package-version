{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- thinks TH is unnecessary, for some reason
{- HLINT ignore "Unused LANGUAGE pragma" -}

module Unit.Data.Version.Package (tests) where

import Control.Exception (Exception (displayException), try)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Version.Package qualified as PV
import Data.Version.Package.Internal
  ( PackageVersion (MkPackageVersion),
    ReadFileError (ReadFileErrorGeneral),
    ReadStringError (ReadStringErrorParse, ReadStringErrorValidate),
    ValidationError (ValidationErrorEmpty, ValidationErrorNegative),
  )
import Gens qualified
import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.HUnit qualified as THU
import Utils (testPropertyCompat)

tests :: TestTree
tests =
  testGroup
    "Data.Version.Package"
    [ cabalTests,
      miscTests,
      listIntProps,
      textProps,
      versionProps
    ]

cabalTests :: TestTree
cabalTests =
  testGroup
    "Reading cabal files"
    [ cabalIOTests,
      cabalTHTests
    ]

cabalIOTests :: TestTree
cabalIOTests =
  testGroup
    "IO"
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
throwIOSuccess = testCase "packageVersionThrowIO retrieves version" $ do
  version <- PV.packageVersionThrowIO "package-version.cabal"
  MkPackageVersion [0, 4] @=? version

throwIOFailure :: TestTree
throwIOFailure = testCase "packageVersionThrowIO throws exception" $ do
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
stringIOSuccess = testCase "packageVersionStringIO retrieves version" $ do
  version <- PV.packageVersionStringIO "package-version.cabal"
  "0.4" @=? version

stringIOFailure :: TestTree
stringIOFailure = testCase "packageVersionStringIO retrieves UNKNOWN" $ do
  version <- PV.packageVersionStringIO "not-found.cabal"
  "UNKNOWN" @=? version

textIOSuccess :: TestTree
textIOSuccess = testCase "packageVersionTextIO retrieves version" $ do
  version <- PV.packageVersionTextIO "package-version.cabal"
  "0.4" @=? version

textIOFailure :: TestTree
textIOFailure = testCase "packageVersionTextIO retrieves UNKNOWN" $ do
  version <- PV.packageVersionTextIO "not-found.cabal"
  "UNKNOWN" @=? version

eitherIOSuccess :: TestTree
eitherIOSuccess = testCase "packageVersionEitherIO retrieves version" $ do
  version <- PV.packageVersionEitherIO "package-version.cabal"
  Right (MkPackageVersion [0, 4]) @=? version

eitherIOFailure :: TestTree
eitherIOFailure = testCase "packageVersionEitherIO retrieves error" $ do
  eVersion <- PV.packageVersionEitherIO "not-found.cabal"
  case eVersion of
    Left (ReadFileErrorGeneral _) -> pure ()
    Left ex ->
      THU.assertFailure $
        "Received unexpected error: " <> show ex
    Right pv ->
      THU.assertFailure $
        "Did not receive error, received: " <> show pv

cabalTHTests :: TestTree
cabalTHTests =
  testGroup
    "TH"
    [ listIntTHSuccess,
      thSuccess,
      stringTHSuccess,
      stringTHFailure,
      textTHSuccess,
      textTHFailure
    ]

listIntTHSuccess :: TestTree
listIntTHSuccess = testCase "mkPackageVersionTH creates PackageVersion" $ do
  let version = $$(PV.mkPackageVersionTH [0, 1, 0, 0])
  MkPackageVersion [0, 1, 0, 0] @=? version

thSuccess :: TestTree
thSuccess = testCase "packageVersionTH retrieves version" $ do
  let version = $$(PV.packageVersionTH "package-version.cabal")
  MkPackageVersion [0, 4] @=? version

stringTHSuccess :: TestTree
stringTHSuccess = testCase "packageVersionStringTH retrieves version" $ do
  let version = $$(PV.packageVersionStringTH "package-version.cabal")
  "0.4" @=? version

stringTHFailure :: TestTree
stringTHFailure = testCase "packageVersionStringTH retrieves UNKNOWN" $ do
  let version = $$(PV.packageVersionStringTH "not-found.cabal")
  "UNKNOWN" @=? version

textTHSuccess :: TestTree
textTHSuccess = testCase "packageVersionTextTH retrieves version" $ do
  let version = $$(PV.packageVersionTextTH "package-version.cabal")
  "0.4" @=? version

textTHFailure :: TestTree
textTHFailure = testCase "packageVersionTextTH retrieves UNKNOWN" $ do
  let version = $$(PV.packageVersionTextTH "not-found.cabal")
  "UNKNOWN" @=? version

miscTests :: TestTree
miscTests =
  testGroup
    "Misc"
    [ testOverloadedRecordDot
    ]

testOverloadedRecordDot :: TestTree
testOverloadedRecordDot = testCase "Compatible with OverloadedRecordDot" $ do
  versNE @=? pv.unPackageVersion
  where
    versNE = 0 :| [1, 3]
    pv = MkPackageVersion versNE

listIntProps :: TestTree
listIntProps =
  testGroup
    "List Int properties"
    [ validListIntSucceeds,
      shortListIntFails,
      negativeListIntFails
    ]

validListIntSucceeds :: TestTree
validListIntSucceeds =
  testPropertyCompat "Valid List Int is decoded" "validListIntSucceeds" $
    H.property $ do
      vs <- H.forAll Gens.genValidListInt
      case PV.mkPackageVersion vs of
        Left err -> H.annotate (displayException err) *> H.failure
        Right _ -> H.success

shortListIntFails :: TestTree
shortListIntFails =
  testPropertyCompat "Short List Int is not decoded" "shortListIntFails" $
    H.property $ do
      vs <- H.forAll Gens.genShortListInt
      case PV.mkPackageVersion vs of
        Left ValidationErrorEmpty -> H.success
        bad -> H.annotateShow bad *> H.failure

negativeListIntFails :: TestTree
negativeListIntFails =
  testPropertyCompat "Negative List Int is not decoded" "negativeListIntFails" $
    H.property $ do
      vs <- H.forAll Gens.genNegativeListInt
      case PV.mkPackageVersion vs of
        Left (ValidationErrorNegative _) -> H.success
        bad -> H.annotateShow bad *> H.failure

textProps :: TestTree
textProps =
  testGroup
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
  testPropertyCompat "PackageVersion -> Text -> PackageVersion is a round trip" "toTextRoundTrip" $
    H.property $ do
      pv <- H.forAll Gens.genPackageVersion
      Right pv === roundTrip pv
  where
    roundTrip = PV.fromText . PV.toText

validStrSucceeds :: TestTree
validStrSucceeds =
  testPropertyCompat "Valid String is decoded" "validStrSucceeds" $
    H.property $ do
      str <- H.forAll Gens.genValidString
      case PV.fromString str of
        Left err -> H.annotate (displayException err) *> H.failure
        Right _ -> H.success

validTextSucceeds :: TestTree
validTextSucceeds =
  testPropertyCompat "Valid Text is decoded" "validTextSucceeds" $
    H.property $ do
      txt <- H.forAll Gens.genValidText
      case PV.fromText txt of
        Left err -> H.annotate (displayException err) *> H.failure
        Right _ -> H.success

shortStringFails :: TestTree
shortStringFails =
  testPropertyCompat "Short String is not decoded" "shortStringFails" $
    H.property $ do
      str <- H.forAll Gens.genShortString
      case PV.fromString str of
        Left (ReadStringErrorParse _) -> H.success
        Left (ReadStringErrorValidate ValidationErrorEmpty) -> H.success
        bad -> H.annotateShow bad *> H.failure

shortTextFails :: TestTree
shortTextFails =
  testPropertyCompat "Short Text is not decoded" "shortTextFails" $
    H.property $ do
      txt <- H.forAll Gens.genShortText
      case PV.fromText txt of
        Left (ReadStringErrorParse _) -> H.success
        Left (ReadStringErrorValidate ValidationErrorEmpty) -> H.success
        bad -> H.annotateShow bad *> H.failure

negativeStringFails :: TestTree
negativeStringFails =
  testPropertyCompat "Negative String is not decoded" "negativeStringFails" $
    H.property $ do
      str <- H.forAll Gens.genNegativeStr
      case PV.fromString str of
        Left (ReadStringErrorValidate (ValidationErrorNegative _)) -> H.success
        bad -> H.annotateShow bad *> H.failure

negativeTextFails :: TestTree
negativeTextFails =
  testPropertyCompat "Negative Text is not decoded" "negativeTextFails" $
    H.property $ do
      txt <- H.forAll Gens.genNegativeText
      case PV.fromText txt of
        Left (ReadStringErrorValidate (ValidationErrorNegative _)) -> H.success
        bad -> H.annotateShow bad *> H.failure

versionProps :: TestTree
versionProps =
  testGroup
    "Version properties"
    [ validVersionSucceeds,
      shortVersionFails,
      negativeVersionFails
    ]

validVersionSucceeds :: TestTree
validVersionSucceeds =
  testPropertyCompat "Valid Version is decoded" "validVersionSucceeds" $
    H.property $ do
      vs <- H.forAll Gens.genValidVersion
      case PV.fromVersion vs of
        Left err -> H.annotate (displayException err) *> H.failure
        Right _ -> H.success

shortVersionFails :: TestTree
shortVersionFails =
  testPropertyCompat "Short Version is not decoded" "shortVersionFails" $
    H.property $ do
      vs <- H.forAll Gens.genShortVersion
      case PV.fromVersion vs of
        Left ValidationErrorEmpty -> H.success
        bad -> H.annotateShow bad *> H.failure

negativeVersionFails :: TestTree
negativeVersionFails =
  Utils.testPropertyCompat "Negative Version is not decoded" "negativeVersionFails" $
    H.property $ do
      vs <- H.forAll Gens.genNegativeVersion
      case PV.fromVersion vs of
        Left (ValidationErrorNegative _) -> H.success
        bad -> H.annotateShow bad *> H.failure
