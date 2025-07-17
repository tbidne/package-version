{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

#if MIN_VERSION_base(4,16,0)
{-# LANGUAGE OverloadedRecordDot #-}
#endif

-- thinks TH is unnecessary, for some reason
{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Use /=" -}

module Unit.Data.Version.Package (tests) where

import Control.Exception (Exception (displayException), try)
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Version.Package (PackageVersion (unPackageVersion))
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
      versionProps,
      typeTests
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
  MkPackageVersion [0, 4, 1] @=? version

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
  "0.4.1" @=? version

stringIOFailure :: TestTree
stringIOFailure = testCase "packageVersionStringIO retrieves UNKNOWN" $ do
  version <- PV.packageVersionStringIO "not-found.cabal"
  "UNKNOWN" @=? version

textIOSuccess :: TestTree
textIOSuccess = testCase "packageVersionTextIO retrieves version" $ do
  version <- PV.packageVersionTextIO "package-version.cabal"
  "0.4.1" @=? version

textIOFailure :: TestTree
textIOFailure = testCase "packageVersionTextIO retrieves UNKNOWN" $ do
  version <- PV.packageVersionTextIO "not-found.cabal"
  "UNKNOWN" @=? version

eitherIOSuccess :: TestTree
eitherIOSuccess = testCase "packageVersionEitherIO retrieves version" $ do
  version <- PV.packageVersionEitherIO "package-version.cabal"
  Right (MkPackageVersion [0, 4, 1]) @=? version

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
  MkPackageVersion [0, 4, 1] @=? version

stringTHSuccess :: TestTree
stringTHSuccess = testCase "packageVersionStringTH retrieves version" $ do
  let version = $$(PV.packageVersionStringTH "package-version.cabal")
  "0.4.1" @=? version

stringTHFailure :: TestTree
stringTHFailure = testCase "packageVersionStringTH retrieves UNKNOWN" $ do
  let version = $$(PV.packageVersionStringTH "not-found.cabal")
  "UNKNOWN" @=? version

textTHSuccess :: TestTree
textTHSuccess = testCase "packageVersionTextTH retrieves version" $ do
  let version = $$(PV.packageVersionTextTH "package-version.cabal")
  "0.4.1" @=? version

textTHFailure :: TestTree
textTHFailure = testCase "packageVersionTextTH retrieves UNKNOWN" $ do
  let version = $$(PV.packageVersionTextTH "not-found.cabal")
  "UNKNOWN" @=? version

miscTests :: TestTree
#if MIN_VERSION_base(4,16,0)
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
#else
miscTests = testGroup "Misc" []
#endif

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

typeTests :: TestTree
typeTests =
  testGroup
    "PackageVersion properties"
    [ testEq,
      testOrd,
      testSemigroupAssoc,
      testMonoidIdentity,
      testSemigroupOrd,
      testSemigroupLeftBias,
      testSemigroupSpecs
    ]

testEq :: TestTree
testEq =
  Utils.testPropertyCompat "Eq laws / equivalence class" "testEq" $
    H.property $ do
      x <- H.forAll Gens.genPackageVersion
      y <- H.forAll Gens.genPackageVersion
      z <- H.forAll Gens.genPackageVersion

      -- reflexitivity
      x === x

      -- symmetry
      (x == y) === (y == x)

      -- transitivity
      when (x == y && y == z) (x === z)

      -- negation
      (x /= y) === not (x == y)

      -- equiv class
      x === appendZero x
      x === appendZero (appendZero x)

testOrd :: TestTree
testOrd =
  Utils.testPropertyCompat "Ord laws" "testOrd" $
    H.property $ do
      x <- H.forAll Gens.genPackageVersion
      y <- H.forAll Gens.genPackageVersion
      z <- H.forAll Gens.genPackageVersion

      -- comparability
      H.diff x (\l r -> l <= r || r <= l) y

      -- transitivity
      when (x <= y && y <= z) (H.diff x (<=) z)

      -- reflexitivity
      H.diff x (<=) x

      -- anti-symmetry
      when (x <= y && y <= x) (x === y)

      -- eq class
      H.diff x (<=) (appendZero x)
      H.diff x (>=) (appendZero x)

testSemigroupAssoc :: TestTree
testSemigroupAssoc =
  Utils.testPropertyCompat "Semigroup associativity" "testSemigroupAssoc" $
    H.property $ do
      x <- H.forAll Gens.genPackageVersion
      y <- H.forAll Gens.genPackageVersion
      z <- H.forAll Gens.genPackageVersion

      x <> (y <> z) === (x <> y) <> z

testMonoidIdentity :: TestTree
testMonoidIdentity =
  Utils.testPropertyCompat "Monoid identity" "testMonoidIdentity" $
    H.property $ do
      x <- H.forAll Gens.genPackageVersion

      x === x <> mempty
      x === mempty <> x

testSemigroupOrd :: TestTree
testSemigroupOrd =
  Utils.testPropertyCompat "Semigroup uses Ord" "testSemigroupOrd" $
    H.property $ do
      x <- H.forAll Gens.genPackageVersion
      y <- H.forAll Gens.genPackageVersion

      -- NOTE: [Left-bias and unPackageVersion]
      --
      -- We sometimes use unPackageVersion because we want to test
      -- left-bias i.e. need to use underlying Eq, not equivalence class based
      -- one.
      case compare x y of
        EQ -> do
          unPackageVersion x === unPackageVersion (x <> y)
          unPackageVersion y === unPackageVersion (y <> x)
        LT -> do
          y === x <> y
          y === y <> x
        GT -> do
          x === x <> y
          x === y <> x

testSemigroupLeftBias :: TestTree
testSemigroupLeftBias = testCase "(<>) is left-biased for Eq instances" $ do
  -- see NOTE: [Left-bias and unPackageVersion]
  unPackageVersion x @=? unPackageVersion (x <> y)
  unPackageVersion y @=? unPackageVersion (y <> x)
  where
    x = MkPackageVersion (9 :| [])
    y = MkPackageVersion (9 :| [0])

testSemigroupSpecs :: TestTree
testSemigroupSpecs = testCase "Explicit Semigroup specs" $ do
  -- Sanity check for doctest that was inexplicably failing on CI.
  MkPackageVersion [9, 0, 0] @=? MkPackageVersion [5, 6, 0] <> MkPackageVersion [9, 0, 0]

appendZero :: PackageVersion -> PackageVersion
appendZero (MkPackageVersion (x :| xs)) = MkPackageVersion (x :| xs ++ [0])
