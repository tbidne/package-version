-- | Exports generators.
--
-- @since 0.1.0.0
module Gens
  ( -- * Package Version
    genPackageVersion,

    -- * Text
    genValidString,
    genValidText,
    genShortString,
    genShortText,
    genNegativeStr,
    genNegativeText,

    -- * Version
    genValidVersion,
    genShortVersion,
    genNegativeVersion,

    -- * List Int
    genValidListInt,
    genShortListInt,
    genNegativeListInt,
  )
where

import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version (..))
import Data.Version.Package.Internal (PackageVersion (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

-- | Generates a valid 'PackageVersion'.
--
-- @since 0.1.0.0
genPackageVersion :: MonadGen m => m PackageVersion
genPackageVersion = UnsafePackageVersion <$> genVers
  where
    genVers = HG.list (HR.exponential 3 10_000) genSingleVersNum

-- | Generates a valid 'PackageVersion' 'String'.
--
-- @since 0.1.0.0
genValidString :: MonadGen m => m String
genValidString = L.intercalate "." <$> genVers
  where
    genVers = HG.list (HR.exponential 3 10_000) genSingleVersNumStr

-- | Generates a valid 'PackageVersion' 'Text'.
--
-- @since 0.1.0.0
genValidText :: MonadGen m => m Text
genValidText = T.pack <$> genValidString

-- | Generates an invalid 'PackageVersion' 'String' that is too short.
--
-- @since 0.1.0.0
genShortString :: MonadGen m => m String
genShortString = L.intercalate "." <$> genVers
  where
    genVers = HG.list (HR.constant 0 1) genSingleVersNumStr

-- | Generates an invalid 'PackageVersion' 'Text' that is too short.
--
-- @since 0.1.0.0
genShortText :: MonadGen m => m Text
genShortText = T.pack <$> genShortString

-- | Generates an invalid 'PackageVersion' 'String' that includes negatives.
--
-- @since 0.1.0.0
genNegativeStr :: MonadGen m => m String
genNegativeStr = do
  valid <- genVers
  invalid <- show <$> HG.int (HR.constant (-9_999) (-1))
  shuffled <- HG.shuffle (invalid : valid)
  pure $ L.intercalate "." shuffled
  where
    genVers = HG.list (HR.exponential 3 10_000) genSingleVersNumStr

-- | Generates an invalid 'PackageVersion' 'Text' that includes negatives.
--
-- @since 0.1.0.0
genNegativeText :: MonadGen m => m Text
genNegativeText = T.pack <$> genNegativeStr

genSingleVersNum :: MonadGen m => m Int
genSingleVersNum = HG.int (HR.exponential 0 9_999)

genSingleVersNumStr :: MonadGen m => m String
genSingleVersNumStr = show <$> genSingleVersNum

-- | Generates a valid 'PackageVersion' 'Version'.
--
-- @since 0.1.0.0
genValidVersion :: MonadGen m => m Version
genValidVersion = Version <$> genVers <*> genTags
  where
    genVers = HG.list (HR.exponential 3 10_000) genSingleVersNum

-- | Generates an invalid 'PackageVersion' 'Version' that is too short.
--
-- @since 0.1.0.0
genShortVersion :: MonadGen m => m Version
genShortVersion = Version <$> genVers <*> genTags
  where
    genVers = HG.list (HR.constant 0 1) genSingleVersNum

-- | Generates an invalid 'PackageVersion' 'Version' that includes negatives.
--
-- @since 0.1.0.0
genNegativeVersion :: MonadGen m => m Version
genNegativeVersion = do
  valid <- genVers
  invalid <- HG.int (HR.constant (-9_999) (-1))
  tags <- genTags
  shuffled <- HG.shuffle (invalid : valid)
  pure $ Version shuffled tags
  where
    genVers = HG.list (HR.constant 3 100) genSingleVersNum

-- | Generates a valid 'PackageVersion' List 'Int'.
--
-- @since 0.1.0.0
genValidListInt :: MonadGen m => m [Int]
genValidListInt = genVers
  where
    genVers = HG.list (HR.exponential 3 10_000) genSingleVersNum

-- | Generates an invalid 'PackageVersion' List 'Int' that is too short.
--
-- @since 0.1.0.0
genShortListInt :: MonadGen m => m [Int]
genShortListInt = genVers
  where
    genVers = HG.list (HR.constant 0 1) genSingleVersNum

-- | Generates an invalid 'PackageVersion' List 'Int' that includes a negative.
--
-- @since 0.1.0.0
genNegativeListInt :: MonadGen m => m [Int]
genNegativeListInt = do
  valid <- genVers
  invalid <- HG.int (HR.constant (-9_999) (-1))
  HG.shuffle (invalid : valid)
  where
    genVers = HG.list (HR.constant 3 100) genSingleVersNum

genTags :: MonadGen m => m [String]
genTags = HG.list (HR.exponential 0 100) genTag
  where
    genTag = HG.string (HR.exponential 0 100) HG.unicodeAll
