{-# LANGUAGE UndecidableInstances #-}

-- | Internal module.
--
-- @since 0.1.0.0
module Data.Version.Package.Internal
  ( PackageVersion (..),
    ValidationError (..),
    ReadStringError (..),
    ReadFileError (..),
    mkPackageVersion,
    toText,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> :set -XOverloadedLists

-- | 'PackageVersion' represents [PVP](https://pvp.haskell.org/) version
-- numbers. It is similar to "Data.Version"'s 'Data.Version' (i.e. wraps a
-- @['Int']@) except:
--
-- 1. 'PackageVersion' has no 'Data.Version.versionTags'.
-- 2. We enforce PVP invariants i.e.
--
--      * Tags must have at least one component.
--      * All components >= 0.
--
-- 3. Trailing zeroes are ignored in 'Eq', 'Ord', 'Semigroup', and 'Monoid'.
--
-- That is, we declare an equivalence class up to trailing zeroes.
-- In particular, the 'Monoid' identity is
--
-- @
-- [0] = { [0], [0,0], [0,0,0], ... }
-- @
--
-- and its 'Semigroup' instance takes the greatest version (based on 'Ord').
--
-- Note: Because we export the underlying list in various ways,
-- (e.g. 'show'), 'Eq'\'s extensionality law,
--
-- @
-- x == y ==> f x == f y
-- @
--
-- can be broken. Take care that you do not rely on this law if you are
-- using its underlying @NonEmpty 'Word'@ (or 'String') representation.
--
-- ==== __Examples__
-- >>> MkPackageVersion [0,0,0,0] == MkPackageVersion [0,0,0]
-- True
--
-- >>> MkPackageVersion [4,0,0] > MkPackageVersion [1,2,0,0]
-- True
--
-- >>> MkPackageVersion [5,6,0] <> MkPackageVersion [9,0,0]
-- MkPackageVersion {unPackageVersion = 9 :| [0,0]}
--
-- >>> MkPackageVersion [0,9] <> MkPackageVersion [0,9,0,0]
-- MkPackageVersion {unPackageVersion = 0 :| [9]}
--
-- @since 0.1.0.0
newtype PackageVersion = MkPackageVersion
  { -- | @since 0.4
    unPackageVersion :: NonEmpty Word
  }
  deriving stock
    ( -- | @since 0.2
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1.0.0
instance Eq PackageVersion where
  MkPackageVersion v1 == MkPackageVersion v2 =
    dropTrailingZeroes v1 == dropTrailingZeroes v2

-- | @since 0.1.0.0
instance Ord PackageVersion where
  MkPackageVersion v1 `compare` MkPackageVersion v2 =
    dropTrailingZeroes v1 `compare` dropTrailingZeroes v2

-- | @since 0.1.0.0
instance Semigroup PackageVersion where
  pv1 <> pv2 =
    case pv1 `compare` pv2 of
      LT -> pv2
      _ -> pv1

-- | @since 0.1.0.0
instance Monoid PackageVersion where
  mempty = MkPackageVersion (0 :| [])


dropTrailingZeroes :: (Eq a, Num a) => NonEmpty a -> [a]
dropTrailingZeroes xs = NE.take (lastNonZero xs) xs
  where
    lastNonZero = snd . F.foldl' go (0, 0)
    go (!idx, !acc) x
      | x /= 0 = (idx + 1, idx + 1)
      | otherwise = (idx + 1, acc)

-- | Errors that can occur when validating PVP version numbers.
--
-- @since 0.1.0.0
data ValidationError
  = -- | PVP version number cannot be empty.
    --
    -- @since 0.3
    ValidationErrorEmpty
  | -- | PVP version numbers cannot be negative.
    --
    -- @since 0.2
    ValidationErrorNegative Int
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.2
      NFData
    )

-- | @since 0.1.0.0
instance Exception ValidationError where
  displayException ValidationErrorEmpty = "PVP number cannot be empty"
  displayException (ValidationErrorNegative i) =
    "PVP numbers cannot be negative: " <> show i

-- | Errors that can occur when reading PVP version numbers.
--
-- @since 0.1.0.0
data ReadStringError
  = -- | Error when parsing a string.
    --
    -- @since 0.2
    ReadStringErrorParse String
  | -- | Validation error.
    --
    -- @since 0.2
    ReadStringErrorValidate ValidationError
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.2
      NFData
    )

-- | @since 0.1.0.0
instance Exception ReadStringError where
  displayException (ReadStringErrorParse err) = "Read error: " <> err
  displayException (ReadStringErrorValidate i) =
    "Validation error: " <> displayException i

-- | Errors that can occur when reading PVP version numbers from a file.
--
-- @since 0.1.0.0
data ReadFileError
  = -- | General error when reading a file.
    --
    -- @since 0.2
    ReadFileErrorGeneral String
  | -- | Error for missing version.
    --
    -- @since 0.2
    ReadFileErrorVersionNotFound FilePath
  | -- | Read/Validation error.
    --
    -- @since 0.2
    ReadFileErrorReadString ReadStringError
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.2
      NFData
    )

-- | @since 0.1.0.0
instance Exception ReadFileError where
  displayException (ReadFileErrorGeneral f) = "File not found: " <> f
  displayException (ReadFileErrorVersionNotFound f) = "Version not found: " <> f
  displayException (ReadFileErrorReadString i) = "Read error: " <> displayException i

-- | Constructs a 'PackageVersion' from an 'Int' list. The length of the list
-- must be > 1 to match PVP's minimal A.B. Furthermore, all digits must be
-- non-negative.
--
-- ==== __Examples__
--
-- >>> mkPackageVersion [1,2]
-- Right (MkPackageVersion {unPackageVersion = 1 :| [2]})
--
-- >>> mkPackageVersion [2,87,7,1]
-- Right (MkPackageVersion {unPackageVersion = 2 :| [87,7,1]})
--
-- >>> mkPackageVersion [1,2,-3,-4,5]
-- Left (ValidationErrorNegative (-3))
--
-- >>> mkPackageVersion [3]
-- Right (MkPackageVersion {unPackageVersion = 3 :| []})
--
-- >>> mkPackageVersion []
-- Left ValidationErrorEmpty
--
-- @since 0.1.0.0
mkPackageVersion :: [Int] -> Either ValidationError PackageVersion
mkPackageVersion vers@(v : vs) = case filter (< 0) vers of
  [] -> Right $ MkPackageVersion (fromIntegral v :| fmap fromIntegral vs)
  (neg : _) -> Left $ ValidationErrorNegative neg
mkPackageVersion [] = Left ValidationErrorEmpty

-- | Displays 'PackageVersion' in 'Text' format.
--
-- ==== __Examples__
-- >>> toText (MkPackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- @since 0.1.0.0
toText :: PackageVersion -> Text
toText =
  T.intercalate "."
    . fmap (T.pack . show)
    . NE.toList
    . unPackageVersion
