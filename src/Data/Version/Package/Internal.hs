{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module. Exposes the invariant-breaking 'UnsafePackageVersion'
-- constructor.
--
-- @since 0.1.0.0
module Data.Version.Package.Internal
  ( PackageVersion (MkPackageVersion, ..),
    ValidationError (..),
    ReadStringError (..),
    ReadFileError (..),
    mkPackageVersion,
    toText,
  )
where

import Control.DeepSeq (NFData (..))
import Control.DeepSeq qualified as DS
import Control.Exception.Safe (Exception)
import Data.Foldable qualified as F
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Read qualified as RD
import Language.Haskell.TH.Syntax (Lift (..))
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
#else
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
#endif
import Text.Read qualified as TR

-- | 'PackageVersion' represents [PVP](https://pvp.haskell.org/) version
-- numbers. It is similar to "Data.Version"'s 'Data.Version' (i.e. wraps a
-- @['Int']@) except:
--
-- 1. 'PackageVersion' has no 'Data.Version.versionTags'.
-- 2. We enforce PVP's "tags must be at least A.B" invariant via the
--    smart-constructor pattern.
-- 3. Trailing zeroes are ignored in 'Eq', 'Ord', 'Semigroup', and 'Monoid'.
--
-- That is, we declare an equivalence class up to trailing zeroes.
-- In particular, the 'Monoid' identity is
--
-- @
-- [0] = { [0,0], [0,0,0], ... }
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
-- using its underlying @['Int']@ (or 'String') representation.
--
-- ==== __Examples__
-- >>> UnsafePackageVersion [0,0,0,0] == UnsafePackageVersion [0,0,0]
-- True
--
-- >>> UnsafePackageVersion [4,0,0] > UnsafePackageVersion [1,2,0,0]
-- True
--
-- >>> UnsafePackageVersion [5,6,0] <> UnsafePackageVersion [9,0,0]
-- UnsafePackageVersion {unPackageVersion = [9,0,0]}
--
-- >>> UnsafePackageVersion [0,9] <> UnsafePackageVersion [0,9,0,0]
-- UnsafePackageVersion {unPackageVersion = [0,9]}
--
-- >>> TR.readEither @PackageVersion "UnsafePackageVersion {unPackageVersion = [3,2,1]}"
-- Right (UnsafePackageVersion {unPackageVersion = [3,2,1]})
--
-- >>> TR.readEither @PackageVersion "UnsafePackageVersion {unPackageVersion = [3]}"
-- Left "Prelude.read: no parse"
--
-- @since 0.1.0.0
newtype PackageVersion = UnsafePackageVersion
  { -- | @since 0.1.0.0
    unPackageVersion :: [Int]
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
pattern MkPackageVersion :: [Int] -> PackageVersion
pattern MkPackageVersion v <- UnsafePackageVersion v

{-# COMPLETE MkPackageVersion #-}

-- | @since 0.1.0.0
instance Eq PackageVersion where
  UnsafePackageVersion v1 == UnsafePackageVersion v2 =
    dropTrailingZeroes v1 == dropTrailingZeroes v2

-- | @since 0.1.0.0
instance Ord PackageVersion where
  UnsafePackageVersion v1 `compare` UnsafePackageVersion v2 =
    dropTrailingZeroes v1 `compare` dropTrailingZeroes v2

-- | @since 0.1.0.0
instance Semigroup PackageVersion where
  pv1 <> pv2 =
    case pv1 `compare` pv2 of
      LT -> pv2
      _ -> pv1

-- | @since 0.1.0.0
instance Monoid PackageVersion where
  mempty = UnsafePackageVersion [0, 0]

-- | Derived by GHC 8.10.7 with validation via 'mkPackageVersion'.
--
-- @since 0.1.0.0
instance Read PackageVersion where
  readPrec = TR.parens $
    TR.prec 11 $ do
      RD.expectP $ TR.Ident "UnsafePackageVersion"
      RD.expectP $ TR.Punc "{"
      intList <- RD.readField "unPackageVersion" (TR.reset RD.readPrec)
      RD.expectP $ TR.Punc "}"
      case mkPackageVersion intList of
        Left _ -> TR.pfail
        Right pv -> pure pv

-- | @since 0.1.0.0
instance NFData PackageVersion where
  rnf (UnsafePackageVersion xs) = DS.deepseq xs ()

-- | @since 0.1.0.0
instance Pretty PackageVersion where
  pretty = pretty . toText

dropTrailingZeroes :: (Eq a, Num a) => [a] -> [a]
dropTrailingZeroes xs = take (lastNonZero xs) xs
  where
    lastNonZero = snd . F.foldl' go (0, 0)
    go (!idx, !acc) x
      | x /= 0 = (idx + 1, idx + 1)
      | otherwise = (idx + 1, acc)

-- | Errors that can occur when validating PVP version numbers.
--
-- @since 0.1.0.0
data ValidationError
  = -- | PVP version numbers must be at least A.B
    --
    -- @since 0.1.0.0
    VTooShortErr [Int]
  | -- | PVP version numbers cannot be negative.
    --
    -- @since 0.1.0.0
    VNegativeErr Int
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )

-- | @since 0.1.0.0
instance Pretty ValidationError where
  pretty (VTooShortErr xs) = pretty @Text "PVP numbers must be at least A.B:" <+> pretty xs
  pretty (VNegativeErr i) = pretty @Text "PVP numbers cannot be negative:" <+> pretty i

-- | Errors that can occur when reading PVP version numbers.
--
-- @since 0.1.0.0
data ReadStringError
  = -- | Error when reading a string.
    --
    -- @since 0.1.0.0
    RsReadStrErr String
  | -- | Validation error.
    --
    -- @since 0.1.0.0
    RsValidateErr ValidationError
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )

-- | @since 0.1.0.0
instance Pretty ReadStringError where
  pretty (RsReadStrErr err) = pretty @Text "Read error:" <+> pretty err
  pretty (RsValidateErr i) = pretty @Text "Validation error:" <+> pretty i

-- | Errors that can occur when reading PVP version numbers from a file.
--
-- @since 0.1.0.0
data ReadFileError
  = -- | Error for missing file.
    --
    -- @since 0.1.0.0
    RfFileNotFoundErr String
  | -- | Error for missing version.
    --
    -- @since 0.1.0.0
    RfVersionNotFoundErr FilePath
  | -- | Read/Validation error.
    --
    -- @since 0.1.0.0
    RfReadValidateErr ReadStringError
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )

-- | @since 0.1.0.0
instance Pretty ReadFileError where
  pretty (RfFileNotFoundErr f) = pretty @Text "File not found:" <+> pretty f
  pretty (RfVersionNotFoundErr f) = pretty @Text "Version not found:" <+> pretty f
  pretty (RfReadValidateErr i) = pretty @Text "Read/validation error:" <+> pretty i

-- | Smart constructor for 'PackageVersion'. The length of the list must be
-- > 2 to match PVP's minimal A.B.C. Furthermore, all digits must be non-negative.
--
-- ==== __Examples__
--
-- >>> mkPackageVersion [1,2,3]
-- Right (UnsafePackageVersion {unPackageVersion = [1,2,3]})
--
-- >>> mkPackageVersion [2,87,7,1]
-- Right (UnsafePackageVersion {unPackageVersion = [2,87,7,1]})
--
-- >>> mkPackageVersion [1,2,-3,-4,5]
-- Left (VNegativeErr (-3))
--
-- >>> mkPackageVersion [3]
-- Left (VTooShortErr [3])
--
-- >>> mkPackageVersion []
-- Left (VTooShortErr [])
--
-- @since 0.1.0.0
mkPackageVersion :: [Int] -> Either ValidationError PackageVersion
mkPackageVersion v@(_ : _ : _) = case filter (< 0) v of
  [] -> Right $ UnsafePackageVersion v
  (neg : _) -> Left $ VNegativeErr neg
mkPackageVersion short = Left $ VTooShortErr short

-- | Displays 'PackageVersion' in 'Text' format.
--
-- ==== __Examples__
-- >>> toText (UnsafePackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- @since 0.1.0.0
toText :: PackageVersion -> Text
toText = T.intercalate "." . fmap (T.pack . show) . unPackageVersion
