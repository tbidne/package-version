{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.Version.Package
-- Copyright: 2021 Thomas Bidne
-- License: BSD-3-Clause
-- Stability: experimental
--
-- This module provides functionality for reading a package's version
-- at compile-time, along with a type representing PVP version numbers.
-- If only the former is of interest then see 'packageVersionStringTH', as
-- this is likely the most useful function.
--
-- @since 0.1.0.0
module Data.Version.Package
  ( -- * Type
    PackageVersion (MkPackageVersion),

    -- ** Creation
    mkPackageVersion,
    mkPackageVersionTH,
    unsafePackageVersion,
    fromVersion,
    fromString,
    fromText,

    -- ** Elimination
    unPackageVersion,
    toVersion,
    toString,
    toText,

    -- * Reading Cabal Files

    -- ** TemplateHaskell
    -- $retrieve-version-th
    packageVersionTH,
    packageVersionStringTH,
    packageVersionTextTH,

    -- ** IO
    packageVersionThrowIO,
    packageVersionStringIO,
    packageVersionTextIO,
    packageVersionEitherIO,

    -- * Errors
    ValidationError (..),
    ReadStringError (..),
    ReadFileError (..),
  )
where

import Control.DeepSeq (NFData (..))
import Control.DeepSeq qualified as DS
import Control.Exception.Safe (Exception, SomeException)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version (..))
import GHC.Generics (Generic)
import GHC.Read qualified as RD
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.String qualified as PrettyS
#else
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
import Data.Text.Prettyprint.Doc qualified as Pretty
import Data.Text.Prettyprint.Doc.Render.String qualified as PrettyS
#endif
import System.IO qualified as IO
import Text.Read qualified as TR

-- $setup
-- >>> :set -XTemplateHaskell

-- | 'PackageVersion' represents [PVP](https://pvp.haskell.org/) version
-- numbers. It is similar to "Data.Version"'s 'Version' (i.e. wraps a
-- @['Int']@) except:
--
-- 1. 'PackageVersion' has no 'versionTags'.
-- 2. We enforce PVP's "tags must be at least A.B.C" invariant via the
--    smart-constructor pattern.
-- 3. Trailing zeroes are ignored in 'Eq', 'Ord', 'Semigroup', and 'Monoid'.
--
-- That is, we declare an equivalence class up to trailing zeroes.
-- In particular, the 'Monoid' identity is
--
-- @
-- [0] = { [0,0,0], [0,0,0,0], ... }
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
-- >>> unsafePackageVersion [0,0,0,0] == unsafePackageVersion [0,0,0]
-- True
--
-- >>> unsafePackageVersion [4,0,0] > unsafePackageVersion [1,2,0,0]
-- True
--
-- >>> unsafePackageVersion [5,6,0] <> unsafePackageVersion [9,0,0]
-- UnsafePackageVersion {unPackageVersion = [9,0,0]}
--
-- >>> unsafePackageVersion [9,0,0] <> unsafePackageVersion [9,0,0,0]
-- UnsafePackageVersion {unPackageVersion = [9,0,0]}
--
-- >>> TR.readEither @PackageVersion "UnsafePackageVersion {unPackageVersion = [3,2,1]}"
-- Right (UnsafePackageVersion {unPackageVersion = [3,2,1]})
--
-- >>> TR.readEither @PackageVersion "UnsafePackageVersion {unPackageVersion = [3,2]}"
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
  mempty = UnsafePackageVersion [0, 0, 0]

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
  = -- | PVP version numbers must be at least A.B.C
    --
    -- @since 0.1.0.0
    VTooShortErr [Int]
  | -- | PVP version numbers cannot be negative.
    --
    -- @since 0.1.0.0
    VNegativeErr Int
  deriving stock
    ( -- | @since 0.1.0.0
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
  pretty (VTooShortErr xs) = pretty @Text "PVP numbers must be at least A.B.C:" <+> pretty xs
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
-- >>> mkPackageVersion [3,2]
-- Left (VTooShortErr [3,2])
--
-- >>> mkPackageVersion []
-- Left (VTooShortErr [])
--
-- @since 0.1.0.0
mkPackageVersion :: [Int] -> Either ValidationError PackageVersion
mkPackageVersion v@(_ : _ : _ : _) = case filter (< 0) v of
  [] -> Right $ UnsafePackageVersion v
  (neg : _) -> Left $ VNegativeErr neg
mkPackageVersion short = Left $ VTooShortErr short

-- | Safely constructs a 'PackageVersion' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkPackageVersionTH [2,4,0])
-- UnsafePackageVersion {unPackageVersion = [2,4,0]}
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkPackageVersionTH :: [Int] -> Code Q PackageVersion
#else
mkPackageVersionTH :: [Int] -> Q (TExp PackageVersion)
#endif
mkPackageVersionTH v = case mkPackageVersion v of
  Right pv -> liftTyped pv
  Left err -> error $ prettyErr err

-- | Unsafe version of 'mkPackageVersion', intended to be used with known
-- constants. Maybe you should use 'mkPackageVersionTH'?
--
-- __WARNING: This function is not total. Exercise restraint!__
--
-- ==== __Examples__
-- >>> unsafePackageVersion [1,2,3]
-- UnsafePackageVersion {unPackageVersion = [1,2,3]}
--
-- >>> unsafePackageVersion [1,2]
-- UnsafePackageVersion {unPackageVersion = *** Exception: PVP numbers must be at least A.B.C: [1, 2]
-- CallStack (from HasCallStack):
--   error, called at src/Data/Version/Package.hs:362:32 in main:Data.Version.Package
--
-- @since 0.1.0.0
unsafePackageVersion :: [Int] -> PackageVersion
unsafePackageVersion = either (error . prettyErr) id . mkPackageVersion

-- | Creates a 'PackageVersion' from 'Version'.
--
-- Note: Because 'PackageVersion' does not have a 'versionTags', 'fromVersion'
-- is not injective even on "well-formed" 'Version's (i.e. non-negative and length > 2).
-- That is, @'toVersion' . 'fromVersion'@ is /not/ an isomorphism.
--
-- ==== __Examples__
-- >>> fromVersion (Version [2,13,0] ["alpha"])
-- Right (UnsafePackageVersion {unPackageVersion = [2,13,0]})
--
-- >>> fromVersion (Version [] [])
-- Left (VTooShortErr [])
--
-- @since 0.1.0.0
fromVersion :: Version -> Either ValidationError PackageVersion
fromVersion = mkPackageVersion . versionBranch

-- | Attempts to read a 'String' into a 'PackageVersion'. Leading and/or
-- trailing dots will result in an error, as will the empty string.
--
-- ==== __Examples__
-- >>> fromString "1.4.27.3"
-- Right (UnsafePackageVersion {unPackageVersion = [1,4,27,3]})
--
-- >>> fromString ""
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromString "1.a.2"
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromString ".1.2"
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromString "1.2."
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromString "1.3"
-- Left (RsValidateErr (VTooShortErr [1,3]))
--
-- >>> fromString "-3.1.2"
-- Left (RsValidateErr (VNegativeErr (-3)))
--
-- @since 0.1.0.0
fromString :: String -> Either ReadStringError PackageVersion
fromString = fromText . T.pack

-- | Attempts to read a 'Text' into a 'PackageVersion'. Leading and/or
-- trailing dots will result in an error, as will the empty string.
--
-- ==== __Examples__
-- >>> fromText "1.4.27.3"
-- Right (UnsafePackageVersion {unPackageVersion = [1,4,27,3]})
--
-- >>> fromText ""
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromText "1.a.2"
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromText ".1.2"
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromText "1.2."
-- Left (RsReadStrErr "Prelude.read: no parse")
--
-- >>> fromText "1.3"
-- Left (RsValidateErr (VTooShortErr [1,3]))
--
-- >>> fromText "-3.1.2"
-- Left (RsValidateErr (VNegativeErr (-3)))
--
-- @since 0.1.0.0
fromText :: Text -> Either ReadStringError PackageVersion
fromText = readInts . splitDots >=> first RsValidateErr . mkPackageVersion
  where
    splitDots = T.split (== '.')
    readInts = first RsReadStrErr . traverse (TR.readEither . T.unpack)

-- | Creates a 'Version' with empty 'versionTags' from 'PackageVersion'.
--
-- ==== __Examples__
-- >>> toVersion (UnsafePackageVersion [3,2,0])
-- Version {versionBranch = [3,2,0], versionTags = []}
--
-- @since 0.1.0.0
toVersion :: PackageVersion -> Version
toVersion (UnsafePackageVersion v) = Version v []

-- | Displays 'PackageVersion' in 'String' format.
--
-- ==== __Examples__
-- >>> toString (UnsafePackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- @since 0.1.0.0
toString :: PackageVersion -> String
toString = L.intercalate "." . fmap show . unPackageVersion

-- | Displays 'PackageVersion' in 'Text' format.
--
-- ==== __Examples__
-- >>> toText (UnsafePackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- @since 0.1.0.0
toText :: PackageVersion -> Text
toText = T.intercalate "." . fmap (T.pack . show) . unPackageVersion

-- $retrieve-version-th
-- These functions allow for reading a cabal's version at compile-time. If
-- the intention is to simply read the value so it can be printed during
-- runtime (e.g. for an executable's @--version@ flag), then
-- 'packageVersionStringTH' (or 'packageVersionTextTH') is the best choice,
-- as any errors encountered will not prevent compilation.

-- | TemplateHaskell for reading the cabal file's version at compile-time.
-- Errors encountered will be returned as compilation errors.
--
-- ==== __Examples__
-- >>> $$(packageVersionTH "package-version.cabal")
-- UnsafePackageVersion {unPackageVersion = [0,1,0,0]}
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
packageVersionTH :: FilePath -> Code Q PackageVersion
#else
packageVersionTH :: FilePath -> Q (TExp PackageVersion)
#endif
packageVersionTH = ioToTH unsafePackageVersionIO
  where
    unsafePackageVersionIO = fmap (either (error . prettyErr) id) . packageVersionEitherIO

-- | Version of 'packageVersionTH' that returns a 'String' representation of
-- 'PackageVersion' at compile-time. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> $$(packageVersionStringTH "package-version.cabal")
-- "0.1.0.0"
--
-- >>> $$(packageVersionStringTH "not-found.cabal")
-- "UNKNOWN"
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
packageVersionStringTH :: FilePath -> Code Q String
#else
packageVersionStringTH :: FilePath -> Q (TExp String)
#endif
packageVersionStringTH = ioToTH packageVersionStringIO

-- | Version of 'packageVersionTH' that returns a 'Text' representation of
-- 'PackageVersion' at compile-time. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> $$(packageVersionTextTH "package-version.cabal")
-- "0.1.0.0"
--
-- >>> $$(packageVersionTextTH "not-found.cabal")
-- "UNKNOWN"
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
packageVersionTextTH :: FilePath -> Code Q Text
#else
packageVersionTextTH :: FilePath -> Q (TExp Text)
#endif
packageVersionTextTH = ioToTH packageVersionTextIO

-- | Version of 'packageVersionEitherIO' that throws an 'Exception' if any
-- errors are encountered.
--
-- ==== __Examples__
-- >>> packageVersionThrowIO "package-version.cabal"
-- UnsafePackageVersion {unPackageVersion = [0,1,0,0]}
--
-- @since 0.1.0.0
packageVersionThrowIO :: FilePath -> IO PackageVersion
packageVersionThrowIO = packageVersionEitherIO >=> either SafeEx.throw pure

-- | Version of 'packageVersionEitherIO' that returns a 'String' representation of
-- 'PackageVersion' at runtime. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> packageVersionStringIO "package-version.cabal"
-- "0.1.0.0"
--
-- >>> packageVersionStringIO "not-found.cabal"
-- "UNKNOWN"
--
-- @since 0.1.0.0
packageVersionStringIO :: FilePath -> IO String
packageVersionStringIO fp = do
  eVersion <- packageVersionEitherIO fp
  pure $ case eVersion of
    Left _ -> "UNKNOWN"
    Right v -> toString v

-- | Version of 'packageVersionEitherIO' that returns a 'Text' representation of
-- 'PackageVersion' at runtime. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> packageVersionTextIO "package-version.cabal"
-- "0.1.0.0"
--
-- >>> packageVersionTextIO "not-found.cabal"
-- "UNKNOWN"
--
-- @since 0.1.0.0
packageVersionTextIO :: FilePath -> IO Text
packageVersionTextIO fp = do
  eVersion <- packageVersionEitherIO fp
  pure $ case eVersion of
    Left _ -> "UNKNOWN"
    Right v -> toText v

-- | Reads the cabal-file's version.
--
-- ==== __Examples__
-- >>> packageVersionEitherIO "package-version.cabal"
-- Right (UnsafePackageVersion {unPackageVersion = [0,1,0,0]})
--
-- @since 0.1.0.0
packageVersionEitherIO :: FilePath -> IO (Either ReadFileError PackageVersion)
packageVersionEitherIO fp = do
  eContents :: Either SomeException [Text] <-
    second (T.lines . T.pack) <$> SafeEx.try (readFile' fp)
  pure $ case eContents of
    Left err -> Left $ RfFileNotFoundErr $ show err
    Right contents -> foldr findVers noVersErr contents
  where
    noVersErr = Left $ RfVersionNotFoundErr fp
    findVers line acc = case T.stripPrefix "version:" line of
      Just rest -> first RfReadValidateErr $ fromText (T.strip rest)
      Nothing -> acc

#if MIN_VERSION_template_haskell(2, 17, 0)
ioToTH :: Lift b => (a -> IO b) -> a -> Code Q b
ioToTH f x = TH.bindCode (TH.runIO (f x)) liftTyped
#else
ioToTH :: Lift b => (a -> IO b) -> a -> Q (TExp b)
ioToTH f x = TH.runIO (f x) >>= liftTyped
#endif

#if MIN_VERSION_base(4, 15, 0)
readFile' :: FilePath -> IO String
readFile' = IO.readFile'
#else
readFile' :: FilePath -> IO String
readFile' name = IO.withFile name IO.ReadMode hGetContents'
  where
    hGetContents' h = IO.hGetContents h >>= \s -> length s `seq` pure s
#endif

prettyErr :: Pretty a => a -> String
prettyErr =
  PrettyS.renderString
    . Pretty.layoutSmart Pretty.defaultLayoutOptions
    . pretty
