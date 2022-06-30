{-# LANGUAGE CPP #-}
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
    Internal.mkPackageVersion,
    mkPackageVersionTH,
    unsafePackageVersion,
    fromVersion,
    fromString,
    fromText,

    -- ** Elimination
    unPackageVersion,
    toVersion,
    toString,
    Internal.toText,

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

import Control.Exception.Safe qualified as SafeEx
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Version (Version (..))
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.String qualified as PrettyS
#else
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
import Data.Text.Prettyprint.Doc qualified as Pretty
import Data.Text.Prettyprint.Doc.Render.String qualified as PrettyS
#endif
import Data.Version.Package.Internal
  ( PackageVersion (..),
    ReadFileError (..),
    ReadStringError (..),
    ValidationError (..),
  )
import Data.Version.Package.Internal qualified as Internal
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))
import Text.Read qualified as TR

-- $setup
-- >>> :set -XTemplateHaskell

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
mkPackageVersionTH v = case Internal.mkPackageVersion v of
  Right pv -> liftTyped pv
  Left err -> error $ prettyErr err

-- | Unsafe version of 'Internal.mkPackageVersion', intended to be used with
-- known constants. Maybe you should use 'mkPackageVersionTH'?
--
-- __WARNING: This function is not total. Exercise restraint!__
--
-- ==== __Examples__
-- >>> unsafePackageVersion [1,2,3]
-- UnsafePackageVersion {unPackageVersion = [1,2,3]}
--
-- @since 0.1.0.0
unsafePackageVersion :: [Int] -> PackageVersion
unsafePackageVersion = either (error . prettyErr) id . Internal.mkPackageVersion

-- | Creates a 'PackageVersion' from 'Version'.
--
-- Note: Because 'PackageVersion' does not have a 'versionTags', 'fromVersion'
-- is not injective even on "well-formed" 'Version's (i.e. non-negative and length > 1).
-- That is, @'toVersion' . 'fromVersion'@ is /not/ an isomorphism.
--
-- ==== __Examples__
-- >>> fromVersion (Version [2,13,0] ["alpha"])
-- Right (UnsafePackageVersion {unPackageVersion = [2,13,0]})
--
-- >>> fromVersion (Version [] [])
-- Left (ValidationErrorTooShort [])
--
-- @since 0.1.0.0
fromVersion :: Version -> Either ValidationError PackageVersion
fromVersion = Internal.mkPackageVersion . versionBranch

-- | Attempts to read a 'String' into a 'PackageVersion'. Leading and/or
-- trailing dots will result in an error, as will the empty string.
--
-- ==== __Examples__
-- >>> fromString "1.4.27.3"
-- Right (UnsafePackageVersion {unPackageVersion = [1,4,27,3]})
--
-- >>> fromString ""
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromString "1.a.2"
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromString ".1.2"
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromString "1.2."
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromString "1"
-- Left (ReadStringErrorValidate (ValidationErrorTooShort [1]))
--
-- >>> fromString "-3.1.2"
-- Left (ReadStringErrorValidate (ValidationErrorNegative (-3)))
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
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromText "1.a.2"
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromText ".1.2"
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromText "1.2."
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromText "1"
-- Left (ReadStringErrorValidate (ValidationErrorTooShort [1]))
--
-- >>> fromText "-3.1.2"
-- Left (ReadStringErrorValidate (ValidationErrorNegative (-3)))
--
-- @since 0.1.0.0
fromText :: Text -> Either ReadStringError PackageVersion
fromText = readInts . splitDots >=> first ReadStringErrorValidate . Internal.mkPackageVersion
  where
    splitDots = T.split (== '.')
    readInts = first ReadStringErrorParse . traverse (TR.readEither . T.unpack)

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

-- | Version of 'packageVersionEitherIO' that throws an
-- 'Control.Exception.Safe.Exception' if any errors are encountered.
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
    Right v -> Internal.toText v

-- | Reads the cabal-file's version.
--
-- ==== __Examples__
-- >>> packageVersionEitherIO "package-version.cabal"
-- Right (UnsafePackageVersion {unPackageVersion = [0,1,0,0]})
--
-- @since 0.1.0.0
packageVersionEitherIO :: FilePath -> IO (Either ReadFileError PackageVersion)
packageVersionEitherIO fp = do
  eContents <- second T.lines <$> SafeEx.tryAny (readFile' fp)
  pure $ case eContents of
    Left err -> Left $ ReadFileErrorFileNotFound $ show err
    Right contents -> foldr findVers noVersErr contents
  where
    noVersErr = Left $ ReadFileErrorVersionNotFound fp
    findVers line acc = case T.stripPrefix "version:" line of
      Just rest -> first ReadFileErrorReadString $ fromText (T.strip rest)
      Nothing -> acc
    readFile' = fmap (decodeUtf8With lenientDecode) . BS.readFile

#if MIN_VERSION_template_haskell(2, 17, 0)
ioToTH :: Lift b => (a -> IO b) -> a -> Code Q b
ioToTH f x = TH.bindCode (TH.runIO (f x)) liftTyped
#else
ioToTH :: Lift b => (a -> IO b) -> a -> Q (TExp b)
ioToTH f x = TH.runIO (f x) >>= liftTyped
#endif

prettyErr :: Pretty a => a -> String
prettyErr =
  PrettyS.renderString
    . Pretty.layoutSmart Pretty.defaultLayoutOptions
    . pretty
