{-# LANGUAGE CPP #-}

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
-- The doctest examples use @-XOverloadedLists@.
--
-- @since 0.1.0.0
module Data.Version.Package
  ( -- * Type
    PackageVersion (..),

    -- ** Creation
    Internal.mkPackageVersion,
    mkPackageVersionTH,
    unsafePackageVersion,
    fromVersion,
    fromString,
    fromText,

    -- ** Elimination
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

import Control.Exception
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    throwIO,
    try,
  )
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first, second))
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Version (Version (Version, versionBranch))
import Data.Version.Package.Internal
  ( PackageVersion (MkPackageVersion, unPackageVersion),
    ReadFileError
      ( ReadFileErrorGeneral,
        ReadFileErrorReadString,
        ReadFileErrorVersionNotFound
      ),
    ReadStringError (ReadStringErrorParse, ReadStringErrorValidate),
    ValidationError (ValidationErrorEmpty, ValidationErrorNegative),
  )
import Data.Version.Package.Internal qualified as Internal
import GHC.Stack (HasCallStack)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Text.Read qualified as TR

-- $setup
-- >>> :set -XOverloadedLists

-- | Safely constructs a 'PackageVersion' at compile-time. If you know that
-- your input satisfies both invariants (non-empty and non-negative) at
-- compile-time, consider using the 'MkPackageVersion' constructor directly.
--
-- ==== __Examples__
-- >>> $$(mkPackageVersionTH [2,4,0])
-- MkPackageVersion {unPackageVersion = 2 :| [4,0]}
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkPackageVersionTH :: [Int] -> Code Q PackageVersion
#else
mkPackageVersionTH :: [Int] -> Q (TExp PackageVersion)
#endif
mkPackageVersionTH v = case Internal.mkPackageVersion v of
  Right pv -> liftTyped pv
  Left err -> error $ displayException err

-- | Unsafe version of 'Internal.mkPackageVersion', intended to be used with
-- known constants. Maybe you should use 'mkPackageVersionTH' or
-- 'MkPackageVersion'?
--
-- __WARNING: This function is not total. Exercise restraint!__
--
-- ==== __Examples__
-- >>> unsafePackageVersion [1,2,3]
-- MkPackageVersion {unPackageVersion = 1 :| [2,3]}
--
-- @since 0.1.0.0
unsafePackageVersion :: (HasCallStack) => [Int] -> PackageVersion
unsafePackageVersion =
  either (error . displayException) id
    . Internal.mkPackageVersion

-- | Creates a 'PackageVersion' from 'Version'.
--
-- Note: Because 'PackageVersion' does not have a 'Data.Version.versionTags',
-- 'fromVersion' is not injective even on "well-formed" 'Version's
-- (i.e. non-negative and length > 1). That is, @'toVersion' . 'fromVersion'@
-- is /not/ an isomorphism.
--
-- ==== __Examples__
-- >>> fromVersion (Version [2,13,0] ["alpha"])
-- Right (MkPackageVersion {unPackageVersion = 2 :| [13,0]})
--
-- >>> fromVersion (Version [] [])
-- Left ValidationErrorEmpty
--
-- @since 0.1.0.0
fromVersion :: Version -> Either ValidationError PackageVersion
fromVersion = Internal.mkPackageVersion . versionBranch

-- | Attempts to read a 'String' into a 'PackageVersion'. Leading and/or
-- trailing dots will result in an error, as will the empty string.
--
-- ==== __Examples__
-- >>> fromString "1.4.27.3"
-- Right (MkPackageVersion {unPackageVersion = 1 :| [4,27,3]})
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
-- Right (MkPackageVersion {unPackageVersion = 1 :| [4,27,3]})
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
-- >>> fromText ""
-- Left (ReadStringErrorParse "Prelude.read: no parse")
--
-- >>> fromText "-3.1.2"
-- Left (ReadStringErrorValidate (ValidationErrorNegative (-3)))
--
-- @since 0.1.0.0
fromText :: Text -> Either ReadStringError PackageVersion
fromText =
  readInts . splitDots
    >=> first ReadStringErrorValidate . Internal.mkPackageVersion
  where
    splitDots = T.split (== '.')
    readInts = first ReadStringErrorParse . traverse (TR.readEither . T.unpack)

-- | Creates a 'Version' with empty 'Data.Version.versionTags' from
-- 'PackageVersion'.
--
-- ==== __Examples__
-- >>> toVersion (MkPackageVersion [3,2,0])
-- Version {versionBranch = [3,2,0], versionTags = []}
--
-- @since 0.1.0.0
toVersion :: PackageVersion -> Version
toVersion (MkPackageVersion v) = Version (NE.toList $ fmap fromIntegral v) []

-- | Displays 'PackageVersion' in 'String' format.
--
-- ==== __Examples__
-- >>> toString (MkPackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- @since 0.1.0.0
toString :: PackageVersion -> String
toString =
  L.intercalate "."
    . fmap show
    . NE.toList
    . unPackageVersion

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
-- MkPackageVersion {unPackageVersion = 0 :| [4,1]}
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
packageVersionTH :: FilePath -> Code Q PackageVersion
#else
packageVersionTH :: FilePath -> Q (TExp PackageVersion)
#endif
packageVersionTH = ioToTH unsafePackageVersionIO
  where
    unsafePackageVersionIO =
      fmap (either (error . displayException) id)
        . packageVersionEitherIO

-- | Version of 'packageVersionTH' that returns a 'String' representation of
-- 'PackageVersion' at compile-time. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> $$(packageVersionStringTH "package-version.cabal")
-- "0.4.1"
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
-- "0.4.1"
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
-- 'Exception' if any errors are encountered.
--
-- ==== __Examples__
-- >>> packageVersionThrowIO "package-version.cabal"
-- MkPackageVersion {unPackageVersion = 0 :| [4,1]}
--
-- @since 0.1.0.0
packageVersionThrowIO :: FilePath -> IO PackageVersion
packageVersionThrowIO = packageVersionEitherIO >=> either throwIO pure

-- | Version of 'packageVersionEitherIO' that returns a 'String' representation of
-- 'PackageVersion' at runtime. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> packageVersionStringIO "package-version.cabal"
-- "0.4.1"
--
-- >>> packageVersionStringIO "not-found.cabal"
-- "UNKNOWN"
--
-- @since 0.1.0.0
packageVersionStringIO :: FilePath -> IO String
packageVersionStringIO = fmap T.unpack . packageVersionTextIO

-- | Version of 'packageVersionEitherIO' that returns a 'Text' representation of
-- 'PackageVersion' at runtime. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> packageVersionTextIO "package-version.cabal"
-- "0.4.1"
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
-- Right (MkPackageVersion {unPackageVersion = 0 :| [4,1]})
--
-- @since 0.1.0.0
packageVersionEitherIO :: FilePath -> IO (Either ReadFileError PackageVersion)
packageVersionEitherIO fp = do
  eContents <- second T.lines <$> try' @SomeException (readFile' fp)
  pure $ case eContents of
    Left err -> Left $ ReadFileErrorGeneral $ displayException err
    Right contents -> foldr findVers noVersErr contents
  where
    noVersErr = Left $ ReadFileErrorVersionNotFound fp
    findVers line acc = case T.stripPrefix "version:" line of
      Just rest -> first ReadFileErrorReadString $ fromText (T.strip rest)
      Nothing -> acc
    readFile' = fmap (decodeUtf8With lenientDecode) . BS.readFile

-- Vendoring safe-exceptions' "isSyncException" logic, as we only need it for
-- this single function, so it seems a shame to add a dependency when we
-- can easily inline it.
try' :: (Exception e) => IO a -> IO (Either e a)
try' io =
  try io >>= \case
    Left ex
      | isSyncException ex -> pure $ Left ex
      | otherwise -> throwIO ex
    Right x -> pure $ Right x

isSyncException :: (Exception e) => e -> Bool
isSyncException e =
  case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing -> True

#if MIN_VERSION_template_haskell(2, 17, 0)
ioToTH :: Lift b => (a -> IO b) -> a -> Code Q b
ioToTH f x = TH.bindCode (TH.runIO (f x)) liftTyped
#else
ioToTH :: Lift b => (a -> IO b) -> a -> Q (TExp b)
ioToTH f x = TH.runIO (f x) >>= liftTyped
#endif
