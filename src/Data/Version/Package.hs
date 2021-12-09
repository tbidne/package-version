{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.Version.Package
-- Copyright: 2021 Thomas Bidne
-- License: MIT
-- Stability: experimental
--
-- This module provides functionality for reading a package's version
-- at compile-time.
--
-- @since 0.1.0.0
module Data.Version.Package
  ( -- * Type
    PackageVersion (..),

    -- ** Creation
    fromVersion,
    fromString,
    fromText,

    -- ** Elimination
    toVersion,
    toString,
    toText,

    -- * TemplateHaskell
    packageVersionTH,
    packageVersionStringTH,
    packageVersionTextTH,
    packageVersionEitherTH,

    -- * IO
    packageVersionStringIO,
    packageVersionTextIO,
    packageVersionEitherIO,
  )
where

import Control.Applicative qualified as A
import Control.Exception.Safe (SomeException)
import Control.Exception.Safe qualified as SafeEx
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Bifunctor (Bifunctor (..))
import Data.Char qualified as C
import Data.Data (Data)
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version (..))
import GHC.Exts (IsList)
import GHC.Generics (Generic)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO qualified as IO
import Text.Read qualified as TR

-- | 'PackageVersion' represents [PVP](https://pvp.haskell.org/) version
-- numbers. It is similar to "Data.Version"'s 'Version' except:
--
-- 1. 'PackageVersion' has no 'versionTags'.
-- 2. Trailing zeroes are ignored in 'Eq', 'Ord', 'Semigroup', and 'Monoid'.
--
-- That is, we declare an equivalence class up to trailing zeroes.
-- In particular, the 'Monoid' identity is
--
-- @
-- [0] = { [], [0], [0,0], ... }
-- @
--
-- and its 'Semigroup' instance takes the greatest version (based on 'Ord').
--
-- Note: Because we export the underlying list in various ways,
-- (e.g. 'MkPackageVersion', 'show'), 'Eq'\'s extensionality can be broken:
--
-- @
-- x == y ==> f x == f y
-- @
--
-- ==== __Examples__
-- >>> MkPackageVersion [] == MkPackageVersion [0,0,0]
-- True
--
-- >>> MkPackageVersion [4] > MkPackageVersion [1,2]
-- True
--
-- >>> MkPackageVersion [5,6] <> MkPackageVersion [9,0]
-- MkPackageVersion {unPackageVersion = [9,0]}
--
-- >>> MkPackageVersion [9] <> MkPackageVersion [9,0]
-- MkPackageVersion {unPackageVersion = [9]}
--
-- @since 0.1.0.0
newtype PackageVersion = MkPackageVersion
  { -- | @since 0.1.0.0
    unPackageVersion :: [Int]
  }
  deriving
    ( -- | @since 0.1.0.0
      Data,
      -- | @since 0.1.0.0
      IsList,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Read,
      -- | @since 0.1.0.0
      Show
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
  mempty = MkPackageVersion []

-- | @since 0.1.0.0
instance Lift PackageVersion where
  liftTyped (MkPackageVersion v) = [||MkPackageVersion v||]

dropTrailingZeroes :: (Eq a, Num a) => [a] -> [a]
dropTrailingZeroes xs = take (lastNonZero xs) xs
  where
    lastNonZero = snd . F.foldl' go (0, 0)
    go (!idx, !acc) x
      | x /= 0 = (idx + 1, idx + 1)
      | otherwise = (idx + 1, acc)

-- | Creates a 'PackageVersion' from 'Version'. Note that because
-- 'PackageVersion' does not have a 'versionTags', 'fromVersion'
-- is not injective, i.e., @'toVersion' . 'fromVersion'@ is /not/
-- an isomorphism.
--
-- ==== __Examples__
-- >>> fromVersion (Version [2,13,0] ["alpha"])
-- MkPackageVersion {unPackageVersion = [2,13,0]}
--
-- @since 0.1.0.0
fromVersion :: Version -> PackageVersion
fromVersion (Version v _) = MkPackageVersion v

-- | Attempts to read a 'String' into a 'PackageVersion'. Leading and/or
-- trailing dots will result in an error, as will the empty string.
--
-- ==== __Examples__
-- >>> fromString "1.4.27.3"
-- Right (MkPackageVersion {unPackageVersion = [1,4,27,3]})
--
-- >>> fromString ""
-- Left "Prelude.read: no parse"
--
-- >>> fromString "1.a.2"
-- Left "Prelude.read: no parse"
--
-- >>> fromString ".1.2"
-- Left "Prelude.read: no parse"
--
-- >>> fromString "1.2."
-- Left "Prelude.read: no parse"
--
-- @since 0.1.0.0
fromString :: String -> Either String PackageVersion
fromString = fromText . T.pack

-- | Attempts to read a 'Text' into a 'PackageVersion'. Leading and/or
-- trailing dots will result in an error, as will the empty string.
--
-- ==== __Examples__
-- >>> fromText "1.4.27.3"
-- Right (MkPackageVersion {unPackageVersion = [1,4,27,3]})
--
-- >>> fromText ""
-- Left "Prelude.read: no parse"
--
-- >>> fromText "1.a.2"
-- Left "Prelude.read: no parse"
--
-- >>> fromText ".1.2"
-- Left "Prelude.read: no parse"
--
-- >>> fromText "1.2."
-- Left "Prelude.read: no parse"
--
-- @since 0.1.0.0
fromText :: Text -> Either String PackageVersion
fromText txt = MkPackageVersion <$> traverse (TR.readEither . T.unpack) sections
  where
    sections = T.split (== '.') txt

-- | Creates a 'Version' with empty 'versionTags' from 'PackageVersion'.
--
-- ==== __Examples__
-- >>> toVersion (MkPackageVersion [3,2,0])
-- Version {versionBranch = [3,2,0], versionTags = []}
--
-- @since 0.1.0.0
toVersion :: PackageVersion -> Version
toVersion (MkPackageVersion v) = Version v []

-- | Displays 'PackageVersion' in 'String' format.
--
-- ==== __Examples__
-- >>> toString (MkPackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- @since 0.1.0.0
toString :: PackageVersion -> String
toString = L.intercalate "." . fmap show . unPackageVersion

-- | Displays 'PackageVersion' in 'Text' format.
--
-- ==== __Examples__
-- >>> toText (MkPackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- @since 0.1.0.0
toText :: PackageVersion -> Text
toText = T.intercalate "." . fmap (T.pack . show) . unPackageVersion

-- | TemplateHaskell for reading the cabal file's version at compile-time.
-- Errors encountered will be returned as compilation errors.
--
-- ==== __Examples__
-- >>> $$(packageVersionTH "package-version.cabal")
-- MkPackageVersion {unPackageVersion = [0,1,0,0]}
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
packageVersionTH :: FilePath -> Code Q PackageVersion
packageVersionTH fp = TH.bindCode qVersion liftTyped
  where
    qVersion = either error id <$> TH.runIO (packageVersionEitherIO fp)
#else
packageVersionTH :: FilePath -> Q (TExp PackageVersion)
packageVersionTH fp =
  TH.runIO (packageVersionEitherIO fp)
    >>= either error liftTyped
#endif

-- | Version of 'packageVersionTH' that returns a string representation of
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
packageVersionStringTH fp = TH.bindCode qVersion liftTyped
  where
    qVersion = TH.runIO (packageVersionStringIO fp)
#else
packageVersionStringTH :: FilePath -> Q (TExp String)
packageVersionStringTH fp = TH.runIO (packageVersionStringIO fp) >>= liftTyped
#endif

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
packageVersionTextTH fp = TH.bindCode qVersion liftTyped
  where
    qVersion = TH.runIO (packageVersionTextIO fp)
#else
packageVersionTextTH :: FilePath -> Q (TExp Text)
packageVersionTextTH fp = TH.runIO (packageVersionTextIO fp) >>= liftTyped
#endif

-- | Version of 'packageVersionTH' that returns an 'Either' rather than a
-- compilation error for when something goes wrong.
--
-- ==== __Examples__
-- >>> $$(packageVersionEitherTH "package-version.cabal")
-- Right (MkPackageVersion {unPackageVersion = [0,1,0,0]})
--
-- >>> $$(packageVersionEitherTH "not-found.cabal")
-- Left "not-found.cabal: openFile: does not exist (No such file or directory)"
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
packageVersionEitherTH :: FilePath -> Code Q (Either String PackageVersion)
packageVersionEitherTH fp = TH.bindCode qVersion liftTyped
  where
    qVersion = TH.runIO (packageVersionEitherIO fp)
#else
packageVersionEitherTH :: FilePath -> Q (TExp (Either String PackageVersion))
packageVersionEitherTH fp =
  TH.runIO (packageVersionEitherIO fp)
    >>= liftTyped
#endif

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
-- Right (MkPackageVersion {unPackageVersion = [0,1,0,0]})
--
-- >>> packageVersionEitherIO "not-found.cabal"
-- Left "not-found.cabal: openFile: does not exist (No such file or directory)"
--
-- @since 0.1.0.0
packageVersionEitherIO :: FilePath -> IO (Either String PackageVersion)
packageVersionEitherIO fp = do
  eContents :: Either SomeException [Text] <-
    second (T.lines . T.pack) <$> SafeEx.try (readFile' fp)
  pure $ case eContents of
    Left err -> Left $ show err
    Right contents -> foldr findVers noVersErr contents
  where
    noVersErr = Left $ "No version found in cabal file: " <> fp
    findVers line acc = case AP.parseOnly parser line of
      Right vers -> vers
      _ -> acc

parser :: Parser (Either String PackageVersion)
parser =
  AP.string "version:"
    *> AP.skipSpace
    *> parseVers
    <&> second MkPackageVersion
  where
    parseVers = traverse (readEither . T.unpack) <$> AP.many1 parseDigits
    parseDigits =
      AP.takeWhile1 C.isDigit
        <* A.many (AP.string ".")

readEither :: Read a => String -> Either String a
readEither str = maybe (Left err) Right (TR.readMaybe str)
  where
    err = "Read error: " <> str

hGetContents' :: IO.Handle -> IO String
hGetContents' h = IO.hGetContents h >>= \s -> length s `seq` pure s

readFile' :: FilePath -> IO String
readFile' name = IO.openFile name IO.ReadMode >>= hGetContents'
