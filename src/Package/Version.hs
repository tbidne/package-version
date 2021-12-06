{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functionality for reading a package's version
-- at compile-time.
--
-- @since 0.1.0.0
module Package.Version
  ( -- * Type
    PackageVersion (..),
    mkPackageVersion,
    showVersion,

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
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version (..))
import Data.Version qualified as V
import Language.Haskell.TH (Q, TExp (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO qualified as IO
import Text.Read qualified as TR

-- | 'PackageVersion' represents [PVP](https://pvp.haskell.org/) version
-- numbers. It is similar to (and in fact wraps) "Data.Version"'s 'Version'
-- except:
--
-- 1. Versions' 'versionTags' is totally ignored in 'Eq', 'Ord', 'Semigroup',
--    and 'Monoid'.
-- 2. Trailing zeroes in 'versionBranch' are similarly ignored.
--
-- That is, we declare an equivalence class on the 'versionBranch' up to
-- trailing zeroes. In particular, the 'Monoid' identity is
--
-- @
-- [0] = { [], [0], [0,0], ... }
-- @
--
-- and its 'Semigroup' instance takes the latest version (based on 'Ord').
--
-- Because we export the underlying 'Version' in various ways,
-- (e.g. 'MkPackageVersion', 'show'), 'Eq' breaks Leibniz's law,
-- sometimes called substitutivity:
--
-- @
-- x == y ==> f x == f y
-- @
--
-- ==== __Examples__
-- >>> mkPackageVersion [] == mkPackageVersion [0,0,0]
-- True
--
-- >>> mkPackageVersion [4] > mkPackageVersion [1,2]
-- True
--
-- >>> mkPackageVersion [5,6] <> mkPackageVersion [9,0]
-- MkPackageVersion {unPackageVersion = Version {versionBranch = [9,0], versionTags = []}}
--
-- >>> mkPackageVersion [9] <> mkPackageVersion [9,0]
-- MkPackageVersion {unPackageVersion = Version {versionBranch = [9], versionTags = []}}
--
-- @since 0.1.0.0
newtype PackageVersion = MkPackageVersion
  { -- | @since 0.1.0.0
    unPackageVersion :: Version
  }
  deriving
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance Eq PackageVersion where
  MkPackageVersion (Version v1 _) == MkPackageVersion (Version v2 _) =
    dropTrailingZeroes v1 == dropTrailingZeroes v2

-- | @since 0.1.0.0
instance Ord PackageVersion where
  MkPackageVersion (Version v1 _) `compare` MkPackageVersion (Version v2 _) =
    dropTrailingZeroes v1 `compare` dropTrailingZeroes v2

-- | @since 0.1.0.0
instance Semigroup PackageVersion where
  pv1 <> pv2 =
    case pv1 `compare` pv2 of
      LT -> pv2
      _ -> pv1

-- | @since 0.1.0.0
instance Monoid PackageVersion where
  mempty = MkPackageVersion (Version [] [])

-- | Convenience function for creating a 'PackageVersion'. The parameter
-- corresponds to the underlying 'Version'\'s 'versionBranch'.
--
-- ==== __Examples__
-- >>> mkPackageVersion [2,13,0]
-- MkPackageVersion {unPackageVersion = Version {versionBranch = [2,13,0], versionTags = []}}
--
-- @since 0.1.0.0
mkPackageVersion :: [Int] -> PackageVersion
mkPackageVersion = MkPackageVersion . flip Version []

dropTrailingZeroes :: (Eq a, Num a) => [a] -> [a]
dropTrailingZeroes xs = take (lastNonZero xs) xs
  where
    lastNonZero = snd . F.foldl' go (0, 0)
    go (!idx, !acc) x
      | x /= 0 = (idx + 1, idx + 1)
      | otherwise = (idx + 1, acc)

-- | Displays 'PackageVersion' in traditional format.
--
-- ==== __Examples__
-- >>> showVersion (mkPackageVersion [2,7,10,0])
-- "2.7.10.0"
--
-- >>> showVersion (MkPackageVersion (Version [3,2] ["alpha"]))
-- "3.2-alpha"
--
-- @since 0.1.0.0
showVersion :: PackageVersion -> String
showVersion = V.showVersion . unPackageVersion

-- | @since 0.1.0.0
instance Lift PackageVersion where
  liftTyped (MkPackageVersion (Version v tags)) =
    [||MkPackageVersion (Version v tags)||]

-- | TemplateHaskell for reading the cabal file's version at compile-time.
-- Errors encountered will be returned as compilation errors.
--
-- ==== __Examples__
-- >>> $$(packageVersionTH "package-version.cabal")
-- MkPackageVersion {unPackageVersion = Version {versionBranch = [0,1,0,0], versionTags = []}}
--
-- @since 0.1.0.0
packageVersionTH :: FilePath -> Q (TExp PackageVersion)
packageVersionTH fp =
  TH.runIO (packageVersionEitherIO fp)
    >>= either error liftTyped

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
packageVersionStringTH :: FilePath -> Q (TExp String)
packageVersionStringTH fp = TH.runIO (packageVersionStringIO fp) >>= liftTyped

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
packageVersionTextTH :: FilePath -> Q (TExp Text)
packageVersionTextTH fp = TH.runIO (packageVersionTextIO fp) >>= liftTyped

-- | Version of 'packageVersionTH' that returns an 'Either' rather than a
-- compilation error for when something goes wrong.
--
-- ==== __Examples__
-- >>> $$(packageVersionEitherTH "package-version.cabal")
-- Right (MkPackageVersion {unPackageVersion = Version {versionBranch = [0,1,0,0], versionTags = []}})
--
-- >>> $$(packageVersionEitherTH "not-found.cabal")
-- Left "not-found.cabal: openFile: does not exist (No such file or directory)"
--
-- @since 0.1.0.0
packageVersionEitherTH :: FilePath -> Q (TExp (Either String PackageVersion))
packageVersionEitherTH fp =
  TH.runIO (packageVersionEitherIO fp)
    >>= liftTyped

-- | Version of 'packageVersionEitherIO' that returns a 'String' representation of
-- 'PackageVersion' at runtime. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> packageVersionStringIO "package-version.cabal"
-- "0.1.0.0"
--
-- >>> packageVersionStringIO "package-version.cabal"
-- "0.1.0.0"
--
-- @since 0.1.0.0
packageVersionStringIO :: FilePath -> IO String
packageVersionStringIO fp = do
  eVersion <- packageVersionEitherIO fp
  pure $ case eVersion of
    Left _ -> "UNKNOWN"
    Right v -> showVersion v

-- | Version of 'packageVersionEitherIO' that returns a 'Text' representation of
-- 'PackageVersion' at runtime. Returns @\"UNKNOWN\"@ if any errors are
-- encountered.
--
-- ==== __Examples__
-- >>> packageVersionTextIO "package-version.cabal"
-- "0.1.0.0"
--
-- >>> packageVersionTextIO "package-version.cabal"
-- "0.1.0.0"
--
-- @since 0.1.0.0
packageVersionTextIO :: FilePath -> IO Text
packageVersionTextIO fp = do
  eVersion <- packageVersionEitherIO fp
  pure $ case eVersion of
    Left _ -> "UNKNOWN"
    Right v -> T.pack $ showVersion v

-- | Reads the cabal-file's version.
--
-- ==== __Examples__
-- >>> packageVersionEitherIO "package-version.cabal"
-- Right (MkPackageVersion {unPackageVersion = Version {versionBranch = [0,1,0,0], versionTags = []}})
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
    <&> second (MkPackageVersion . V.makeVersion)
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
