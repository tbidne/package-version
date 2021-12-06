{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functionality for reading the cabal version
-- at compile-time.
--
-- @since 0.1.0.0
module Package.Version
  ( -- * Type
    PackageVersion (..),
    showVersion,

    -- * TemplateHaskell
    packageVersionTH,
    packageVersionEitherTH,
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

-- | Newtype wrapper over 'Version' so we can give it a 'Lift'
-- instance.
--
-- @since 0.1.0.0
newtype PackageVersion = MkPackageVersion
  { -- | @since 0.1.0.0
    unPackageVersion :: Version
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

showVersion :: PackageVersion -> String
showVersion = V.showVersion . unPackageVersion

-- | @since 0.1.0.0
instance Lift PackageVersion where
  liftTyped (MkPackageVersion (Version v tags)) =
    [||MkPackageVersion (Version v tags)||]

-- | TemplateHaskell for reading the cabal file's version at compile-time.
--
-- @since 0.1.0.0
packageVersionTH :: FilePath -> Q (TExp PackageVersion)
packageVersionTH fp =
  TH.runIO (parsePackageVersion fp)
    >>= either error liftTyped

-- | Version of 'packageVersionTH' that returns an 'Either' rather than a
-- compilation error for when something goes wrong.
--
-- @since 0.1.0.0
packageVersionEitherTH :: FilePath -> Q (TExp (Either String PackageVersion))
packageVersionEitherTH fp =
  TH.runIO (parsePackageVersion fp)
    >>= liftTyped

parsePackageVersion :: FilePath -> IO (Either String PackageVersion)
parsePackageVersion fp = do
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
    err = "Could not read: " <> str

hGetContents' :: IO.Handle -> IO String
hGetContents' h = IO.hGetContents h >>= \s -> length s `seq` pure s

readFile' :: FilePath -> IO String
readFile' name = IO.openFile name IO.ReadMode >>= hGetContents'
