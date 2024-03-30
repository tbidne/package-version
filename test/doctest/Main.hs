-- | Entrypoint for doctests.
--
-- @since 0.1.0.0
module Main (main) where

import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.DocTest qualified as DT

-- | @since 0.1.0.0
main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DT.doctest args)
    (putStrLn "*** Doctests Disabled ***")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Data/Version/Package.hs",
    "src/Data/Version/Package/Internal.hs"
  ]

exts :: [String]
exts =
  [ "-XBangPatterns",
    "-XDeriveAnyClass",
    "-XDeriveGeneric",
    "-XDeriveLift",
    "-XDerivingStrategies",
    "-XDerivingVia",
    "-XImportQualifiedPost",
    "-XLambdaCase",
    "-XNumericUnderscores",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XTypeApplications"
  ]
