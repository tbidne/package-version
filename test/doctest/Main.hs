-- | Entrypoint for doctests.
--
-- @since 0.1.0.0
module Main (main) where

import System.Environment qualified as Env
import Test.DocTest qualified as DT

-- | @since 0.1.0.0
main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just "true" -> DT.doctest args
    _ -> putStrLn "*** Doctests Disabled"
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
    "-XDeriveGeneric",
    "-XDeriveLift",
    "-XDerivingStrategies",
    "-XImportQualifiedPost",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XTypeApplications"
  ]
