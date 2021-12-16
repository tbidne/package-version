{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import System.Environment qualified as Env
import Test.DocTest qualified as DT

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
    "src/Data/Version/Package.hs"
  ]

exts :: [String]
exts =
  [ "-XOverloadedStrings",
    "-XTypeApplications"
  ]