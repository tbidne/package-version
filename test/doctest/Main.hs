{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.DocTest qualified as DT

main :: IO ()
main = DT.doctest args
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Data/Version/Package.hs"
  ]

exts :: [String]
exts =
  [ "-XTemplateHaskell"
  ]