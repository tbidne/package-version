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
    "src/Package/Version.hs"
  ]

exts :: [String]
exts =
  [ "-XTemplateHaskell"
  ]