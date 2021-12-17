-- | Entrypoint for unit tests.
--
-- @since 0.1.0.0
module Main (main) where

import Test.Tasty qualified as Tasty
import Tests.IO qualified
import Tests.TH qualified

-- | Runs unit tests.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup
      "Unit tests"
      [ Tests.TH.tests,
        Tests.IO.tests
      ]
