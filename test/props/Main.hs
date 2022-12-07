-- | Entrypoint for property tests.
--
-- @since 0.1.0.0
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Tests.ListInt qualified
import Tests.Text qualified
import Tests.Version qualified

-- | Runs property tests.
--
-- @since 0.1.0.0
main :: IO ()
main =
  defaultMain $
    testGroup
      "Property tests"
      [ Tests.ListInt.props,
        Tests.Text.props,
        Tests.Version.props
      ]
