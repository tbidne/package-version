-- | Entrypoint for doctests.
--
-- @since 0.1.0.0
module Main (main) where

import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.DocTest qualified as DT

-- | @since 0.1.0.0
main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DT.mainFromCabal "package-version" [])
    (putStrLn "*** Doctests Disabled ***")
