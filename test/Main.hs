module Main (main) where

import Control.Monad (void)
import Hedgehog (checkParallel)

import Haskemathesis.Test.Properties (tests)

main :: IO ()
main = void (checkParallel tests)
