module Main where

import Parse.Test
import Test.HUnit.Text

main = runTestTT parseSuite
