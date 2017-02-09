module Main where

import Test.Framework (defaultMain)

import Parse.Test
import ReadAndResolve.Test

main = defaultMain [parseSuite, fileSuite]

