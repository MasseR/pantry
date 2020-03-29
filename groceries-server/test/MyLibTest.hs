module Main (main) where

import           Test.Hspec

import qualified Test.Server as Server

main :: IO ()
main = hspec
  Server.spec
