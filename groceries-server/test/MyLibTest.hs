module Main (main) where

import           Test.Hspec

import qualified Test.Server as Server
import qualified Test.Database.Groceries as Database.Groceries

main :: IO ()
main = hspec $ do
  Server.spec
  Database.Groceries.spec
