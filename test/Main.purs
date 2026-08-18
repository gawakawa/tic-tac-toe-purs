module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

main :: Effect Unit
main = runTest do
  suite "basic tests" do
    test "example test" do
      Assert.equal 1 1
