module Language.Spot.Integration.IntegrationTest where

import Test.Hspec

-- This is mainly a test of the code generator. But it is an integration test because
-- we don't care about the opcodes it churns out, as long as everything behaves as expected.

spec :: Spec
spec = do
  describe "Spot" $ do

    it "does nothing" $ do
      pending
