module Language.Spot.Integration.IntegrationTest where

import Test.Hspec

-- This is mainly a test of the code generator. We don't care about the opcodes it
-- churns out, though, as long as everything behaves as expected

spec :: Spec
spec = do
  describe "Spot" $ do

    it "does nothing" $ do
      pending
