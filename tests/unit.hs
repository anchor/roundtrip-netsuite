--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.RoundTrip
import           Test.Hspec

import           Netsuite.Parsers

main :: IO ()
main = hspec $
    describe "currency syntax" $ do
        it "builds netsuite currency" $
            runBuilder currency 42.34567 `shouldBe` Just (String "42.34567")

        it "parses netsuite currency" $ do
            runParser currency "00.00" `shouldBe`  Just 0
            runParser currency "00.0001" `shouldBe`  Just 0.0001
            runParser currency " 0008. 0001 " `shouldBe`  Just 8.0001
