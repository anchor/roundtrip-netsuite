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
import           Data.Aeson.Roundtrip
import           Test.Hspec
import qualified Data.Text as T

import           Netsuite.Parsers

main :: IO ()
main = hspec $
    describe "currency syntax" $ do
        it "round trips netsuite currency" $ do
            runBuilder currency 42.34567 `shouldBe` Just (String "42.34567")
            let n = 1e1024 + 1e-1024
            let Just (String x) = runBuilder currency n
            T.length x `shouldBe` 2050
            runParser currency  (String x) `shouldBe` Just n

        it "parses corner cases" $ do
            runParser currency "00.00" `shouldBe`  Just 0
            runParser currency "00.0001" `shouldBe`  Just 0.0001
            runParser currency " 0008. 0001 " `shouldBe`  Just 8.0001
