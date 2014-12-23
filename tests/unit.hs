--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Roundtrip
import qualified Data.Text            as T
import           Test.Hspec

import           Netsuite.Parsers

main :: IO ()
main = hspec $ do
    describe "currency syntax" $ do
        it "round trips netsuite currency" $ do
            runBuilder currency 42.34567 `shouldBe` Right (String "42.34567")
            let n = 1e1024 + 1e-1024
            let Right (String x) = runBuilder currency n
            T.length x `shouldBe` 2050
            runParser currency  (String x) `shouldBe` Right n

        it "parses corner cases" $ do
            runParser currency "00.00" `shouldBe`  Right 0
            runParser currency "00.0001" `shouldBe`  Right 0.0001
            runParser currency " 0008. 0001 " `shouldBe`  Right 8.0001

    describe "datetime syntax" $
        it "round trips different datetime strings" $
            forM_ netsuiteDates $  \d ->
                let v = String $ T.pack d
                in case runParser datetime v of
                    Left e -> error $ "Failed to parse: " ++ show e
                    Right t  -> runBuilder datetime t `shouldBe` Right v

netsuiteDates :: [String]
netsuiteDates = [ "17/11/2014 6:31 pm"
                , "28/8/2013 1:59 pm"
                , "28/8/2013 2:07 pm"
                , "9/12/2014 3:15 pm"
                , "10/12/2014 11:29 am"
                , "1/1/2010 12:00 am"
                , "24/12/2014 11:59 pm"
                ]
