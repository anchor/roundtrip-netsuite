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
import           Data.Time
import           System.Locale

import           Netsuite.Parsers

main :: IO ()
main = hspec $ do
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

    describe "datetime syntax" $ do
        it "round trips netsuite datetime" $ do
            let td = readTime defaultTimeLocale "%Y-%m-%d" "2010-01-01"
            runBuilder datetime td `shouldBe` Just (String "1/1/2010 12:00 am")
            let expected_x = readTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2014-12-24 23:59:00"
            let n = "24/12/2014 11:59 pm"
            runParser datetime n `shouldBe` Just expected_x

        it "round trips different datetime strings" $ do
            _ <- mapM roundtripOneDate netsuiteDates
            return ()

roundtripOneDate :: String -> Expectation
roundtripOneDate d = do
    let d' = (String $ T.pack d)
    let (Just p) = runParser datetime d'
    runBuilder datetime p `shouldBe` Just d'

netsuiteDates :: [String]
netsuiteDates = [ "17/11/2014 6:31 pm"
                , "28/8/2013 1:59 pm"
                , "28/8/2013 2:07 pm"
                , "9/12/2014 3:15 pm"
                , "10/12/2014 11:29 am"
                , "1/1/2010 12:00 am"
                , "24/12/2014 11:59 pm"
                ]
