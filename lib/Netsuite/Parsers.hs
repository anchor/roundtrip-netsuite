--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Helpful parsers for reading netsuite's "Enterprise" JSON.
module Netsuite.Parsers where

import           Control.Isomorphism.Partial
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Roundtrip
import           Data.Char
import           Data.Scientific
import qualified Data.Text                         as ST
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.Builder            as LT
import qualified Data.Text.Lazy.Builder.Scientific as LT
import           Data.Time
import           System.Locale
import           Text.Read                         (readMaybe)
import           Text.Regex

-- | Parse a netsuite currency field, which is a blob of text looking like:
--
--   "00.43"
-- 
-- This un-/parser retains all precision avaliable.
currency :: JsonSyntax s => s Scientific
currency = demote (prism' f g) <$> value
  where
    f = String . LT.toStrict . LT.toLazyText . fmt
    g = readMaybe . ST.unpack . ST.filter (not . isSpace) <=< preview _String
    -- We render with arbitrary precision (Nothing) with standard decimal
    -- notation (Fixed)
    fmt = LT.formatScientificBuilder Fixed Nothing

-- | Parse a netsuite datetime field, which is looks like:
--
--    "20/6/2014 4:25 pm"
datetime :: JsonSyntax s => s UTCTime
datetime = demote (prism' f g) <$> value
  where
    f = String . ST.pack . removeLeading . cleanSpaces . opts formatTime
    g = opts parseTime . addLeading . ST.unpack <=< preview _String
    opts h = h defaultTimeLocale "%d/%m/%Y %l:%M %P"
    cleanSpaces x  = subRegex (mkRegex "[[:space:]]+") x " "
    addLeading x = subRegex (mkRegex "^([1-9])\\/")
                            (subRegex (mkRegex "\\/([1-9])\\/") x "/0\\1/")
                            "0\\1/"
    removeLeading x = subRegex (mkRegex "0([1-9])\\/") x "\\1/"
