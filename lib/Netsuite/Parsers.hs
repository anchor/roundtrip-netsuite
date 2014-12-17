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
import           Data.Aeson.RoundTrip
import           Data.Char
import           Data.Scientific
import qualified Data.Text                         as ST
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.Builder            as LT
import qualified Data.Text.Lazy.Builder.Scientific as LT
import           Text.Read                         (readMaybe)

-- Parse a netsuite currency field, which is a blob of text looking like:
-- "00.43"
currency :: JsonSyntax s => s Scientific
currency = demote (prism' f g) <$> value
  where
    f = String . LT.toStrict . LT.toLazyText . LT.scientificBuilder
    g = preview _String >=> readMaybe . ST.unpack . ST.filter (not . isSpace)
