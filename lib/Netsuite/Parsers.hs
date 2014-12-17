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

import Control.Isomorphism.Partial
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Monoid
import Data.Text (Text)
import Text.Roundtrip.Classes
import Data.Aeson.RoundTrip
import Text.Roundtrip.Combinators

currency :: JsonSyntax s => s Rational
currency = undefined


