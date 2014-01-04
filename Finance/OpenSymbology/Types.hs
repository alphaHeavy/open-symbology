module Finance.OpenSymbology.Types (
    BloombergEntry (..)
  , BloombergId (..)
  , BloombergName (..)
  , BloombergSymbol (..)
  , PricingSource (..)
  ) where

import Data.Text (Text)
import Finance.OpenSymbology.PricingSourceAbbreviations
import Finance.OpenSymbology.PricingSourceCategories
import Finance.OpenSymbology.PricingSourceDescriptions

data BloombergId = BloombergId {bId :: Text} deriving (Eq,Ord,Show)

data BloombergName = BloombergName {bName :: Text} deriving (Eq,Ord,Show)

data BloombergSymbol = BloombergSymbol {bSymbol :: Text} deriving (Eq,Ord,Show)

data BloombergEntry =
  BloombergHeader |
  BloombergEntry {
    beName :: BloombergName,
    beSymbol :: BloombergSymbol,
    bePricingSource :: Maybe Abbreviation,
    beSecurityType :: Maybe Text,
    beMarketSector :: Text,
    beBloombergId :: BloombergId,
    beBloombergCompositeId :: BloombergId,
    beSourceId :: Maybe Text,
    beUniqueId :: Maybe Text } deriving (Show,Eq)

data PricingSource = PricingSource Category Abbreviation Description deriving (Ord,Eq,Show,Read)
