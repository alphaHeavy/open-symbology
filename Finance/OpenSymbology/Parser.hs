{-# LANGUAGE OverloadedStrings #-}
module Finance.OpenSymbology.Parser (entryParser) where

import Control.Applicative
import Data.Attoparsec.Text (Parser,(<?>))
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T
import Finance.OpenSymbology.PricingSourceParsers
import Finance.OpenSymbology.Types

--import Finance.OpenSymbology.PricingSourceAbbreviations

entryParser :: Parser BloombergEntry
entryParser = parseHeader <|> (mkBloombergEntry
    <$> (parseColumn                          <?> "0 Name")
    <*> (parseColumn                          <?> "1 Ticker")
    <*> ((pricingSourceParser <* (A.try $ A.char '|'))  <?> "2 PricingSource")
--    <*> ((A.string "US" *> pure (Just US) <* A.char '|')  <?> "2 PricingSource")
    <*> ((textToMaybe <$> parseColumn)        <?> "3 SourceId")
    <*> ((textToMaybe <$> parseColumn)        <?> "4 UniqueId")
    <*> ((textToMaybe <$> parseColumn)        <?> "5 SecurityType")
    <*> (parseColumn                          <?> "6 MarketSector")
    <*> ((BloombergId <$> parseColumn)        <?> "7 BloombergId")
    <*> ((BloombergId <$> parseColumn)        <?> "8 BloombergCompositeId"))
  where parseColumn = A.takeWhile (/= '|') <* (A.try $ A.char '|')
        mkBloombergEntry name ticker prcSrc srcId uniqId secType mktSect bid bidComp = BloombergEntry
          { beName = BloombergName name
          , beSymbol = BloombergSymbol ticker
          , bePricingSource = prcSrc
          , beSecurityType = secType
          , beMarketSector = mktSect
          , beBloombergId = bid
          , beBloombergCompositeId = bidComp
          , beSourceId = srcId
          , beUniqueId = uniqId }
        parseHeader = A.string "NAME|ID_BB_SEC_NUM_DES|FEED_SOURCE|ID_BB_SEC_NUM_SRC|ID_BB_UNIQUE|SECURITY_TYP|MARKET_SECTOR_DES|ID_BB_GLOBAL|COMPOSITE_ID_BB_GLOBAL|" *> pure BloombergHeader

textToMaybe :: Text -> Maybe Text
textToMaybe x =
  case T.null x of
    True -> Nothing
    False -> Just x
