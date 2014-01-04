{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.CSV.Conduit (CSVSettings(..))
import Data.CSV.Conduit.Parser.Text
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.IO

parseFile :: String -> IO [(Text,Text,Text)]
parseFile path = runResourceT $
  CB.sourceFile path $$
    CT.decode CT.utf8 =$=
    CT.lines =$=
    CL.map (\ line -> either (\ x -> Nothing) id $ parseRow (CSVSettings ',' (Just '"')) $ T.concat [line,"\n"]) =$=
    CL.catMaybes =$=
    CL.map (\ x -> (head x,x !! 1,x !! 2)) =$=
    CL.consume

emitFiles :: [(Text,Text,Text)] -> IO ()
emitFiles sources = do
  emitPricingSourceDescriptions $ L.zip [0..] $ L.filter (\x  -> T.length x > 0) $ L.nub $ fmap (\ (_,_,desc) -> fixDescription desc) $ L.drop 1 sources
  emitPricingSourceAbbreviations $ L.zip [0..] $ L.nub $ fmap (\ (_,abbr,_) -> uppercaseFirstLetter abbr) $ L.drop 1 sources
  emitCategories $ L.zip [0..] $ L.nub $ fmap (\ (cat,_,_) -> fixCategory cat) $ L.drop 1 sources
  emitPricingSources $ L.drop 1 sources
  emitPricingSourceParsers $ L.zip [0..] $ L.nub $ fmap (\ (_,abbr,_) -> uppercaseFirstLetter abbr) $ L.drop 1 sources

emitPricingSourceDescriptions :: [(Int,Text)] -> IO ()
emitPricingSourceDescriptions sources =
  withFile "PricingSourceDescriptions.hs" WriteMode $ \ handle -> do
    T.hPutStrLn handle "module Finance.OpenSymbology.PricingSourceDescriptions where"
    T.hPutStrLn handle "data Description ="
    runResourceT $
      CL.sourceList sources $$
      CL.map (\ (index,name) ->  if index == 0 then formatFirstLine name else formatLine name) =$=
      CT.encode CT.utf8 =$=
      CB.sinkHandle handle
    T.hPutStrLn handle "  deriving (Ord,Eq,Show,Read)"
  where
    formatLine name = T.concat ["  | ", name,"\n"]
    formatFirstLine name = T.concat ["    ", name,"\n"]

emitPricingSourceAbbreviations :: [(Int,Text)] -> IO ()
emitPricingSourceAbbreviations sources =
  withFile "PricingSourceAbbreviations.hs" WriteMode $ \ handle -> do
    T.hPutStrLn handle "module Finance.OpenSymbology.PricingSourceAbbreviations where"
    T.hPutStrLn handle "data Abbreviation ="
    runResourceT $
      CL.sourceList sources $$
      CL.map (\ (index,abbr) -> if index == 0 then formatFirstLine abbr else formatLine abbr) =$=
      CT.encode CT.utf8 =$=
      CB.sinkHandle handle
    T.hPutStrLn handle "  deriving (Ord,Eq,Show,Read)"
  where
    formatLine abbr = T.concat ["  | ", abbr,"\n"]
    formatFirstLine abbr = T.concat ["    ", abbr,"\n"]

emitCategories :: [(Int,Text)] -> IO ()
emitCategories sources =
  withFile "PricingSourceCategories.hs" WriteMode $ \ handle -> do
    T.hPutStrLn handle "module Finance.OpenSymbology.PricingSourceCategories where"
    T.hPutStrLn handle "data Category ="
    runResourceT $
      CL.sourceList sources $$
      CL.map (\ (index,cat) ->if index == 0 then formatFirstLine cat else formatLine cat) =$=
      CT.encode CT.utf8 =$=
      CB.sinkHandle handle
    T.hPutStrLn handle "  deriving (Ord,Eq,Show,Read)"
  where
    formatLine abbr = T.concat ["  | ", abbr,"\n"]
    formatFirstLine abbr = T.concat ["    ", abbr,"\n"]

emitPricingSources :: [(Text,Text,Text)] -> IO ()
emitPricingSources sources =
  withFile "PricingSources.hs" WriteMode $ \ handle -> do
    T.hPutStrLn handle "module Finance.OpenSymbology.PricingSources where"
    T.hPutStrLn handle "import Finance.OpenSymbology.PricingSourceAbbreviations"
    T.hPutStrLn handle "import Finance.OpenSymbology.PricingSourceCategories"
    T.hPutStrLn handle "import Finance.OpenSymbology.PricingSourceDescriptions"
    T.hPutStrLn handle "import Finance.OpenSymbology.Types"
    T.hPutStrLn handle ""
    T.hPutStrLn handle "pricingSources :: [PricingSource]"
    T.hPutStr handle "pricingSources = ["
    let sources' = T.concat[(T.concat $ L.intersperse ",\n" $ fmap (\ (cat,abbr,desc) -> 
                     T.concat ["  PricingSource ",
                               fixCategory cat,
                               " Finance.OpenSymbology.PricingSourceAbbreviations.",
                               uppercaseFirstLetter abbr, " Finance.OpenSymbology.PricingSourceDescriptions.", fixDescription desc]) sources), "]"]
    T.hPutStr handle sources'

fixCategory name = T.replace " " "" $ T.replace "only" "Only" $ T.replace ")" "" $ T.replace "(" "" $ T.replace "," "" name

fixDescription "" = "None"
fixDescription name = uppercaseFirstLetter $
                 T.replace "/" "" $
                 T.replace "+" "Plus" $
                 T.replace "'" "" $
                 T.replace ":" "" $
                 T.replace "," "" $
                 T.replace "." "" $
                 T.replace "-" "" $
                 T.replace ")" "" $
                 T.replace "(" "" $
                 T.replace ">" "LargerThan" $
                 T.replace "<" "LessThan" $
                 T.replace "20%" "TwentyPercent" $
                 T.replace "&" "And" $
                 T.replace  " " "" name

uppercaseFirstLetter :: Text -> Text
uppercaseFirstLetter x =
  let (a,b) = T.splitAt 1 x
  in T.concat [T.toUpper a, b]

{-
emitParsers :: [()]
-}

emitPricingSourceParsers :: [(Int,Text)] -> IO ()
emitPricingSourceParsers sources =
  withFile "PricingSourceParsers.hs" WriteMode $ \ handle -> do
    T.hPutStrLn handle "{-# LANGUAGE OverloadedStrings #-}"
    T.hPutStrLn handle "module Finance.OpenSymbology.PricingSourceParsers where"
    T.hPutStrLn handle ""
    T.hPutStrLn handle "import Control.Applicative"
    T.hPutStrLn handle "import qualified Data.Attoparsec.Text as A"
    T.hPutStrLn handle "import Finance.OpenSymbology.PricingSourceAbbreviations"
    T.hPutStrLn handle "import Prelude hiding(EQ,GT)"
    T.hPutStrLn handle ""
    T.hPutStrLn handle "pricingSourceParser :: A.Parser (Maybe Abbreviation)"
    T.hPutStrLn handle "pricingSourceParser ="
    runResourceT $
      CL.sourceList sources $$
      CL.map (\ (index,abbrev) ->if index == 0 then formatFirstLine abbrev else formatLine abbrev) =$=
      CT.encode CT.utf8 =$=
      CB.sinkHandle handle
    T.hPutStrLn handle "  <|> pure Nothing"

  where
    formatFirstLine abbrev = T.concat["    A.string \"",abbrev,"\" *> pure (Just ", abbrev, ")\n"]
    formatLine abbrev = T.concat["  <|> A.string \"",abbrev,"\" *> pure (Just ", abbrev, ")\n"]

main :: IO ()
main = do
  args <- getArgs
  if L.length args /= 1 then print "Usage: BuildPricingSources <pricing_sources_file>" else (parseFile $ L.head args) >>= emitFiles
