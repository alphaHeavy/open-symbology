module Finance.OpenSymbology.PricingSourceCategories where
data Category =
    Equity
  | EquityIndex
  | CorporateGovernmentPreferred
  | Currency
  | Government
  | Mortgage
  | CorporateCurrencyGovernmentPreferred
  | Commodity
  | EquityFundsOnly
  | CommodityIndex
  | CommodityEquityIndex
  | CorporateGovernmentMortgagePreferred
  | CurrencyEquityIndex
  | CorporateEquityGovernmentPreferred
  | CurrencyCommodity
  | CorporateCurrencyGovernmentMortgagePreferred
  | Index
  | CorporateGovernmentIndexPreferred
  | CorporateEquityGovernmentIndexPreferred
  | CommodityCurrencyIndex
  | CommodityCurrencyEquityIndex
  | CorporateGovernmentCurrencyPreferred
  | CommodityCorporateGovernmentPreferred
  | CommodityCurrency
  | CurrencyIndex
  | CorporateGovernmentIndexPreferredMortgage
  | CorporateCurrencyEquityGovernmentIndexPreferred
  | CorporateCurrencyGovernmentIndexPreferred
  | CorporateGovernmentMortgageMuniPreferred
  | EquityFutures
  | EquityCommonStocks
  | EquityOptionsOnly
  deriving (Ord,Eq,Show,Read)
