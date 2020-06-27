library(tidyverse) 
source("analysis/marketWatchWeb/balanceSheet.R")

net.Current.Asset <- function( symbol ){
#net current asset value is a company's liquidation value.
  netAsset <- 
    balanceSheetYear( symbol ) %>% 
    replace(., is.na(.), 0.00 ) %>% 
    filter( BalanceSheet %in% c(  "TotalAssets", "NetGoodwill", 
                                  "NetOtherIntangibles", "IntangibleAssets", 
                                  "TotalLiabilities") ) %>% 
    pivot_longer( cols=starts_with("year"), 
                 names_to="year", 
                 names_pattern="(\\-*\\d+\\.*\\d*)", 
                 values_to="asset") %>% 
    pivot_wider( names_from="BalanceSheet", values_from="asset" ) %>% 
    transmute(  year,TotalAssets, IntangibleAssets, 
                NetGoodwill, NetOtherIntangibles, TotalLiabilities,  
              net.asset = TotalAssets - (NetOtherIntangibles+NetGoodwill+IntangibleAssets+TotalLiabilities) )
    return(netAsset ) 
}

calculateGrossProfit <- function( incomeStatementTbl ){
  incomeStatementTbl <- 
    incomeStatementTbl %>%
    mutate( Gross.Profit.Margin = gross.income/revenue ) 
  return( incomeStatementTbl )  
}

calculateOperatingMargin <- function( incomeStatementTbl ){
  incomeStatementTbl <- 
    incomeStatementTbl %>% 
    mutate( Operating.Margin = EBITDA/revenue )
  return( incomeStatementTbl ) 
}

calculateInterestCoverageRatio <- function( incomeStatementTbl ){
  incomeStatementTbl <- 
    incomeStatementTbl %>% 
    mutate( Interest.Coverage = EBITDA/interest.expenses )
  return( incomeStatementTbl ) 
}

calculateNetProfitMargin <- function( incomeStatementTbl ){
  incomeStatementTbl <-
    incomeStatementTbl %>% 
    mutate( Net.Profit.Margin = net.income/revenue )
  return( incomeStatementTbl )
}

calculateReturnOnSales <- function( incomeStatementTbl ){
  incomeStatementTbl <-
    incomeStatementTbl %>%
    mutate( Return.On.Sales = EBITDA/revenue )
  
  return( incomeStatementTbl )
}

calculateDebtToAsset <- function( balanceSheet ){
  debtAsset <- 
    balanceSheet %>% 
    mutate( Debt.To.Asset = Total.Liabilities/ Total.Assets ) 

  return(debtAsset ) 
}

calculateDebtToEquity <- function( liabilitiesTable ){
  debtToEquity <-
    liabilitiesTable %>% 
    mutate( Debt.To.Equity = Total.Liabilities/Total.Equity ) 
  return( debtToEquity ) 
}

calculateEquityMultiplier <- function( balanceSheet ){
  equityMultiply <- 
    balanceSheet %>% 
    mutate( Asset.To.Equity = Total.Assets/Total.Equity ) 

  return( equityMultiply ) 
}

calculateReturnOnEquity <- function( duPontTable ){
  roaTable <-
    duPontTable %>% 
    mutate( Return.On.Equity = Return.On.Asset*Asset.To.Equity  )
  return( roaTable ) 
}

calculateAssetTurnover <- function( duPontTable ){
  assetTurnoverTable <-
    duPontTable %>% 
    mutate( Asset.Turnover = Gross.Revenue/Total.Assets  )
  return( assetTurnoverTable ) 
}

calculateReturnOnAsset <- function( duPontTable ){
  roaTable <-
    duPontTable %>% 
    mutate( Return.On.Asset = Return.On.Sales/Asset.Turnover  )
  return( roaTable ) 
}


