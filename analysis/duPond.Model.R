library(tidyverse)
source("financialRatios.R")
source("/home/joel/Documents/stocks/analysis/fundamentalAnalysis.R")


generateAssetTable <- function( currentAssetTbl, totalAssetTbl ){
  balanceSheetTbl <- 
    inner_join(  currentAssetTbl, totalAssetTbl )
  return( balanceSheetTbl )
}

generateIncomeTable <- function( incomeTbl, netIncomeTbl ){
  incomeStatementTbl <- 
    inner_join( incomeTbl, netIncomeTbl )
  return( incomeStatementTbl )
}

incomeStatement.ModelTree <- function( symbol ){
  grossIncome <- 
    incomeStatementURL( symbol ) %>% 
    createHTMLSession() %>% 
    marketWatchGrossIncomeTable2() 
  
  netIncome <- 
    incomeStatementURL( symbol ) %>% 
    createHTMLSession() %>% 
    marketWatchNetIncomeTable2()
  
  incomeTable <-
    generateIncomeTable(grossIncome, netIncome) %>% 
    mutate( symbol=symbol ) %>% 
    select( symbol, everything() ) %>% 
    calculateGrossProfit() %>% 
    calculateInterestCoverageRatio() %>% 
    calculateNetProfitMargin() %>% 
    calculateOperatingMargin() %>% 
    calculateReturnOnSales()
  
  return(incomeTable)
}

balanceSheet.ModelTree <- function( symbol ) {
  currentAssets <- 
    balanceSheetURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchCurrentAssetTable()
  
  totalAsset <- 
    balanceSheetURL( symbol ) %>% 
    createHTMLSession() %>% 
    marketWatchTotalAssetTable2()
  
  assetTable <- 
    generateAssetTable( currentAssets, totalAsset ) 
  
  liabilities <- 
   balanceSheetURL( symbol ) %>% 
   createHTMLSession() %>% 
   marketWatchLiabilitiesTable2()

  balanceSheetTable <- 
    inner_join( assetTable, liabilities ) %>% 
    mutate( symbol=symbol ) %>% 
    select( symbol, everything() )
  balanceSheetTable <-
    balanceSheetTable %>% 
    calculateDebtToAsset( ) 
    
  balanceSheetTable <- 
    balanceSheetTable %>% 
    calculateDebtToEquity()
  balanceSheetTable <- 
    balanceSheetTable %>% 
    calculateEquityMultiplier() 

    return( balanceSheetTable ) 
}

duPont.ModelTree <- function ( symbol ) {
  balanceSheet <- 
    balanceSheet.ModelTree( symbol ) 
  incomeStatement <- 
    incomeStatement.ModelTree( symbol )

  duPont <- 
    inner_join( balanceSheet, incomeStatement ) 
  
  duPont <- 
    duPont %>% 
    calculateAssetTurnover() %>%  
    calculateReturnOnAsset()
  duPont <-
    duPont %>% 
    calculateReturnOnEquity() 

  #order 
  #duPont <- 
  #  duPont %>% 
  #  select( Year,
  #          symbol,
  #          Gross.Revenue, 
  #          COGS.DA, 
  #          Gross.Income, 
  #          Non.Product.Cost.SGA, 
  #          Interest.Expenses,	
  #          Pretax.Income, 
  #          Net.Income, 
  #          EBITDA, 
  #          Cash.ST.Investments, 
  #          Total.Accounts.Receivable, 
  #          Inventories, 
  #          Total.Current.Assets, 
  #          Net.Property, 
  #          Total.Assets, 
  #          Total.Liabilities, 
  #          Total.Shareholder.Equity, 
  #          Total.Equity, 
  #          everything())
  

  duPontRatios <-
    duPont %>% 
    select( Year, 
           symbol, 
           
           Debt.To.Asset, 
           Asset.To.Equity, 
           Debt.To.Equity,
           Gross.Profit.Margin,
           Interest.Coverage,
           Net.Profit.Margin,
           Operating.Margin,
           Return.On.Sales,
           Asset.Turnover, 
           Return.On.Asset,
           Return.On.Equity
           )

  return( duPontRatios ) 

}
