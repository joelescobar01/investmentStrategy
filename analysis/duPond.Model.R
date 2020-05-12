library(tidyverse)
source("financialRatios.R")
source("fundamentalAnalysis.R")


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
    select( symbol, everything() ) %>% 
    mutate( Debt.To.Asset = Total.Liabilities/Total.Assets ) %>%
    mutate( Debt.To.Equity = Total.Liabilities/Total.Equity)%>% 
    mutate( Equity.Multiplier = Total.Assets/Total.Equity ) 

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
    mutate( Return.On.Equity = Return.On.Asset*Equity.Multiplier )
    
  #order 
  duPont <- 
    duPont %>% 
    select( Year,
            symbol,
            Gross.Revenue, 
            COGS.DA, 
            Gross.Income, 
            Non.Product.Cost.SGA, 
            Interest.Expenses,	
            Pretax.Income, 
            Net.Income, 
            EBITDA, 
            Cash.ST.Investments, 
            Total.Accounts.Receivable, 
            Inventories, 
            Total.Current.Assets, 
            Net.Property, 
            Total.Assets, 
            Total.Liabilities, 
            Total.Shareholder.Equity, 
            Total.Equity, 
            everything())
  
  return( duPont ) 

}
