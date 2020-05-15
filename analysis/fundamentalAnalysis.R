library(rvest)
library(dplyr)
library( stringr ) 
library(tidyverse)
library(httr)
source("financialRatios.R")
source("sp500.R") 

marketWatchWebPageURL <- "https://www.marketwatch.com" 
marketWatchWebPagePath <- "investing/stock" 
marketWatchWebPageQuery <-"financials" 

marketWatchWebPageTableClass <- ".crDataTable"
marketWatchWebPageBalanceSheet <- "balance-sheet" 
marketWatchWebPageCashFlowSheet <- "cash-flow"

replaceBillion <- function( columnVector ){
  columnVector <- 
    columnVector %>%  
    str_replace( "([0-9]{1,2}).([0-9]{2})B", "\\1\\20000000") %>% 
    str_replace( "([0-9]{1,2}).([0-9]{1})B", "\\1\\200000000") %>% 
    str_replace( "([0-9]{1,3})B", "\\100000000")
  return( columnVector ) 
}

replaceMillion <- function( columnVector ){
  #order MATTERS 
  columnVector <- columnVector %>%  
    str_replace( "([0-9]{1,3}).([0-9]{2})M", "\\1\\20000") %>% 
    str_replace( "([0-9]{1,3}).([0-9]{1})M", "\\1\\200000") %>% 
    str_replace( "([0-9]{1,3})M", "\\1000000")
  return( columnVector ) 
}

grossIncomeTable <- function(){
  grossIncome <- 
    tibble( year=0000, Revenue=0.00, COGS.DA=0.0, gross.income=0.0 )  
  return( grossIncomeTbbl )
}

grossIncomeTableRmRatios <- function( incomeStatementList ){
  incomeStatementList <- incomeStatementList[c(-2, -8, -10, -11),]
  return( incomeStatementList ) 
}

createHTMLSession <- function( url ){
  session <-
    html_session( url ) 
  if( http_status(session)$reason != "OK" ){
    print("Error connection to url")
    return(NA)
  }    
  return( session ) 
}

incomeStatementURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           sep="/")
  return(urlPath)
}

balanceSheetURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           marketWatchWebPageBalanceSheet, 
           sep="/")  
  return(urlPath)
}

liabilitiesURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
          marketWatchWebPagePath, 
          tolower(symbol), 
          marketWatchWebPageQuery, 
          marketWatchWebPageBalanceSheet, 
          sep="/")
  return(urlPath)
}

marketWatchTableClass <- function ( htmlSession ) {
  incomeStatement <-
    htmlSession %>%  #status_code to check it  
    read_html()%>% 
    html_table( ".crDataTable", header=TRUE )
  return( incomeStatement )
}

incomeTableExcludedRows <- c(-2, -8, -10, -11 )
liabilitiesTableIncludeRows <- c(1,2,4, 7, 11, 24, 30, 39, 42, 43)

marketWatchGrossIncomeTable2 <- function( htmlSession,n=1 ){
  incomeStatement <-
    marketWatchTableClass( htmlSession )   
  
  yearTable <- 
    incomeStatement[[n]] %>%
    select(matches("[0-9]{3,5}"))
   
  incomeMatrix <- 
    yearTable %>% 
    slice( incomeTableExcludedRows ) %>% 
    t() 
    
  incomeTbl <- tibble( "Year"= names(incomeMatrix[,1]) )  
  
  for( ii in 1:nrow(incomeMatrix) ){
    incomeMatrix[ii,] <-
      incomeMatrix[ii,] %>% 
      replaceBillion() %>% 
      replaceMillion()
  }
  
  incomeTbl <-
    addIncomeTableRowNames( incomeTbl, incomeMatrix,
                           colN=7)
  incomeTbl <- 
    incomeTbl %>% 
    select( Year, Gross.Revenue, COGS.DA, Gross.Income )

  return(incomeTbl) 
}

addIncomeTableRowNames <- function( incomeTbl, incomeMatrix, 
                                    colN=0 ){
  if( colN == 0 )
    return(NA) 
  
  temp <- 
      incomeTbl %>% 
      mutate( Gross.Revenue=as.numeric(incomeMatrix[,1]) ) %>%
      mutate( COGS.DA=as.numeric(incomeMatrix[,2] )) %>%
      #mutate( COGS=as.numeric(incomeMatrix[,3] )) %>%
      #mutate( Depreciation.Amortization=as.numeric(incomeMatrix[,4] )) %>%
      #mutate( Depreciation=as.numeric(incomeMatrix[,5] )) %>%
      #mutate( Amortization=as.numeric(incomeMatrix[,6] ) ) %>% 
      mutate( Gross.Income=as.numeric(incomeMatrix[,7] ))
  return(temp) 
}

marketWatchNetIncomeTable2 <- function( htmlSession,n=2 ){
  incomeStatement <-
    marketWatchTableClass( htmlSession )   
    
  yearTable <- 
    incomeStatement[[n]] %>%
    select(matches("[0-9]{3,5}"))  

  incomeMatrix <- #get desired rows 
    yearTable %>%   #class(yearTable) = data.frame 
    slice( c(1, 11, 15, 28, 44 ) ) %>% 
    t() 

  netIncome <- 
    tibble( "Year"= names(incomeMatrix[,1]) )  
 
  for( ii in 1:nrow(incomeMatrix) ){
    incomeMatrix[ii,] <-
      incomeMatrix[ii,] %>% 
      replaceBillion() %>% 
      replaceMillion()
  }
 
  netIncome <- 
    netIncome %>% 
      addNetIncomeTableRowNames( incomeMatrix, colN=nrow(incomeMatrix) )

  return( netIncome ) 
}

addNetIncomeTableRowNames <- function( incomeTbl, incomeMatrix, 
                                    colN=0 ){
  if( colN == 0 )
    return(NA) 
  
  temp <- 
      incomeTbl %>% 
      mutate( Non.Product.Cost.SGA=as.numeric(incomeMatrix[,1]) ) %>%
      mutate( Interest.Expenses=as.numeric(incomeMatrix[,2] )) %>%
      mutate( Pretax.Income=as.numeric(incomeMatrix[,3] )) %>%
      mutate( Net.Income=as.numeric(incomeMatrix[,4] )) %>%
      #mutate( Depreciation.Amortization=as.numeric(incomeMatrix[,4] )) %>%
      #mutate( Depreciation=as.numeric(incomeMatrix[,5] )) %>%
      #mutate( Amortization=as.numeric(incomeMatrix[,6] ) ) %>% 
      mutate( EBITDA=as.numeric(incomeMatrix[,5] ))
  return(temp) 
}

marketWatchNetIncomeTable2 <- function( htmlSession,n=2 ){
  incomeStatement <-
    marketWatchTableClass( htmlSession )   
    
  yearTable <- 
    incomeStatement[[n]] %>%
    select(matches("[0-9]{3,5}"))  

  incomeMatrix <- #get desired rows 
    yearTable %>%   #class(yearTable) = data.frame 
    slice( c(1, 11, 15, 28, 44 ) ) %>% 
    t() 

  netIncome <- 
    tibble( "Year"= names(incomeMatrix[,1]) )  
 
  for( ii in 1:nrow(incomeMatrix) ){
    incomeMatrix[ii,] <-
      incomeMatrix[ii,] %>% 
      replaceBillion() %>% 
      replaceMillion()
  }
 
  netIncome <- 
    netIncome %>% 
      addNetIncomeTableRowNames( incomeMatrix, colN=nrow(incomeMatrix) )

  return( netIncome ) 
}

marketWatchCurrentAssetTable <- function( htmlSession,n=1 ){
  balanceSheet <-
    marketWatchTableClass( htmlSession )   
    
  yearTable <- 
    balanceSheet[[n]] %>%
    select(matches("[0-9]{3,5}"))  
 
  assetMatrix <-
    yearTable %>% 
    slice( c(1, 6, 13, 20) ) %>% 
    t() 
  
  currentAsset <- 
    tibble( "Year"= names(assetMatrix[,1]) )  
  
  for( ii in 1:nrow(assetMatrix )){
    assetMatrix[ii,] <-
      assetMatrix[ii,] %>% 
      replaceBillion() %>% 
      replaceMillion()
  }
  
  currentAsset <- 
    currentAsset %>% 
    addCurrentAssetRowNames( assetMatrix, colN=nrow(assetMatrix) )
  
  
  return( currentAsset )  
}

addCurrentAssetRowNames <- function( currentAssetTbl, assetMatrix, 
                                    colN=0 ){
  if( colN == 0 )
    return(NA) 
  
  temp <- 
    currentAssetTbl %>% 
    mutate( Cash.ST.Investments=as.numeric(assetMatrix[,1]) ) %>%
    mutate( Total.Accounts.Receivable=as.numeric(assetMatrix[,2] )) %>%
    mutate( Inventories=as.numeric(assetMatrix[,3] )) %>%
    #mutate( Depreciation.Amortization=as.numeric(incomeMatrix[,4] )) %>%
    #mutate( Depreciation=as.numeric(incomeMatrix[,5] )) %>%
    #mutate( Amortization=as.numeric(incomeMatrix[,6] ) ) %>% 
    mutate( Total.Current.Assets=as.numeric(assetMatrix[,4] ))
  return(temp) 
}

marketWatchTotalAssetTable2 <- function( htmlSession,n=2 ){
  balanceSheet <-
    marketWatchTableClass( htmlSession )   
  
  yearTable <- 
    balanceSheet[[n]] %>%
    select(matches("[0-9]{3,5}"))  
  
  assetMatrix <-
    yearTable %>% 
    slice( c(1, 16) ) %>% 
    t() 
  
  totalAsset <- 
    tibble( "Year"= names(assetMatrix[,1]) )  
  
  for( ii in 1:nrow(assetMatrix )){
    assetMatrix[ii,] <-
      assetMatrix[ii,] %>% 
      replaceBillion() %>% 
      replaceMillion()
  }
  
  totalAsset <- 
    totalAsset %>% 
    addTotalAssetRowNames( assetMatrix, colN=nrow(assetMatrix) )
  
  
  return( totalAsset )  
}

addTotalAssetRowNames <- function( currentAssetTbl, assetMatrix, 
                                     colN=0 ){
  if( colN == 0 )
    return(NA) 
  
  temp <- 
    currentAssetTbl %>% 
    mutate( Net.Property=as.numeric(assetMatrix[,1]) ) %>%
    mutate( Total.Assets=as.numeric(assetMatrix[,2] )) #%>%
    #mutate( Inventories=as.numeric(assetMatrix[,3] )) %>%
    #mutate( Depreciation.Amortization=as.numeric(incomeMatrix[,4] )) %>%
    #mutate( Depreciation=as.numeric(incomeMatrix[,5] )) %>%
    #mutate( Amortization=as.numeric(incomeMatrix[,6] ) ) %>% 
    #mutate( Total.Current.Assets=as.numeric(assetMatrix[,4] ))
  return(temp) 
}

marketWatchLiabilitiesTable2 <-function( htmlSession,n=3 ){
  balanceSheet <-
    marketWatchTableClass( htmlSession )   
  
  yearTable <- 
    balanceSheet[[n]] %>%
    select(matches("[0-9]{3,5}"))  
  
  liabilitiesMatrix <- 
    yearTable %>% 
    slice( c(24, 39, 42) ) %>% 
    t() 
  
  liabilities <- 
    tibble( "Year"= names(liabilitiesMatrix[,1]) )  
  
  for( ii in 1:nrow(liabilitiesMatrix )){
    liabilitiesMatrix[ii,] <-
      liabilitiesMatrix[ii,] %>% 
      replaceBillion() %>% 
      replaceMillion()
  }
  
  liabilities <- 
    liabilities %>% 
    addLiabilitiesTableRowNames( liabilitiesMatrix, colN=nrow(liabilitiesMatrix) )
  
  
  return( liabilities )
}

addLiabilitiesTableRowNames <- function( liabilitiesTbl, liabilitiesMatrix, 
                                   colN=0 ){
  if( colN == 0 )
    return(NA) 
  
  temp <- 
    liabilitiesTbl %>% 
    mutate( Total.Liabilities=as.numeric(liabilitiesMatrix[,1]) ) %>%
    #mutate( Treasury.Stock=as.numeric(liabilitiesMatrix[,2] )) %>%
    mutate( Total.Shareholder.Equity=as.numeric(liabilitiesMatrix[,2] )) %>%
    mutate( Total.Equity=as.numeric(liabilitiesMatrix[,3] )) #%>%
  #mutate( Depreciation=as.numeric(incomeMatrix[,5] )) %>%
  #mutate( Amortization=as.numeric(incomeMatrix[,6] ) ) %>% 
  #mutate( Total.Current.Assets=as.numeric(assetMatrix[,4] ))
  return(temp) 
}


runFundamentals <- function( sectorList, n=1, filename="sector" ){
  fileName <- paste( "/home/joel/Documents/stocks/analysis/", filename, sep="" ) 
  for( ii in n:length(sectorList) ){
    print(ii)
    ticker <- sectorList[ii] 
    print(ticker)

    ratios <- duPont.ModelTree( ticker ) 
    write_excel_csv( ratios, fileName, append=TRUE) 
    ratiosMean <- ratios %>% summarise_if( is.numeric, list( mean=mean ), na.rm=TRUE ) %>% 
    write_excel_csv( ratiosSD, fileName, append=TRUE)

    write_excel_csv( ratiosMean, fileName, append=TRUE)
    ratiosSD <- ratios %>% summarise_if( is.numeric, list( sd ), na.rm=TRUE )
    write_excel_csv( ratiosSD, fileName, append=TRUE)

  }

}
