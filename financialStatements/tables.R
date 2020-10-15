source('data.transfer.lib.R')
source("analysis/marketWatchWeb/financialStatementInteface.R")

commonSizeIncomeStatementTable <- function( symbol ){
 commonSize <- 
  incomeStatementURL(symbol) %>% 
  createHTMLSession() %>% 
  fetchTable2() %>%
  combineTables() %>% 
  removeDashes("defaultName") %>%
  removePercentage("defaultName") %>% 
  convertFinanceFormat("defaultName") %>%
  cleanRowItem() %>% 
  cleanTable2("defaultName") %>%
  mutate_at( vars(-"defaultName"), ~ ./first(..1) ) %>% 
  rename( income.statement = "defaultName") 
  return( commonSize ) 
}

trendIncomeStatementTable <- function( symbol ) {
  trendTable <- 
    incomeStatementURL(symbol) %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    combineTables() %>% 
    removeDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat("defaultName") %>%
    cleanRowItem() %>% 
    cleanTable2("defaultName") %>% 
    mutate( index = .[[2]] ) %>% 
    mutate_if( is.numeric, ~ .x/index ) %>% 
    rename( income.statement = "defaultName") %>% 
    select( -index )
    

  return(trendTable)
}

trendBalanceSheetAssetTable <- function( symbol ) {
  trendTable <- 
    balanceSheetURL(symbol) %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    first(2) %>% 
    map( ~ .x %>% 
            rename( "defaultName" = 1 ) ) %>%
    reduce( bind_rows) %>%
    removeDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat("defaultName") %>%
    cleanRowItem() %>% 
    cleanTable2("defaultName") %>% 
    mutate( index = .[[2]] ) %>% 
    mutate_if( is.numeric, ~ .x/index ) %>% 
    rename( balance.sheet.assets = "defaultName") %>% 
    select( -index )

  return(trendTable)
}

commonSizeBalanceSheetAssetTable <- function( symbol ){
 commonSize <- 
  balanceSheetURL(symbol) %>% 
  createHTMLSession() %>% 
  fetchTable2() %>%
  first(2) %>% 
    map( ~ .x %>% 
            rename( "defaultName" = 1 ) ) %>%
  reduce( bind_rows) %>%
  removeDashes("defaultName") %>%
  removePercentage("defaultName") %>% 
  convertFinanceFormat("defaultName") %>%
  cleanRowItem() %>% 
  cleanTable2("defaultName") %>%
  mutate_at( vars(-"defaultName"), ~ ./last(..1) )

  return( commonSize ) 
}


trendBalanceSheetEquityTable <- function( symbol ) {
  trendTable <- 
    balanceSheetURL(symbol) %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    last() %>% 
    map( ~ .x %>% 
            rename( "defaultName" = 1 ) ) %>%
    reduce( bind_rows) %>%
    removeDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat("defaultName") %>%
    cleanRowItem() %>% 
    cleanTable2("defaultName") %>% 
    mutate( index = .[[2]] ) %>% 
    mutate_if( is.numeric, ~ .x/index ) %>% 
    rename( balance.sheet.assets = "defaultName") %>% 
    select( -index )

  return(trendTable)
}

commonSizeBalanceSheetEquityTable <- function( symbol ){
 commonSize <- 
  balanceSheetURL(symbol) %>% 
  createHTMLSession() %>% 
  fetchTable2() %>%
  last() %>% 
   map( ~ .x %>% 
            rename( "defaultName" = 1 ) ) %>%
  reduce( bind_rows) %>%
  removeDashes("defaultName") %>%
  removePercentage("defaultName") %>% 
  convertFinanceFormat("defaultName") %>%
  cleanRowItem() %>% 
  cleanTable2("defaultName") %>%
  mutate_at( vars(-"defaultName"), ~ ./last(..1) )

  return( commonSize ) 
}

trendCashFlowTable <- function( symbol ) {

  trendTable <- 
    cashFlowURL(symbol) %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    combineTables() %>%
    removeDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat("defaultName") %>% 
    cleanRowItem() %>% 
    cleanTable2("defaultName") %>%  
    mutate( index=.[[2]] ) %>% 
    mutate_if( is.numeric, ~ .x/index ) %>% 
    select( -index )
    
    return(trendTable)
}

commonCashFlowTable <- function( symbol, revenue ){
  revenue <- 
    incomeStatementURL(symbol) %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    combineTables() %>% 
    removeDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat("defaultName") %>% 
    cleanRowItem() %>% 
    cleanTable2("defaultName") %>% 
    slice(1)

  commonSize <- 
    cashFlowURL(symbol) %>% 
    createHTMLSession() %>% 
    fetchTable2() %>%
    combineTables() %>% 
    removeDashes("defaultName") %>%
    removePercentage("defaultName") %>% 
    convertFinanceFormat("defaultName") %>%
    cleanRowItem() %>% 
    cleanTable2("defaultName") %>%
    bind_rows( revenue ) %>% 
    mutate_at( vars(-"defaultName"), ~ ./last(..1) )

  return( commonSize ) 
}
