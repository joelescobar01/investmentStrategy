source('data.transfer.lib.R')
source("analysis/marketWatchWeb/financialStatementInteface.R")

getTable <- function( statementURL  ){
  tabl <- 
    tryCatch( 
      statementURL %>% 
      createHTMLSession() %>% 
      fetchTable2() %>%
      combineTables() %>% 
      removeDashes("defaultName") %>%
      removePercentage("defaultName") %>% 
      convertFinanceFormat("defaultName") %>%
      cleanRowItem() %>% 
      cleanTable2("defaultName") %>% 
      rename_at( -1 ,~ c("year1", "year2", "year3", "year4", "year5" )  ) %>% 
      mutate_if( is.numeric, ~ na_if(.x, 0 ) ), error=function(err) NA ) 
  return( tabl ) 
}

getTable2 <- function( statementURL  ){
  tabl <- 
    tryCatch( 
      statementURL %>% 
      createHTMLSession() %>% 
      fetchTable2() %>%
      combineTables() %>% 
      removeDashes("defaultName") %>%
      removePercentage("defaultName") %>% 
      convertFinanceFormat("defaultName") %>%
      cleanRowItem() %>% 
      cleanTable2("defaultName") %>% 
      mutate_if( is.numeric, ~ na_if(.x, 0 ) ), error=function(err) NA ) 
  return( tabl ) 
}

commonSizeIncomeStatementTable <- function( symbol ){
 commonSize <- 
  incomeStatementURL(symbol) %>% 
  getTable() %>%
  mutate_at( vars(-"defaultName"), ~ ./first(..1) ) %>% 
  rename( income.statement = "defaultName") 
  return( commonSize ) 
}

trendIncomeStatementTable <- function( symbol ) {
  trendTable <- 
    incomeStatementURL(symbol) %>% 
    getTable() %>% 
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
    getTable() %>%  
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
