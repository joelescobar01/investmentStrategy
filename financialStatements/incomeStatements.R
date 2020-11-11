source('data.transfer.lib.R')
source("analysis/marketWatchWeb/financialStatementInteface.R")

#incomeStatementURL("AAPL") %>% createHTMLSession() %>% fetchTable2() %>%
#combineTables() %>% removeDashes("defaultName") %>%
#removePercentage("defaultName") %>% convertFinanceFormat("defaultName") %>%
#cleanRowItem() %>% cleanTable2("defaultName") %>% 
#mutate_at( vars(-"defaultName"), ~ ./first(..1) )

commonSizeStatement <- function( incomeStatement ) {
  commonSize <- 
    incomeStatement %>% 
    mutate( across( where( is.numeric ), ~ .x / sales.revenue ) )  
  return( commonSize ) 
}

commonSizeStatement2 <- function( incomeStatement ) {
  commonSize <- 
    incomeStatement %>% 
    mutate_at( vars(-"defaultName"), ~ ./first(..1) )
  return( commonSize ) 
}


trendStatement <- function( incomeStatement ) {
  trend <- 
    incomeStatement %>% 
    mutate( across(where(is.numeric), ~ .x / .[[1]] ) ) 

  return(trend)
}

assetTrendStatement2 <- function( incomeStatement ) {
  trend <- 
    incomeStatement %>% 
    mutate( index = .[[2]] ) %>% 
    mutate_if( is.numeric, ~ .x/index ) %>% 
    select( -index )
    
    return(trend)
}


test <- 
  incomeStatementURL("AAPL") %>% 
  createHTMLSession() %>% 
  fetchTable2() %>%
  combineTables() %>% 
  removeDashes("defaultName") %>%
  removePercentage("defaultName") %>% 
  convertFinanceFormat("defaultName") %>%
  cleanRowItem() %>% 
  cleanTable2("defaultName") %>% 
  trendStatement()


#balanceSheetURL("AAPL") %>% createHTMLSession() %>% fetchTable2() %>% first(2)
#%>% map( ~ .x %>% rename( "defaultName" = 1 ) ) %>% reduce( bind_rows ) %>%
#removeDashes("defaultName") %>% removePercentage("defau
#ltName") %>% convertFinanceFormat("defaultName") %>% cleanRowItem() %>%
#cleanTable2("defaultName")
