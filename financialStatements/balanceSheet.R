source("analysis/marketWatchWeb/financialStatementInteface.R")

joinAssetTable <- function( statementSeperated ){
  assets <- 
    left_join( statementSeperated$current.assets, statementSeperated$fixed.assets, by="period" )  
  return( assets ) 
}

# symbol %>% balanceSheetURL() %>% financialStatementSeperated() 
commonSizeStatementAssets <- function( statementSeperated ) {
  assets <-
    left_join( statementSeperated$current.assets, statementSeperated$fixed.assets, by="period" ) %>% 
    mutate( across( where( is.numeric), ~ .x / total.assets ) ) 

  return( assets ) 
}

trendStatementAssets <- function( statementSeperated ) {
  trend <- 
    statementSeperated %>%
    joinAssetTable %>% 
    mutate( across(where(is.numeric), ~ .x / .[[1]] ) ) 

  return(trend)
}


