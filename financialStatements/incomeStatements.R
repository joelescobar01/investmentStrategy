source("analysis/marketWatchWeb/financialStatementInteface.R")

commonSizeStatement <- function( incomeStatement ) {
  commonSize <- 
    incomeStatement %>% 
    mutate( across( where( is.numeric ), ~ .x / sales.revenue ) )  
  return( commonSize ) 
}

trendStatement <- function( incomeStatement ) {
  trend <- 
    incomeStatement %>% 
    mutate( across(where(is.numeric), ~ .x / .[[1]] ) ) 

  return(trend)
}

adjustedPretax <- function( incomeStatement ){
  pretax <- 
    incomeStatement %>% 
    mutate( adjusted.pretax.income = 
           pretax.income - 
           nonoperating.interest.income - 
           non.operating.incomeexpense - 
           unusual.expense ) %>% 
    select( period, pretax.income, adjusted.pretax.income ) %>% 
    mutate( diff.pretax.income = pretax.income - adjusted.pretax.income ) %>% 
    mutate( diff.percent = diff.pretax.income / adjusted.pretax.income ) 
  return( pretax ) 
}
