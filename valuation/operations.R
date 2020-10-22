source("analysis/marketWatchWeb/financialStatementInteface.R")


workingCapital <- function( ticker ){
  wc <- 
    balanceSheetYear(ticker) %>% 
    select( period, Total_Current_Assets, Total_Current_Liabilities ) %>% 
    replace(is.na(.), 0 ) %>%     
    mutate( working.capital = Total_Current_Assets - Total_Current_Liabilities ) %>% 
    mutate( symbol = ticker ) 
  return(wc)
}

operatingExpense <- function( ticker ) {
  opex <- 
    incomeStatementYear(ticker) %>% 
    select( period, COGS_excluding_DA, SGA_Expense, Other_Operating_Expense, Unusual_Expense ) %>% 
    replace(is.na(.), 0 ) %>% 
    mutate( opex = COGS_excluding_DA, SGA_Expense, Other_Operating_Expense, Unusual_Expense ) %>% 
    mutate( symbol = ticker )
  return(opex) 
}

capitalExpense <- function( ticker ){
  capex <- 
    cashFlowYear(ticker) %>%
    select( period, Capital_Expenditures ) %>% 
    mutate( symbol = ticker )

  return(capex)
}

currentPosition <- function( ticker ) {
  revenue <- 
    incomeStatementYear(ticker) %>%
    select( period, 'SalesRevenue' ) %>% 
    rename( revenue = 'SalesRevenue' ) %>%  
    replace( is.na(.), 0 )
  
  cu <- 
    balanceSheetYear( ticker ) %>% 
    select( period, Total_Current_Assets, Total_Current_Liabilities, Inventories ) %>% 
    mutate( working.capital = Total_Current_Assets - Total_Current_Liabilities ) %>% 
    mutate( current.ratio = Total_Current_Assets / Total_Current_Liabilities ) %>% 
    mutate( symbol = ticker ) %>% 
    left_join( revenue, by='period' ) %>% 
    mutate( revenue.per.working.capital = revenue / working.capital )  

  return( cu ) 
}

inventoryTurnover <- function( ticker ) { 
  revenue <- 
    incomeStatementYear(ticker) %>% 
    select( period, revenue )
  bs <- 
    balanceSheetYear(ticker) %>% 
    select( period, Inventories ) 
  turnover <- 
    left_join( revenue, bs ) %>% 
    mutate( inventory.turnover = (revenue / Inventories) ) 
  return( turnover ) 
}


