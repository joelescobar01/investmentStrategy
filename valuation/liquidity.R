source("data.transfer.lib.R") 
source("analysis/marketWatchWeb/financialStatementInteface.R")
source("valuation/lib.R")
workingCapital <- function( symbol ){
  wc <- 
  balanceSheetURL(symbol) %>% 
  financialStatementRatioLess() %>% 
  tidyTable() %>% 
  transmute( period, total.current.assets, 
            total.current.liabilities, 
            working.capital = total.current.assets - total.current.liabilities) %>%
  excelFormatTable()
  return(wc)
}

totalDebt <- function( symbol ){
  debt <-
    balanceSheetURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>% 
    transmute( period, st.debt.current.portion.lt.debt, longterm.debt )
  return(debt) 
}

nonOperatingCashFlow <- function( symbol ) {
}

long_term_solvency <- function( symbol ){

}

investedCapital <- function( symbol ){
  # = total.debt + total.equity + non-operating.cash + cash.equivalents 
}

longTermSolvency <- function( symbol ) {
  income <- 
    incomeStatementURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>% 
    transmute( period, 
                sales.revenue,
                cogs.excluding.da, 
                operating.income =  gross.income - 
                                    sga.expense - 
                                    other.operating.expense, 
                interest.expense ) 

  balance <- 
    balanceSheetURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>% 
    transmute(  period, 
                total.assets,
                longterm.debt, 
                total.current.liabilities,
                avg.current.liabilities = ( total.current.liabilities + lag(total.current.liabilities  )) / 2,  
                total.liabilities.excluding.deferred.taxes = avg.current.liabilities + longterm.debt, 
                net.tangible.assets = total.assets - 
                                      intangible.assets - 
                                      net.goodwill - 
                                      net.other.intangibles, 
                long.term.debt.to.tangible.asset = longterm.debt / net.tangible.assets )
 
  cashFlow <- 
    cashFlowURL(symbol) %>%
    financialStatementCashFlow() %>% 
    transmute( period, 
                net.operating.cash.flow.operating.activities )
 
  solvencyTable <- 
    left_join( income, balance, by="period") %>%
    left_join( cashFlow, by="period") %>% 
    mutate( 
           interest.coverage = operating.income / interest.expense, 
           operating.cash.flow.to.total.liabilities = net.operating.cash.flow.operating.activities / total.liabilities.excluding.deferred.taxes  
           ) %>%
    left_join( annualReturn( symbol ), by='period' )  
  return( solvencyTable ) 
}


