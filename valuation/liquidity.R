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

bookValue <- function( symbol ) {
  print( symbol ) 
  income <- 
    incomeStatementURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>%
    transmute( period, 
                net.income.available.to.common, 
                diluted.shares.outstanding ) 
  balance <- 
    balanceSheetURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>% 
    transmute( period, 
                total.shareholders.equity ) 

  bookValue <- 
    left_join( income, balance, by="period") %>%
    mutate( book.value.per.share = total.shareholders.equity / diluted.shares.outstanding,
            earnings.per.share = net.income.available.to.common / diluted.shares.outstanding 
           ) 

  return( bookValue )
}

investedCapital <- function( symbol ){
  # = total.debt + total.equity + non-operating.cash + cash.equivalents 
}

longTermSolvency <- function( symbol ) {
  print( symbol ) 
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
                effective.tax.rate = income.tax/pretax.income,
                cost.of.debt.post.tax = interest.expense*(1-effective.tax.rate ), 
                operating.income.post.tax = operating.income*(1-effective.tax.rate), 
                interest.to.operating.income.post.tax = cost.of.debt.post.tax/operating.income.post.tax, 
                operations.to.sales = operating.income / sales.revenue , 
                 
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
                net.operating.cash.flow.operating.activities, 
                capital.expenditures.investing.activities, 
                capital.expenditures.fixed.assets.investing.activities, 
                net.investing.cash.flow.investing.activities ) 

  solvencyTable <- 
    left_join( income, balance, by="period") %>%
    left_join( cashFlow, by="period") %>% 
    mutate( 
           interest.coverage = operating.income / interest.expense, 
           operating.cash.flow.to.total.liabilities = net.operating.cash.flow.operating.activities / total.liabilities.excluding.deferred.taxes, 
           symbol = symbol ) %>%
    left_join( annualReturn( symbol ), by='period' )  
  return( solvencyTable ) 
}


