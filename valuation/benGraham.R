source("analysis/marketWatchWeb/financialStatementInteface.R")

#companies <- tq_index("SP500") %>% filter(sector=="Energy") %>% select( symbol
#) %>% pull() %>% sp500Companies() %>% companyValuation() %>% pmap( ~ mutate(
#..1, company=..2) %>% select( company, everything() ) ) 

companyFinancialDocumentsYearly <- function( company ){
  incomeS <- 
    company %>% 
    incomeStatementYear()
  balanceS <- 
    company %>% 
    balanceSheetYear()   
  cashFlowS <- 
    company %>% 
    cashFlowYear()

 document <- 
   list(  company = company, 
          income.statement = incomeS,
          balance.sheet = balanceS, 
          cash.flow = cashFlowS ) 

  return( document )
}

companyFinancialDocumentsQuaterly <- function( company ){
  companyStatement <- 
    incomeStatementQuarter(company) %>% 
    left_join( balanceSheetQuarter(company), by='period' ) %>% 
    left_join( cashFlowQuarter( company ), by='period' ) 

  return( companyStatement )
}


companyRatios <- function(companies){
  indexRat <- 
    companies %>% 
    select( symbol ) %>% 
    map_dfr( ~ .x %>% get.Financial.Ratios() ) %>% 
    rename_all( tolower )
  return( indexRat ) 
}



dividendToPriceRatio <- function( sp500Companies ){
  dividends <-
    sp500Companies %>% 
    yahoo.Dividend.Payout(from="2015-01-01") 
  stocks <- 
    sp500Companies %>% 
    yahoo.Stock.Prices( from="2015-01-01")
  
  stockReturns <- 
    stocks %>% 
    group_by( symbol ) %>%
    tq_transmute( select=close, mutate_fun=periodReturn, period="yearly", col_rename= "annual.returns" ) %>% 
    transmute( symbol, period = year(date), annual.returns ) %>% 
    rename( company = symbol ) 

  companies <- 
    left_join( dividends, stocks, by=c("symbol", "date" ) ) %>%
    select( symbol, date, dividend, close ) %>%
    mutate( dividend.price.ratio = dividend/close ) %>% 
    rename( company = symbol  ) %>% 
    group_by( company, period=year(date) ) %>%
    summarise(  avg.price = mean( close ), 
                avg.dividend.ratio = mean( dividend.price.ratio ), 
                year.dividends = sum( dividend ) ) %>% 
    left_join( stockReturns, by=c("company", "period" ) ) 
  
  return( companies ) 
}

dividendToPriceRatio2 <- function( company){
  dividends <-
    company %>% 
    yahoo.Dividend.Payout(from="2015-01-01") 
  stocks <- 
    company %>% 
    yahoo.Stock.Prices( from="2015-01-01")
  
  stockReturns <- 
    stocks %>% 
    group_by( symbol ) %>%
    tq_transmute( select=close, mutate_fun=periodReturn, period="yearly", col_rename= "annual.returns" ) %>% 
    transmute( symbol, period = year(date), annual.returns ) %>% 
    rename( company = symbol ) 

  companies <- 
    left_join( dividends, stocks, by=c("symbol", "date" ) ) %>%
    select( symbol, date, dividend, close ) %>%
    mutate( dividend.price.ratio = dividend/close ) %>% 
    rename( company = symbol  ) %>% 
    group_by( company, period=year(date) ) %>%
    summarise(  avg.price = mean( close ), 
                avg.dividend.ratio = mean( dividend.price.ratio ), 
                year.dividends = sum( dividend ) ) %>% 
    left_join( stockReturns, by=c("company", "period" ) ) 
  
  return( companies ) 
}


companyValuation <- function( companyStatements ) {
  valuation <- 
    companyStatements$data %>% 
    map_dfr( ~ .x %>% 
            select_if( colnames(.) %in% requiredColumns  )  %>% 
            select_if( ~ sum( !is.na(.) ) > 0 ) %>% 
            nest() 
          ) %>% 
    bind_cols( company = companyStatements$company ) %>% 
    pmap( ~ mutate( ..1, company=..2 ) %>%
            select( company, everything() ) ) 
    return( valuation ) 
}

companyValuation2 <- function( companyStatements ) {
  valuation <- 
    companyStatements %>% 
    select_if( colnames(.) %in% requiredColumns  ) 
    return( valuation ) 
}


sp500Weights <- function(){
  weight <- 
    tq_index("SP500") %>% 
    select( symbol, weight ) %>% 
    rename( company = symbol ) 
  return( weight ) 
}
# %>% re
financialRatios <- function( valuation ){
  ratios <- 
    valuation %>% 
    replace( is.na(.), 0 ) %>% 
    group_by( company ) %>% 
    transmute( period, company,
      revenue.growth = (SalesRevenue - lag(SalesRevenue) )/lag(SalesRevenue),
      intantible.assets = Intangible_Assets,  
      profit = Net_Income - Preferred_Stock_Carrying_Value, 
      physical.assets = Total_Assets - Intangible_Assets, 
      fix.cost = Net_Property_Plant_Equipment - Total_Investments_and_Advances,
      working.capital = Total_Current_Assets - Total_Current_Liabilities, 
      current.ratio = Total_Current_Assets / Total_Current_Liabilities, 
      earning.per.share = profit / Basic_Shares_Outstanding, 
      return.on.assets = Net_Income / physical.assets,
      book.value = physical.assets - Preferred_Stock_Carrying_Value - Total_Liabilities 
    ) %>% left_join( dividendToPriceRatio( unique( valuation$company ) ), by=c("company", "period" ) ) %>% 
    mutate( 
        price.per.earning = avg.price / earning.per.share, 
        dividend.rate = year.dividends, 
        dividend.yield = dividend.rate / avg.price ) 
    return( ratios ) 
}

financialRatios2 <- function( valuation ){
  ratios <- 
    valuation %>% 
    replace( is.na(.), 0 ) %>% 
    transmute( period, company,
      revenue.growth = (SalesRevenue - lag(SalesRevenue) )/lag(SalesRevenue),
      intantible.assets = Intangible_Assets,  
      profit = Net_Income - Preferred_Stock_Carrying_Value, 
      physical.assets = Total_Assets - Intangible_Assets, 
      fix.cost = Net_Property_Plant_Equipment - Total_Investments_and_Advances,
      working.capital = Total_Current_Assets - Total_Current_Liabilities, 
      current.ratio = Total_Current_Assets / Total_Current_Liabilities, 
      earning.per.share = profit / Basic_Shares_Outstanding, 
      return.on.assets = Net_Income / physical.assets,
      book.value = physical.assets - Preferred_Stock_Carrying_Value - Total_Liabilities 
    ) 
    return( ratios ) 
}

financialRatiosQuarterly <- function( Company ){

  statement <- 
    companyFinancialDocumentsQuaterly(Company)  %>% 
    mutate( company = Company, period=dmy(period) ) %>% 
    select( company, everything() ) %>% 
    select_if( colnames(.) %in% requiredColumns ) %>% 
    financialRatios2()

  return( statement ) 
}

avgCurrentRatio <- function( valuation ){
  currentR <- 
    valuation %>% 
    map_dfr( ~ .x %>% 
                select( company, period, current.ratio )  ) %>% 
    left_join( sp500Weights(), by="company" ) %>% 
    group_by( period ) %>% 
    summarize(  avg.current.ratio = mean( current.ratio ), 
                weighted.current.ratio = weighted.mean(current.ratio, weight ) )

  return( currentR )
}

fixDates <- function( valuation ){
  valuationFixed <- 
    valuation %>% 
    mutate( period = year(years(period) ) ) %>% 
    filter( !is.na(period) ) 
  return( valuationFixed ) 
}
