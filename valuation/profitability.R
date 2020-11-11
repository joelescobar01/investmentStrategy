source("analysis/marketWatchWeb/financialStatementInteface.R")
# airlines %>% unnest( cols=c(income.statement) ) %>% select( -sub.industry
# ) %>% group_by( symbol ) %>% pivot_longer( -c(symbol, defaultName),
# names_to="period", values_to="values" ) %>% pivot_wider(
# names_from=defaultName, values_from=values ) %>% transmute( symbol ,period,
# gross.profit = gross.income / sales.revenue, net.profit = net.income
# / sales.revenue, ebit = gross.income - sga.expense - other.operating.expense
# , operating.profit = ebit/sales.revenue )
#
profitClass <- function( symbol ) {
  income <- 
    incomeStatementURL(symbol) %>% 
    financialStatementRatioLess2() %>% 
    tidyTable() %>% 
    transmute( period, 
                sales.revenue,
                cogs.excluding.da, 
                depreciation, 
                ebit =  gross.income - 
                                    sga.expense - 
                                    other.operating.expense, 
                effective.tax.rate = income.tax/pretax.income,
                NOPAT = ebit*(1-effective.tax.rate) ) 

  balance <- 
    balanceSheetURL(symbol) %>% 
    financialStatementRatioLess2() %>% 
    tidyTable() %>% 
    transmute(  period, 
                total.assets,
                total.current.liabilities,
                net.tangible.assets = total.assets - 
                                      intangible.assets - 
                                      net.goodwill - 
                                      net.other.intangibles, 
                capital.employed = net.tangible.assets - total.current.liabilities )

  profitTable <- 
    left_join( income, balance, by="period") %>%
    transmute( period,  
              operating.profit.ratio = ebit / sales.revenue, 
              operating.profit.margin = NOPAT / sales.revenue,
              return.on.capital.employed = ebit / capital.employed ) %>% 
    excelFormatTable() 
  return( profitTable ) 
}


