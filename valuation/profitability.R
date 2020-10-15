source("analysis/marketWatchWeb/financialStatementInteface.R")

profitClass <- function( symbol ) {
  income <- 
    incomeStatementURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>% 
    transmute( period, 
                sales.revenue, 
                operating.income =  gross.income - 
                                    sga.expense - 
                                    other.operating.expense, 
                effective.tax.rate = income.tax/pretax.income,
                NOPAT = operating.income*(1-effective.tax.rate) ) 

  balance <- 
    balanceSheetURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>% 
    transmute(  period, 
                total.assets, 
                net.tangible.assets = total.assets - 
                                      intangible.assets - 
                                      net.goodwill - 
                                      net.other.intangibles, 
                avg.assets = (net.tangible.assets + lag( net.tangible.assets))/2 )

  profitTable <- 
    left_join( income, balance, by="period") %>%
    mutate( operating.profit.margin = NOPAT / sales.revenue, 
            asset.turnover = sales.revenue / avg.assets, 
            roa = NOPAT/avg.assets ) %>% 
    excelFormatTable() 
  return( profitTable ) 
}
