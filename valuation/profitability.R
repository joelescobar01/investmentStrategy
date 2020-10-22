source("analysis/marketWatchWeb/financialStatementInteface.R")

profitClass <- function( symbol ) {
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
                NOPAT = operating.income*(1-effective.tax.rate) ) 

  balance <- 
    balanceSheetURL(symbol) %>% 
    financialStatementRatioLess() %>% 
    tidyTable() %>% 
    transmute(  period, 
                total.assets,
                inventories,
                accounts.payable, 
                avg.account.payable = ( accounts.payable - lag( accounts.payable )/2 ), 
                avg.inventory = ( inventories - lag(inventories) )/2, 
                net.tangible.assets = total.assets - 
                                      intangible.assets - 
                                      net.goodwill - 
                                      net.other.intangibles, 
                avg.assets = (net.tangible.assets + lag( net.tangible.assets))/2, 
                current.ratio = total.current.assets / total.current.liabilities , 
                quick.ratio = (cash.short.term.investments + total.accounts.receivable) / total.current.liabilities,
                avg.account.receivables = ( total.accounts.receivable + lag( total.accounts.receivable ) )/2 
                )

  cashFlow <- 
    cashFlowURL(symbol) %>%
    financialStatementCashFlow() 

  profitTable <- 
    left_join( income, balance, by="period") %>%
    mutate( operating.profit.margin = NOPAT / sales.revenue, 
            asset.turnover = sales.revenue / avg.assets, 
            roa = NOPAT/avg.assets, 
            account.receivable.turnover = sales.revenue / avg.account.receivables, 
            days.account.receivable.outstanding =  365 / account.receivable.turnover, 
            inventory.turnover = cogs.excluding.da / avg.inventory, 
            days.inventory.held = 365 / inventory.turnover, 
            inventory.purchased = cogs.excluding.da + inventories - lag( inventories ),
            account.payable.turnover = inventory.purchased / avg.account.payable, 
            days.account.payable.outstanding = 365 / account.payable.turnover, 
            operating.cycle = days.account.receivable.outstanding + days.inventory.held + days.account.payable.outstanding,
            cash.cycle = days.account.receivable.outstanding + days.inventory.held - days.account.payable.outstanding 
            ) %>% 
    excelFormatTable() 
  return( profitTable ) 
}


