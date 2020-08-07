source("analysis/marketWatchWeb/financialStatementInteface.R")

fetchInfo <- function( company ){
  ratios <- 
    get.Financial.Ratios( company ) 
  returns <- 
    yahoo.Stock.Prices("AAPL") %>% 
    tq_transmute( select=close, mutate_fun=periodReturn, period='yearly' )

  documents <- 
    companyFinancialDocumentsYearly( company ) 
  
  capitalization <- 
    documents[['balance.sheet']] %>% 
    transmute( period, total.debt = total.liabilities - cash.short.term.investments ) 

}

companyDebt <- function( financialList ){
  debt <- 
    financialList[-1] %>% 
    map( ~ .x %>% 
        select_if(names(.) %in% c('period', 
                                  'longterm.debt', 
                                  'st.debt.current.portion.lt.debt', 
                                  'cash.short.term.investments' )) ) %>% 
    reduce( left_join, by='period' ) %>% 
    transmute( period, 
                total.debt = longterm.debt + st.debt.current.portion.lt.debt - cash.short.term.investments,
                short.to.long.term.debt = st.debt.current.portion.lt.debt / longterm.debt  )

    return ( debt ) 
}

companyProfitability <- function( financialList ){
  earnings <- 
    financialList[-1] %>% 
    map( ~ .x %>% 
            select_if(names(.) %in% c('period', 'salesrevenue', 
                                      'net.income', 'diluted.shares.outstanding', 
                                      'preferred.dividends', 'total.liabilities', 
                                      'total.shareholders.equity' )) ) %>% 
    reduce( left_join, by='period' ) %>% 
    mutate( preferred.dividends = pmax( preferred.dividends.x, preferred.dividends.y ) ) %>% 
    select( -preferred.dividends.y, -preferred.dividends.x ) %>% 
    transmute( earning.per.share = (net.income - preferred.dividends ) / diluted.shares.outstanding, 
              retained.earnings = net.income - preferred.dividends, 
              invested.capital = total.liabilities - total.shareholders.equity, 
              return.on.invested.capital = retained.earnings / invested.capital )  
    return( earnings ) 
}

companyLiquidity <- function( financialList ){
  liquid <- 
    financialList[-1] %>% 
    map( ~ .x %>% 
        select_if(names(.) %in% c('period', 
                                  'total.current.assets',
                                  'total.assets', 
                                  "intangible.assets",
                                  "net.goodwill",
                                  "net.other.intangibles",
                                  'total.liabilities',
                                  'total.current.liabilities')) ) %>% 
    reduce( left_join, by='period' ) %>% 
    transmute( period, 
                current.ratio = total.current.assets / total.current.liabilities,
                book.value = total.assets - intangible.assets - net.goodwill - net.other.intangibles,
                )

    return ( liquid ) 
}
