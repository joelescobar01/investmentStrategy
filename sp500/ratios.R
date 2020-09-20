library( tidyquant )
library( tidyverse )
source("data.transfer.lib.R")
source("visual.lib.R") 
source("analysis/marketWatchWeb/financialStatementInteface.R")

sp500SectorPerformance <- function(fromD=ymd("2015-01-01")){
  p1 <- 
    yahoo.Stock.Prices(c("XLC", "XLY", "XLP","XLE", "XLF", "XLV", "XLI", "XLB", "XLRE", "XLK", "XLU"),
                       from=fromD ) %>% 
    group_by(symbol) %>% 
    ggplot( aes(x=date, colour=symbol) ) + 
    geom_line( aes(y=close, linetype=factor(symbol)), size=1 ) + 
    scale.date.axis.large() +
    scale.price.axis() + 
    max.plot.space() +  
    theme(legend.position = "bottom") + 
    guides(colour="none", linetype='none') + 
    facet_wrap(. ~ symbol, scales='free' ) 
  return(p1) 
}

indexPERatios <- function(index){
  sp500Ratios <- 
    tq_index(index) %>% 
    select( symbol ) %>% 
    map_dfr( ~ .x %>% get.Financial.Ratios() )

  sp500Avg <- 
    tq_index(index) %>% 
    select( symbol, sector, weight ) %>% 
    right_join( sp500Ratios, by='symbol' ) %>% 
    select( -date, -trade.time, -Market.Capitilization, -last.close.price, -Average.Daily.Volume, -Shares.Outstanding, -Dividend.per.Share, -Dividend.Yield  ) %>% 
    rename_all( tolower )
  return( sp500Avg ) 
}

indexPriceToProfitRatios <- function(index){
  sp500Ratios <- 
    tq_index(index) %>% 
    select( symbol ) %>% 
    map_dfr( ~ .x %>% get.Financial.Ratios() )

  #sp500Avg <- 
  #  tq_index(index) %>% 
  #  select( symbol, sector, weight ) %>% 
  #  right_join( sp500Ratios, by='symbol' ) %>% 
  #  select( -date, -trade.time, -Market.Capitilization, -last.close.price, -Average.Daily.Volume, -Shares.Outstanding, -Dividend.per.Share, -Dividend.Yield  ) %>% 
  #  rename_all( tolower )
  #return( sp500Avg ) 
}


indexSectorPEAvg <- function( sp500PE=indexPERatios("DOW") ){
  avgPE <- 
    sp500PE %>% 
    group_by( sector ) %>% 
    summarize(  avg.pe = mean(pe.ratio, na.rm=TRUE), 
                w.avg.pe = weighted.mean( pe.ratio, weight, na.rm=TRUE ) ) 
  return( avgPE ) 
}

indexRatios <- function(index){
  indexRat <- 
    tq_index(index) %>% 
    select( symbol ) %>% 
    map_dfr( ~ .x %>% get.Financial.Ratios() ) %>% 
    rename_all( tolower )
  return( indexRat ) 
}

indexQBalanceSheet <- function(index, company=1:10){
  qBalanceS <- 
    tq_index(index) %>% 
    select( symbol ) %>% 
    slice(company) %>% 
    filter( !str_detect(symbol, "\\.") ) %>% 
    pull %>% 
    map( ~ tibble( symbol=.x, balance.sheet=balanceSheetQuarter(.x)  ) ) %>% 
    map_dfr( ~ .x$balance.sheet %>% mutate( symbol = .x$symbol )   ) %>% 
    group_by(symbol) %>% 
    select( symbol, everything() ) 

  return( qBalanceS ) 
}

indexQIncomeStatement <- function(index, company=1:10){
  qBalanceS <- 
    tq_index(index) %>% 
    select( symbol ) %>% 
    slice(company) %>% 
    filter( !str_detect(symbol, "\\.") ) %>% 
    pull %>% 
    map( ~ tibble( symbol=.x, income.statement=incomeStatementQuarter(.x)  ) ) %>% 
    map_dfr( ~ .x$income.statement %>% mutate( symbol = .x$symbol )   ) %>% 
    group_by(symbol) %>% 
    rename( revenue = salesrevenue ) %>% 
    select( symbol, everything() ) 

  return( qBalanceS ) 
}
indexWorkingCapital <- function( qBalanceS ){
  capital <- 
    qBalanceS %>% 
    group_by( symbol ) %>% 
    transmute(  period,   
                working.capital = total.current.assets - total.current.liabilities,
                current.ratio = total.current.assets / total.current.liabilities,
                total.equity )

  return( capital ) 
}


sp500PERatios <- function(){
  sp500Ratios <- 
    tq_index("SP500") %>% 
    select( symbol ) %>% 
    map_dfr( ~ .x %>% get.Financial.Ratios() )

  sp500Avg <- 
    tq_index("SP500") %>% 
    select( symbol, sector, weight ) %>% 
    right_join( sp500Ratios, by='symbol' ) %>% 
    select( -date, -trade.time, -Market.Capitilization, -last.close.price, -Average.Daily.Volume, -Shares.Outstanding, -Dividend.per.Share, -Dividend.Yield  ) %>% 
    rename_all( tolower )
  return( sp500Avg ) 
}

sp500SectorPEAvg <- function( sp500PE=sp500PERatios() ){
  avgPE <- 
    sp500PE %>% 
    group_by( sector ) %>% 
    summarize(  avg.pe = mean(pe.ratio, na.rm=TRUE), 
                w.avg.pe = weighted.mean( pe.ratio, weight, na.rm=TRUE ),
                median.pe = median( pe.ratio, na.rm=TRUE ) ) 
  return( avgPE ) 
}

sp500PriceBookRatios <- function(){
  sp500Ratios <- 
    tq_index("SP500") %>% 
    select( symbol ) %>% 
    map_dfr( ~ .x %>% get.Financial.Ratios() )

  sp500Avg <- 
    tq_index("SP500") %>% 
    select( symbol, sector, weight ) %>% 
    right_join( sp500Ratios, by='symbol' ) %>% 
    select( -date, 
            -trade.time, 
            -Market.Capitilization, 
            -last.close.price, 
            -Average.Daily.Volume, 
            -Shares.Outstanding, 
            -Dividend.per.Share, 
            -Dividend.Yield  ) %>% 
    rename_all( tolower )
  return( sp500Avg ) 
}

sp500SectorPriceBookAvg <- function( sp500PE=sp500PriceBookRatios() ){
  avgPB <- 
    sp500PE %>% 
    group_by( sector ) %>% 
    summarize(  avg.price.book = mean(price.book, na.rm=TRUE), 
                w.avg.price.book = weighted.mean( price.book, weight, na.rm=TRUE ),
                median.price.book = median( price.book, na.rm=TRUE ) ) 
  return( avgPB ) 
}
