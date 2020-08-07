library( tidyquant )
library( tidyverse )
source("visual.lib.R") 

sp500Performance <- function(){
  p1 <- 
    yahoo.Stock.Prices(c("SPY")) %>% 
    ggplot( aes(x=date ) )+ 
    geom_line( aes(y=close), size=1 ) + 
    scale.price.axis() + 
    max.plot.space() +  
    scale.date.axis.large()
  return(p1) 

}

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
    guides(linetype="none") + 
    facet_wrap(. ~ symbol, scales='free' ) 
  return(p1) 
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
}
