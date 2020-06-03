library(tidyverse) 
library(tidyquant)
library(TTR) 


volatilityMACD <- function(stockTbbl, nFast=12, nSlow=26, nSignal=9 ){
  stockDf <- 
    stockTbbl %>%
    tq_mutate( select=close, mutate_fun=volatility, n=nFast, col_rename="volatility.short") %>% 
    tq_mutate( select=close, mutate_fun=volatility, n=nSlow, col_rename="volatility.long" ) %>% 
    mutate( historic.volatility.short = runSum(volatility.short,n=nFast) )  %>% 
    mutate( alpha.short = volatility.short / historic.volatility.short ) %>% 
    mutate( historic.volatility.long = runSum(volatility.long,n=nSlow)  ) %>% 
    mutate( alpha.long = volatility.long/historic.volatility.long ) %>%
    drop_na() %>%  
    mutate( macd = TTR::EMA(close,nFast, ratio=alpha.short) - TTR::EMA(close,nSlow, ratio=alpha.long) ) %>% 
    drop_na() %>% 
    mutate( signal = EMA( macd, nSignal) ) %>% 
    mutate(divergence = macd - signal) %>% 
    select( -volatility.short, -volatility.long, 
            -historic.volatility.short, -historic.volatility.long, -alpha.long, -alpha.short ) 
  return( stockDf ) 
}

 macdPlot <- 
   stockDf %>% 
   ggplot( aes(x=date) ) + 
   geom_line( aes(y=macd, colour="MACD") ) + 
   geom_line( aes(y=signal, colour="Signal") ) + 
   guides(colour="none")

closePlot <- 
  stockDf %>% 
  ggplot( aes(x=date) ) + 
  geom_line( aes(y=close) )

 gp1<- 
   ggarrange( closePlot, macdPlot, nrow=2, ncol=1, align=c("h") )
#
#mutate(divergence = macd - signal)
 # drop_na() 
