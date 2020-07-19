library(tidyverse) 
library(tidyquant)
library(TTR) 
source( "data.transfer.lib.R" )
source("var/economicIndicators.R")


returns.Dividend.Yield <- function( symbol ){
  
  dividend.yield <- 
    yahoo.fetch(symbol) %>% 
    right_join( yahoo.Dividend.Payout(symbol), by="date" ) %>% 
    transmute( date, close,adjusted, dividend ) %>% 
    mutate( dividend.yield = dividend/close ) 
  return(dividend.yield ) 
}

multiple.Compounding.1year.Returns <- function( startingVal, intRate, compPeriod ){
  principal <- startingVal
  rate <- intRate
  period <- compPeriod
  ending.value <- principal*(1 + rate/period)^(period)
  return.avg <- ending.value/principal - 1
  return( ending.value )
}

returnsTrendDiversification <- function( stocks ){
  # group stocks by symbol 
  stockReturns <- 
    stocks %>% 
    group_by( symbol ) %>% 
    transmute( date, symbol, daily.returns = TTR::ROC(adjusted) ) %>% 
    drop_na() 

  covMatrix <- 
    stockReturns %>% 
    spread( symbol, daily.returns ) %>% 
    select( -date ) %>% 
    drop_na() %>% 
    cov()

  varia <- 
    diag( covMatrix ) 

  covTibb <- 
    as_tibble( covMatrix ) %>% 
    gather() %>% 
    rename( symbol=key, variance=value ) 
  
  covar <- 
    unique( covTibb$symbol ) 

  covTibb$co.variance <- 
    rep(unique( covar), time=length(covar) )

  p1 <-
    stockReturns %>% 
    group_by( symbol ) %>% 
    mutate( growth = cumprod(1 + daily.returns) ) %>% 
    ggplot( aes( x=date) ) +
    geom_line( aes(y=growth) ) +

    facet_grid( ~ symbol, scales="free_x" ) + 
    labs( caption=paste( names(varia), "Variance =", varia, collapse="; ", sep=" " )) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = scales::dollar) +
    scale_x_date( breaks = scales::breaks_width("6 months"),
                labels = scales::label_date_short() ) + 
    guides( colour="none" ) 

  return( p1 ) 
}
  

assetReturn <- function( ticker ){
  assetReturn <- 
    ticker %>%
    tq_get( get='stock.prices' ) %>% 
    tq_transmute( select=adjusted, 
                  mutate_fun=periodReturn, 
                  period="monthly", 
                  type="log", 
                  col_rename=c("returns") ) %>% 
    left_join( marketProxyReturns(), by='date' ) 
    return( assetReturn ) 

}

first.Last.Quote <- function( symbol=c(), fromDate=ymd("2015-01-01") ) {
  stock <-
    symbol %>% 
    tq_get( get='stock.prices', from=fromDate ) %>% 
    group_by( symbol, year=year(date) ) %>% 
    filter( date==min(date)|date==max(date) ) %>% 
    ungroup() %>% 
    select( symbol, date, open, close, adjusted ) 
  return( stock ) 
}


#tq_mutate( select=close, mutate_fun=periodReturn, period="daily" ) %>% group_by( symbol, week=week(date) ) %>% summarize( volume.sd = sd(volume)/1000000, weekly.return=mean(daily.returns ) ) %>% group_by( symbol, week ) %>% ggplot( aes(y=weekly.return, x=volume.sd, colour=symbol ) ) + geom_point() + geom_text(aes(colour=symbol,label=symbol), vjust = 0, nudge_y = 0.001) + geom_hline( yintercept=0, linetype="dashed", alpha=0.5 ) + facet_wrap( vars(week), scales="free" ) + ggtitle( glue::glue("Industrial Sectors Weekly Summaries" ) ) + labs(x="Weekly Volatility Volume (scaled by 100000)", y="Average Weekly Returns" ) + guides( colour="none")
