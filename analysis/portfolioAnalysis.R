library(tidyverse) 
library(tidyquant)
library(TTR) 
library( ggpubr ) 
library( broom ) 
source( "data.transfer.lib.R" )

marketProxy <-
  c("SPY")

# The Wilshire 5000 is therefor a broad-based market index. A broad-based index is designed to reflect the movement of an entire market.
broadMarketProxy <- 
  c("^W5000" ) 


stockSectorsID <- function(){
  sectors <- 
    tq_index( "SP500" ) %>% 
    group_by( sector ) %>% 
    summarize( symbol ) %>% 
    select( sector ) %>% 
    distinct  

  return( sectors ) 
}

getSectorStocks <- function( sectorName ){
  
  sp500 <- 
    tq_index("SP500") %>% 
    group_by( sector ) %>% 
    summarize( symbol ) %>% 
    filter( sector == sectorName ) %>% 
    ungroup %>% 
    select( symbol ) %>% 
    pull() %>% 
    tq_get( get='stock.prices', from="2020-01-01" ) 
  sp500Sector <- 
    sp500 %>% 
    group_by( symbol ) %>% 
    tally() %>% 
    arrange(n) %>% 
    filter( n >= 60 ) %>% 
    select( symbol ) %>% 
    left_join( sp500, by='symbol' ) %>% 
    group_by(symbol)

  return( sp500Sector ) 
}

sectorVolatility <- function( sectors ) {  
  sectorVolatility <- 
    sectors %>% 
    group_by( symbol ) %>% 
    tq_transmute( select=close, mutate_fun=volatility, n=10, col_rename="volatility" ) %>% 
    group_by( symbol ) %>% 
    mutate( date = ymd(date) ) %>% 
    drop_na() %>% 
    group_by( monthly = month( date, label=TRUE  ) ) %>% 
    ggplot( aes(x=date,y=volatility,colour=symbol) ) + 
    geom_point() + 
    facet_wrap(vars(monthly), scales="free" ) + 
    scale_x_date(NULL,breaks = scales::breaks_width("1 weeks"), labels = scales::label_date_short() )

}

sectorWeekVolume <- function( sectors ){
  sectors %>%
    group_by( symbol, week=week(date) ) %>% 
    summarize( volume.sd = sd(volume) ) 
}

marketProxyReturns <- function( nPeriod="monthly" ) { 
  market <- 
    tq_get( marketProxy, get='stock.prices' ) %>% 
    tq_transmute( select=close, 
                mutate_fun=periodReturn, 
                period=nPeriod, 
                type="log", 
                col_rename=c("market.returns" ) 
              ) 
  return( market ) 
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
  

totalUSMarket <- function( ){
  market <- 
    alphavantage.Stock.Prices.Daily(broadMarketProxy) %>% 
    mutate( symbol = "W5000" ) %>% 
    rename( date=timestamp ) 
  return( market ) 
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

portfolioReturns <- function( assets=c() ){
  portfolio <-
    assets %>%
    tq_get( get='stock.prices' ) %>% 
    group_by( symbol ) %>% 
    tq_transmute( select=adjusted, 
                  mutate_fun=periodReturn, 
                  period="monthly", 
                  type="log", 
                  col_rename=c("returns") ) %>% 
    nest()  
    return( portfolio ) 
}

portfolioBeta <- function( portfolio ){
  tickers <- 
    portfolio$symbol 
  portolioBea <- 
    portfolio %>%
    pmap( ~c(...) ) %>% 
    map( ~ as_tibble(.) %>% 
          select( symbol, date, returns ) %>% 
          left_join( marketProxyReturns(), by="date") %>% 
          tq_performance( Ra=returns, 
                          Rb=market.returns,
                          performance_fun= CAPM.beta ) %>% 
          select_if( is.numeric ) %>% 
          pull() )
    
    portfolioBeta <- 
      tibble( symbol=tickers, CAPM.beta = unlist( portolioBea ) ) 
 return( portfolioBeta ) 
}

chart.PortfolioReturns <- function(portfolio){
  p1 <- 
    ggplot(portfolio, aes( x=market.returns, y=returns) ) + 
                    geom_point( ) +
                    geom_smooth(method = "lm", se = FALSE, color = "green", size = .5) + 
                    ggtitle(glue::glue("{.$symbol}Portfolio returns v. market Returns" ) ) 

    return(p1)
}

portfolioAugmentd <- function( assetReturn ) {
  portfolio <- 
    assetReturn %>%
    do(model = lm(returns ~ market.returns, data = .))%>% 
    augment(model) %>% 
    mutate(date = assetReturn$date) %>% 
    select( date, everything() )
  return( portfolio ) 
}

chart.PortfolioAugmentd <- function( portfolio ) {
  p1 <-
    portfolio %>% 
    ggplot(aes(x = date)) + 
    geom_line(aes(y = returns, color = "actual returns")) + 
    geom_line(aes(y = .fitted, color = "fitted returns")) +
    scale_colour_manual("", 
                        values = c("fitted returns" = "green", 
                                   "actual returns" = "cornflowerblue")) +
    xlab("date") + 
    ggtitle("Fitted versus actual returns")

  return(p1)
}

chart.PortfolioAugmentdDollarGrowth <- function( portfolio ) {
  p1 <- 
    portfolio %>% 
    mutate(actual_growth = cumprod(1 + returns), 
           fitted_growth = cumprod(1 + .fitted)) %>% 
    ggplot(aes(x = date)) + 
    geom_line(aes(y = actual_growth, color = "actual growth")) + 
    geom_line(aes(y = fitted_growth, color = "fitted growth")) +
    xlab("date") +
    ylab("actual and fitted growth") + 
    ggtitle("Growth of a dollar: actual v. fitted") +
    scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
    scale_y_continuous(labels = scales::dollar) +
    scale_colour_manual("", 
                        values = c("fitted growth" = "green", 
                                   "actual growth" = "cornflowerblue")) 
  return(p1) 
}




#tq_mutate( select=close, mutate_fun=periodReturn, period="daily" ) %>% group_by( symbol, week=week(date) ) %>% summarize( volume.sd = sd(volume)/1000000, weekly.return=mean(daily.returns ) ) %>% group_by( symbol, week ) %>% ggplot( aes(y=weekly.return, x=volume.sd, colour=symbol ) ) + geom_point() + geom_text(aes(colour=symbol,label=symbol), vjust = 0, nudge_y = 0.001) + geom_hline( yintercept=0, linetype="dashed", alpha=0.5 ) + facet_wrap( vars(week), scales="free" ) + ggtitle( glue::glue("Industrial Sectors Weekly Summaries" ) ) + labs(x="Weekly Volatility Volume (scaled by 100000)", y="Average Weekly Returns" ) + guides( colour="none")
