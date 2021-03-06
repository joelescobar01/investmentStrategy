library(tidyverse) 
library(tidyquant)
library(TTR) 
library( ggpubr ) 
library( broom ) 
marketProxy <-
  c("SPY")



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

marketProxyReturns <- 
  tq_get( marketProxy, get='stock.prices' ) %>% 
  tq_transmute( select=adjusted, 
                mutate_fun=periodReturn, 
                period="monthly", 
                type="log", 
                col_rename=c("market.returns" ) ) 


assetReturn <- function( ticker ){
  assetReturn <- 
    ticker %>%
    tq_get( get='stock.prices' ) %>% 
    tq_transmute( select=adjusted, 
                  mutate_fun=periodReturn, 
                  period="monthly", 
                  type="log", 
                  col_rename=c("returns") ) %>% 
    left_join( marketProxyReturns, by='date' ) 
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
          mutate( date=data.date, 
                  returns=data.returns ) %>% 
          select( symbol, date, returns ) %>% 
          left_join( marketProxyReturns, by="date") %>% 
          tq_performance( Ra=returns, 
                          Rb=market.returns,
                          performance_fun= CAPM.beta ) %>% 
          select_if( is.numeric ) %>% 
          pull() )
    
    portfolioBeta <- 
      tibble( symbol=tickers, CAPM.beta = unlist( portolioBea ) ) 
 return( portfolioBeta ) 
}

chart.PortfolioReturns <- function(assetReturn){
  p1 <-
    assetReturn %>% 
    pmap( ~ c(...) ) %>% 
    map( ~ as_tibble(.) %>% 
          rename( date = data.date, 
                  returns = data.returns )  %>% 
          left_join( marketProxyReturns, by="date" ) %>% 
          do( p.returns = ggplot(., aes( x=market.returns, y=returns) ) + 
                          geom_point( ) +
                          geom_smooth(method = "lm", se = FALSE, color = "green", size = .5) + 
                          ggtitle(glue::glue("{.$symbol} Scatterplot of Portfolio returns v. market Returns" ) )) )

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
