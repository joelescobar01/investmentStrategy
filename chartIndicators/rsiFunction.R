library(TTR)
source('visual.lib.R')
#source("lib/utils.R") #source("var/settings.R")

rsi.Interface <- function( stockTbbl){
  stock.RSI.Tb <- 
    stockTbbl %>% 
    get.RSI() 
  
  return( stock.RSI.Tb )
}


get.RSI <- function(stockTbbl, days=9  ){
  if( !is_tibble( stockTbbl ) )
    return(NA)
  
  stockTbbl <- 
    stockTbbl %>%
      tq_mutate(select     = adjusted, 
                mutate_fun = RSI,
                n=days,
                maType     = EMA) %>%
     drop_na() 

  return(stockTbbl) 
}

get.Uptrend.RSI <- function( rsiTbbl ){
  positiveSlope <- 
    rsiTbbl %>% 
    select(rsi) %>% 
    pull() %>% 
    diff() 
  
  rsiTbbl <-
    rsiTbbl %>%
    mutate( uptrendRSI = c(0, positiveSlope ) ) %>% 
    filter( uptrendRSI > 0 ) 

  return(rsiTbbl )
    
}

remove.Overvalued.RSI <- function( rsiTbbl, threshold=70 ){
  rsiTbbl <- 
    rsiTbbl %>% 
    filter( rsi < threshold ) 

  return(rsiTbbl)
}

signal.Buy.RSI <- function( stockTbbl ){
  buyRSITbbl <-    
    stockTbbl %>% 
    get.RSI() %>%
    remove.Overvalued.RSI() %>%
    get.Uptrend.RSI()
 
  if( buyRSITbbl %>% tally() == 0  )
    return(NA)
  else {
    g1 <- 
      buyRSITbbl %>% 
      chart.RSI()
    return(g1)
  }
}
chart.Stock.Rsi <- function( stockTbbl ){
  stockRSITbbl <- 
    stockTbbl %>% 
    get.RSI() 

  return( chart.RSI( stockRSITbbl ) ) 
}

chart.RSI <- function( rsiTbbl, plotTitle="RSI Version 1.0",
                      overValueThreshold=70, overSoldThreshold=30){
  g1 <- ggplot( rsiTbbl, aes(x=date)) + 
        geom_line( aes(y=rsi, colour="rsi"), size=1 ) + 
        geom_hline(aes( yintercept = overSoldThreshold, colour="Oversold" ), linetype="dashed") +
        geom_hline(yintercept = 70 , linetype="dashed" )+
        scale_x_date( date_breaks = '1 month', 
                    date_labels = "%b %Y",
                    minor_breaks = '2 weeks' ) +
        labs(title=plotTitle,
             caption="9 days, EMA",
              y="", 
              x="Date")+
        scale_colour_manual( 
          guide="none",
          values=c("blue", "red")
          )+
        theme(
          legend.position = c(0.1, 0.2),
          legend.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))

    return(g1)
}

