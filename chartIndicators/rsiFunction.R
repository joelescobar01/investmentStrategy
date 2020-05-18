library(TTR)
source('visual.lib.R')
#source("lib/utils.R") #source("var/settings.R")

rsi.Interface <- function( stockTbbl){
  stock.RSI.Tb <- 
    stockTbbl %>% 
    get.RSI() 
  
  return( stock.RSI.Tb )
}

GetRSI <- function(stockTbbl, days=9  ){
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

#Test 1
GetUptrendRSI <- function( rsiTbbl ){
  rsiTbbl <-
    rsiTbbl %>%
    mutate( rsiSlope = rsi - lag(rsi) ) %>%
    mutate( rsi.2.day.lag = lag(rsi) - lag( rsi,2) ) %>% 
    mutate( rsi.3.day.lag = lag(rsi,2 ) - lag(rsi, 3 ) )  
  
  rsiTbbl <-
    rsiTbbl %>% 
    mutate( uptrend.RSI = rsiSlope > 0 &
                          rsi.2.day.lag > 0 &
                          rsi.3.day.lag > 0 ) %>% 
    select( -rsiSlope, -rsi.2.day.lag, -rsi.3.day.lag )
  return(rsiTbbl )
}

RemoveOvervaluedRSI <- function( rsiTbbl, threshold=70 ){
  rsiTbbl <- 
    rsiTbbl %>% 
    filter( rsi < threshold ) 

  return(rsiTbbl)
}

VerifyRecentOvervaluedRSI <- function( rsiTbbl, nDays=7, threshold=70 ){
  rsiTbbl <- 
  rsiTbbl %>% 
    tail(n=nDays) %>% 
    filter( rsi > threshold )

  return( rsiTbbl ) 
}

signal.Buy.RSI <- function( stockTbbl, symbol="RSI", 
                           nDays=5, overValueThreshold=70 ){
  stockTbbl <-  
    stockTbbl %>% 
    GetRSI() 
  
  buyRSITbbl <-    
    stockTbbl %>% 
    filter( date >= Sys.Date() - days(nDays) ) %>%  
    filter( any( rsi < overValueThreshold ) )

  if( buyRSITbbl %>% tally() == 0  )
    return(NA)
  else {
    return( chart.RSI( stockTbbl, plotTitle=symbol) )
  }
}

chart.RSI <- function( rsiTbbl, plotTitle="RSI Version 1.0",
                      overValueThreshold=70, overSoldThreshold=30){
  g1 <- ggplot( rsiTbbl, aes(x=date)) + 
        geom_line( aes(y=rsi, colour="rsi"), size=1 ) +
        geom_text(  x=nth(rsiTbbl$date,n=-1), 
                    y=nth(rsiTbbl$rsi,n=-1),
                    label="RSI", aes(colour="rsi"), 
                    vjust=0, nudge_y=0.5,
                    size=5) + 
        geom_hline(aes( yintercept = overSoldThreshold, colour="Oversold" ), linetype="dashed") +
        geom_hline(yintercept = 70 , linetype="dashed" )+
        scale_x_date( date_breaks = '1 month', 
                    date_labels = "%b %Y",
                    minor_breaks = '2 weeks' ) +
        labs(title=plotTitle,
             caption="9 days, EMA",
              y="Momentum Strenght/Weakness", 
              x="Date")+
        scale_colour_manual( 
          guide="none",
          values=c("blue", "red")
          )+
        scale_y_continuous(position = "right") +
        theme(
          legend.position = c(0.1, 0.2),
          legend.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))

    return(g1)
}

