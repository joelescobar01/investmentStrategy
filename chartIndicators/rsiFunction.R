library(TTR)
source('visual.lib.R')
#source("lib/utils.R") #source("var/settings.R")

GetRSI <- function(stockTbbl, days=9  ){
  stockTbbl <- tryCatch({  
    stockTbbl %>%
      tq_mutate(select     = close, 
                mutate_fun = RSI,
                n=days,
                maType     = EMA)}, 
    error = function(e){
      stockTbbl %>% mutate( rsi = NA ) 
   })

  return(stockTbbl) 
}

RemoveOvervaluedRSI <- function( rsiTbbl, threshold=50 ){
  rsiTbbl <- 
    rsiTbbl %>%
    mutate( rsi.oversold = rsi < threshold ) 
  return(rsiTbbl)
}

signal.Buy.RSI <- function( stockRSI, symbol="RSI", 
                           nDays=5, overValueThreshold=50 ){
  
  signalFound <- tryCatch({
    stockRSI %>% 
      mutate( rsi.buy.signal = rsi < threshold ) 
    }, 
    error = function(e){
      mutate( rsi.buy.signal = FALSE ) 
    })
  return(signalFound )  
}

chart.RSI <- function( rsiTbbl, plotTitle="RSI Version 1.0",
                      overValueThreshold=70, overSoldThreshold=30, zoomDays=21){
  g1 <- 
    rsiTbbl %>% 
    drop_na() %>%
    ggplot( aes(x=date)) + 
        geom_line( aes(y=rsi, colour="rsi"), size=1 ) +
        geom_text(  x=nth(rsiTbbl$date,n=-1), 
                    y=nth(rsiTbbl$rsi,n=-1),
                    label="RSI", aes(colour="rsi"), 
                    position = position_nudge(y = -0.1),
                    size=5) + 
        geom_hline(aes( yintercept = overSoldThreshold, colour="Oversold" ), linetype="dashed") +
        geom_hline(yintercept = overValueThreshold , linetype="dashed" )+
        labs(title=plotTitle,
              y="Momentum Strenght/Weakness", 
              x="Date")+
        scale_colour_manual( 
          guide="none",
          values=c("blue", "red")
          ) +
        zoom.last_n( rsiTbbl, n=zoomDays ) +
        scale.date.axis() +
        max.plot.space() 

    return(g1)
}
