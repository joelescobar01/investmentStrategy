library(TTR)
source('visual.lib.R')
#source("lib/utils.R") #source("var/settings.R")

GetRSI <- function(stockTbbl, days=9  ){
  stockTbbl <- tryCatch({  
    stockTbbl %>%
      tq_mutate(select     = close, 
                mutate_fun = RSI,
                n=days,
                maType     = EMA) %>%
     drop_na()}, 
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
        coord_cartesian(xlim=c( 
                            nth(rsiTbbl$date,n=1)+days(zoomDays), 
                            nth(rsiTbbl$date,n=-1)) )+ 
        theme(
          legend.position = c(0.1, 0.2),
          legend.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))

    return(g1)
}

chart.RSI.Group <- function( rsiTbbl, plotTitle="RSI Version 1.0",
                      overValueThreshold=70, overSoldThreshold=30, zoomDays=21){
  g1 <- ggplot( rsiTbbl, aes(x=date, colour=group)) + 
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
        coord_cartesian(xlim=c( 
                            nth(rsiTbbl$date,n=1)+days(zoomDays), 
                            nth(rsiTbbl$date,n=-1)) )+ 
        theme(
          legend.position = c(0.1, 0.2),
          legend.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        facet_wrap( ~ symbol ) 

    return(g1)
}
