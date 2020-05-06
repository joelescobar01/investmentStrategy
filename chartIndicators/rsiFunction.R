library(TTR)
#source("lib/utils.R")
#source("var/settings.R")

stockSmaRSI <- function( stockDF, pastDays=14 ){
    rsi <- 
        RSI(Cl(stockDF), 
            SMA, n=pastDays)
    return( rsi) 
}
stockEmaRSI <- function( stockDF, pastDays=14 ){
    
    rsi <- 
        RSI(Cl(stockDF), 
            EMA, 
                n=pastDays)
    return( rsi) 
}

rsiSetup <- function(rsiDataFrame){
    rsiSignal <- c() 
    for(i in 1:nrow(rsiDataFrame) ){
        if( rsiDataFrame$rsi[i] > rsiOverboughtConstant ){
            rsiSignal[i] = 1 
        } else if( rsiDataFrame$rsi[i] < rsiOversoldConstant ){
            rsiSignal[i] = -1 
        } else {
            rsiSignal[i] = 0 
        }
    }
    return( rsiSignal ) 
}

rsibuyxvalues <- function(buydates, startdate ){
    buyxvalues <- c()
    buydates <- buydates[ buydates > startdate ]
    for( i in 1:length(buydates) ){
        buyxvalues[i] <- businessdaycounter(startdate, buydates[i])
    }
    return( buyxvalues )
}

rsisellxvalues <- function(selldates, startdate ){
    sellxvalues <- c() 
    selldates <- selldates[ selldates > startdate ]
    for( i in 1:length(selldates) ){
        sellxvalues[i] <- businessdaycounter( startdate, selldates[i])
    }
    return( sellxvalues ) 
}


rsi.Interface <- function( stockTbbl){
  stock.RSI.Tb <- 
    stockTbbl %>% 
    get.RSI() 
  
  return( stock.RSI.Tb )
}


get.RSI <- function(stockTbbl, days=9  ){
  stockRSITbbl <- 
    stockTbbl %>%
      tq_mutate(select     = adjusted, 
                mutate_fun = RSI,
                n=days,
                maType     = EMA) %>%
     drop_na() 

  return(stockRSITbbl) 
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

chart.RSI <- function( rsiTbbl, plotTitle="rsi Version 1.1" ){
  
  startDate <- 
    rsiTbbl %>% 
    select(date) %>% 
    first(n=3) %>%
    last() %>% 
    pull() 
   g1 <- ggplot( rsiTbbl, aes(x=date)) + 
        geom_line( aes(y=rsi, colour="rsi"), size=1 ) + 
        geom_hline(aes( yintercept = 30, colour="Oversold" ), linetype="dashed") +
        geom_text(x=startDate, aes(y=32, label="Oversold"), size=4)+
        geom_hline(yintercept = 70 , linetype="dashed" )+
        geom_text(x=startDate, aes(y=72, label="Overvalued"), size=4)+
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
          legend.title = element_blank() )

    # Note that, the argument legend.position can be also a numeric vector c(x,y). 
    #In this case it is possible to position the legend inside the plotting area. x and y 
    #are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) 
    #corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
    return(g1)
}
