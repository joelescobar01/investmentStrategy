library(TTR)
source('visual.lib.R')
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
  symbol <- 
    stockTbbl %>% 
    select(symbol) %>% 
    first() %>% 
    pull() 

  stockRSITbbl <- 
    stockTbbl %>% 
    get.RSI() 

  return( chart.RSI( stockRSITbbl ) ) 
}

chart.RSI <- function( rsiTbbl, plotTitle=NA,
                      overValueThreshold=70, overSoldThreshold=30){
  textDate <- 
    rsiTbbl %>% 
    first(n=3) %>% 
    select(date) %>% 
    last() %>% 
    pull() 
  
  if( is.na( plotTitle ) ){
    symbol <- 
      rsiTbbl %>% first() %>% select(symbol) %>% pull() 
    plotTitle <- 
      rsiTbbl %>% 
      filter( row_number() == 1 | row_number() == n() ) %>% 
      select( date ) %>% 
      pull() %>% 
      paste( collapse=' - ' ) 
    plotTitle <- 
      paste( symbol, plotTitle, sep=": ") 
  }
  g1 <- ggplot( rsiTbbl, aes(x=date)) + 
        geom_line( aes(y=rsi, colour="rsi"), size=1 ) + 
        geom_hline(aes( yintercept = overSoldThreshold, colour="Oversold" ), linetype="dashed") +
        geom_text( x=textDate, aes(y=overSoldThreshold+2, label="Oversold" ) )+
        geom_hline(yintercept = 70 , linetype="dashed" )+
        geom_text( x=textDate, aes(y=overValueThreshold+2, label="Overvalued"), size=4)+
        scale_x_date( date_breaks = '1 month', 
                    date_labels = "%b",
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

