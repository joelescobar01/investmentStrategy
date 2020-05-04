library(TTR)
source("lib/utils.R")
source("var/settings.R")
library(devtools)
devtools::install_github("business-science/tidyquant")


stockBBands <- function( stock, sma=13, stdeviation=2 ){
    #where it is greater 1 if it is above the upper band, and less than 0 when it is below the lower band
    bb <- BBands( Cl( stock ), n=sma, sd=stdeviation ) 
    return( na.omit(bb)) 
}

get.BBAND <- function(stockTbbl, lookbackDays=2, volatility=2, type="SMA" ){
  #BBands(HLC, _n = 2, _maType="SMA", _sd = 2, ...)
  stockBBandTbbl <- 
    stockTbbl %>%
      tq_transmute( select = adjusted, 
                    mutate_fun = BBands, 
                    n = lookbackDays, 
                    sd = volatility,
                    maType=type) %>%
      mutate(symbol= stockTbbl$symbol%>%first()) %>%
      drop_na() 

  return( stockBBandTbbl )  
}

bband.Interface <- function( stockTbbl, startDate=NA, 
                           endDate=NA ){
  stock.BBAND.Tb <- 
    stockTbbl %>% 
    get.BBAND()  

  if( !is.na(startDate) ){
    stock.BBAND.Tb <-
      stock.BBAND.Tb %>% 
      filter( date >= startDate )
  }
  
  if( !is.na(endDate) ){
    stock.BBAND.Tb <-
      stock.BBAND.Tb %>% 
      filter( date <= endDate )
  }
  
  return( stock.BBAND.Tb )
}
get.DBBAND <- function(stockTbbl, lookbackDays=2, 
                       fvolatility=1, svolatility=2,
                       type="SMA" ){
  #BBands(HLC, _n = 2, _maType="SMA", _sd = 2, ...)
  stockBBandTbbl <- 
    stockTbbl %>%
      group_by(symbol) %>% 
      tq_transmute( select = adjusted, 
                    mutate_fun = BBands, 
                    n = lookbackDays, 
                    sd = fvolatility,
                    maType=type) 
      tempBbands <- 
        stockTbbl %>% 
        group_by(symbol) %>% 
        tq_transmute( select = adjusted, 
                        mutate_fun = BBands, 
                        n = lookbackDays, 
                        sd = svolatility,
                        maType=type)
    
      stockBBandTbbl$Sdn <- tempBbands$dn 
      stockBBandTbbl$Sup <- tempBbands$up 

      stockBBandTbbl <- 
        stockBBandTbbl %>% 
        drop_na() 

  return( stockBBandTbbl )  
}

aboveUpBand <- function( bbandDF ){
    aboveUpDates <- bbandDF[ bbandDF$pctB > 1,'Date' ]
    return( aboveUpDates )
} 


belowDownBand <- function( bbandDF ){
    belowDownDates <- bbandDF[ bbandDF$pctB < 0,'Date' ]
    return( belowDownDates )
}

bbSetup <- function( BBandDF ){ 
    sellSignal <- belowDownBand(BBandDF)
    buySignal <- aboveUpBand(BBandDF)
    
    class(buySignal) <- "Date"
    class(sellSignal) <- "Date"
    dateList <- list( "buy" = buySignal, "sell" = sellSignal ) 
    return( dateList ) 
}

bbandBuyXValues <- function( buyDates, startdate ){
    buyXValues <- c()
    buydates <- buyDates[ buyDates > startdate ]
    if(length(buydates) == 0 )
        return(c())
    for( i in 1:length(buydates) ){
        buyXValues[i] <- businessDayCounter(startdate, buydates[i])
    }
    return( buyXValues )
}

bbandSellXValues <- function( sellDates, startdate ){
    sellXValues <- c() 
    selldates <- sellDates[ sellDates > startdate ]
    if(length(selldates) == 0 )
        return(c())
    for( i in 1:length(selldates) ){
        sellXValues[i] <- businessDayCounter( startdate, selldates[i])
    }
    return( sellXValues ) 
}

chart.BBAND <- function( bbandTbl, doubleBand=FALSE ){

  p <-  ggplot(bbandTbl ,aes(x=date)) + 
      geom_line(aes( y=mavg, x=as.Date(date), colour = "SMA"))+
      geom_ribbon(aes(ymin=dn, ymax=up, x=as.Date(date), 
                      fill = "2 SD"), alpha = 0.3) +
      scale_colour_manual("",values="blue")+
      scale_fill_manual("",values="red")+
      labs( x="Date", y="Price", title=bbandTbl %>% select(symbol) %>% first() ) +
      theme_gray()
  return(p)
}
