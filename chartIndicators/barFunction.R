library( tidyquant )
library( ggplot2 ) 
library( tidyverse)
source("visual.lib.R")

findDiscreteSlope <- function( stockTbbl ){
  slopeTbbl <-
    stockTbbl %>% 
    mutate_at( c("open","high","low","close"), ~(lead(.) - .) ) %>% 
    select( date, symbol, open, high, low, close ) 
  return( slopeTbbl )
}

localMin <- function( stockTbbl ){
  localMin <- 
    stockTbbl %>% 
    mutate( low.point = pmin( close, open ) ) %>% 
    mutate( high.point = pmax(close,open) ) %>% 
    mutate( first.derivative = sign( lead( low.point ) - low.point ) ) %>%
    mutate( second.derivative = lead(first.derivative) - first.derivative   ) 
  return( localMin ) 
}

chart.BAR <- function( stockTbbl, plotTitle="BAR Graph Version 1.1", zoomDays=21 ){ 
 
  g1 <- 
    stockTbbl %>% 
      ggplot( aes(y=close, x=date) ) + 
      geom_barchart(aes(open = open, high = high, low = low, close = close)) + 
      geom_ma(color = "darkgreen") +
      labs( title=plotTitle, 
            y="Closing Price", 
            x="Date") +
        #zoom.last_n( stockTbbl, n=zoomDays ) +
        scale.date.axis() +
        scale.price.axis() 
        max.plot.space() 

 
    return(g1) 
}

chart.BAR.Daily <- function( stockTbbl, plotTitle="BAR Graph Version 1.0", zoomDays=21 ){ 
 
  g1 <- 
    stockTbbl %>% 
      ggplot( aes(y=close, x=date) ) + 
      geom_barchart(aes(open = open, high = high, low = low, close = close)) + 
      scale_x_date( date_breaks = '1 weeks', 
                    date_labels = "%b-%d",
                    minor_breaks = '1 days' ) +
      labs( title=plotTitle, 
            y="USD $", 
            x="Date") +
        coord_cartesian(xlim=c( 
                            nth(stockTbbl$date,n=1)+days(zoomDays), 
                            nth(stockTbbl$date,n=-1)) )+ 
      theme_gray() 
    return(g1) 
}

chart.Price.Daily <- function( stockTbbl ){ 
  g1 <- 
    stockTbbl %>% 
      ggplot( aes(x=date) ) + 
      geom_line( aes(y=( high-low) ), size=1)+
      scale_x_date( date_breaks = '1 weeks', 
                    date_labels = "%b-%d",
                    minor_breaks = '1 days' ) +
      labs( y="Volatility USD($)", 
            x="Date") +
      scale_colour_manual( 
          guide="none",
          values=c("blue", "red")
          )+
    theme_gray() 
    return(g1) 
}


chart.BAR2 <- function( stockTbbl, plotTitle=NA ){ 
  slopeTbbl <- 
    stockTbbl %>% 
    findDiscreteSlope() 

  localMin <- 
    stockTbbl %>% 
    findDiscreteMin()

  zeroSlope <- 
    slopeTbbl %>% 
    filter( close == 0.00 ) %>%
    select( date ) %>% 
    pull() 

  zeroSlope <- 
    stockTbbl %>% 
    filter( date == zeroSlope ) 

  print(localMin ) 

    g1 <- 
    stockTbbl %>% 
      ggplot( aes(y=close, x=date) ) + 
      geom_barchart(aes(open = open, high = high, low = low, close = close)) + 
      geom_ma(color = "darkgreen")  +
      geom_point(data=zeroSlope, aes( x=date, y=close ), color="red", alpha = 0.4, size=3 ) +
      geom_point(data=localMin, aes( x=date, y=close ), color="blue", alpha = 0.4, size=3 ) +
      scale_x_date( date_breaks = '1 month', 
                    date_labels = "%b",
                    minor_breaks = '2 weeks' ) +
      labs( title=plotTitle, 
            y="Closing Price", 
            x="Date") +
      theme_gray() 
    return(g1) 
}
