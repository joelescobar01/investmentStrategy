library( tidyquant )
library( ggplot2 ) 
library( tidyverse)

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
      scale_x_date( date_breaks = '1 month', 
                    date_labels = "%b-%d",
                    minor_breaks = '2 weeks' ) +
      labs( title=plotTitle, 
            y="Closing Price", 
            x="Date") +
      coord_cartesian(xlim=c( 
                            nth(stockTbbl$date,n=-1)-days(zoomDays), 
                            nth(stockTbbl$date,n=-1)) )+ 
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



chart.BAR.RSI <- function( stockTbbl, plotTitle=NA ){ 
 
  if( !any( 'rsi' %in% names( stockTbbl ) ) )
    return(NA) 

  trendLines <- 
    stockTbbl %>% 
    select(close) %>% 
    pull() %>% 
    discreteFirstOrder() %>% 
    diff()
  trendLines <- 
    c(NA, trendLines)
  
  if( is.na( plotTitle ) ){
    symbol <- 
      stockTbbl %>% first() %>% select(symbol) %>% pull() 
    plotTitle <- 
      stockTbbl %>% 
      filter( row_number() == 1 | row_number() == n() ) %>% 
      select( date ) %>% 
      pull() %>% 
      paste( collapse=' - ' ) 
    plotTitle <- 
      paste( symbol, plotTitle, sep=": ") 
  }

  trend <- 
    stockTbbl %>% 
    mutate(min.max = trendLines) %>% 
    filter( min.max == 2 ) %>% 
    select( date,close )

  g1 <- 
    stockTbbl %>% 
      ggplot( aes(y=close, x=date) ) + 
      geom_barchart(aes(open = open, high = high, low = low, close = close)) + 
      geom_ma(color = "darkgreen")  +
      geom_line( aes( y=rsi) ) + 
      scale_x_date( date_breaks = '1 month', 
                    date_labels = "%b",
                    minor_breaks = '2 weeks' ) +
      scale_y_continuous(
        
        # Features of the first axis
        name = "Temperature (Celsius °)",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~., name="Price ($)")
      ) + 
      labs( title=plotTitle, 
            x="Date") +
      theme_gray() 
    return(g1) 
}

