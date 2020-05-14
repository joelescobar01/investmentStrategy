library( tidyquant )
library( ggplot2 ) 
library( tidyverse)

discreteSecondOrder <- function ( consecutiveVal ){
  
  oneDayAgo <- NULL 
  twoDayAgo <- NULL 
  derivative <- c( NA, NA )
  for(ii in seq_along(consecutiveVal) ){
    if( ii == 1 ){
      next()
    }
    if( ii == 2 ){
      oneDayAgo = consecutiveVal[ii]
      twoDayAgo = consecutiveVal[ii-1]
      next() 
    }
    currentDay <- consecutiveVal[ii]
    derivative[ii] <- sign(( currentDay - 2*oneDayAgo + twoDayAgo )/2 )
    twoDayAgo = oneDayAgo 
    oneDayAgo = currentDay 
  }

  return(derivative) 
}

discreteFirstOrder <- function( consecutiveVal ){
  oneDayAgo <- NULL 
  derivative <- c(NA) 
  for(ii in seq_along(consecutiveVal)){
    if( ii == 1 ){
      oneDayAgo = consecutiveVal[ii] 
      next() 
    }

    currentDay <- consecutiveVal[ii]
    derivative[ii] <- sign( currentDay - oneDayAgo  ) 
    oneDayAgo = currentDay
  }

  return(derivative)
}

#findPeakValley <- function( stocktTbbl, type=1 ){
  #type 1 for local min
  #type -1 for local max 
#  stockTbbl <- 
#    stockTbbl %>% 
#    select(close) %>% 
#    pull() %>% 
#    discreteFirstOrder() %>% 
#    mutate( stockTbbl, first.order= .)
  
#  stockTbbl <- 
#    stockTbbl %>% 
#    select(close) %>% 
#    pull() %>% 
#    discreteSecondOrder() %>% 
#    mutate( stockTbbl, second.order= .)
  #return(minMax) 
#}

#findValley <- function( stocktTbbl ){
#  return( findPeakValley( stockTbbl, type=1 ) )
#}

#findPeak <- function( stocktTbbl ){
#  return( findPeakValley( stockTbbl, type=-1 ) )
#}

chart.BAR <- function( stockTbbl, plotTitle=NA ){ 
 
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
      geom_point(data=trend, aes( x=date, y=close ), alpha = 0.4, size=2 ) +
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

