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
  stockTbbl <- 
    stockTbbl %>% 
    select(close) %>% 
    pull() %>% 
    discreteFirstOrder() %>% 
    mutate( stockTbbl, first.order= .)
  
  stockTbbl <- 
    stockTbbl %>% 
    select(close) %>% 
    pull() %>% 
    discreteSecondOrder() %>% 
    mutate( stockTbbl, second.order= .)
  #return(minMax) 
#}

#findValley <- function( stocktTbbl ){
#  return( findPeakValley( stockTbbl, type=1 ) )
#}

#findPeak <- function( stocktTbbl ){
#  return( findPeakValley( stockTbbl, type=-1 ) )
#}

chart.BAR <- function( stockTbbl, plotTittle="BAR chart V-0.01" ){ 
 
  trendLines <- 
    stockTbbl %>% 
    select(close) %>% 
    pull() %>% 
    discreteFirstOrder() %>% 
    diff()
  trendLines <- 
    c(NA, trendLines)
  
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
      geom_point(data=trend, aes( x=date, y=close ), size=2 ) +
      geom_line( data= connectLine, aes( x=date, y = close)) +
      scale_x_date( date_breaks = '1 month', 
                    date_labels = "%b",
                    minor_breaks = '2 weeks' ) +
      labs( title=plotTittle) +
      theme_gray() 
    return(g1) 
}
