library( tidyquant )
library( ggplot2 ) 
library( tidyverse)

localMax <- function(){
  tt <- 
    stocktTbbl %>% 
    select( close ) %>% 
    pull() 
  index <- 
    which(diff(tt) == 0 ) 
  
  index2 <- 
    which(diff(sign(diff(tt)))==-2)+2
  
  index <- 
    c(index, index2 ) 
  
  temp <- 
    stockTbbl %>% 
    slice( index )
  
  symbol <- 
    stockTbbl %>% 
    select(symbol) %>%
    pull() %>% 
    first() 
}

chart.BAR <- function( stockTbbl ){ 
  g1 <- 
    stockTbbl %>% 
      ggplot( aes(y=close, x=date) ) + 
      geom_barchart(aes(open = open, high = high, low = low, close = close)) + 
      geom_ma(color = "darkgreen")  + 
      scale_x_date( date_breaks = '1 month', minor_breaks = '1 weeks' ) +
      theme_gray() 
    return(g1) 
}
