library( tidyquant )
library( ggplot2 ) 
library( tidyverse)

chart.BAR <- function( stockTbbl, from, to ){ 
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

  g1 <- 
    stockTbbl %>% 
      ggplot( aes(y=close, x=date) ) + 
      geom_barchart(aes(open = open, high = high, low = low, close = close)) + 
      geom_ma(color = "darkgreen")  + 
      geom_point(data=temp, mapping= aes(x=date, y=close), size=2) +
      theme_gray() 
    return(g1) 
}
