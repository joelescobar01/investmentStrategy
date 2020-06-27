library(tidyverse)
library(ggplot2)
library( ggpubr ) 

chart.Return.Dividends <- function( dividendYield ){
  p1 <- 
    dividendYield %>% 
    ggplot( aes(x=date, y=dividend) ) + 
    geom_point() + 
    geom_text(aes(label=glue::glue(" {format(dividend.yield*100, digit=2)}%")), 
                  hjust = 0, 
                  nudge_x = 0.006, 
                  angle=45, 
                  check_overlap=TRUE) + 
    geom_line() + 
    scale_y_continuous( labels=scales::dollar, 
                        breaks=scales::pretty_breaks(n=10)) + 
    scale_x_date( breaks=scales::pretty_breaks(n=10)) + 
    labs(x="", y="Dividends per Share", 
         title="Dividends per Share", 
         subtitle="Dividend with Closing Price Ratio Percent" )

  return( p1 ) 
}
