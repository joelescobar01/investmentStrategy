library(TTR)
library(tidyverse)
library(tidyquant)
library(ggplot2)


chart.Default.Primitive <- function( tsTibbleObj ){
  g1 <- ggplot( tsTibbleObj, aes(x=date)) 
  return(g1) 
}

chart.Default.Scale <- function( gg.Plot ){
  gg.Plot <-
    gg.Plot + scale_x_date( date_labels= "%m/%d/%y",
                            date_breaks = "1 month",
                            date_minor_breaks = "1 week" ) + 
    scale_y_continuous( expand=c(0,0) )
  return(gg.Plot)
}
