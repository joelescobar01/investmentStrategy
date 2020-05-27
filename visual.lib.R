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

max.plot.space <- function(){
  max.plot <-
    theme(panel.border=element_blank(), 
          panel.spacing = unit(0, "cm"), 
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt") )

  return( max.plot ) 
}

scale.date.axis <- function(){
  sc <- 
    scale_x_date (  date_labels="%Y/%m", 
                    date_breaks='1 months', 
                    minor_breaks='2 weeks' )
  
  return(sc) 
}
