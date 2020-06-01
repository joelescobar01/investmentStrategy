library(TTR)
library(tidyverse)
library(tidyquant)
library(ggplot2)

zoom.last_n <- function( stockTbbl, n=14 ){
  zoom <- 
    coord_cartesian(xlim=c( 
                            nth(stockTbbl$date,n=-1)-days(n), 
                            nth(stockTbbl$date,n=-1)) 
                    )
  return( zoom ) 
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
  scale_x_date (  labels=scales::label_date_short(), 
                    date_breaks='1 months',
                    date_minor_breaks='5 days'  )
  
  return(sc) 
}

scale.price.axis <- function(){

  sc <- 
      scale_y_continuous(labels = scales::dollar_format() )
}
