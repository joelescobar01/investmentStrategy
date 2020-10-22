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

scale.date.axis.small <- function(){
  sc <- 
  scale_x_date (  breaks=scales::breaks_width("1 months"), 
                  labels=scales::label_date_short() )
  
  return(sc) 
}

scale.date.axis.large <- function(){
  sc <- 
  scale_x_date (  breaks=scales::breaks_width("6 months"), 
                  labels=scales::label_date_short() )
  return(sc) 
}

scale.date.axis.yearly <- function(){
  sc <- 
  scale_x_date (  breaks=scales::breaks_width("1 years"), 
                  labels=scales::label_date("'%y") )
  return(sc) 
}

scale.price.axis <- function(){
  sc <- 
    scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_dollar() )
  return(sc)
}

scale.price.xaxis <- function(){
  sc <- 
    scale_x_continuous( breaks=scales::breaks_extended(8), labels=scales::label_dollar() )
  return(sc)
}


scale.percent.axis <- function(){
  sc <- 
    scale_y_continuous( breaks=scales::breaks_extended(16), labels=scales::label_percent() ) 
  return(sc)
}

scale.percent.xaxis <- function(){
  sc <- 
    scale_x_continuous( breaks=scales::breaks_extended(8), labels=scales::label_percent(  )  ) 
  return(sc)
}
