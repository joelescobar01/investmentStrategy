library(TTR)
source("lib/utils.R")
source("var/settings.R")

#Sys.setnev(TZ="UTC")

GetMomentum <- function( stockTbbl, period = 14 ){
  momTbbl <-
    stockTbbl %>% 
    mutate( momentum = volume - lag( volume, period ) ) 
    return( momTbbl ) 
}

chart.Momentum <- function( momentumTbbl,plotTitle="momentum Version 1.2",
                            zoomDays=21, factorValue=10000 ){
  momentumTbbl <- 
    momentumTbbl %>% 
    drop_na() 
  g1 <- ggplot( momentumTbbl, aes(x=date)) + 
        geom_line( aes(y=momentum/factorValue ), size=2 ) + 
        geom_hline( yintercept=0, linetype="dashed", color="yellow", size=2, alpha=0.5 ) + 
        labs(title=plotTitle, 
              y=glue::glue("Momentum (Factored by {factorValue})"), 
              x="Date") +
        zoom.last_n( momentumTbbl, zoomDays ) +
        scale.date.axis() + 
        max.plot.space() 
  return(g1)
}
