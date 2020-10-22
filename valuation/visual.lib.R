library(ggplot2)
source("lib/visual.lib.R") 
solvencyGraph <- function( solvencyTbl, dependentVar ){
  gg <-
    solvencyTbl %>% 
    ggplot( aes_string( x=dependentVar, y="annual.returns" ) ) +
      geom_point( ) + 
      geom_text( aes( label=period), nudge_y=0.01 ) +
      facet_wrap( ~ symbol, scales="free" ) + 
      scale.price.axis() +
      max.plot.space() 
    return(gg)
}

solvencyGraphPercent <- function( solvencyTbl, dependentVar ){
  gg <-
    solvencyTbl %>% 
    ggplot( aes_string( x=dependentVar, y="annual.returns" ) ) +
      geom_point( ) + 
      geom_text( aes( label=period), nudge_y=0.01 ) +
      facet_wrap( ~ symbol, scales="free" ) + 
      scale.price.axis() +
      scale.percent.xaxis() +
      max.plot.space() 
    return(gg)
}
