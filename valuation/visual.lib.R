library(ggplot2)
source("lib/visual.lib.R") 

solvencyGraph <- function( solvencyTbl, dependentVar, title="model" ){
  gg <-
    solvencyTbl %>% 
    ggplot( aes_string( x=dependentVar, y="annual.returns" ) ) +
      geom_point( ) + 
      geom_text( aes( label=period), nudge_y=0.01 ) +
      facet_wrap( ~ symbol, scales="free" ) + 
      scale.price.axis() +
      max.plot.space() +
      labs(y="Annual Adjusted Returns" ) + 
      ggtitle( title ) 
    return(gg)
}

solvencyGraph2 <- function( solvencyTbl, dependentVar, title="model" ){
  gg <-
    solvencyTbl %>%
    mutate( label = paste(period, symbol, sep="_") ) %>%  
    ggplot( aes_string( x=dependentVar, 
                        y="annual.returns", 
                        col=quote(label) ) )  +
      geom_point( ) + 
      geom_text( aes( label=label), nudge_y=0.04 ) + 
      scale.price.axis() +
      max.plot.space() + 
      guides(colour="none") + 
      labs(y="Annual Adjusted Returns" ) + 
      ggtitle( title ) 
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
      labs(y="Annual Adjusted Returns" ) + 
      max.plot.space() 
    return(gg)
}

solvencyGraphPercent2 <- function( solvencyTbl, dependentVar, title="model" ){
  gg <-
    solvencyTbl %>%
    mutate( label = paste(period, symbol, sep="_") ) %>%  
    ggplot( aes_string( x=dependentVar, 
                        y="annual.returns", 
                        col=quote(label) ) )  +
      geom_point( ) + 
      geom_text( aes( label=label), nudge_y=0.04 ) + 
      scale.price.axis() +
      scale.percent.xaxis() +
      max.plot.space() + 
      guides(colour="none") + 
      labs(y="Annual Adjusted Returns" ) + 
      ggtitle( title ) 
    return(gg)
}
