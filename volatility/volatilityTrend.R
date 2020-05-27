library( tidyverse )
library( tidyquant ) 
source("visual.lib.R")

OpenCloseCycles <- function( stockTbbl ) {
  
  minPrice <- 
    stockTbbl %>% 
    transmute( minClOp = pmin( close, open ) ) %>% 
    pull() 

  maxPrice <-
    stockTbbl %>% 
    transmute( maxClOp = pmax( close,open) ) %>% 
    pull()

  priceChange <- 
    maxPrice - minPrice 
  
  group_id <- c(1) 
  idValue <- 1 

  currentVal <- 
    c( cumsum( priceChange[1] ) ) 
  for( ii in 2:length( priceChange ) ){
    if( currentVal[ii-1] > maxPrice[ii] ){
      idValue <- idValue + 1 
      group_id <- c( group_id, idValue ) 
      currentVal[ii] <- 
        cumsum( priceChange[ii] )
    } else {
      group_id <- c(group_id, idValue ) 
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + priceChange[ii] )
    }
  }

  stockCycle <- 
    stockTbbl %>% 
    mutate( OC.volatility.slope = currentVal, OC.slope.id = group_id ) 

  return( stockCycle ) 
}

HighLowCycles <- function( stockTbbl ) {
  
  minPrice <- 
    stockTbbl$low 

  maxPrice <-
    stockTbbl$high
  
  priceChange <- 
    maxPrice - minPrice 
  
  HL.group.id <- c(1) 
  idValue <- 1 

  currentVal <- 
    c( cumsum( priceChange[1] ) ) 
  for( ii in 2:length( priceChange ) ){
    if( currentVal[ii-1] > maxPrice[ii] ){
      idValue <- idValue + 1 
      HL.group.id <- c( HL.group.id, idValue ) 
      currentVal[ii] <- 
        cumsum( priceChange[ii] )
    } else {
      HL.group.id <- c( HL.group.id, idValue ) 
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + priceChange[ii] )
    }
  }

  stockCycle <- 
    stockTbbl %>% 
    mutate( HL.volatility.slope = currentVal, HL.slope.id = HL.group.id ) 

  return( stockCycle ) 
}

CloseCloseCycles <- function( stockTbbl ) {
  
  stockTbbl <- 
    stockTbbl %>% 
    tq_mutate(  mutate_fun=volatility, 
                col_rename="volatility" ) %>% 
    drop_na() 

  priceChange <- 
    stockTbbl %>% 
    select( "volatility" ) %>% 
    pull() 
  
  maxPrice <-
    stockTbbl %>% 
    select( close ) %>% 
    pull()

  CC.group.id <- c(1) 
  idValue <- 1 
 
  currentVal <- 
    c( priceChange[1] ) 
  
  for( ii in 2:length( priceChange ) ){
    if( currentVal[ii-1] > maxPrice[ii] ){
      idValue <- idValue + 1 
      CC.group.id <- c( CC.group.id, idValue ) 
      currentVal[ii] <- 
        cumsum( priceChange[ii] )
    } else {
      CC.group.id <- c( CC.group.id, idValue ) 
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + priceChange[ii] )
    }
  }

  stockCycle <- 
    stockTbbl %>%
    mutate( CC.volatility.slope = currentVal, CC.slope.id = CC.group.id ) %>% 
    select( -volatility ) 


  return( stockCycle ) 
}

chart.OpenCloseCycles <- function( stockTbbl ){
  volatilityStock <- 
    stockTbbl %>% 
    OpenCloseCycles()

  breakPoints <- 
    volatilityStock %>% 
    filter( lag(OC.slope.id ) < OC.slope.id ) %>% 
    select( date ) %>% 
    pull() 
    
  p <- 
    volatilityStock %>% 
    ggplot( aes( x=date ) ) + 
    geom_line( aes( y=close ) ) +
    geom_vline( xintercept=breakPoints, color='red', linetype='dashed' )

  return( p ) 

}


chart.OpClHiLoCycles <- function( stockTbbl ){
  volatilityStock <- 
    stockTbbl %>% 
    OpenCloseCycles() %>% 
    HighLowCycles() 

  breakPoints <- 
    volatilityStock %>% 
    filter( lag(OC.slope.id ) < OC.slope.id ) %>% 
    select( date ) %>% 
    pull() 
  
  breakPoints.HL <- 
    volatilityStock %>% 
    filter( lag(HL.slope.id ) < HL.slope.id ) %>% 
    select( date ) %>% 
    pull() 
   
  p <- 
    volatilityStock %>% 
    ggplot( aes( x=date ) ) + 
    geom_line( aes( y=close ) ) +
    geom_vline( xintercept=breakPoints, color='red', linetype='dashed' ) + 
    geom_vline( xintercept=breakPoints.HL, color='blue', linetype='dashed' ) +
    scale.date.axis() + 
    scale_y_continuous( labels=scales::dollar, 
                        breaks=seq( min(stockTbbl$close), 
                                    max(stockTbbl$close), 
                                    by=5.00 ) ) + 
    coord_cartesian( xlim=c( nth(stockTbbl$date,n=15), nth( stockTbbl$date,n=-1 ) ) ) +
    labs( caption="Red: OpenClose, Blue: HighLow", title="Volatility Slope" ) + 
    max.plot.space()  
    
  return( p ) 
}

chart.ClClCycles <- function( stockTbbl ){
  volatilityStock <- 
    stockTbbl %>% 
    CloseCloseCycles  

  breakPoints <- 
    volatilityStock %>% 
    filter( lag(CC.slope.id ) < CC.slope.id ) %>% 
    select( date ) %>% 
    pull() 
  
  p <- 
    volatilityStock %>% 
    ggplot( aes( x=date ) ) + 
    geom_line( aes( y=close ) ) +
    geom_vline( xintercept=breakPoints, color='red', linetype='dashed' ) + 
    scale.date.axis() + 
    scale_y_continuous( labels=scales::dollar, 
                        breaks=seq( min(stockTbbl$close), 
                                    max(stockTbbl$close), 
                                    by= (max(stockTbbl$close)-min(stockTbbl$close))/5  ) ) + 
    coord_cartesian( xlim=c( nth(stockTbbl$date,n=15), nth( stockTbbl$date,n=-1 ) ) ) +
    labs( caption="Red: CloseClose", title="Volatility Slope" ) + 
    max.plot.space()  
    
  return( p ) 
}


chart.ClHiCycles <- function( stockTbbl ){
  volatilityStock <- 
    stockTbbl %>% 
    OpenCloseCycles() %>% 
    HighLowCycles() 

  breakPoints <- 
    volatilityStock %>% 
    filter( lag(OC.slope.id ) < OC.slope.id ) %>% 
    select( date ) %>% 
    pull() 
  
  breakPoints.HL <- 
    volatilityStock %>% 
    filter( lag(HL.slope.id ) < HL.slope.id ) %>% 
    select( date ) %>% 
    pull() 
   
  p <- 
    volatilityStock %>% 
    ggplot( aes( x=date ) ) + 
    geom_line( aes( y=close ) ) +
    geom_vline( xintercept=breakPoints, color='red', linetype='dashed' ) + 
    geom_vline( xintercept=breakPoints.HL, color='blue', linetype='dashed' ) +
    scale.date.axis() + 
    scale_y_continuous( labels=scales::dollar, 
                        breaks=seq( min(stockTbbl$close), 
                                    max(stockTbbl$close), 
                                    by=5.00 ) ) + 
    coord_cartesian( xlim=c( nth(stockTbbl$date,n=15), nth( stockTbbl$date,n=-1 ) ) ) +
    labs( caption="Red: OpenClose, Blue: HighLow", title="Volatility Slope" ) + 
    max.plot.space()  
    
  return( p ) 
}
