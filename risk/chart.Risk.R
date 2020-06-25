chart.TimePeriodVolatility <- function( periodVolTbl ){
 p1 <- 
  periodVolTbl %>% 
  gather("Time.Period", "Volatility", -year ) %>% 
     ggplot( aes(x=year, y=Volatility, fill=Time.Period ) ) +
     geom_bar( position="dodge", stat="identity") + 
     scale_fill_discrete( labels=c("Annual", "Monthly", "Weekly") ) +
     theme() 
  return(p1) 
}


chart.PastCummulativeReturns <- function( returnTbbl ){
  
  #roE <- returnTbbl %>% first %>% select( cummalative.return ) %>% pull() 
  roE <- paste( "Cummulative Return ($USD)" ) 
  g1 <- 
    returnTbbl %>% 
    ggplot() + 
    geom_line( aes( x=date, y=cummalative.return) ) + 
    labs( x='Date', y=roE )+ 
    scale_x_date( breaks='5 days',
                  minor_breaks='1 days',
                  date_labels='%b-%d') +
    scale_y_continuous(position = "right") +
    theme() 

  return(g1) 
}
