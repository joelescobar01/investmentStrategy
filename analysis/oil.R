source("data.transfer.lib.R") 


WTI <-
  "DCOILWTICO" 
BRENT <-
  "POILBREUSDM" 
NGL <-
  "DHHNGSP" 


oilIndicators <- function(){
  p1 <- 
    fred.Data( c( WTI, BRENT ) ) %>% 
    ggplot( aes(x=date, colour=symbol)) + 
    geom_line( aes(y=price), size=1  ) + 
    geom_line( aes(y=price), size=1 )  + 
    scale_y_continuous(sec.axis = dup_axis(), 
                       labels=scales::label_dollar(), 
                       minor_breaks=scales::breaks_extended(16)) + 
    scale_x_date( breaks=scales::breaks_width("1 years" ), 
                  minor_breaks="3 months", 
                  labels=scales::label_date_short() ) + 
    labs(y="US Dollar per Barrel " ) + 
    scale_colour_discrete( labels=c("WTI", "BRENT") ) + 
    theme( legend.position="bottom" )

  return( p1 ) 
}
