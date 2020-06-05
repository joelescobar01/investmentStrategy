
stock1 <- 
  tibble( 
      purchase.date=c( ymd("2020/03/20"), ymd("2020/03/20"), ymd("2020/03/27"), ymd("2020/03/30" ) ),       symbol="WMB", 
      qty = c( 8, 1, 1, 2 ), 
      cost.per.share=c(11.50, 11.78, 14.00, 13.55 ) )

stock2 <- 
  tibble( 
      purchase.date=ymd("2020/06/01"), 
      symbol="AMRN", 
      qty= 20, 
      cost.per.share=c(6.80) ) 

stock3 <- 
  tibble( 
      purchase.date=c(ymd("2020/03/27"), ymd("2020/04/01"), ymd("2020/04/01"), ymd("2020/04/20"), ymd("2020/05/18") ), 
      symbol="PAA", 
      qty = c( 10, 1, 5, 5, 1.026 ), 
      cost.per.share=c( 5.80, 4.95, 4.95, 7.00, 8.95 ) )

stock4 <- 
  tibble( 
      purchase.date= ymd( "2020/06/01" ),
      symbol="REPH",
      qty=40,
      cost.per.share=4.66 
      )


currentPortfolio <- 
  bind_rows( stock1, stock2 ) %>% 
  bind_rows( stock3 ) %>% 
  bind_rows( stock4 ) %>% 
  select( symbol, everything() ) 
