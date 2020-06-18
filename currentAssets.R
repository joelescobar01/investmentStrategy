source("data.transfer.lib.R") 
source("analysis/portfolioAnalysis.R")

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

stock5 <- 
  tibble( 
      purchase.date= c( ymd( "2020/05/21" ), ymd("2020/06/05" ) ), 
      symbol="TK",
      qty=c( 13, 13 ),
      cost.per.share=c( 3.14, 2.92 ) 
      )


currentPortfolio <- 
  bind_rows( stock1, stock2 ) %>% 
  bind_rows( stock3 ) %>% 
  bind_rows( stock4 ) %>%
  select( symbol, everything() )


portfolio <- 
  currentPortfolio %>% 
  transmute( symbol, date = ymd( purchase.date ), close=cost.per.share  ) %>% 
  pmap_dfr( function( ... ) { 
             current <- tibble(...)
             stock_returns <-
                tq_get( current$symbol, get='stock.prices', from=ymd(current$date ) ) %>%
                select( symbol, date, close ) %>% 
                anti_join( current, by="date") %>% 
                union( current ) %>%
                arrange( date ) %>%
                tq_mutate( select=close, mutate_fun=periodReturn, period="daily", type="log" ) %>% 
                group_by( symbol ) %>% 
                nest() 
          }) 

portfolioBeta <- 
  portfolio %>% 
  pmap( ~ c(...) )  %>% 
  map( function(x) { 
      betaCAPM <-
      as_tibble(x) %>% 
      rename( date=data.date, daily.returns=data.daily.returns) %>% 
      select( symbol, date, daily.returns ) %>% 
      left_join( marketProxyReturns(nPeriod="daily"), by="date") %>% 
      tq_performance( Ra=daily.returns, Rb=market.returns, performance_fun=CAPM.beta) %>%
      mutate( symbol = x$symbol ) 
    }) %>% 
  map_df( ~ as_tibble(.) ) %>% 
  mutate( purchase.date = currentPortfolio$purchase.date ) 


portfolio %>% 
  pmap( ~ ggplot(..2, aes( x=market.returns, y=returns) ) + 
                          geom_point( ) +
                          geom_smooth(method = "lm", se = FALSE, color = "green", size = .5) + 
                          ggtitle(glue::glue("{.$symbol} Scatterplot of Portfolio returns v. market Returns" ) ))

assets <- 
  currentPortfolio %>% 
  select( symbol ) %>% 
  map_df( ~ fetch.latest.quote(.) %>% 
         select( symbol,last ) %>% 
         rename(close=last) ) %>% 
  left_join( currentPortfolio, by="symbol" ) %>% 
  rename( latest.close = close ) %>% 
  mutate( returns = latest.close*qty - cost.per.share*qty ) %>% 
  select( symbol, returns ) %>% 
  group_by( symbol ) %>% 
  summarize( return=sum(returns) ) 
