stocks <- 
  ticker %>% 
  slice( 375:400 ) %>% 
  pull() %>% 
  yahoo.fetch() %>% 
  group_by(symbol) %>% 
  mutate( adj.daily.returns = TTR::ROC( adjusted ) )

stockDeviation <- stocks %>% drop_na() %>% mutate( ticker=symbol ) %>%  group_by(symbol ) %>% group_map( ~ as_tibble_row(c( symbol=first(.x$ticker), deviation=as.numeric(sd( .x$adj.daily.returns )) ) ))
stockDeviation %>% pmap( ~c(...) ) %>% map( function(x) { dev = as.numeric( x["deviation"] )
                                          ticker = x["symbol"]
                                          stocks %>% filter( symbol == ticker & ( adj.daily.returns >= dev | adj.daily.returns <= (-1*dev) ) ) } )

sp500 <- yahoo.Stock.Prices(c("^GSPC") ) %>% mutate( symbol="SP500" ) %>% mutate( adj.daily.returns = TTR::ROC( adjusted ) )

stockReturnsOutliers <- stockDeviation %>% pmap( ~c(...) ) %>% map( function(x) { dev = as.numeric( x["deviation"] )
stockReturnsOutliers %>% mutate( ticker=symbol) %>%  group_by( symbol ) %>% group_map( ~  semi_join( sp500, .x, by="date" ) %>% mutate( comparisson = .x$ticker) )

stockReturnsOutliers %>% mutate( ticker=symbol) %>%  group_by( symbol ) %>% group_map( ~  semi_join( sp500, .x, by="date" ) %>% mutate( comparisson = .x$ticker) )

spReturns <- stockReturnsOutliers %>% mutate( ticker=symbol) %>%  group_by( symbol ) %>% group_map( ~  semi_join( sp500, .x, by="date" ) %>% mutate( daily.volatility = (close - open ),comparisson = .x$ticker) )

spReturns <- bind_rows( spReturns )

p1 <- spReturns %>% group_by( comparisson ) %>% ggplot( aes( x=date, colour=comparisson ) ) + geom_line( aes(y=adj.daily.returns ) ) + facet_wrap( .~comparisson, scale="free" ) + guides(colour="none") + ggtitle("SP500" )
p2 <- stockReturnsOutliers %>% group_by( symbol ) %>% ggplot( aes( x=date, colour=symbol ) ) + geom_line( aes(y=adj.daily.returns ) ) + facet_wrap( .~symbol, scale="free" ) + guides( colour="none" ) + ggtitle("Stocks")
