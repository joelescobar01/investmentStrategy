library( tidyquant )
library( tidyverse )
library(rvest) 


sp500 <- 
  yahoo.Stock.Prices(c("XLC", "XLY", "XLP","XLE", "XLF", "XLV", "XLI", "XLB", "XLRE", "XLK", "XLU", "SPY")) %>% 
  group_by(symbol) %>% 
  tq_transmute( select=adjusted, mutate_fun=periodReturn, period="monthly") %>% 
  group_by(symbol, year=year(date) ) %>% 
  ggplot( aes(x=date, colour=symbol) ) + 
  geom_line( aes(y=monthly.returns, linetype=factor(symbol)), size=2 ) + 
  scale.date.axis.small() + 
  scale.price.axis() + 
  max.plot.space() +  
  theme(legend.position = "bottom") + 
  guides(linetype="none") + 
  facet_grid( . ~ symbol, scale="free" ) 
