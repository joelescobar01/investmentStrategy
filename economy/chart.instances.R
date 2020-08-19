source("economy/chartEconomy.R")


credit.card.deliquency.rate <- 
  fred.Data(CREDIT.CARD.LOANS) %>% 
  mutate( percent = price/100 ) %>% 
  chart.Generic.Percent( cTitle="Credit Card Loans Deliquency Rate" ) 

commercial.loans <- 
  fred.Data(BUSINESS.LOANS ) %>% 
  chart.Generic.Price( cTitle = "Commercial and Industry Loans" ) 

used.auto.purchases <-
  fred.Data(REAL.USED.AUTO.PURCHASES) %>% 
  chart.Generic( cTitle="Real Used Auto Purchases" ) 

inflation.valued.money <- 
  ggarrange( 
    treasury.inflation.rate() %>% chart.Generic.Percent(cTitle="Treasury Bill Inflation Rate"), 
    chart.SP500.Log(),
    chart.Gold.Price(),
    chart.PPI.Price(), 
    nrow=4, ncol=1, 
    align="v" ) 
