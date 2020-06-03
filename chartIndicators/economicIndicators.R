library(ggplot2)
source("data.transfer.lib.R") 
ConsumerPriceIndex <- 
  "CPIAUCSL"
RealGDP <- 
  "GDPC1"
UnemploymentRate <-
  "UNRATE"
UM.ConsumerSentiment <- 
  "UMCSENT" 


chart.EconomicData1 <- function(){
  
}

indicators <- fred.Data( c(RealGDP) ) %>% 
  rename( RealGDP = price ) %>% 
  select( -symbol ) 
indicators <- fred.Data( c(UnemploymentRate) ) %>% 
  group_by( date = floor_date( date, "quaterly" ) ) %>% 
  summarize( Unemployment = mean( price ) ) %>% 
  right_join( indicators, by="date" ) 
indicators <- fred.Data( c(ConsumerPriceIndex) ) %>% 
  group_by( date = floor_date( date, "quaterly" ) ) %>% 
  summarize( CPI = mean( price ) ) %>% 
  right_join( indicators, by="date" ) 
