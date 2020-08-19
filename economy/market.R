source("var/economicIndicators.R") 
source("var/benchmarks.R")
source("visual.lib.R")
library(lubridate)
library( ggpubr )


market.Monthly.Return <- function( market=market.Return(), period="monthly" ){
  monthlyReturns <-
    market %>% 
    group_by(symbol) %>% 
    tq_transmute( select=adjusted, 
                  mutate_fun=periodReturn, 
                  period="monthly") 
  return( monthlyReturns ) 
}
# to log type (compound continous ) log( 1 + return ) 

