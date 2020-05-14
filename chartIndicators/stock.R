library(quantmod)

get.Stock.Tbbl <- function( stockSymbolChar, ... ){
  stockTbbl <- tq_get( toupper(stockSymbolChar),
                       get="stock.prices",
                       ... ) #NA generates when errors 
  return(stockTbbl) 
}

add.RSI.To.Stock <- function( stockTbbl, days=9 , ... ){
  if( !is_tibble( stockTbbl ) )
    return(NA)
  stockTbbl <- 
      stockTbbl %>% 
      tq_mutate( select = adjusted, 
                    mutate_fun = RSI,
                    n=days,
                    maType=EMA) 
    return(stockTbbl)
}