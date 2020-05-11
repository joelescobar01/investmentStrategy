

tbill3MDailyRate <- function(  ){
  tbillLogRate <- 
    tq_get( "DGS3MO", 
           get="economic.data" ) %>%
    drop_na() 
  #tbillLogRate <- tbillLogRate[!is.infinite(rowSums(tbillLogRate)),] 
  #tbillLogRate <- na.omit( tbillLogRate )
  
  #tbillLogRate <- 
  #  tail( tbillLogRate, n=92 )
  #colnames(tbillLogRate) <- c( "Rate")
  #tbillDF <- zooToDataFrame(tbillLogRate)
  #
  #return(tbillLogRate)
}

tbill3MDailyLogtRate <- function( ){
  tbillLogRate <- 
    getSymbols( "DGS3MO", 
                src='FRED', 
                auto.assign = FALSE ) %>% 
    dailyReturn(type='log')
  
  tbillLogRate <- tbillLogRate[!is.infinite(rowSums(tbillLogRate)),] 
  tbillLogRate <- na.omit( tbillLogRate )
  
  tbillLogRate <- 
    tail( tbillLogRate, n=92 )
  colnames(tbillLogRate) <- c( "Log.Returns")
  tbillDF <- convertStockToDataFrame(tbillLogRate)
  
  return(tbillLogRate)
}

week13Rates <- function( dailyRate ){
  dailyRate <- 
    tail( dailyRate, n=92 )
  colnames(dailyRate) <- c( "Rate")
  dailyRateDF <- zooToDataFrame( dailyRate)
  return(dailyRate) 
}

dailyRiskFreeRates <- function( tbillDailyRates ){
  #ep <- endpoints(prices, "months")
  dailyYield <- (1+(tbill/100))^(1/252) - 1
 # threeMoPrice <- cumprod(1+dailyYield)
  return(dailyYield)
#  threeMoPrice <- threeMoPrice["2019-12-16::"]
#  threeMoPrice <- threeMoPrice[endpoints(threeMoPrice, "days"),]
#  return(threeMoPrice)  
}


