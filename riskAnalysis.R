require(quantmod)
require(PerformanceAnalytics)
source("utils.R")
require(forecast)

# width="600px" border="0" cellspacing="0" cellpadding="0" bgcolor="#336699"

stockReturns <- function( stockClosePriceXts, stockOpenPriceXts ){
  stockReturnDF <- zooToDataFrame((stockClosePriceXts - stockOpenPriceXts) ) 
  maxLoss <- min( stockReturnDF$Close )
  returnMean <- mean(stockReturnDF$Close, na.rm = TRUE)
  deviation <- sd( stockReturnDF$Close, na.rm= TRUE )
  vLines <- c( rev( seq(from=-deviation, to=-deviation*4, by=-deviation)), 
               seq(from=deviation, to=deviation*5, by=deviation ) )
  outLiersDate <- stockReturnDF[which( stockReturnDF$Close < deviation*-4 ), c("Date") ]
  outLiersDate <- paste(outLiersDate ," ", collapse= '', sep=" ")
  #All hypothesis tests ultimately use a p-value to weigh the strength of the evidence (what the data are telling you about the population).
  #The p-value is a number between 0 and 1 and interpreted in the following way:
  #A small p-value (typically ≤ 0.05) indicates strong evidence against the null hypothesis, so you reject the null hypothesis.
  #A large p-value (> 0.05) indicates weak evidence against the null hypothesis, so you fail to reject the null hypothesis.
  #p-values very close to the cutoff (0.05) are considered to be marginal (could go either way). Always report the p-value so your readers can draw their own conclusions.
  normalityTest <- shapiro.test(dnorm( stockReturnDF$Close, mean = returnMean, sd = deviation))
  p1 <- ggplot( stockReturnDF, aes(x=Close) ) +
          geom_histogram(aes(y = ..density..),binwidth=0.75)+
          geom_vline(xintercept = vLines, 
               size = 1, 
               colour = "#00FF00",
               linetype = "dashed") +
          geom_vline(xintercept = returnMean, 
               size = 1, 
               colour = "blue",
               linetype = "dashed") +
          scale_x_continuous(name = "Returns (Cl-Op)")+
            stat_function(fun = dnorm, colour = "red",
                          args = list(mean = mean(stockReturnDF$Close, na.rm = TRUE),
                          sd = sd(stockReturnDF$Close, na.rm = TRUE))) +
            labs( title=paste( "Returns from: ", first( stockReturnDF$Date ), " - ", last(stockReturnDF$Date), sep="" ),
                  subtitle=paste("Standard Deviation: +- $", deviation, sep=""),
                  caption= paste("Date of Outliers: ", outLiersDate , sep="") )
  return(p1)
}

pricePrediction <- function( stockQuartersXts ){
  autoplot(stockQuartersXts)+geom_point()+
    ylab("Stock Prices")+ggtitle("Quarterly Stock Prices")+xlab("Quarterly") 
  df_new <- data.frame(close = coredata( Cl(stockQuartersXts)),
                       time = time(stockQuartersXts))
  
    ggplot(df_new)+geom_point(aes(x=time,y=CAT.Close))+
      geom_line(aes(x=time,y=CAT.Close))+
      ylab("Ventas")+
      ggtitle("Ventas Trimestrales APPLE")+
      xlab("Trimestres")
    
    #Select number of observation to compare forecast
    cOmit=3
    
    #Data Size
    nObs=length(stockQuartersXts)
    
    #sub_sample
    oVentas <- window(stockQuartersXts,start=index(stockQuartersXts[1]),end=index(stockQuartersXts[nObs-cOmit]))
    
    #out sample (real data to forecast performance)
    stockQuartersXts <- window(stockQuartersXts,start=index(stockQuartersXts[nObs-cOmit+1]),end=index(stockQuartersXts[nObs]))
    
    #Forecast= last data
    fKOnaive <- naive(as.ts(stockQuartersXts),h=cOmit)
    autoplot(stockQuartersXts) + geom_forecast(fKOnaive,alpha=0.4)+ggtitle("Forecast Naive")
    
    
}


# tbillInterestRate <- getSymbols( "DGS3MO", src='FRED', auto.assign = FALSE )
# 
# dailyYield <- (1+(tbillInterestRate[,1]/100))^(1/252) - 1
# quarterYield <- to.quarterly(dailyYield)
# 
# endpoints(x, on="months", k=1)
# 
# getSymbols("^IRX", from="1990-01-01")
# dailyYield <- (1+(Cl(IRX)/100))^(1/252) - 1
# threeMoPrice <- cumprod(1+dailyYield)
# threeMoPrice <- threeMoPrice["1997-03::"]
# threeMoPrice <- threeMoPrice[endpoints(threeMoPrice, "months"),]