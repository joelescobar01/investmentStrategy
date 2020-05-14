library(TTR)
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBearishTrend.R")    
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBody.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBullishTrend.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleHammer.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleSizes.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleWick.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBody.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleDoji.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleKick.R") 
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleStar.R") 
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleThreeGroup.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleGap.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleMedianReversal.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleTrend.R")
source("/home/joel/Documents/stocks/lib/utils.R") 
library(dplyr) 

#movement patterns 


chart.CandleStick.Recognizer <- function( stockTbbl ) {
  

  hammer <- 
    stockTbbl %>% 
    Hammer() %>% 
    filter( hammer == TRUE ) 

  ihammer <- 
    stockTbbl %>% 
    InvertedHammer() %>% 
    filter( inverted.hammer == TRUE ) 

  doji <- 
    stockTbbl %>% 
    Doji() %>% 
    filter( doji == TRUE ) 
  dragonfly <- 
    stockTbbl %>% 
    DragonflyDoji() %>% 
    filter( dragonfly.doji == TRUE ) 
  gravestone <- 
    stockTbbl %>% 
    GravestoneDoji() %>% 
    filter( gravestone.doji == TRUE ) 

  g1 <- 
    ggplot( stockTbbl, aes( x=date ) ) + 
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    geom_point(data=dragonfly,aes( x=date, y=low, shape="dragonfly doji"),color="green", size=5, alpha=0.4) +
    geom_point( data=doji, aes( x=date, y=low, shape="doji" ),color="aquamarine", size=5, alpha=0.4) +
    geom_point( data=hammer, aes( x=date, y=high, shape="hammer"), size=5, alpha=0.4, color="blue" )+ 
    geom_point( data=gravestone, aes( x=date, y=high, shape="gravestone doji"), size=5, alpha=0.4, color="red" ) 

  
  return(g1) 
}

chartCandlesticks <- function( stockDF, plotTitle="Candlestick Version 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    #stockDF <- zooToDataFrame(stock)
    colnames(stockDF) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted" )
    g1 <- ggplot(stockDF, aes(x = as.Date(row.names(stockDF)) , y = Close)) +
            geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
            geom_ma(ma_fun = SMA, n = 13, linetype = 5, size = 1.25) +  
            labs(title = "Candlestick Chart", y = "Closing Price", x = "", subtitle="13 Day Moving Average") + 
            scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
            theme_tq(
            ) 
     return(g1)
}

bullishSingleCandlePattern <- function( stockOHLC ){
  #single candle patterns 
  hammerVector <- hammer(stockOHLC)
  invertedHammerVector <- inverted.hammer(stockOHLC)
  dragonflyDojiVector <- dragonfly.doji(stockOHLC)
  bullishSinglePatternXTS <-
  merge( merge( hammerVector, invertedHammerVector ), dragonflyDojiVector ) 

  return(bullishSinglePatternXTS)
}

#candle size
xtsCandleSize <- function( stockOHLC ){
  #small body: delta*WC(t)>BL(t) for small delta=0.1
  smallBodyCandle <- short.candle( stockOHLC ) 
  longBodyCandle <- long.candle( stockOHLC )
  smallUpperShadow <- short.upper.shadow( stockOHLC ) 
  longUpperShadow <- long.upper.shadow( stockOHLC )
}


tenDayCandlePattern <- function( stockOHLC ){
  lastWeekStocks <-  tail(stockOHLC, n=10 )

  prevWeekPattern <- as.data.frame( lastWeekStocks )
  prevWeekPattern$hammer <- coredata( hammer(lastWeekStocks )) 
  prevWeekPattern$invertedHammer <- coredata( inverted.hammer(lastWeekStocks) ) 
  prevWeekPattern$bearishCandle <- coredata( bearish.candle(lastWeekStocks ) ) 
  prevWeekPattern$bearishEngulf <- coredata( bearish.engulf( lastWeekStocks ))
  prevWeekPattern$bearishHarami <- coredata( bearish.harami( lastWeekStocks))
  prevWeekPattern$CandleBodyBottom  <- coredata( CandleBodyBottom(lastWeekStocks))
  prevWeekPattern$CandleBodyCenter <- coredata( CandleBodyCenter(lastWeekStocks)) 
  prevWeekPattern$CandleBodyLength <- coredata( CandleBodyLength(lastWeekStocks)) 
  prevWeekPattern$CandleBodyTop  <- coredata( CandleBodyTop (lastWeekStocks)) 
  prevWeekPattern$CandleStickLength <- coredata(CandleStickLength(lastWeekStocks)) 
  prevWeekPattern$bullishCandle <- coredata( bullish.candle(lastWeekStocks)) 
  prevWeekPattern$bullishEngulf <- coredata( bullish.engulf(lastWeekStocks)) 
  prevWeekPattern$bullishHarami <- coredata( bullish.harami(lastWeekStocks)) 
  prevWeekPattern$doji  <- coredata( doji(lastWeekStocks)) 
  prevWeekPattern$dragonflyDoji <- coredata( dragonfly.doji(lastWeekStocks)) 
  prevWeekPattern$gravestoneDoji <- coredata( gravestone.doji(lastWeekStocks) )
  prevWeekPattern$gapDown <- coredata( gap.down( lastWeekStocks))
  prevWeekPattern$gapUp <- coredata( gap.up( lastWeekStocks))
  prevWeekPattern$kick.down <- coredata( gap.down( lastWeekStocks))
  prevWeekPattern$kickUp <- coredata( kick.up( lastWeekStocks))
  prevWeekPattern$darkCloudCover <- coredata( dark.cloud.cover( lastWeekStocks))
  prevWeekPattern$piercingLine <- coredata( piercing.line( lastWeekStocks))
  prevWeekPattern$longCandle <- coredata( long.candle( lastWeekStocks, n=10))
  prevWeekPattern$shortCandle <- coredata( short.candle( lastWeekStocks, n=10))
  prevWeekPattern$everningStar <- coredata( evening.star ( lastWeekStocks, n=10))
  prevWeekPattern$morningStar <- coredata ( morning.star( lastWeekStocks, n=10))
  prevWeekPattern$fallingThree <- coredata( falling.three( lastWeekStocks,n=10))
  prevWeekPattern$risingThree <- coredata (  rising.three( lastWeekStocks,n=10))
  prevWeekPattern$threeBlackCrows <- coredata( three.black.crows( lastWeekStocks,n=10))
  prevWeekPattern$threeWhiteSoldiers <- coredata( three.white.soldiers( lastWeekStocks,n=10))
  prevWeekPattern$downTrend <- coredata ( down.trend( lastWeekStocks, S=3, L=8))
  prevWeekPattern$upTrend <- coredata ( up.trend( lastWeekStocks, S=3, L=8))
  prevWeekPattern$LowerShadowLength  <- coredata (LowerShadowLength( lastWeekStocks))
  prevWeekPattern$UpperShadowLength  <- coredata ( UpperShadowLength( lastWeekStocks))
  
  return( prevWeekPattern) 
}
