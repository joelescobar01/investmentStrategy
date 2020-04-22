library(quantmod)
library(TTR)
library(ggpubr)
source("technicalIndicators.R") 
source("chartTools.R")
source("settings.R")
source("momFunction.R")
source("bBandFunction.R")
source("adxFunction.R")


chartMACD <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MACD Chart 0.1" ){
    getMACD <- stockMACD( stock )
    macdIndicator <- indicatorMACD( zooToDataFrame( getMACD ) )
    buyDates <- macdIndicator$buy[ macdIndicator$buy > startDate ]
    buyXValues <- c()
    for( i in 1:length(buyDates) ){
        buyXValues[i] <- businessDayCounter(startDate, buyDates[i])
    }
    sellDates <- macdIndicator$sell[ macdIndicator$sell > startDate ]
    sellXValues <- c()
    for( i in 1:length(sellDates) ){
        sellXValues[i] <- businessDayCounter( startDate, sellDates[i])
    }
    ta <-addLinesToMACD(buysXValues, sellXValues) 
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset=constructXtsDate(startDate, endDate),
            TA= ta
        )
}

chartRSI <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="RSI Chart 0.1" ){
    getRSI <-  stockEmaRSI( stock )
    rsiDF <- zooToDataFrame( getRSI ) 
    rsiSignal <- c() 
    for(i in 1:nrow(rsiDF) ){
        if( rsiDF$rsi[i] > rsiOverboughtConstant ){
            rsiSignal[i] = 1 
        } else if( rsiDF$rsi[i] < rsiOversoldConstant ){
            rsiSignal[i] = -1 
        } else {
            rsiSignal[i] = 0 
        }
    }
    rsiDF$signal <- rsiSignal
    #rsiDF$signal <- rsiSetup( rsiDF )
    limitDates <- indicatorRSI( rsiDF )  
    buyXValues <- c()
    #buyXValues <- rsiBuyXValues( limitDates$buy, startDate ) 
    buyDates <- limitDates$buy[ limitDates$buy > startDate ]
    for( i in 1:length(buyDates) ){
        buyXValues[i] <- businessDayCounter(startDate, buyDates[i])
    }
    sellXValues <- c()
    #sellXValues <- rsiBuyXValues( limitDates$sell, startDate ) 
    sellDates <- limitDates$sell[ limitDates$sell > startDate ]
    for( i in 1:length(sellDates) ){
        sellXValues[i] <- businessDayCounter( startDate, sellDates[i])
    }
    ta <-addLinesToRSI(buyXValues, sellXValues) 
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset=constructXtsDate(startDate, endDate),
            TA= ta
        )
}

chartMomentum <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MOM Chart 0.1" ){
    getMOM <-  stockMomentum( stock )
    momDF <- zooToDataFrame( getMOM ) 
    momDF <- momentumParity( momDF )
    dateChange <- changeInSlopeDir( momDF ) 
    buyXValues <- momBuyXValues( dateChange$buy, startDate ) 
    sellXValues <- momSellXValues( dateChange$sell, startDate ) 
    ta <- addLinesToMOM(buyXValues, sellXValues) 
    print(ta)
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset=constructXtsDate(startDate, endDate),
            TA= ta
        )

}    
chartBBands <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="BBands Chart 0.1" ){
    getBBAND <- stockBBands(stock)
    bbandDF <- zooToDataFrame(getBBAND)
    bSignal <- bbSetup(bbandDF)
    buyXValues <- bbandBuyXValues(bSignal$buy , startDate)
    
    sellXValues <- bbandSellXValues(bSignal$sell, startDate )
    
    ta <- addLinesToBBAND(buyXValues, sellXValues )
    
    chartSeries( stock,
                 theme=chartTheme('white'),
                 subset=constructXtsDate(startDate, endDate),
                 TA=ta,
                 name=cName
    )
}

chartADX <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    #ADX values help traders identify the strongest and most profitable trends to trade. 
    #The values are also important for distinguishing between trending and non-trending 
    #conditions. Many traders will use ADX readings above 25 to suggest that the trend 
    #is strong enough for trend-trading strategies. Conversely, when ADX is below 25, 
    #many will avoid trend-trading strategies.
    stockADX <- stockADX(stock)
    adxDF <- zooToDataFrame(stockADX)
    adxDF <- adxDF[ which( adxDF$Date >= startDate ),  ]
    adxDF <- adxDF[ which( adxDF$Date <= endDate ), ]
    
    adxSig <- adxSignals( adxDF )
    adxUpX <- signalXValues( adxSig$uptrend, startDate )
    adxDoX <- signalXValues( adxSig$downtrend, startDate )
 
    g1 <- ggplot( adxDF, aes(x=adxDF[,'Date'], y=max(adxDF[,'DIp'], adxDF[,'DIn']))) + 
        geom_line( aes(y=adxDF[,'DIp']), colour="red", size=1 ) + 
        geom_line( aes(y=adxDF[,'DIn']), color='blue', size=1) +
        scale_colour_discrete(guide = 'none')+
        ggtitle("Trend Strength") +
        xlab("Dates") + ylab("DMI") +
        geom_vline(xintercept=adxSig$uptrend , linetype=2, colour="orange") +
        geom_vline(xintercept=adxSig$downtrend , linetype=2, colour="black")
    g2 <- ggplot(adxDF, aes( x=adxDF[,'Date'], adxDF[,'ADX'])) + 
        geom_bar(stat="identity", width=0.5) + 
        geom_abline(slope=0, intercept=20,  col = "red",lty=2) +
        geom_abline(slope=0, intercept=50,  col = "blue",lty=2) +
        geom_vline(xintercept=adxSig$uptrend , linetype=2, colour="orange") +
        geom_vline(xintercept=adxSig$downtrend , linetype=2, colour="black")+
        ggtitle("Trend Strength") +
        xlab("Dates") + ylab("ADX") 
    ggarrange(g1, g2, ncol=1, nrow=2 )
    
}
