library(quantmod)
library(TTR)
library(ggpubr)
library(tidyquant) 
source("technicalIndicators.R") 
source("chartTools.R")
source("settings.R")
source("momFunction.R")
source("bBandFunction.R")
source("adxFunction.R")
source("wprFunction.R")
library(ggplot2)
library('forecast')

#chartVolatility <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MACD Chart 0.1" ){
    #ohlc <- ttrc[,c("Open","High","Low","Close")]
    #vClose <- volatility(ohlc, calc="close")
    #vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
    #vGK <- volatility(ohlc, calc="garman")
    #vParkinson <- volatility(ohlc, calc="parkinson")
    #vRS <- volatility(ohlc, calc="rogers")
    getSymbols("CAT")
    ohlc <- CAT[,c("CAT.Open","CAT.High","CAT.Low","CAT.Close")]
    Close <- volatility(ohlc, calc="close")
    closeVolDF <- zooToDataFrame(Close)
    colnames(closeVolDF) <- c("Date", "Volatility")
    p1 <- ggplot( closeVolDF, aes(x=Date, y= Volatility )) +
        geom_line() + 
        labs( x="Close-to-Close")
    vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
    vClose0DF <- zooToDataFrame(vClose0)
    colnames(vClose0DF) <- c("Date", "Volatility")
    p2 <- ggplot( vClose0DF, aes(x=Date, y= Volatility )) +
        geom_line() + 
        labs( x="Garman and Klass")
    vGK <- volatility(ohlc, calc="garman")
    vGKDF <- zooToDataFrame(vGK)
    colnames(vGKDF) <- c("Date", "Volatility")
    p3 <- ggplot( vClose0DF, aes(x=Date, y= Volatility )) +
        geom_line() + 
        labs( x="OHLC Volatility")
    vParkinson <- volatility(ohlc, calc="parkinson")
    vParkinsonDF <- zooToDataFrame(vParkinson)
    colnames(vParkinsonDF) <- c("Date", "Volatility")
    p4 <- ggplot( vParkinsonDF, aes(x=Date, y= Volatility )) +
        geom_line() + 
        labs( x="Parkinson High-Low")
    
    vRS <- volatility(ohlc, calc="rogers")
    vRSDF <- zooToDataFrame(vRS)
    colnames(vRSDF) <- c("Date", "Volatility")
    p5 <- ggplot( vRSDF, aes(x=Date, y= Volatility )) +
        geom_line() +
        labs( x="OHLC Rogers and Satchell")
    
    p12 <- ggarrange(p1, p2, p3, p4, p5 ,  ncol = 1, nrow = 5)

chartCandlesticks <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MACD Chart 0.1" ){
    stockDF <- zooToDataFrame(stock)
    colnames(stockDF) <- c( "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted" )
    g1 <- ggplot(stockDF, aes(x = Date , y = Close)) +
            geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
            geom_ma(ma_fun = SMA, n = 13, linetype = 5, size = 1.25) +  
            labs(title = "Candlestick Chart", y = "Closing Price", x = "") + 
            #xlim(as.Date(c( startDate , endDate ))) +
            scale_x_date(   limits = as.Date(c( startDate, endDate )),
                            date_labels = "%m/%d") +
            theme_tq(
            ) 
     return(g1)
}

chartMACD <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MACD Chart 0.1" ){
    getMACD <- stockMACD( stock )
    macdDF <- zooToDataFrame( getMACD )
    macdIndicator <- indicatorMACD( macdDF )
    buyDates <- macdIndicator$buy[ macdIndicator$buy > startDate ]
    buyXValues <- c()
    for( i in seq_along(buyDates) ){
        buyXValues[i] <- businessDayCounter(startDate, buyDates[i])
    }
    sellDates <- macdIndicator$sell[ macdIndicator$sell > startDate ]
    sellXValues <- c()
    for( i in seq_along(sellDates) ){
        sellXValues[i] <- businessDayCounter( startDate, sellDates[i])
    }
    ta <-addLinesToMACD(buyXValues, sellXValues) 
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset=paste(startDate,endDate,sep="::"),
            TA=ta
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
    for( i in seq_along(buyDates) ){
        buyXValues[i] <- businessDayCounter(startDate, buyDates[i])
    }
    
    sellXValues <- c()
    #sellXValues <- rsiBuyXValues( limitDates$sell, startDate ) 
    sellDates <- limitDates$sell[ limitDates$sell > startDate ]
    for( i in seq_along(sellDates) ){
        sellXValues[i] <- businessDayCounter( startDate, sellDates[i])
    }
    ta <-addLinesToRSI(buyXValues, sellXValues) 
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset= paste(startDate,endDate,sep="::"),
            TA= ta
        )
    
}

chartMomentum <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MOM Chart 1.1" ){
    getMOM <-  stockMomentum( stock )
    momDF <- zooToDataFrame( getMOM ) 
    momDF <- momentumParity( momDF )
    
    g <- ggplot(momDF, aes(Date, momentum, na.rm = TRUE)) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept = 0, 
                   size = 1, 
                   colour = "red",
                   linetype = "dashed") +
        labs(title=cName, y="Momentum Strength (slope)", x="Date") + 
        theme(legend.position="None") + 
        scale_x_date( date_minor_breaks = "5 days", limits= c( startDate, endDate) )
    plot(g)
    # chartSeries(stock,
    #         name=cName,
    #         theme=chartTheme('white'),
    #         subset=paste(startDate,endDate,sep="::"),
    #         TA= ta
    #     )

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
                 subset=paste(startDate,endDate,sep="::"),
                 TA=ta,
                 name=cName
    )
}

chartBBands2 <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="BBands Chart 1.1" ){
    getBBAND <- stockBBands(stock)
    bbandDF <- zooToDataFrame(getBBAND)
    print(bbandDF)
    ggplot(stock, aes(x= index(stock), y= Cl(stock) ))
}

chartADX <- function(  stock, cName="ADX Chart 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90  ){
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
 
    g1 <- ggplot( adxDF, aes(x=adxDF[,'Date'], y=max(adxDF[,'DIp'], adxDF[,'DIn']), na.rm = TRUE)) + 
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
        ggtitle(cName) +
        xlab("Dates") + ylab("ADX") 
    gall <- ggarrange(g1, g2, ncol=1, nrow=2 )
    return( gall )
    
}

chartWPR <- function( stock, cName="William %R Chart 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90  ){
    stockW <- stockWPR( stock )
    stockDF <- zooToDataFrame(stock)
    stockDF$WPR <- stockW
    stockDF <- stockDF[ which( stockDF$Date >= startDate ),  ]
    stockDF <- stockDF[ which( stockDF$Date <= endDate ), ] 
    colnames(stockDF) <- c( "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted", "WPR" )

    p1 <- ggplot( stockDF, aes(x=Date, y= Close)) +
        geom_line() + 
        labs(y="Closing Price")
    p2 <- ggplot(stockDF, aes(x=Date, y=WPR*-100)) + 
        geom_line() +
        geom_hline(yintercept = c( -80, -20 ), 
                   size = 1, 
                   colour = "red",
                   linetype = "dashed") +
        geom_hline(yintercept = c( -50 ), 
                   size = 1, 
                   colour = "grey",
                   linetype = "dashed") + 
        labs( y="%R", caption="%R < -80 = oversold, %R > -20 = overbought")
    

    p12 <- ggarrange(p1, p2,  ncol = 1, nrow = 2)
    return(p12)
}

chartVolume <- function( stock,  cName="Volume Chart 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    stockDF <- zooToDataFrame(stock)
    stockDF <- stockDF[ which( stockDF$Date >= startDate ),  ]
    stockDF <- stockDF[ which( stockDF$Date <= endDate ), ] 
    colnames(stockDF) <- c( "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted" )
    g1 <- ggplot(stockDF, aes(x = Date , y = Volume)) +
        geom_bar(stat="identity", fill="steelblue")+
        #geom_text(aes(label=len), vjust=-0.3, size=3.5)+
        theme_minimal()
    return(g1)
}