library(quantmod)
library(TTR)
library(ggpubr)
library(tidyquant) 
library(ggplot2)
source("../chartTools.R")
source("../settings.R")
source("momFunction.R")
source("bBandFunction.R")
source("adxFunction.R")
source("wprFunction.R")

chartVolatilityCloseToClose <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="Volatility Close Chart 0.1" ){
    ohlc <- stock[,1:4]
    Close <- volatility(ohlc, calc="close")
    closeVolDF <- zooToDataFrame(Close)
    colnames(closeVolDF) <- c("Volatility")
    p1 <- ggplot( closeVolDF, aes(x=index(closeVolDF), y= Volatility )) +
        geom_line() + 
        labs( x="Close-to-Close")
    return(p1)
}

# chartVolatilityClose0 <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="Volatility Close Chart 0.1" ){
#     ohlc <- stock[,1:4]
#     vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
#     vClose0DF <- zooToDataFrame(vClose0)
#     colnames(vClose0DF) <- c("Date", "Volatility")
#     p2 <- ggplot( vClose0DF, aes(x=Date, y= Volatility )) +
#         geom_line() + 
#         labs( x="Garman and Klass")
# }
# 
# charVolatilityParkinson <- function(stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="Volatility Close Chart 0.1"){
#     stock <- stock %>%
#         adjustOHLC(use.Adjusted = TRUE)
#     ohlc <- stock[,1:4]
#     vParkinson <- volatility(ohlc, calc="parkinson")
#     vParkinsonDF <- zooToDataFrame(vParkinson)
#     colnames(vParkinsonDF) <- c("Date", "Volatility")
#     p4 <- ggplot( vParkinsonDF, aes(x=Date, y= Volatility )) +
#         geom_line() + 
#         labs( x="Parkinson High-Low")
# }
# 
# charVolatilityGK <- function(stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="Volatility Close Chart 0.1"){
#     ohlc <- stock[,1:4]
#     vGK <- volatility(ohlc, calc="garman")
#  vGKDF <- zooToDataFrame(vGK)
#  colnames(vGKDF) <- c("Date", "Volatility")
#  p3 <- ggplot( vClose0DF, aes(x=Date, y= Volatility )) +
#      geom_line() + 
#      labs( x="OHLC Volatility")
# }
# 
# charVolatilityGK <- function(stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="Volatility Close Chart 0.1"){
#     ohlc <- stock[,1:4] 
#     vRS <- volatility(ohlc, calc="rogers")
#      vRSDF <- zooToDataFrame(vRS)
#      colnames(vRSDF) <- c("Date", "Volatility")
#      p5 <- ggplot( vRSDF, aes(x=Date, y= Volatility )) +
#         geom_line() +
#         labs( x="OHLC Rogers and Satchell")
#}

chartPriceSimulationMC <- function( price_sim ){
    N     <- 252 # Number of Stock Price Simulations
    M     <- 250  # Number of Monte Carlo Simulations
    p1 <- 
        price_sim %>%
        ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) +
        geom_line(alpha = 0.1) +
        ggtitle(str_c("Random Walk: ", M,
                      " Monte Carlo Simulations for Prices Over ", N,
                      " Trading Days"))
    return(p1)
}
chartReturnDistribution <- function( stock, plotTitle="Log Daily Returns" ){
    
    stockDailyReturns <- 
          stockDailyLogReturns(stock)
    volatilityMeasure <- 
        stockDistributionMetric( stockDailyReturns )
    p1 <- ggplot(dailyLogReturns, aes(x = Log.Returns)) + 
        geom_histogram(aes(y = ..density..), bins=100) + 
        geom_density() +
        geom_rug(alpha = 0.5) +
        geom_vline(xintercept = unname(volatilityMeasure), colour="red", linetype=2 )+
        labs(title=symbols, y="Count", x="Daily Returns Log")
    return(p1)
}
chartReturnRate <- function(dailyYield, plotTitle="Daily Return Rate"){
    dailyRate <- cumprod(1+dailyYield)
    p1 <- ggplot(dailyRate, aes(x = index(dailyRate), y=Rate)) +
        geom_line() +
        labs(title = plotTitle, x = "Date", y ="Return") + 
        theme_tq() +
        labs(caption="Log Base")
    return(p1)
}

chartExcessiveReturn <- function( riskFreeRate, stockRate , plotTitle="Excess Return" ){
    p2 <- chartReturnRate(riskFreeRate, plotTitle="RiskFree Rate")
    p3 <- chartReturnRate(stockRate)
    riskFreeRate <- cumprod(1+riskFreeRate)
    stockRate <- cumprod(1+stockRate)
    p1 <- ggplot() + 
        geom_line(data = riskFreeRate, aes(x = index(riskFreeRate), y = Rate, colour = "blue"), size=1) +
        geom_line(data = stockRate, aes(x = index(stockRate), y = Rate, colour = "red"), size=1) +
        xlab('Dates') +
        ylab('Return' ) + 
        labs(title="Excessive Return", caption="Stock 3 month return rate compared to a Risk Free Rate") + 
        scale_color_discrete(name = "Return Rate", labels = c("T-Bill-3M", "stock"))
    
    pall <- ggarrange(p1,                                                 # First row with scatter plot
              ggarrange(p2, p3, ncol = 2 ), # Second row with box and dot plots
              nrow = 2                               # Labels of the scatter plot
    ) 
    return(pall)
}

chartCandlesticks <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MACD Chart 0.1" ){
    stockDF <- zooToDataFrame(stock)
    colnames(stockDF) <- c( "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted" )
    g1 <- ggplot(stockDF, aes(x = Date , y = Close)) +
            geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
            geom_ma(ma_fun = SMA, n = 13, linetype = 5, size = 1.25) +  
            labs(title = "Candlestick Chart", y = "Closing Price", x = "", subtitle="13 Day Moving Average") + 
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
    stockDF <- zooToDataFrame(stock)
    print(stockDF)
    ggplot(stockDF, aes(x=index(stockDF), y=Cl(stock) )) +
        geom_line()
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

chartSeasons <- function( stock,  cName="Season Chart 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    #data.name <- stock
    
    print( paste("Starting: ", cName, sep="" ) )
    monthlyHighs <- as.numeric(period.max(Hi(stock),endpoints(stock,on='months')))
    df <- data.frame(date=index(monthlyReturn(stock)), value=coredata(monthlyHighs), row.names=NULL )
    df$month <- factor(format(df$date, "%b"), levels = month.abb)
    df$year <- format(df$date, "%Y")
    title <- paste( "Monthly Highs for", cName )
    #jpeg(filename, width = 1250, height = 900)
    #print( plot1 <- ggplot(df) + aes(month, monthlyHighs, group = year, color = year) + geom_line() + ggtitle(title) ) 
    print( paste("Completed: ", cName ))  
    g1 <- ggplot(df, aes(x=month, y=monthlyHighs))  + 
        geom_line( aes(group = year, color = year) ) + 
        geom_point( aes(color=year)) + 
        ggtitle( paste( "Monthly Closing Prices", cName )) + 
        xlab("Months") + 
        ylab("Closing Prices") + 
        labs(color="Year") 
    return(g1)
}
