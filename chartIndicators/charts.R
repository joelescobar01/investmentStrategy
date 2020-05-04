library(quantmod)
library(TTR)
library(ggpubr)
library(tidyquant) 
library(ggplot2)
source('lib/chartTools.R')
#source('var/settings.R')
source("chartIndicators/momFunction.R")
source("chartIndicators/bBandFunction.R")
source("chartIndicators/adxFunction.R")
source("chartIndicators/wprFunction.R")
source("chartIndicators/rsiFunction.R")
source("chartIndicators/macdFunction.R")

chartVolatilityCloseToClose <- function( stockOHLC, endDate=Sys.Date(), startDate=Sys.Date()-90, 
                                         plotTitle="Volatility Close Chart 0.1"){
    ohlc <- stockOHLC[,1:4]
    Close <- volatility(ohlc, calc="close")
    closeVolDF <- zooToDataFrame(Close)
    colnames(closeVolDF) <- c("Volatility")
    closeVolDF$Close <- coredata(Cl(stockOHLC))
    closeVolDF$Date <- as.Date( index(stockOHLC) )
    
    p1 <- ggplot( closeVolDF, aes(Date )) +
        geom_line(aes(y=Volatility, colour = "Volatility")) + 
        geom_line(aes(y=log(Close), colour = "Log Closing Price")) +
        scale_x_date(lim = ( c(startDate, endDate ) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        scale_colour_manual(values=c("red","green","blue")) +
        theme(legend.position = c(0.8, 0.9))
    return(p1)
}

chartSeasonPlot <- function( stockSymbol ){
    data.name <- stockSymbol
    
    print( paste("Starting: ", data.name, sep="" ) )
    stock <- getSymbols( data.name,
                         src="csv",
                         dir="./material/_data",
                         auto.assign = FALSE)
    monthlyHighs <- as.numeric(period.max(Hi(stock),endpoints(stock,on='months')))
    df <- data.frame(date=index(monthlyReturn(stock)), value=coredata(monthlyHighs), row.names=NULL )
    df$month <- factor(format(df$date, "%b"), levels = month.abb)
    df$year <- format(df$date, "%Y")
    title <- paste( "Monthly Highs for", data.name )
    #jpeg(filename, width = 1250, height = 900)
    #print( plot1 <- ggplot(df) + aes(month, monthlyHighs, group = year, color = year) + geom_line() + ggtitle(title) ) 
    print( paste("Completed: ", data.name ))  
    g1 <- ggplot(df, aes(x=month, y=monthlyHighs))  + 
        geom_line( aes(group = year, color = year) ) + 
        geom_point( aes(color=year)) + 
        ggtitle( paste( "Monthly Closing Prices", stockSymbol )) + 
        xlab("Months") + 
        ylab("Closing Prices") + 
        labs(color="Year") 
    #dev.off()
    
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

chartMACD <- function( stockDF, plotTitle="MACD Version 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    getMACD <- getMACD( stockDF )
    macdDF <- data.frame(Date=as.Date(rownames(stockDF)),getMACD, check.names=FALSE, row.names=1)
    macdDF <- macdDF[complete.cases(macdDF[,1:2]),]
    
    g1 <- ggplot( macdDF, aes(x=as.Date(rownames(macdDF)))) + 
        geom_line( aes(y=macd, colour="MACD") ) + 
        geom_line( aes(y=signal, colour="Signal")) +
        geom_hline( aes(yintercept = 0, colour="Baseline"), linetype="dashed") +
        #geom_col( aes(y=diff))+
        labs(title=plotTitle, subtitle="Minor ticks = 3 days", y="", x="Date",
                caption="nFast=12, nSlow=26, nSig=9, maType=EMA") + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        scale_colour_manual(values=c( "black", "red","green")) +
        theme(legend.position = c(0.1, 0.2))
    # Note that, the argument legend.position can be also a numeric vector c(x,y). 
    #In this case it is possible to position the legend inside the plotting area. x and y 
    #are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) 
    #corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
    
    return(g1)
}

chartRSI <- function( stockOHLC, plotTitle="RSI Version 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    getRSI <-  
        stockEmaRSI( stockOHLC )
    rsiDF <- 
        zooToDataFrame( getRSI ) 
    
    label <- c() 
    for(ii in seq_along(rsiDF$rsi ) ) {
        if( ii > 70 )
            label[ii] = 70 
        else if ( ii < 30 )
            label[ii] = 30
        else 
            label[ii] = 0 
    }
    rsiDF$label < label
    
    g1 <- ggplot( rsiDF, aes(as.Date(row.names(rsiDF)))) + 
        geom_line( aes(y=rsi, colour="RSI" )) +
        geom_hline( aes(yintercept = 70, colour="Overvalued"), linetype="dashed") +
        geom_hline( aes(yintercept = 30, colour="Oversold"), linetype="dashed") +
        labs(title=plotTitle, subtitle="Minor ticks = 3 days", y="", x="Date",
             caption="maType=EMA") + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        scale_colour_manual(values=c( "red","green", "blue"), 
                            name="Lines") +
        theme(legend.position = c(0.1, 0.9))
    # Note that, the argument legend.position c
    return(g1)
}

chartMomentum <- function( stockOHLC,  plotTitle="MOM Chart 1.1",endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    getMOM <-  stockMomentum( stockOHLC )
    momDF <- zooToDataFrame( getMOM ) 
    #momDF <- momentumParity( momDF )
    
    g <- ggplot(momDF, aes(as.Date(rownames(momDF)))) +
        geom_line( aes(y=momentum ), color='black') +
        geom_hline( aes(yintercept = 0), color='red', linetype="dashed") +
        labs(title=plotTitle, subtitle="Minor ticks = 3 days", y="", x="Date",
             caption="2 day momentum") + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        theme(legend.position = c(0.1, 0.9))
    return(g)
    # chartSeries(stock,
    #         name=plotTitle,
    #         theme=chartTheme('white'),
    #         subset=paste(startDate,endDate,sep="::"),
    #         TA= ta
    #     )

}    


chartBBands2 <- function( stockOHLC, endDate=Sys.Date(), startDate=Sys.Date()-90, plotTitle="BBands Chart 1.1" ){
    getBBAND <- stockBBands(stockOHLC)
    bbandDF <- zooToDataFrame(getBBAND)
    stockDF <- zooToDataFrame(stockOHLC)
    firstDate <- first( row.names(bbandDF) )
    lastDate <- last( row.names(bbandDF) )
    #bbandDF$Close <- Cl( stockDF[seq( as.Date(firstDate):as.Date(lastDate) ), ] )
    
    closeList <- c()
    for(ii in row.names(bbandDF)){
        closeList[ii] <- stockDF[ii,4] 
    }
    bbandDF$ClosePrice <- closeList 
    dateRange <- seq(as.Date(startDate):as.Date(endDate) )
    
    bbandDF <- bbandDF[ which( as.Date(rownames(bbandDF)) >= as.Date(startDate) ), ]
    bbandDF <- bbandDF[ which( as.Date(rownames(bbandDF)) <= as.Date(endDate) ), ]
    
    yMin <- min( bbandDF$dn)
    yMax <- max( bbandDF$up)
    
    g1 <- ggplot(bbandDF, aes(x=as.Date(row.names(bbandDF)) )) +
        geom_ribbon(aes(ymin = dn, ymax = up), fill = "grey80") +
        geom_line(aes(y=mavg, colour="Moving Average") ) +
        geom_line(aes(y=ClosePrice, colour="Close Price")) +
        #geom_point( aes( y=pctB, colour= factor(pctB > 1)) ) +
        #geom_point( aes( y=pctB, colour= factor(pctB < 0)) ) +
        labs(title=plotTitle, subtitle="Minor ticks = 3 days", y="", x="Date",
             caption="sma=13, stdeviation=2") + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        scale_colour_manual(values=c( "black", "red")) +
        #ylim(yMin, yMax) +
        theme(legend.position = c(0.1, 0.9))
    
    return(g1)
}

chartADX <- function(  stockOHLC, plotTitle="ADX Chart 0.1",endDate=Sys.Date(), startDate=Sys.Date()-90  ){
    #ADX values help traders identify the strongest and most profitable trends to trade. 
    #The values are also important for distinguishing between trending and non-trending 
    #conditions. Many traders will use ADX readings above 25 to suggest that the trend 
    #is strong enough for trend-trading strategies. Conversely, when ADX is below 25, 
    #many will avoid trend-trading strategies.
    stockADX <- stockADX(stockOHLC)
    adxDF <- zooToDataFrame(stockADX)
    #dxDF <- adxDF[ which( as.Date(rownames(adxDF)) >= as.Date(startDate) ),  ]
    #adxDF <- adxDF[ which( as.Date(rownames(adxDF)) <= as.Date(endDate) ), ]
    
    # adxSig <- adxSignals( adxDF )
    # adxUpX <- signalXValues( adxSig$uptrend, startDate )
    # adxDoX <- signalXValues( adxSig$downtrend, startDate )
 
    g1 <- ggplot( adxDF, aes(x=as.Date(rownames(adxDF) ), na.rm = TRUE)) + 
        geom_line( aes(y=DIp,colour="+DMI"), size=1 ) + 
        geom_line( aes(y=DIn,colour="-DMI"), size=1) +
        geom_line( aes(y=ADX, colour="ADX"), size=2) +
        geom_hline(yintercept = 20, linetype="dashed")+
        geom_hline(yintercept = 50, linetype="dashed") +
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        labs(title=plotTitle, subtitle="Minor ticks = 3 days", y="", x="Date",
             caption="dxCal=14") + 
        scale_colour_manual(values=c( "green", "red", "blue"),name="Lines") +
        #ylim(yMin, yMax) +
        theme(legend.position = c(0.1, 0.9))
        #geom_vline(xintercept=adxSig$uptrend , linetype=2, colour="orange") +
        #geom_vline(xintercept=adxSig$downtrend , linetype=2, colour="black")
    return( g1 )
    
}

chartWPR <- function( stockDF, plotTitle="William %R Chart 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90  ){
    wpr <- stockWPR( stockDF )
    
    wprDF <- data.frame(Date=as.Date(row.names(stockDF)),wpr, check.names=FALSE, row.names=1)
    # stockDF$WPR <- stockW

   #colnames(stockDF) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "WPR" )

    p1 <- ggplot(wprDF, aes(x= as.Date(rownames(wprDF)), y=wpr*-100)) + 
        geom_line() +
        geom_hline(aes(colour= "Overvalued", yintercept = -20), 
                   size = 1, 
                   linetype = "dashed") +
        geom_hline(aes(colour= "Oversold", yintercept = -80), 
                   size = 1,
                   linetype = "dashed") +
        geom_hline(yintercept = -50, 
                   size = 1, 
                   color = "grey",
                   linetype = "dashed") + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        labs(title=plotTitle, subtitle="Minor ticks = 3 days", y="", x="Date",
             caption="lookback period: 14") + 
        scale_colour_manual(values=c( "green", "red" ),name="Bounded Lines") +
        #ylim(yMin, yMax) +
        theme(legend.position = c(0.1, 0.9))
    return(p1)
}

chartVolume <- function( stockDF,  plotTitle="Volume Chart 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    #tockDF <- zooToDataFrame(stock)
    stockDF <- data.frame(Date=as.Date(row.names(stockDF)), stockDF$Volume, check.names=FALSE, row.names=1)
    #stockDF <- stockDF[ which( stockDF$Date >= startDate ),  ]
    #stockDF <- stockDF[ which( stockDF$Date <= endDate ), ] 
    colnames(stockDF) <- c( "Volume" )
    g1 <- ggplot(stockDF, aes(x = as.Date(rownames( stockDF)) , y = Volume)) +
        geom_bar(stat="identity", fill="steelblue")+
        #geom_text(aes(label=len), vjust=-0.3, size=3.5)+
        labs(title=plotTitle, subtitle="Minor ticks = 3 days", y="", x="Date") + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        scale_colour_manual(values=c( "green", "red", "blue"),name="Lines") +
        #ylim(yMin, yMax) +
        theme(legend.position = c(0.1, 0.9))
    return(g1)
}

chartSeasons <- function( stockOHLC,  plotTitle="Season Chart 0.1", endDate=Sys.Date(), startDate=Sys.Date()-90 ){
    #data.name <- stockOHLC
    
    print( paste("Starting: ", plotTitle, sep="" ) )
    monthlyHighs <- as.numeric(period.max(Hi(stockOHLC),endpoints(stockOHLC,on='months')))
    df <- data.frame(date=index(monthlyReturn(stockOHLC)), value=coredata(monthlyHighs), row.names=NULL )
    df$month <- factor(format(df$date, "%b"), levels = month.abb)
    df$year <- format(df$date, "%Y")
    title <- paste( "Monthly Highs for", plotTitle )
    #jpeg(filename, width = 1250, height = 900)
    #print( plot1 <- ggplot(df) + aes(month, monthlyHighs, group = year, color = year) + geom_line() + ggtitle(title) ) 
    print( paste("Completed: ", plotTitle ))  
    g1 <- ggplot(df, aes(x=month, y=monthlyHighs))  + 
        geom_line( aes(group = year, color = year) ) + 
        ggtitle( paste( "Monthly Closing Prices", plotTitle )) + 
        xlab("Months") + 
        ylab("Closing Prices") + 
        labs(color="Year") 
    return(g1)
}
