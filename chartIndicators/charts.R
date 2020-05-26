library(quantmod)
library(TTR)
library(ggpubr)
library(tidyquant) 
library(ggplot2)
source('lib/chartTools.R')
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


