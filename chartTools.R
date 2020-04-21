source("settings.R")

#create xts type dates 
constructXtsDate <- function( date1, date2 ){
    return( paste( date1, date2, sep="::") ) 
}

candleStickChartTicks <- function( stock, cName="" ){
    l <- xts(!as.logical(stock[,1]),index(stock))
    l[ length(stock[,1]) ] <- TRUE
    chart_Series(stock,
                 name=cName,
                 TA="add_TA(l,on=-1,col='grey',border='grey')", )
}
#distance between dates as numerical values 
numericalDateDiff <- function( date1, date2 ){
    return( as.numeric( as.Date( date1 ) - as.Date( date2 )) )
}

#addVerticalLines 
addVLinesToTSChart <- function( pointV=c(), chartNum=c(), 
                               col='red', extraArg="" ){
    if( length(pointV) == 0 )
        return("")
    if( length(chartNum) == 0 )
        return("")
    return( toString( paste(extraArg, "addLines(v=c(", 
                    toString(pointV) ,
                    "),col='",
                    col, 
                    "',on=c(", 
                    toString(chartNum), 
                    "))", sep="") )) 
}

addHLinesToTSChart <- function( pointV=c(), chartNum=c(), 
                                col='red', extraArg="" ){
    if( length(pointV) == 0 )
        return("")
    if( length(chartNum) == 0 )
        return("")
    return( toString( paste(extraArg, "addLines(h=c(", 
                            toString(pointV) ,
                            "),col='",
                            col, 
                            "',on=c(", 
                            toString(chartNum), 
                            "))", sep="") )) 
}


addLinesToMACD <- function( buyXVals, sellXVals){

    return( paste( "addMACD()", 
                  addVLinesToTSChart(buyXValues, c(1,2), buyIndicatorColor), 
                  addVLinesToTSChart(sellXValues, c(1,2), sellIndicatorColor), 
                  sep=";") ) 
}

addLinesToRSI <- function( buyXValues, sellXValues){

    return( paste( "addRSI(n='14', maType='EMA')", 
                  addVLinesToTSChart(buyXValues, c(1,2), buyIndicatorColor), 
                  addVLinesToTSChart(sellXValues, c(1,2), sellIndicatorColor),
                  addHLinesToTSChart(c( 30,70), 2, 'grey'),
                  sep=";") ) 
}

addLinesToMOM <- function( buyXValues, sellXValues){
    print("Working")
    return( paste( "addMomentum()", 
                  addVLinesToTSChart(buyXValues, c(1,2), buyIndicatorColor), 
                  addVLinesToTSChart(sellXValues, c(1,2), sellIndicatorColor),
                  sep=";") ) 
}
