
#create xts type dates 
constructXtsDate <- function( date1, date2 ){
    return( paste( date1, date2, sep="::") ) 
}

#distance between dates as numerical values 
numericalDateDiff <- function( date1, date2 ){
    return( as.numeric( as.Date( date1 ) - as.Date( date2 )) )
}

#addVerticalLines 
addVLinesToTSChart <- function( pointV=c(), chartNum=c(), col='red' ){
    if( length(pointV) == 0 )
        return("")
    if( length(chartNum) == 0 )
        return("")
    return( toString( paste("addLines(v=c(", 
                    toString(pointV) ,
                    "),col='",
                    col, 
                    "',on=c(", 
                    toString(chartNum), 
                    "))", sep="") )) 
}

prepareMACD <- function( macd ){
    macd <- na.omit( macd )  
    macdLine <- coredata( macd$macd ) 
    sigLine <- coredata( macd$signal ) 
}
