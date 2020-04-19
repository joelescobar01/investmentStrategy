library(quantmod)
library(TTR)

simpleChart <- function( stock, time='last 3 months'){
    chartSeries( stock,
                type="line",
                theme=chartTheme('white'),
                subset=time, 
                )
}

candleStickChartTicks <- function( stock, cName="" ){
    l <- xts(!as.logical(stock[,1]),index(stock))
    l[ length(stock[,1]) ] <- TRUE
    chart_Series(stock,
                 name=cName,
                 TA="add_TA(l,on=-1,col='grey',border='grey')", )
}

movingAverageChart <- function( stock, time='last 3 months'){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset=time, 
                TA="addSMA(13);addSMA(55)"
                )
}

#EMA(stock, lookbackPeriod) 
chartEMA <- function( stock, time='last 3 months'  ){
    chartSeries( stock,
                name=,
                subset='last 3 months',  
                theme=chartTheme('white'),
                TA="addEMA(13,on=1,col='blue');addEMA(55, on=1,col='red')"
                )
}

#BBands(Cl(), n=20, sd=2)
# Cl, AvgHi, H, L, Cl can be used 
chartBBands <- function( stock, from=20, sd=2, time='last 3 months'  ){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset='last 3 months', 
                TA="addBBands( n=20, sd=2 )"
                )

}

chartMomentum <- function( stock, time='last 3 months'  ){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset='last 3 months', 
                TA="addMomentum(n=1)"
                )
}

chartADX <- function( stock, time='last 3 months' ){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset=time,
                TA="addADX(n=14, maType=EMA)"
                )
}
