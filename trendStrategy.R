source( "utils.R")
library(PerformanceAnalytics)
require(PerformanceAnalytics)

stock <- initStock() 

candleFig <- stockCandlesticksChart( stock )

candleFig <- addDateVerticalLine(candleFig, "2020-04-01" )
volumeFig <- addVolumePlot(candleFig, stock )

seasonFig <- seasonalHighCharts(stock$stocks, stock$name)
lowFig <- seasonalLowCharts(stock$stocks, stock$name )


stock$stocks %>% chartSeries(TA='addBBands();addVo();addMACD()',subset='last 4 months')

closingPrice <- Cl( stock$stocks )
n <- length(closingPrice);

lrest <- with(closingPrice, diff(log(closingPrice)))
lrest <- na.omit( lrest )

reward <- mean( coredata(lrest)[,1], na.rm = TRUE  )
risk <- sd( coredata(lrest)[,1], na.rm = TRUE  )

endDate <- index( last(lrest) )
startDate <- index( first(lrest) )

risk_reward <- reward/risk
data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))))
