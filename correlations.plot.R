library(quantmod)
library(xts)
library(ggplot2)
library(PerformanceAnalytics)
source("utils.R")

x <- scan(file="material/stockSymbols", what="character")

sp500 <- getSymbols("IXB", 
                    from="2020-01-13",
                    auto.assign=FALSE )

for(ii in seq_along(x) ){
  data.name <- x[ii]
  filename <- paste( getwd(), "/material/_plot/", "SectorCorrelation" , data.name , ".plot.jpg", sep="" )  
  if( file.exists( filename ) ){
    print( paste("Skipping:", data.name) )
    next() 
  }
  
  print( paste("Starting: ", data.name, sep="" ) )
  stock <- getSymbols( c( data.name ),
                       src="csv", 
                       from="2020-01-13", 
                       dir="./material/_data", 
                       auto.assign = FALSE)
  
  # 
  title <- paste( "SP500 Material Sector", data.name )
  # 
  
  cpmData<-cbind( diff(log(Cl(sp500))), diff(log(Cl(stock)))) 

  
  #jpeg(filename, width = 1250, height = 900)
  #print( plot1 <- ggplot(df) + aes(month, monthlyHighs, group = year, color = year) + geom_line() + ggtitle(title) ) 
  chart.Correlation(cpmData) 
  dev.off()
  print( paste("Completed: ", data.name ))
  
}