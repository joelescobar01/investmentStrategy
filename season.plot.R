library(quantmod)
library(ggplot2)
library(xts)

generateSeasonPlot <- function( stockSymbol ){
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
  return( ggplot(df, aes(x=month, y=monthlyHighs))  + 
         geom_line( aes(group = year, color = year) ) + 
         geom_point( aes(color=year)) + 
         ggtitle( paste( "Monthly Closing Prices", stockSymbol )) + 
         xlab("Months") + 
         ylab("Closing Prices") + 
         labs(color="Year") 
       )
  #dev.off()

}

generateCustomSeasonPlot <- function( x = c() ){
  for(ii in seq_along(x) ){
    data.name <- x[ii]
    filename <- paste( getwd(), "/reports/seasonal/", "SeasonHigh" , data.name , ".plot.jpg", sep="" )
    plot1 <- generateSeasonPlot(x[ii])
    jpeg(filename, width = 1250, height = 900)
      plot(plot1)
    dev.off()
  }
}

generateMaterialsSeasonalPlot <- function( ){
  x <- scan(file="material/stockSymbols", what="character")
  for(ii in seq_along(x) ){
    data.name <- x[ii]
    filename <- paste( getwd(), "/material/_plot/", "SeasonHigh" , data.name , ".plot.jpg", sep="" )  
    if( file.exists( filename ) ){
      print( paste("Skipping:", data.name) )
      next() 
    }
    plot1 <- generateSeasonPlot(x[ii])
    jpeg(filename, width = 1250, height = 900)
      plot(plot1)
    dev.off()
  }
}
# for(ii in seq_along(x) ){
#   data.name <- x[ii]
#   filename <- paste( getwd(), "/material/_plot/", "SeasonHigh" , data.name , ".plot.jpg", sep="" )  
#   if( file.exists( filename ) ){
#     print( paste("Skipping:", data.name) )
#     next() 
#   }
#   
#   print( paste("Starting: ", data.name, sep="" ) )
#   stock <- getSymbols( c( data.name ),src="csv", dir="./material/_data", auto.assign = FALSE)
#   
#   monthlyHighs <- as.numeric(period.max(Hi(stock),endpoints(stock,on='months')))
#   
#   df <- data.frame(date=index(monthlyReturn(stock)), value=coredata(monthlyHighs), row.names=NULL )
#   df$month <- factor(format(df$date, "%b"), levels = month.abb)
#   df$year <- format(df$date, "%Y")
#   # 
#   title <- paste( "Monthly Highs for", data.name )
#   # 
#   
#   
#   jpeg(filename, width = 1250, height = 900)
#   print( plot1 <- ggplot(df) + aes(month, monthlyHighs, group = year, color = year) + geom_line( aes(size=4)) + ggtitle(title) ) 
#   dev.off()
#   print( paste("Completed: ", data.name ))
#   monthlyHighs <- as.numeric(period.max(Hi(stock),endpoints(stock,on='months')))
#   
#   df <- data.frame(date=index(monthlyReturn(stock)), value=coredata(monthlyHighs), row.names=NULL )
#   df$month <- factor(format(df$date, "%b"), levels = month.abb)
#   df$year <- format(df$date, "%Y")
#   # 
#   title <- paste( "Monthly Highs for", data.name )
#   # 
#   
#   
#     jpeg(filename, width = 1250, height = 900)
#     print( plot1 <- ggplot(df) + aes(month, monthlyHighs, group = year, color = year) + geom_line() + ggtitle(title) ) 
#   dev.off()
#   print( paste("Completed: ", data.name ))
# }
# # 
# # 
# # df <- data.frame(date=index(monthlyReturn(stock)), value=coredata(monthlyLows), row.names=NULL )
# # df$month <- factor(format(df$date, "%b"), levels = month.abb)
# # df$year <- format(df$date, "%Y")
# # 
# # title <- paste( "Monthly Lows for ", data.name)
# # plot2 <- ggplot(df) + aes(month, monthlyLows, group = year, color = year) + geom_line() + ggtitle(title)
# # 
# # grid.arrange(plot1, plot2, ncol=1)