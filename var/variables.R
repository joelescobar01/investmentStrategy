
quoteNames <- yahooQF( c( 
    "Market Capitalization", "Last Trade (Price Only)", "P/E Ratio", 
    "Price/Book", "Book Value", 
    "Average Daily Volume", "Shares Outstanding", 
    "Dividend/Share", "Dividend Yield", 
    "Earnings/Share" )
)

GOV.SECURITIES <- 
    c("4WEEK"="DTB4WK", 
      "3MONTH"="DTB3", 
      "6MONTH"="DTB6", 
      "1YEAR"="DGS1" , 
      "2YEAR"="DGS2", 
      "5YEAR"="DGS5",
      "5YEARTIPS"="DFII5",
      "7YEAR"="DGS7", 
      "10YEAR"="DGS10",
      "20YEAR"="DGS20", 
      "30YEAR"="DGS30")
