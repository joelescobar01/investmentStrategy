library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

toDate <- Sys.Date() #current Date 
fromDate <- toDate - (30)*6



q1Earnings <- c( "ALPN", "MOTS", "PHAS", "DYAI",
                  "APTX", "SNGX", "ACIU", "EAST",
                  "FSI", "ADMP", "EDAP", "SUNW",
                  "CSSE", "HSON", "GNLN", "LEGH",
                  "CYD", "RH")
par(mfrow = c(4, 5))

for(i in 1:length(q1Earnings)){
  stock <- getSymbols(q1Earnings[i], auto.assign = F, from=fromDate) #access CLose and Open with stock$MOTS.Open 
  dat <- as.data.frame(stock)
  dat$date <- index(stock)
  dat <- subset(dat, date >= fromDate )
  
  str <- sprintf( "^%s\\.", q1Earnings[i] )
  names(dat) <- sub(str, "", names(dat))
  
  # annotation
  a <- list(text = "Stock Split",
            x = '2020-04-01',
            y = 1.02,
            xref = 'x',
            yref = 'paper',
            xanchor = 'left',
            showarrow = FALSE
  )
  
  # use shapes to create a line
  l <- list(type = line,
            x0 = '2020-04-01',
            x1 = '2020-04-01',
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            line = list(color = 'black',
                        width = 0.5)
  )
  
  fig <- plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open,
                 colors = c("red", "forestgreen"), hoverinfo = "none") 
  fig <- fig %>% add_segments(y = ~Low, yend = ~High, size = I(1)) 
  fig <- fig %>% add_segments(y = ~Open, yend = ~Close, size = I(3)) 
  fig <- fig %>% layout(showlegend = FALSE, yaxis = list(title = "Price")) 
  fig <- fig %>% rangeslider()
  
  fig <- fig %>% layout(annotations = a,
                        shapes = l)
  fig  
}
