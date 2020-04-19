library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(forecast)
library(ggplot2)
library(PerformanceAnalytics)
require(gridExtra)

data.name <- readline(prompt = "Enter Stock Symbol: ")

stock <-getSymbols(data.name, auto.assign = F ) #access CLose and Open with stock$MOTS.Open


monthlyHighs <- as.numeric(period.max(Hi(stock),endpoints(stock,on='months')))
monthlyLows <- as.numeric(period.min(Lo(stock),endpoints(stock,on='months')))

df <- data.frame(date=index(monthlyReturn(stock)), value=coredata(monthlyHighs), row.names=NULL )
df$month <- factor(format(df$date, "%b"), levels = month.abb)
df$year <- format(df$date, "%Y")

title <- paste( "Monthly Highs for ", data.name)

plot1 <- ggplot(df) + aes(month, monthlyHighs, group = year, color = year) + geom_line() + ggtitle(title)



df <- data.frame(date=index(monthlyReturn(stock)), value=coredata(monthlyLows), row.names=NULL )
df$month <- factor(format(df$date, "%b"), levels = month.abb)
df$year <- format(df$date, "%Y")

title <- paste( "Monthly Lows for ", data.name)
plot2 <- ggplot(df) + aes(month, monthlyLows, group = year, color = year) + geom_line() + ggtitle(title)

grid.arrange(plot1, plot2, ncol=1)