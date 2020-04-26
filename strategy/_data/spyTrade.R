library(quantstrat)
library( ggplot2 )
library(dplyr)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(lattice)
library(pander)
library(webshot)
source("checkBlotterUpdate.R")

rm(list=ls())
rm(list=basic_symbols())
setwd("/home/joel/Documents/stocks/strategy")

Sys.setenv(TZ = "UTC")
currency('USD') #US market 

init_date <- "2017-12-31"  #date we initialize our account & portfolio 
start_date <- "2018-01-01" #first date to retrieve 
end_date <- "2019-12-31" #last day of data retrieve 
init_equity <- "ie4" #10000 initial account equity 
adjustment <- TRUE #adjust the price for dividen payout, stock splits

source("basicSymbols.R")

symbols <- basic_symbols()

getSymbols(Symbols = symbols, src = "yahoo", index.class = "POSIXct", 
           from = start_date, to = end_date, adjust = adjustment)
initPortf(name = portfolio.st, symbols = symbols, initDate = init_date)
initAcct(name = account.st, portfolios = portfolio.st, initDate = init_date, 
         initEq = init_equity)
initOrders(portfolio = portfolio.st, symbols = symbols, initDate = init_date)
applyStrategy(strategy.st, portfolios = portfolio.st)
checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)