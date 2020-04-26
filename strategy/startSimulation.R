
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
devtools::install_github("braverock/blotter")
devtools::install_github("braverock/blotter")

rm(list=ls())
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


getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment
   )


#getSymbols(Symbols = "DGS10", src = "FRED")

stock(symbols, 
      currency="USD",
      multiplier=1 )


portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

rm.strat(portfolio.st)
rm.strat(account.st)


initPortf( name = portfolio.st, 
           symbols = symbols,
           initDate = init_date )

initAcct( name = account.st, 
          portfolios = portfolio.st,
          initDate = init_date, 
          initEq = init_equity )

initOrders( portfolio = portfolio.st, 
            symbols = symbols, 
            initDate = init_date )

strategy( strategy.st, store=TRUE )

add.indicator( strategy = strategy.st, 
                name = "SMA", 
                arguments = list(x=quote(Cl(mktdata)), n=10 ), 
                label="nFast" )

add.indicator( strategy = strategy.st, 
               name="SMA", 
               arguments=list(x=quote(Cl(mktdata)),
                              n=30), label="nSlow")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

add.rule( strategy = strategy.st, 
          name="ruleSignal", 
          arguments = list(sigcol = "long", 
                           sigval = TRUE, 
                           orderqty = 100, 
                           ordertype = "stoplimit", 
                           threshold = 0.0005, 
                           prefer = "High", 
                           TxnFees = -10, 
                           replace = FALSE), 
          type="enter",
          label="EnterLONG" )

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005, 
                          orderside = "short", 
                          replace = FALSE, 
                          TxnFees = -10, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "short", 
                          sigval = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2SHORT")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "long", 
                          sigval = TRUE, 
                          orderside = "short", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2LONG")


#cwd <- getwd()
#setwd(paste(cwd, "/_data", sep="") )
results_file <- paste("/_data/", "results_", strategy.st, ".RData", sep = "")

results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
 updateAcct(account.st)
 updateEndEq(account.st)
 source("checkBlotterUpdate.R")
 if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
   save(list = "results", file = results_file)
   save.strategy(strategy.st)
 }

 