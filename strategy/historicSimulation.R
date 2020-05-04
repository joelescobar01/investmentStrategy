if (!require("TTR")) {
  install.packages("TTR")
  library(TTR)
}
if (!require("devtools")) {
  install.packages("devtools")
}
require(devtools)
install_github("braverock/blotter") # dependency
install_github("braverock/quantstrat")
require('FinancialInstrument')

library(quantstrat)
library(quantmod)
library(foreach)
source("osMaxDollar.R")


initdate = "2015-04-23"
from = "2015-04-23"
to = "2020-4-22"

currency ("USD")
Sys.setenv (TZ = "UTC")

getSymbols( "AA", 
						dir="/home/joel/Documents/stocks/materialSector/_data/",
	          src = "csv",
            header=TRUE ) 

tradesize <- 100000
initeq <- 100000
strategy.st <- portfolio.st <- account.st <- "firststrat"
rm.strat(strategy.st)
initPortf(portfolio.st,
          symbols = "AA",
          initDate = initdate,
          currency = "USD")
initAcct(account.st,
         portfolios = portfolio.st,
         initDate = initdate,
         currency = "USD",
         initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 200),
              label = "SMA200")

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 50),
              label = "SMA50")

add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("SMA50", "SMA200"),
                            relationship = "gt"),
           label = "longfilter")

add.signal(strategy.st,
           name = "sigComparison",
           arguments = list(columns = c("SMA50", "SMA200"),
                            relationship = "lt" ),
           label = "filterexit")

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "filterexit", sigval = TRUE,
                          orderqty = "all", ordertype = "market",
                          orderside = "long", replace = FALSE,
                          prefer = "Open"),
         type = "exit")

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longfilter", sigval = TRUE,
                          orderqty = tradesize, ordertype = "market",
                          orderside = "long", replace = FALSE,
                          prefer = "Open"),
         type = "enter")


out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

updateAcct(account.st, daterange)
updateEndEq(account.st)


tstats <- tradeStats(Portfolios = portfolio.st)

tStats <- tradeStats(Portfolios = portfolio.st )
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
#print(data.frame(t(tStats[, -c(1,2)])))
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq 

returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")

#getSymbols("AA", from = start, to = end)
# A crude estimate of end portfolio value from buying and holding SPY
#1000000 * (AA$AA.Adjusted["2020-04-21"][[1]] / AA$AA.Adjusted[[1]])

p1 <- plot(final_acct$summary$End.Eq["2015/2020"]/1000000, main = "Portfolio Equity", ylim = c(0.8, 2.5))
lines(AA$AA.Adjusted / AA$AA.Adjusted[[1]], col = "blue")
