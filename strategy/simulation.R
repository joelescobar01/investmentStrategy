if (!require("TTR")) {
  install.packages("TTR")
  library(TTR)
}
if (!require("quantstrat")) {
  install.packages("quantstrat", repos="http://R-Forge.R-project.org")
  library(quantstrat)
}

library(devtools)
library(quantmod)
source("osMaxDollar.R")

start <- as.Date("2010-01-01")
end <- as.Date("2016-10-01")

# Let's get Apple stock data; Apple's ticker symbol is AAPL. We use the quantmod function getSymbols, and pass a string as a first argument to identify the desired ticker symbol, pass "yahoo" to src for Yahoo! Finance, and from and to specify date ranges

# The default behavior for getSymbols is to load data directly into the global environment, with the object being named after the loaded ticker symbol. This feature may become deprecated in the future, but we exploit it now.

#getSymbols("AAPL", src="yahoo", from = start, to = end)

rm(list = ls(.blotter), envir = .blotter)  # Clear blotter environment
currency("USD")  # Currency being used
start <- as.Date("2010-01-01")
end <- as.Date("2011-10-01")

Sys.setenv(TZ = "MDT")  # Allows quantstrat to use timestamps
getSymbols("AA", src="yahoo", from = start, to = end)
AA_adj <- adjustOHLC(AA)
stock("AA_adj", currency = "USD", multiplier = 1)

#initDate <- "1990-01-01"
initDate <- "2010-01-01" 

# Let's get Apple stock data; Apple's ticker symbol is AAPL. We use the quantmod function getSymbols, and pass a string as a first argument to identify the desired ticker symbol, pass "yahoo" to src for Yahoo! Finance, and from and to specify date ranges

# The default behavior for getSymbols is to load data directly into the global environment, with the object being named after the loaded ticker symbol. This feature may become deprecated in the future, but we exploit it now.



strategy_st <- portfolio_st <- account_st <- "SMAC-8-13"  # Names of objects
rm.strat(portfolio_st)  # Need to remove portfolio from blotter env
rm.strat(strategy_st)   # Ensure no strategy by this name exists either
initPortf(portfolio_st, symbols = "AA_adj",  # This is a simple portfolio
          # trading AAPL only
          initDate = initDate, currency = "USD")

initAcct(account_st, portfolios = portfolio_st,  # Uses only one portfolio
         initDate = initDate, currency = "USD",
         initEq = 1000)  # Start with a million dollars

initOrders(portfolio_st, store = TRUE)  # Initialize the order container; will
# contain all orders made by strategy


strategy(strategy_st, store = TRUE)  # store = TRUE tells function to store in
# the .strategy environment

# Now define trading rules
# Indicators are used to construct signals
add.indicator(strategy = strategy_st, name = "SMA",     # SMA is a function
              arguments = list(x = quote(Cl(mktdata)),  # args of SMA
                               n = 8),
              label = "fastMA")

add.indicator(strategy = strategy_st, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 13),
              label = "slowMA")
# Next comes trading signals
add.signal(strategy = strategy_st, name = "sigComparison",  # Remember me?
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "gt"),
           label = "bull")


add.signal(strategy = strategy_st, name = "sigComparison",
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "lt"),
           label = "bear")

# Finally, rules that generate trades
add.rule(strategy = strategy_st, name = "ruleSignal",  # Almost always this one
         arguments = list(sigcol = "bull",  # Signal (see above) that triggers
                          sigval = TRUE,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open",
                          osFUN = osMaxDollar,
                          # The next parameter, which is a parameter passed to
                          # osMaxDollar, will ensure that trades are about 10%
                          # of portfolio equity
                          maxSize = quote(floor(getEndEq(account_st,
                                                         Date = timestamp) * .1)),
                          tradeSize = quote(floor(getEndEq(account_st,
                                                           Date = timestamp) * .1))),
         type = "enter", path.dep = TRUE, label = "buy")

add.rule(strategy = strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "bear",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "exit", path.dep = TRUE, label = "sell")


applyStrategy(strategy_st, portfolios = portfolio_st)
updatePortf(portfolio_st)
dateRange <- time(getPortfolio(portfolio_st)$summary)[-1]
updateAcct(portfolio_st, dateRange)
updateEndEq(account_st)
tStats <- tradeStats(Portfolios = portfolio_st, use="trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[, -c(1,2)])))
final_acct <- getAccount(account_st)

getSymbols("SPY", from = start, to = end)
# A crude estimate of end portfolio value from buying and holding SPY
1000 * (SPY$SPY.Adjusted["2016-09-30"][[1]] / SPY$SPY.Adjusted[[1]])

p1 <- plot(final_acct$summary$End.Eq["2010/2016"]/1000, main = "Portfolio Equity", ylim = c(0.8, 2.5))
lines(SPY$SPY.Adjusted / SPY$SPY.Adjusted[[1]], col = "blue")
