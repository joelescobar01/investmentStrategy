#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.2: luxor with slippage and transaction costs

require(quantstrat)

rm(list=ls())
setwd("/home/joel/Documents/stocks/strategy")

#stock.str='AAPL' # what are we trying it on
stock.str='AA'
currency('USD')

stock(stock.str,currency='USD',multiplier=1)

startDate="1999-12-31"
initEq=1000000

rm("account.macross",pos=.blotter)
rm("portfolio.macross",pos=.blotter)

portfolio.st='macross'
account.st='macross'
initPortf(portfolio.st,symbols=stock.str)
#> [1] "macross"
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
#> [1] "macross"
initOrders(portfolio=portfolio.st)
stratMACROSS<- strategy(portfolio.st)


stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=50),label= "ma50" )
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)[,1]), n=200),label= "ma200")

stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(columns=c("ma50","ma200"), relationship="gte"),label="ma50.gt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(column=c("ma50","ma200"),relationship="lt"),label="ma50.lt.ma200")

stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=100, ordertype='market', orderside='long'),type='enter')
stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty='all', ordertype='market', orderside='long'),type='exit')

# if you want a long/short Stops and Reverse MA cross strategy, you would add two more rules for the short side:

# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty=-100, ordertype='market', orderside='short'),type='enter')
# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=100, ordertype='market', orderside='short'),type='exit')

getSymbols(stock.str,
           src='csv', 
           dir='/home/joel/Documents/stocks/materialSector/_data', 
           from=startDate)
#> [1] "AAPL"
for(i in stock.str)
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

out<-applyStrategy(strategy=stratMACROSS , portfolios=portfolio.st)

updatePortf(Portfolio='macross',Dates=paste('::',as.Date(Sys.time()),sep=''))

chart.Posn(Portfolio='macross',Symbol=stock.str, TA=c("add_SMA(n=50,col='red')","add_SMA(n=200,col='blue')"))

tstats <- tradeStats(Portfolios = portfolio.st)

tstats[, 4:ncol(tstats)] <- round(tstats[, 4:ncol(tstats)],2)
print(data.frame(t(tstats[,-c(1,2)])))
# source("luxor.include.R")
# .fast = 10
# .slow = 30
# 
# source("luxor.getSymbols.R")
# 
# ### blotter
# 
# initPortf(portfolio.st, symbols=stock.str)
# initAcct(account.st, portfolios=portfolio.st, currency='USD')
# 
# ### quantstrat
# 
# initOrders(portfolio.st)
# 
# ### define strategy
# 
# strategy(strategy.st, store=TRUE)
# 
# ### indicators
# 
# add.indicator(strategy.st, name = "SMA",
#               arguments = list(
#                 x = quote(Cl(mktdata)[,1]),
#                 n = .fast
#               ),
#               label="nFast"
# )
# 
# add.indicator(strategy.st, name="SMA",
#               arguments = list(
#                 x = quote(Cl(mktdata)[,1]),
#                 n = .slow
#               ),
#               label="nSlow"
# )
# 
# ### signals
# 
# add.signal(strategy.st, name='sigCrossover',
#            arguments = list(
#              columns=c("nFast","nSlow"),
#              relationship="gte"
#            ),
#            label='long'
# )
# 
# add.signal(strategy.st, name='sigCrossover',
#            arguments = list(
#              columns=c("nFast","nSlow"),
#              relationship="lt"
#            ),
#            label='short'
# )
# 
# ### rules
# 
# add.rule(strategy.st, name='ruleSignal',
#          arguments=list(sigcol='long' , sigval=TRUE,
#                         orderside='short',
#                         ordertype='market',
#                         orderqty='all',
#                         TxnFees=.txnfees,
#                         replace=TRUE
#          ),
#          type='exit',
#          label='Exit2LONG'
# )
# 
# add.rule(strategy.st, name='ruleSignal',
#          arguments=list(sigcol='short', sigval=TRUE,
#                         orderside='long' ,
#                         ordertype='market',
#                         orderqty='all',
#                         TxnFees=.txnfees,
#                         replace=TRUE
#          ),
#          type='exit',
#          label='Exit2SHORT'
# )
# 
# add.rule(strategy.st, name='ruleSignal',
#          arguments=list(sigcol='long' , sigval=TRUE,
#                         orderside='long' ,
#                         ordertype='stoplimit', prefer='High', threshold=.threshold,
#                         orderqty=+.orderqty,
#                         replace=FALSE
#          ),
#          type='enter',
#          label='EnterLONG'
# )
# 
# add.rule(strategy.st, name='ruleSignal',
#          arguments=list(sigcol='short', sigval=TRUE,
#                         orderside='short',
#                         ordertype='stoplimit', prefer='Low', threshold=-.threshold,
#                         orderqty=-.orderqty,
#                         replace=FALSE
#          ),
#          type='enter',
#          label='EnterSHORT'
# )
# 
# getSymbols(stock.str, from=.from)
# for(i in stock.str){
#     assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))
# }
# 
# ###############################################################################
# 
# applyStrategy(strategy.st, portfolio.st)
# 
# print(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD)
# 
# ###############################################################################
# 
# updatePortf(portfolio.st, Symbols='AAPL', Dates=paste('::',as.Date(Sys.time()),sep=''))
# 
# chart.Posn(portfolio.st, "forex")
# 
# ###############################################################################
# 
# print(t(tradeStats(portfolio.st, 'forex')))
# 
# ###############################################################################
# 
# # save the strategy in an .RData object for later retrieval
# 
# save.strategy(strategy.st)
