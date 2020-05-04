install.packages("devtools")
library(devtools)
# Install from github directly
require(devtools)
install_github("braverock/blotter")
devtools::install_github("braverock/quantstrat")
#install_github("braverock/quanstrat")
library(blotter)
library(quantstrat)
#require(quantstrat)
suppressWarnings(rm("order_book.macd",pos=.strategy))
suppressWarnings(rm("account.macd","portfolio.macd",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratMACD","startDate","initEq",'start_t','end_t'))


oldtz<-Sys.getenv('TZ')
if(oldtz=='') {
          Sys.setenv(TZ="GMT")
}

stock.str='AA' # what are we trying it on

#MA parameters for MACD
fastMA = 12 
slowMA = 26 
signalMA = 9
maType="EMA"

currency('USD')
stock(stock.str,currency='USD',multiplier=1)

startDate=as.Date('2015-04-23')
end_t=as.Date('2015-06-01')
initEq=1000000
portfolio.st='macd'
account.st='macd'

#rm("account.buyHold",pos=.blotter)
#rm("portfolio.buyHold",pos=.blotter)

#Porfolio: stores which stocks to be traded
#Account: stores which money transactions

initPortf(portfolio.st,symbols=stock.str) #portfolio stores which stocks to be traded 
initAcct(account.st,portfolios=portfolio.st) #account stores which money transactions 
initOrders(portfolio=portfolio.st)


strat.st<-portfolio.st
# define the strategy
strategy(strat.st, store=TRUE)

add.indicator(strat.st, name = "MACD", 
      			  arguments = list(x=quote(Cl(mktdata)),
      			                   nFast=fastMA, 
      			                   nSlow=slowMA),
      			  label='_' 
)

#two signals
add.signal(strat.st,name="sigThreshold",
    		   arguments = list(column="signal._",
    				   			        relationship="gt",
    							          threshold=0,
    							          cross=TRUE),
    		   label="signal.gt.zero"
)
   
add.signal(strat.st,name="sigThreshold",
    		   arguments = list(column="signal._",
    				                relationship="lt",
    							          threshold=0,
    							          cross=TRUE),
    	     label="signal.lt.zero"
)

#alternatives for risk stops:
## simple stoplimit order, with threshold multiplier
## #add.rule(strat.st,name='ruleSignal', arguments
## = list(sigcol="signal.gt.zero",sigval=TRUE, orderqty='all',
## ordertype='stoplimit', orderside='long', threshold=-.05,tmult=TRUE,
## orderset='exit2'),type='chain', parent='enter', label='risk',storefun=FALSE)
## # alternately, use a trailing order, also with a threshold multiplier
## #add.rule(strat.st,name='ruleSignal', arguments
## = list(sigcol="signal.gt.zero",sigval=TRUE, orderqty='all',
## ordertype='stoptrailing', orderside='long', threshold=-1,tmult=FALSE,
## orderset='exit2'), type='chain', parent='enter', label='trailingexit')
##
## # exit

####
add.rule(strat.st,name='ruleSignal', 
    		 arguments = list(sigcol="signal.gt.zero",
    				              sigval=TRUE, 
    						          orderqty=100, 
    						          ordertype='market', 
    						          orderside='long', 
    						          threshold=NULL),
    	               type='enter',
    		             label='enter',
    		             storefun=FALSE
)

add.rule(strat.st,name='ruleSignal', 
    		 arguments = list(sigcol="signal.lt.zero",
    				              sigval=TRUE, 
    						          orderqty='all', 
    						          ordertype='market', 
    						          orderside='long', 
    						          threshold=NULL,
    						          orderset='exit2'),
         type='exit',
    		 label='exit'
)

#getSymbols(	stock.str,
#						from=startDate, 
#						to='2014-06-01', 
#						src='yahoo')

getSymbols(stock.str, 
						src="csv",
						dir="/home/joel/Documents/stocks/materialSector/_data" )

start_t <-
Sys.time()
out <-
	applyStrategy(strat.st , 
								portfolios=portfolio.st,
								parameters=list(nFast=fastMA, 
																nSlow=slowMA, 
																nSig=signalMA,
																maType=maType),
								verbose=TRUE)

#end_t <- 
#	Sys.time()
print(end_t-start_t)

start_t <- 
	Sys.time()
updatePortf(Portfolio=portfolio.st,
						Dates=paste('::',as.POSIXct('2020-04-22'),sep=''))
#end_t <- 
#	Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)

chart.Posn(	Portfolio=portfolio.st,
						Symbol=stock.str )
plot(add_MACD(fast=fastMA, 
							slow=slowMA, 
							signal=signalMA,
							maType="EMA"))

#look at the order book
obook<-getOrderBook('macd')
out <- perTradeStats( 'macd', stock.str )



## set tz as it was before the demo
Sys.setenv(TZ=oldtz)
##
