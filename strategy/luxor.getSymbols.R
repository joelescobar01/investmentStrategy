#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# loading symbol data

Sys.setenv(TZ="UTC")

### packages
#
# quantstrat package will pull in some other packages:
# FinancialInstrument, quantmod, blotter, xts

require(quantstrat)

stock.str='AAPL'

#getSymbols('AAPL', from=.from, to=.to, verbose=FALSE)

stock(stock.str, 
      currency = "USD", 
      multiplier = 1)

### FinancialInstrument

currency('USD')

### quantmod

# getSymbols.FI(Symbols='GBPUSD',
#               dir=system.file('extdata',package='quantstrat'),
#               #	      dir='~/R/OHLC',
#               from=.from, to=.to
#               , extension = 'rda'
#               , use_identifier=NA
# )

#chartSeries(GBPUSD)

# ALTERNATIVE WAY TO FETCH SYMBOL DATA
#setSymbolLookup.FI(system.file('extdata',package='quantstrat'), 'GBPUSD')


### xts

