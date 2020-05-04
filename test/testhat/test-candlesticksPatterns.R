source("../../chartIndicators/candlestickPatterns.R")
context("Checking Doji")

 library(quantmod)
 getSymbols("MSFT")

test_that("Doji", {
  x <- doji(MSFT)
  expect_true(x['2011-07-11'])
  expect_true(x['2011-07-12'])
  expect_true(x['2011-07-13'])
  expect_false(x['2011-07-14'])
  expect_false(x['2011-07-15'])
  expect_true(x['2011-07-18'])
  expect_false(x['2011-07-19'])
})

test_that("Dragonfly Doji", {
  x <- dragonfly.doji(MSFT)
  expect_false(x['2014-10-30'])
  expect_true(x['2014-10-31'])
  expect_false(x['2014-11-03'])
})


test_that("Gravestone Doji", {
  x <- gravestone.doji(MSFT)
  expect_false(x['2015-04-29'])
  expect_true(x['2015-04-30'])
  expect_false(x['2015-05-01'])

})

# hammer
# 2011-10-18  false
# 2011-10-19  false
# 2011-10-20  false
# 2011-10-21   true
# 2011-10-24  false
# 2011-10-25  false

#x <- inverted.hammer(msft)
#x['2011-11-20/2011-11-30']

# inverted hammer
# 2011-11-21           false
# 2011-11-22           false
# 2011-11-23           false
# 2011-11-25            true
# 2011-11-28           false
# 2011-11-29           false
# 2011-11-30           false

#x <- bullish.engulf(msft)
#x['2011-11-02/2011-11-08']

# bullish engulfing
# 2011-11-02          false
# 2011-11-03          false
# 2011-11-04          false
# 2011-11-07           true
# 2011-11-08          false

#x <- bearish.engulf(msft)
#x['2011-12-15/2011-12-20']
# bearish engulfing
# 2011-12-15             false
# 2011-12-16             false
# 2011-12-19              true
# 2011-12-20             false

#x <- bullish.harami(msft)
#x['2011-12-27/2011-12-30']
# bearish engulfing
# 2011-12-15             false
# 2011-12-16             false
# 2011-12-19              true
# 2011-12-20             false

#x <- bearish.harami(msft)
#x['2011-11-02/2011-11-07']
#            bearish haraming
# 2011-11-02            false
# 2011-11-03            false
# 2011-11-04             true
# 2011-11-07            false

#x <- piercing.line(msft)
#x['2011-07-26/2011-07-30']
#           piercing line
# 2011-07-26         false
# 2011-07-27         false
# 2011-07-28          true
# 2011-07-29         false

#x <- dark.cloud.cover(msft)
#x['2011-09-16/2011-09-21']
#            dark cloud cover
# 2011-09-16            false
# 2011-09-19            false
# 2011-09-20             true
# 2011-09-21            false

#x <- kick.up(msft)
#x['2011-11-08/2011-11-14']
#           kick up
# 2011-11-08   false
# 2011-11-09   false
# 2011-11-10   false
# 2011-11-11    true
# 2011-11-14   false

#x <- kick.down(msft)
#x['2010-08-16/2010-08-19']
#           kick up
# 2011-11-08   false
# 2011-11-09   false
# 2011-11-10   false
# 2011-11-11    true
# 2011-11-14   false




#x <- three.white.soldiers(msft, n=10)
#x['2007-05-20/2007-06-01']
#candlechart(msft['2007-05-20/2007-06-01'], theme='white')
# three white soliders
# 2007-05-21                false
# 2007-05-22                false
# 2007-05-23                false
# 2007-05-24                false
# 2007-05-25                false
# 2007-05-29                false
# 2007-05-30                 true
# 2007-05-31                false
# 2007-06-01                false


#x <- three.black.crows(msft, n=10)
#x['2010-01-15/2010-01-25']
#candlechart(msft['2010-01-15/2010-02-09'], theme='white')
# three black crows
# 2010-01-15             false
# 2010-01-19             false
# 2010-01-20             false
# 2010-01-21             false
# 2010-01-22              true
# 2010-01-25             false




#x <- morning.star(msft)
#candlechart(msft['2013-06-10/2013-06-18'], theme='white')
#x['2013-06-10/2013-06-18']
# morning star
# 2013-06-10        false
# 2013-06-11        false
# 2013-06-12        false
# 2013-06-13        false
# 2013-06-14        false
# 2013-06-17         true
# 2013-06-18        false

#x <- evening.star(msft)
#candlechart(msft['2011-11-01/2011-11-11'], theme='white')
#x['2011-11-01/2011-11-11']
# evening star
# 2011-11-01        false
# 2011-11-02        false
# 2011-11-03        false
# 2011-11-04        false
# 2011-11-07        false
# 2011-11-08        false
# 2011-11-09         true
# 2011-11-10        false
# 2011-11-11        false


#x <- rising.three(msft)
#x['2012-01-10/2012-01-22']
#candlechart(msft['2012-01-10/2012-01-22'], theme='white')
# rising three
# 2012-01-10        false
# 2012-01-11        false
# 2012-01-12        false
# 2012-01-13        false
# 2012-01-17        false
# 2012-01-18        false
# 2012-01-19        false
# 2012-01-20         true

#x <-falling.three(msft)
#x['2012-01-10/2012-01-22']


#candlechart(msft, theme='white')
#addta(up.trend(msft), on=1,col='green')
#addta(down.trend(msft), on=1,col='red')
