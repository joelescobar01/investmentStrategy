source("../../chartIndicators/rsiFunction.R", chdir=TRUE)
library(tidyverse)
library(tidyquant)

context("RIS Function")

test_that("RSI Uptrend valid", {
  x <- 
    as_tibble( list( "rsi"=c(1,2,3,4,5,6,7,9,10,11,12) ) ) 
  
  x <- 
    x %>% 
    get.Uptrend.RSI() 
  expect_true( x$uptrend.RSI[5] )
})

test_that("RSI Uptrend ignores negative slopes", {
  x <- 
    as_tibble( list( "rsi"=c(15,14,13,12,11,10,9,8,7,6,5) ) ) 
  
  x <- 
    x %>% 
    GetUptrendRSI() 
  expect_false( x$uptrend.RSI[5] )
})

test_that("RSI Removes Overvalues", {
  x <- 
    as_tibble( list( "rsi"=c(70,22,23,72,22,15,77) ) ) 
  
  tCount <- x %>% count %>% pull() 

  x <- 
    x %>% 
  RemoveOvervaluedRSI()
  expect_true( x %>% tally() %>% pull < tCount )
})


