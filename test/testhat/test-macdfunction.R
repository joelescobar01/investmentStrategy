source("../../chartIndicators/macdFunction.R", chdir=TRUE)
library(tidyverse)
library(tidyquant)

context("MACD Function")

test_that("MACD Divergence is valid", {
  x <- 
    as_tibble( list( "divergence"=c(1,2,3,4,5,6,7,9,10,11,12) ) ) 
  
  x <- 
    x %>% 
    RateOfChangeDivergence() 
    expect_true( x$signal.diverging[5] )
})

test_that("MACD Divergence is invalid", {
  x <- 
    as_tibble( list( "divergence"=c(10,9,8,7,6,5,4,3,2,1) ) ) 
  
  x <- 
    x %>% 
    RateOfChangeDivergence() 
    expect_false( x$signal.diverging[5] )
})


