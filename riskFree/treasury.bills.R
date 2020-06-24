library(tidyquant)
library(tidyverse)
source("data.transfer.lib.R")
library(ggpubr)

DAYSTOYEAR <- 360    
CALENDARYEAR <- 365 
FOUR.WEEK.TBILL <- "DTB4WK"
THREE.MONTH.TBILL <- "DTB3"
SIX.MONTH.TBILL <- "DTB6"
ONE.YEAR.TBILL <- "DTB1YR" 
TWO.YEAR.TREASURY <- "DGS2"
FIVE.YEAR.TREASURY <- "DGS5"
TEN.YEAR.TREASURY <- "DGS10"
TWENTY.YEAR.TREASURY <- "DGS20" 
THIRTY.YEAR.TREASURY <- "DGS30"

zero.Coupon.Yield <- function( faceValue, purchasePrice, maturityDays ){
  yield <- 
    (faceValue/purchasePrice)^(1/maturityDays) - 1 
  return(yield)
}

bank.Discount.Purchase.Price <- function( parValue, discountRate, maturityDays ){
  price <-
    parValue * ( 1 - (maturityDays*(rate))/360 ) 
    return( price ) 
}

get.TBILL <- function( tbillName=FOUR.WEEK.TBILL, maturity=28 ){
  tbill <- 
    fred.Data( tbillName ) %>% 
    drop_na() %>%
    mutate( discount.yield = price/100 ) %>% 
    rename( rate = price ) %>% 
    mutate( pp = (100*(1-(maturity*discount.yield)/360) ) ) %>%
    mutate( interest.yield = ((100-pp)/pp)*(365/maturity ) ) %>% 
    mutate( daily.yield = (100/pp)^(1/maturity)-1 ) 
  return( tbill ) 
}

bank.Discount.Yield <- function( parValue, price, maturityDays ){
  discountRate <- 
    ( (parValue-price)/parValue )* (DAYSTOYEAR/maturityDays )
  return( discountRate ) 
}

treasury.Bond.Coupon <- function( parValue, price, daysToMaturity ) {
  #formula used is 
  #p[1+(r-y/2)(i/y)](1+i/2)=100  
  a <- (daysToMaturity/(2*CALENDARYEAR))-0.25 
  b <- daysToMaturity/CALENDARYEAR
  c <- (price-parValue)/price 
  print( glue::glue("a = {a}, b = {b}, c = {c}" ) ) 
  cey <- 
    ( -b + sqrt( (b^2 - 4*a*c) ) )/(2*a)

  return(cey) 
}
