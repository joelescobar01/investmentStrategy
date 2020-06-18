library(tidyquant)
library(tidyverse)
library(ggpubr)

DAYSTOYEAR <- 360    
CALENDARYEAR <- 365 

discountBasisPrice <- function( parValue, discountRate, maturityDays ){
  price <-
    parValue*(1-(discountRate*maturityDays)/DAYSTOYEAR) 
  return( price ) 
}

bankDiscountBasisYield <- function( parValue, price, maturityDays ){
  discountRate <- 
    ( (parValue-price)/parValue )* (DAYSTOYEAR/maturityDays )
  return( discountRate ) 
}

#Holding period return is thus the total return received from holding an asset or portfolio of assets over a specified period of time, generally expressed as a percentage 
holdingPeriodYield <- function( parValue, price, dividend=0 ){
  roi <-
    (parValue-price+dividend)/price 
  return(roi)
}

# maturityDays = holding period 
# Yield to maturity is the total rate of return that will have been earned by a bond when it makes all interest payments and repays the original principal.
effectiveAnnualYield <- function( parValue, price, maturityDays, dividend=0 ){
  hpy <- 
    holdingPeriodYield( parValue, price, dividend ) 
  eay <- 
    ((1 + hpy)^(CALENDARYEAR/maturityDays ))  
  return( eay-1 ) 
}

couponEquivYield_6Months <- function( parValue, price, daysToMaturity ) {
  cey <- 
    holdingPeriodYield(parValue, price )* (CALENDARYEAR/daysToMaturity )
  return(cey) 
}

couponEquivYield_12Months <- function( parValue, price, daysToMaturity ) {
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
