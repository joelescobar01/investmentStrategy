library(tidyquant)
library(tidyverse)
source("data.transfer.lib.R")
library(ggpubr)

DAYSTOYEAR <- 360    
CALENDARYEAR <- 365 
FOUR.WEEK.TBILL.FRED <- "DTB4WK"
FOUR.WEEK.TBILL <- 
  list( name=FOUR.WEEK.TBILL.FRED, maturity=28 ) 
THREE.MONTH.TBILL.FRED <- "DTB3" 
THREE.MONTH.TBILL <-
  list( name=THREE.MONTH.TBILL.FRED, maturity=92 )
SIX.MONTH.TBILL.FRED <- "DTB6"
SIX.MONTH.TBILL <-
  list( name=SIX.MONTH.TBILL.FRED, maturity=182 )
ONE.YEAR.TBILL.FRED <- "DTB1YR" 
ONE.YEAR.TBILL <- 
  list( name=ONE.YEAR.TBILL.FRED, maturity=365 )
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
get.TBILL <- function( tbillName=FOUR.WEEK.TBILL.FRED, maturity=28 ){
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

fetchTBill <- function( infoList=FOUR.WEEK.TBILL ){
  tbillName <-
    infoList$name 
  maturity <-
    infoList$maturity
  
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
risk.Free.Returns <- function( startDate, maturity=28 ){
  dailyYield <- 
    get.TBILL() %>%
    filter( date == startDate ) %>% 
    select( daily.yield ) %>% 
    pull()
  growth <- 
    cumprod( 1 + rep(dailyYield,maturity) ) 
  riskFree <- 
    tibble( "date"=seq(ymd(startDate), ymd(startDate)+days(maturity-1), by="1 days"), returns=growth ) 
  return( riskFree ) 
}
