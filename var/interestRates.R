
BANK.CALENDAR.YEAR <- 360    
CALENDAR.YEAR <- 365 


compound.Interest.Accrued <- function( principal, rate, 
                                compoundingPeriod ){
  #compoundingPeriod is the number of compound periods ( how many "interest on
  #interst" ) not considering the principal  
  interest <- 
    principal*( ((1+rate)^(compoundingPeriod) ) - 1 ) 
  return( interest ) 
}

apr.Daily.Periodic.Rate <- function(APR){
  daily.rate.percent <- 
    APR/365 
  daily.rate <- 
    (daily.rate.percent/100) + 1
  effective.APR <- 
    daily.rate^(365) - 1  
  return( effective.APR ) 
}

interest.Rate <- function( parValue, purchaseValue ){
  returnRate <- 
    (parValue-purchaseValue)/purchaseValue 

  return( returnRate ) 
}

discount.Rate <- function( parValue, purchaseValue ){
  returnRate <- 
    ( parValue-purchaseValue)/parValue 

  return( returnRate )
}

daily.Interest.Rate <- function( parValue, purchaseValue, maturity=CALENDAR.YEAR ){
  daily.Return <- 
    interest.Rate( parValue, purchaseValue )/maturity
  
  return( daily.Return ) 
}

