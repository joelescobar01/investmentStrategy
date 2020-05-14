library(tidyquant)
library(tidyverse)
library(ggpubr)

tbill3MDailyRate <- function(  ){
  tbillLogRate <- 
    tq_get( "DGS3MO", 
           get="economic.data" ) %>%
    drop_na() 
  #tbillLogRate <- tbillLogRate[!is.infinite(rowSums(tbillLogRate)),] 
  #tbillLogRate <- na.omit( tbillLogRate )
  
  #tbillLogRate <- 
  #  tail( tbillLogRate, n=92 )
  #colnames(tbillLogRate) <- c( "Rate")
  #tbillDF <- zooToDataFrame(tbillLogRate)
  #
  #return(tbillLogRate)
}

tbill3MDailyLogtRate <- function( ){
  tbillLogRate <- 
    getSymbols( "DGS3MO", 
                src='FRED', 
                auto.assign = FALSE ) %>% 
    dailyReturn(type='log')
  
  tbillLogRate <- tbillLogRate[!is.infinite(rowSums(tbillLogRate)),] 
  tbillLogRate <- na.omit( tbillLogRate )
  
  tbillLogRate <- 
    tail( tbillLogRate, n=92 )
  colnames(tbillLogRate) <- c( "Log.Returns")
  tbillDF <- convertStockToDataFrame(tbillLogRate)
  
  return(tbillLogRate)
}

week13Rates <- function( dailyRate ){
  dailyRate <- 
    tail( dailyRate, n=92 )
  colnames(dailyRate) <- c( "Rate")
  dailyRateDF <- zooToDataFrame( dailyRate)
  return(dailyRate) 
}

dailyRiskFreeRates <- function( tbillDailyRates ){
  #ep <- endpoints(prices, "months")
  dailyYield <- (1+(tbill/100))^(1/252) - 1
 # threeMoPrice <- cumprod(1+dailyYield)
  return(dailyYield)
#  threeMoPrice <- threeMoPrice["2019-12-16::"]
#  threeMoPrice <- threeMoPrice[endpoints(threeMoPrice, "days"),]
#  return(threeMoPrice)  
}

scalePriceAxis <- function( priceVector ){
  priceRange <-
    seq(  from=min(priceVector), 
          to=max(priceVector) ) 
  return( priceRange ) 
}

get.Industry.Production.Index <- function(...){
  index <-
    tq_get( "INDPRO", 
            get="economic.data" 
           )
  return( index ) 
}

get.House.Mortgage.Fixed.Rate <- function(...) {
  index <-
    tq_get( "MORTGAGE15US", 
            get="economic.data", 
            ... )
  index <- 
    index %>% 
    mutate( rate = price/100 ) 
  return( index ) 
}

get.TBill.3Months.Annual.Rate <- function( ... ){
  index <-
    tq_get( "DTB3", 
            get="economic.data", 
            ... )
  index <- 
    index %>% 
    mutate( Discount.Base.Annual.Return = price ) 


  return( index ) 
}

get.TBill.1Year.Annual.Rate <- function( ... ){
  index <-
    tq_get( "DTB1YR", 
            get="economic.data", 
            ... )
  index <- 
    index %>% 
    mutate( Discount.Base.Annual.Return = price ) 


  return( index ) 
}
get.Diesel.Sales.Price <- function(...) {
  index <-
    tq_get( "GASDESM", 
            get="economic.data", 
            ... )
  return( index ) 
}

get.Electric.Power.Generation.PPI <- function( ... ){
  index <- 
    tq_get( "PCU2211102211104", 
            get="economic.data", 
            ... ) 
  return( index ) 
}

get.Electric.Transmission.PPI <- function( ... ){
  index <- 
    tq_get( "CAPUTLG2211S", 
            get="economic.data", 
            ... ) 
  return( index ) 
}

chart.Industry.Production.Index <- function( indexTbl, xBreak="6 months" ){
   g1 <- 
    ggplot( data = indexTbl, aes( x=date) ) + 
    geom_line( aes( y=price) ) + 
    scale_x_date(   date_labels="%b %Y",
                    date_breaks=xBreak ) + 
    labs( title="Industry Production Index", 
          x="Date", 
          y="2012 Base Index" 
    ) + 
    theme_bw() 

  return(g1) 
}

chart.Electric.Power.Generation.PPI <- function( indextbl, xBreak="6 months" ){
  g1 <- 
    ggplot( data = indextbl, aes( x=date) ) + 
    geom_line( aes( y=price) ) + 
    scale_x_date(   date_labels="%b %y",
                    date_breaks=xBreak ) + 
    labs( title="electric power generation",
          subtitle="this industry comprises facilities that convert other forms of energy into electric energy",
          caption="price changes for the initial commercial transaction received by power generating establishments", 
          x="date", 
          y="price, base year = dec 2003" 
    ) + 

    theme_bw() 

  return(g1)
}

chart.Electric.Power.Transmission.PPI <- function( indexTbl, xBreak="6 months" ){
   g1 <- 
    ggplot( data = indexTbl, aes( x=date) ) + 
    geom_line( aes( y=price) ) + 
    scale_x_date(   date_labels="%b %Y",
                    date_breaks=xBreak ) + 
    labs( title="Electric Power generation, transmission and distribution", 
          subtitle="capacity utilization rate is equal to an output index divided by a capacity index",
          caption= "Maximum output given the circumstances", 
          #cpriceA <- companyA %>% slice( n() ) %>% select( close ) %>% pullaption=" capacity indexes attempt to capture the concept of sustainable maximum output-the greatest level of output a plant can maintain within the framework of a realistic work schedule, after factoring in normal downtime and assuming sufficient availability of inputs to operate the capital in place.", 
          x="Date", 
          y="Percent of Capacity" 
    ) + 
    theme_bw() 

  return(g1) 
}
chart.Diesel.Sales.Price <- function( indexTbl, xBreak="6 months" ){
   g1 <- 
    ggplot( data = indexTbl, aes( x=date) ) + 
    geom_line( aes( y=price) ) + 
    scale_x_date(   date_labels="%b %Y",
                    date_breaks=xBreak ) + 
    labs( title="Diesel Sales Price", 
          x="Date", 
          y="Dollar per Gallon" 
    ) + 
    theme_bw() 

  return(g1) 
}

chart.Diesel.Sales.Price.Change <- function( indexTbl, xBreak="6 months" ){
    diesel <- 
      indexTbl %>% 
      filter(n() > 1) %>%
      mutate(price = price - lag(price)) 
  g1 <- 
    ggplot( data = diesel, aes( x=date) ) + 
    geom_line( aes( y=price) ) + 
    scale_x_date(   date_labels="%b %Y",
                    date_breaks=xBreak ) + 
    labs( title="Diesel Change in Sales Price", 
          x="Date", 
          y="Price Change for Diesel" 
    ) + 
    theme_bw() 

  return(g1) 
}
chart.FRED.15Fixed.Rate <- function( indexTbl, xBreak="6 months" ){
   g1 <- 
    ggplot( data = indexTbl, aes( x=date) ) + 
    geom_line( aes( y=price )) + 
    scale_x_date(   date_labels="%b %Y",
                    date_breaks=xBreak ) +
    labs( title="15-Year Fixed Rate Mortgage",
          x="Date", 
          y="Percent" 
    ) + 
    theme_bw() 

  return(g1) 
}

chart.3TBill.Month.Rate <- function( indexTbl, xBreak="6 months" ){
   g1 <- 
    ggplot( data = indexTbl, aes( x=date) ) + 
    geom_line( aes( y=price )) + 
    scale_x_date(   date_labels="%b %Y",
                    date_breaks=xBreak ) +
    labs( title="Treasury Bill 3 Months Annual Returns",
          subtitle="Discount Basis, uses Bank Days (360)", 
          x="Date", 
          y="Percent" 
    ) + 
    theme_bw() 

  return(g1) 
}

chart.TBill.1Year.Rate <- function( indexTbl, xBreak="6 months" ){
   g1 <- 
    ggplot( data = indexTbl, aes( x=date) ) + 
    geom_line( aes( y=price )) + 
    scale_x_date(   date_labels="%b %Y",
                    date_breaks=xBreak ) +
    labs( title="Treasury Bill 1 Year Annual Returns",
          subtitle="Discount Basis, uses Bank Days (360)", 
          x="Date", 
          y="Percent" 
    ) + 
    theme_bw() 

  return(g1) 
}


interval.Chart.Generation.6Months <- function(startD, endD, symbol=c(), nMonths=6, logPrice=FALSE ) {
  if( length( symbol ) != 4 )
    return(NA)
  
  startDate <- startD 
  endDate <- startD + months(nMonths) 
  
  dieselPrice <- get.Diesel.Sales.Price() 
  electricGen <- get.Electric.Power.Generation.PPI() 
  electricTran <- get.Electric.Transmission.PPI() 
  industry <- get.Industry.Production.Index() 
  home <- get.House.Mortgage.Fixed.Rate() 
  symbolA <- tq_get( symbol[1], get='stock.prices' )  
  symbolB <- tq_get( symbol[2], get='stock.prices' ) 
  symbolC <- tq_get( symbol[3], get="stock.prices" )
  symbolD <- tq_get( symbol[4], get='stock.prices' ) 
  
  yLabel <- "Close" 

  if( logPrice == TRUE ){
    yLabel <- "Log Close"
    
    symbolA <-
      symbolA  %>% 
      mutate( close = log(close) )
    symbolB <-
      symbolB  %>% 
      mutate( close = log(close) )

    symbolC <-
      symbolC  %>% 
      mutate( close = log(close) )
    
  symbolD <-
      symbolD  %>% 
      mutate( close = log(close) )
  }

  fileSuffix <- paste( symbol, collapse="_" ) 
  fileDir <- "/home/joel/Documents/stocks/research/plots" 
  while( endDate < endD ){
    
    companyA <- symbolA %>% filter( date >= startDate & date <= endDate ) 
    companyB <- symbolB %>% filter( date >= startDate & date <= endDate ) 
    companyC <- symbolC %>% filter( date >= startDate & date <= endDate ) 
    companyD <- symbolD %>% filter( date >= startDate & date <= endDate ) 

    p1 <- dieselPrice %>% filter( date >= startDate & date <= endDate ) %>% chart.Diesel.Sales.Price( xBreak="1 months" )
    #p2 <- electricTran %>% filter( date >= startDate & date <= endDate ) %>% chart.Electric.Power.Transmission.PPI( xBreak="1 months" )
    p2 <- dieselPrice %>% filter( date >= startDate & date <= endDate ) %>%  chart.Diesel.Sales.Price.Change( xBreak="1 months" )

    p3 <- electricGen %>% filter( date >= startDate & date <= endDate ) %>% chart.Electric.Power.Generation.PPI( xBreak="1 months" )
    p4 <- home %>% filter( date >= startDate & date <= endDate ) %>% chart.FRED.15Fixed.Rate( xBreak="1 months" ) 
    p5 <- industry %>% filter( date >= startDate & date <= endDate ) %>% chart.Industry.Production.Index( xBreak="1 months" ) 

    priceA <- companyA %>% slice( n() ) %>% select( close ) %>% pull 
    priceB <- companyB %>% slice( n() ) %>% select( close ) %>% pull
    priceC <- companyC %>% slice( n() ) %>% select( close ) %>% pull
    priceD <- companyD %>% slice( n() ) %>% select( close ) %>% pull
  
    pricePlot <- ggplot() + 
      geom_line( data=companyA, aes( x=date, y=close, colour=symbol )) + 
      geom_text( x=endDate, aes(y=priceA, colour=symbol[1], label=symbol[1]), nudge_y=0.10)+
      geom_line( data=companyB, aes( x=date, y=close, colour=symbol) ) + 
      geom_text( x=endDate, aes(y=priceB, colour=symbol[2], label=symbol[2]), nudge_y=0.10)+
      geom_line( data=companyC, aes( x=date, y=close, colour=symbol) ) +
      geom_text( x=endDate, aes(y=priceC, colour=symbol[3], label=symbol[3]),  nudge_y=0.10)+
      geom_line( data=companyD, aes( x=date, y=close, colour=symbol) ) + 
      geom_text( x=endDate, aes(y=priceD, colour=symbol[4], label=symbol[4]), nudge_y=0.10)+
      scale_x_date( date_label="%b %Y", 
                   date_breaks='1 months' ) + 
      labs( y=yLabel, x="" ) +
      theme(legend.position="none" )

    #gp1 <- ggarrange( pricePlot, p2, p5, align=c("v"), ncol=1, nrow=3)
    #fileName <- paste( startDate, endDate, fileSuffix, "II_ElectricTran.png", sep="_")
    #fileName <- paste( fileDir, fileName, sep="/" ) 
    #ggsave(fileName, gp1)
    
    #gp5 <- ggarrange( pricePlot, p1, p2, align=c("v"), ncol=1, nrow=3)
    #fileName <- paste( startDate, endDate, fileSuffix, "PriceChange.png", sep="_")
    #fileName <- paste( fileDir, fileName, sep="/" ) 
    #ggsave(fileName, gp5)

    #gp2 <- ggarrange( pricePlot, p3, p4, align=c("v"), ncol=1, nrow=3)
    #fileName <- paste( startDate, endDate, fileSuffix, "ElectricTran_HomeRate.png", sep="_")
    #fileName <- paste( fileDir, fileName, sep="/" )
    #ggsave(fileName, gp2)
    
    gp3 <- ggarrange( pricePlot, p1, p2, align=c("v"), ncol=1, nrow=3)  
    fileName <- paste( startDate, endDate,  fileSuffix ,"Diesel.png", sep="_")
    fileName <- paste( fileDir, fileName, sep="/" )
    ggsave(fileName, gp3)
    
    startDate <- endDate 
    endDate <- endDate + months(nMonths) 
  }
}
