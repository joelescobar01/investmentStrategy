source("var/economicIndicators.R") 
source("var/benchmarks.R")
source("visual.lib.R")
library(lubridate)
library( ggpubr )

chart.Yield.Curve <- function( yieldData ){
  p1 <- 
    yieldData %>% 
      ggplot( aes(x=maturity.date, y=price ) ) + 
      geom_point( aes(colour=symbol), size=3 ) + 
      geom_label( aes(label=name) ) + 
      geom_line() + 
      labs( x="Maturity Date", y="Daily Yield Rates" ) + 
      guides(colour='none') + 
      scale_x_date( labels = scales::label_date_short("%Y") ) 
  return(p1)
}

chart.Market.Health <- function( fromD="2010-01-01" ){
  p1 <-
    market.Health( fromDate=fromD ) %>% 
    ggplot( aes(x=date) ) +
    geom_line( aes(y=price) ) + 
    scale_x_date( breaks=scales::breaks_width("1 months"), labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_dollar() ) +
    labs( y="Close Price", x="Dates", title="W5000 Index" )
  return(p1) 
}

market.Monthly.Return <- function( market=market.Return(), period="monthly" ){
  monthlyReturns <-
    market %>% 
    group_by(symbol) %>% 
    tq_transmute( select=adjusted, 
                  mutate_fun=periodReturn, 
                  period="monthly") 
  return( monthlyReturns ) 
}
# to log type (compound continous ) log( 1 + return ) 

chart.Market.Return.Yearly <- function(){
  p1 <- market.Return() %>% 
    tq_transmute( select=adjusted, mutate_fun=periodReturn, period="monthly", type="log" ) %>% 
    group_by( year = year( date ) ) %>% 
    mutate( growth = cumprod( 1 +monthly.returns ) ) %>% 
    ggplot( aes(x=floor_date( date, unit="month") ) ) + 
    geom_line( aes(y=growth) ) + 
    facet_wrap( .~year, scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 months"), labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_dollar() ) +
    labs( y="Growth", x="Dates", title="SP500 Monthly Growth with principal of $1.00" ) 
  return(p1) 
}

chart.Market.Health.Yearly <- function(fromD){
  p1 <- market.Health(fromD) %>% 
    group_by( year = year( date ) ) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=close) ) + 
    facet_wrap( .~year, scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 months"), labels=scales::label_date_short() ) + scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_dollar() ) +
    labs( y="Closing Price", x="Dates", title="W5000 Index" ) 
  return(p1) 
}

chart.Market.Health <- function(fromD){
  p1 <- market.Health(fromD) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=close) ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("6 months"), 
                 labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_dollar() ) +
    labs( y="Closing Price", x="Dates", title="W5000 Index" ) 
  return(p1) 
}

chart.AAA.Bonds.Yields <- function( fromD="2010-01-01" ){
  p1 <- 
    bonds.AAA.Corporate.Yields(fromDate=fromD) %>%
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=rate) ) + 
    scale.date.axis.large() +
    scale.percent.axis()

  return(p1)
}

chart.Federal.Funds <- function( fromD="2010-01-01" ){
  p1 <- 
    monetaryPolicy(fromDate=ymd(fromD)) %>%
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=rate) ) + 
    scale.date.axis.large() +
    scale.percent.axis() +
    labs( y="Rates", x="", title="Federal Reserves Rates" ) %>%
    facet_grid( . ~ symbol, scales="free") 
  return(p1)
}
chart.PMI.Yearly <- function(){
  p1 <- 
    manufacturing.PMI() %>% 
    group_by( year = year( date ) ) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) +
    geom_hline( yintercept=50, linetype="dashed", alpha=0.5) + 
    facet_wrap( .~year, scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 months"), labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="PMI", x="Dates", title="Purchase Manager Index" ) 
  return(p1) 
}

chart.CPI.Yearly <- function(){
  p1 <- 
    consumer.Price.Index() %>% 
    group_by( year = year( date ) ) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=index.price) ) + 
    facet_wrap( .~year, scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 months"), labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="Rate of Change", x="Dates", title="Consumer Price Index Rate of Change" ) 
  return(p1) 
}

chart.PMI <- function( fromD="2010-01-01"){
  p1 <- 
    manufacturing.PMI(fromDate=fromD) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) +
    geom_hline( yintercept=50, linetype="dashed", alpha=0.5) + 
    scale_x_date( breaks=scales::breaks_width("6 months"), 
                 labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="PMI", x="", title="Purchase Manager Index" ) 
  return(p1) 
}

chart.TSI <- function(fromD="2010-01-01"){
  p1 <- 
    transport.Service.Index(fromDate=fromD) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) + 
    scale_x_date( breaks=scales::breaks_width("6 months"), 
                  labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="Index, 2000 = 100", x="", 
         title=" Freight Transportation Services Index", 
         subtitle="Changes in the Transportation Services Index reflect changes in the demand for goods and service", 
         caption="periods of economic expansion the demand for goods and services typically increases, which in turn increases the demand for transportation reflected by an increase in the TSI.") 
  return(p1) 
}

chart.Real.GDP <- function(fromD="2010-01-01"){
  p1 <- 
    real.GDP(fromDate=fromD) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) + 
    scale_x_date( breaks=scales::breaks_width("6 months"), 
                  labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="Real GDP, 2012 = 100", x="", title="Real GDP") 
  return(p1) 
}

chart.GDP.Price.Deflator <- function( fromD="2010-01-01"){
  p1 <- 
    gdp.Price.Deflator(fromDate=fromD) %>% 
    group_by( year = year( date ), quarter=quarter(date)) %>% 
    ggplot( aes(x=date) ) + 
    geom_col( aes(y=price, fill=quarter) ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 years"), 
                  labels=scales::label_date_short() ) + 
    scale_y_discrete( breaks=scales::breaks_extended(8) ) +
    labs( subtitle="GDP Price Deflation = Nominal GDP / Real GDP", 
          caption=" A measure of the level of prices of all new, domestically produced, final goods and services in an economy.",  y="Percent Change from Preceding Month", x="Dates", title="GDP Price Deflator" ) 
  return(p1) 
}

chart.Inv.To.Sale <- function(fromD="2010-01-01"){ 
  p1 <-
    inventory.Sale.Ratio(fromDate=fromD) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) + 
    scale_x_date( breaks=scales::breaks_width("6 months"), 
                  labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="Ratio", x="", title="Inventory to Sales Ratio" ) 
  return(p1) 
}

chart.Unemployment.Rate <- function(fromD="2010-01-01"){ 
  p1 <-
    unemployment.Rate(fromDate=fromD) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) + 
    scale_x_date( breaks=scales::breaks_width("6 months"), 
                  labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_percent() ) +
    labs( y="Percent", x="", title="Unemployment Rate" ) 
  return(p1) 
}
chart.TSI.Yearly <- function(){
  p1 <- 
    transport.Service.Index() %>% 
    group_by( year = year( date ) ) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) + 
    facet_wrap( .~year, scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 months"), labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="Index, 2000 = 100", x="Dates", 
         title=" Freight Transportation Services Index", 
         subtitle="Changes in the Transportation Services Index reflect changes in the demand for goods and service", 
         caption="periods of economic expansion the demand for goods and services typically increases, which in turn increases the demand for transportation reflected by an increase in the TSI.") 
  return(p1) 
}


chart.Unit.Labor.Cost.Yearly <- function(){
  p1 <- 
    unit.Labor.Cost() %>% 
    group_by( year = year( date ) ) %>% 
    ggplot( aes(x=date,y=price) ) + 
    geom_point() + 
    geom_line() + 
    facet_wrap( year ~ ., scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8) ) +
    labs( y="Index, Base = 2012", 
          x="Dates", 
          title="Manufacturing Unit Labor Cost = Compensation/Output or (Compesation/Hour)/(Output/Hour)", 
          subtitle="how much a business pays its workers to produce one unit of output", 
          caption="Increases in hourly compensation tend to increase unit labor costs, and increases in output per hour worked—labor productivity—tend to reduce them." ) 
  return(p1) 
}

chart.Unemployment.Yearly <- function(){
  p1 <- 
    unemployment.Rate() %>% 
    group_by( year = year( date ) ) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=price) ) + 
    facet_wrap( .~year, scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 months"), labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_percent() ) +
    labs( y="Percent", x="Dates", title="Unemployment Rate" ) 
  return(p1) 
}

chart.Inflation <- function(inflation){
  p1 <-
    inflation %>% 
    group_by( year = year( date ) ) %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=inflation.rate) ) +
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("4 months"), labels=scales::label_date_short() ) + 
    scale_y_continuous( breaks=scales::breaks_extended(8), labels=scales::label_percent() ) +
    labs( y="Percent", x="Dates", title="Inflation Rate" ) 
  return(p1) 
}


chart.GDP.Price.Deflator.Yearly <- function(){
  p1 <- 
    gdp.Price.Deflator() %>% 
    group_by( year = year( date ), quarter=quarter(date)) %>% 
    ggplot( aes(x=date) ) + 
    geom_col( aes(y=price, fill=quarter) ) + 
    facet_wrap( year ~., scale="free" ) + 
    guides(colour="none") + 
    scale_x_date( breaks=scales::breaks_width("1 months"), 
                  labels=scales::label_date_short() ) + 
    scale_y_discrete( breaks=scales::breaks_extended(8) ) +
    labs( subtitle="GDP Price Deflation = Nominal GDP / Real GDP", 
          caption=" A measure of the level of prices of all new, domestically produced, final goods and services in an economy.",  y="Percent Change from Preceding Month", x="Dates", title="GDP Price Deflator" ) 
  return(p1) 
}

chart.Indicator1 <- function( fromD="2010-01-01"){
  p1 <-
    ggarrange(  chart.TSI(fromD), 
                chart.Real.GDP(fromD), 
                chart.Inv.To.Sale(fromD), 
                chart.PMI(fromD), nrow=4, ncol=1, align=c("v") ) 
  return(p1) 
}
