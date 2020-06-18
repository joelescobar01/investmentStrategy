source("data.transfer.lib.R")

TexasHousing <- 
  list( "Abilene" = "DESCMSA10180", 
        "Amarillo" = "DESCMSA11100", 
        "Austin-RoundRock" = "DESCMSA12420", 
        "Beaumont-PortArthur" = "DESCMSA13140", 
        "Brownsville" = "DESCMSA15180", 
        "CollegeStation" = "DESCMSA17780", 
        "Corpus-Christi" = "DESCMSA18580", 
        "Dallas" = "DESCMSA19100", 
        "El-Paso" = "DESCMSA21340", 
        "Houston" = "DESCMSA26420", 
        "Midland" = "DESCMSA31180", 
        "San-Antonio" = "DESCMSA41700",
        "Wichita-Fall" = "DESCMSA48660" )

TexasHousingData <- 
  tibble( 
         symbol=c( "AVELISPRITX", "MEDLISPRITX", 
                   "AVELISPRIMMTX", "MEDLISPRIPERSQUFEEYYTX", 
                   "AVELISPRIYYTX", "MEDLISPRIPERSQUFEETX", 
                   "MEDLISPRIMMTX", "MEDLISPRIPERSQUFEEMMTX", 
                   "MEDLISPRIYYTX" 
                  ), 
         caption=c( "Average Listing Price in Texas", 
                    "Median Listing Price in Texas", 
                    "Average Listing Price Month-Over-Month in Texas", 
                    "Median Listing Price per Square Feet Year-Over-Year in Texas",
                    "Average Listing Price Year-Over-Year in Texas", 
                    "Median Listing Price per Square Feet in Texas", 
                    "Median Listing Price Month-Over-Month in Texas", 
                    "Median Listing Price p.Square Feet Month-Over-Month in Texas",
                    "Median Listing Price Year-Over-Year in Texas" 
                  ), 
         subCaption=c(  "Avg. listing price during the specified month",
                        "Median listing price during the specified month", 
                        "The percentage change in the average listing price from the previous month",
                        "The percentage change in the median listing price p.square foot from one year ago", 
                        "The percentage change in the average listing price from one year ago", 
                        "The median listing price per square foot within the specified month", 
                        "The percentage change in the median listing price p. square foot from the previous month", 
                        "The median listing price per square foot for the month",
                        "The percentage change in the median listing price from one year ago"
                      )
         )


TexasHousingDemand <-
  tibble( city=names( TexasHousing ), symbol=flatten_chr( TexasHousing) )


TexasHousingCity <- function(){ 
  texasData1 <- 
    fred.Data( TexasHousingDemand$symbol ) %>% 
    left_join( TexasHousingDF, by="symbol" ) %>%  
    rename( score=price ) %>% 
    ggplot( aes(x=date, colour=city) ) +
    geom_line( aes(y=score) ) +
    scale_x_date( breaks = scales::breaks_width("6 months"), 
                  labels=scales::label_date("'%y") ) +
    guides(colour="none" ) +
    labs( y="Demand Score",
          x="Date", 
          caption="The demand score is an index representing a market's average listing views on realtor.com relative to other markets." ) + 
    facet_wrap( ~ city, scale="free" ) + 
    ggtitle( "Market Demmand" ) 

  return( texasData1 ) 
}

TexasHousingState <- function(){
  texasData2 <- 
    fred.Data( TexasHousingData$symbol ) %>% 
    left_join( TexasHousingData, by="symbol" ) %>%  
    ggplot( aes(x=date, colour=symbol) ) +
    geom_line( aes(y=price) ) +
    scale_x_date( breaks = scales::breaks_width("6 months"), 
                  labels=scales::label_date("%m/%y"),
                  minor_breaks=scales::breaks_width("1 months")) +
    guides(colour="none" ) +
    labs( y="",
          x="Date" ) +  
    facet_wrap(. ~ subCaption, scale="free" ) + 
    ggtitle( "Housing Inventory" )

  return( texasData2 )
}

