library(rvest)
library(dplyr)
library( stringr ) 
library(tidyverse)
library(httr)
source("analysis/utils.R")


morningStarWebPageURL <- "https://financials.morningstar.com"
morningStarKeyRatio <- "/ratios/r.html?="

firm.Key.Ratios <- function( symbol ){
  ratioURL <- 
    glue::glue("{morningStarWebPageURL}{morningStarKeyRatio}{symbol}")

  return( ratioURL ) 
}

createHTMLSession <- function( url ){
  session <-
    html_session( url ) 
  if( http_status(session)$reason != "OK" ){
    print("Error connection to url")
    return(NA)
  }    
  return( session ) 
}

fetchMSTable <- function ( htmlSession ) {
  webTable <- 
    htmlSession %>% 
    read_html() %>% 
    html_nodes( "r_table1 text2" ) %>% 
    html_table(header = TRUE, fill = TRUE, trim=TRUE) %>% 
    map(., ~ as_tibble(., .name_repair="unique" ) ) 
  
  return( webTable )
}

