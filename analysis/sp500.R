library( tidyquant )
library( tidyverse )
library(rvest) 

sp500ListWebScrape <- function( ){
  sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_node("#constituents") %>%
    html_table() %>%
    select('Symbol','Security', 'GICS Sector', 'GICS Sub Industry') %>%
    as_tibble()
  names(sp_500) <- c("symbol", "security", "sector", "sub-industry") 
  #Remove periods 
  sp_500 <- 
    sp_500 %>% 
    mutate( symbol = gsub("\\.", "-", symbol) )
  sp_500 %>%
    lapply(function(x) x %>% unique() %>% length()) %>% 
    unlist() # show in condensed format
  return(sp_500)
}

for(ii in LETTERS[seq(2:26)]){
  letter <- ii
  url <- paste( "http://eoddata.com/stocklist/NYSE/", letter, ".htm", sep="")
  ticker <- read_html( url ) %>%
    html_node(".quotes") %>%
    html_table() 
  
  symbol <- ticker['Code'] %>% pull() 
  symbol <- gsub( "\\.", "-", symbol )
  stockMarket <- c(stockMarket, symbol ) 
}
