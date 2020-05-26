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
stockMarketTickers <- function() {
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
}

businessInsiderURL <- 
  "https://markets.businessinsider.com" 

ticker <- 
  read_html( "https://markets.businessinsider.com/earnings-calendar#date=05/22/2020&name=&countries=&eventtypes=103,99&tab=ALL" ) %>% 
  html_nodes(".table-responsive .table" ) %>% 
  html_nodes("td a") %>% 
  html_attr("href") %>% 
  str_match( "^/stock.*financials$" ) %>% 
  discard( is.na )

siteTable <- 
  paste( businessInsiderURL, ticker[1], sep="" ) %>% 
  read_html() %>% 
  html_nodes( ".box" )

chartTitles <- 
  siteTable %>% 
  html_nodes( ".box-headlines" ) %>% 
  xml_text() %>% 
  str_replace_all( "^(\\r{1,}\\n{1,})|\\t{1,}$", "" ) %>%
  str_match( "^[Earnings|Revenue].*" ) %>% 
  discard(is.na) 

reportsTable <-
  siteTable %>% 
  html_nodes(".table" ) %>% 
  html_table()

tibble <- 
  reportsTable[[1]][,-1] %>% 
  as_tibble() %>% 
  add_column( column.name = reportsTable[[1]][,1] ) 




