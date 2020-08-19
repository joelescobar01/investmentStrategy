library( tidyverse )
library( rvest ) 
library(edgarWebR) 


filingsIndex <- 
  company_filings( "AAPL", type='10-K', count=10 ) %>% 
  as_tibble()

filings <- 
  filingsIndex %>% 
  select( href ) %>% 
  pull() %>% 
  strsplit( "/" ) %>% 
  map( ~ .x %>% head(-1) ) %>% 
  map_chr( ~ .x %>%
            append("") %>% 
            paste( collapse="/" ) ) 

documentURL <- 
  filingsIndex %>% 
  select( href ) %>% 
  pull() %>% 
  map( ~ .x %>% 
          read_html() %>% 
          html_node('table') %>% 
          html_table() %>% 
          as_tibble() %>% 
          rename_all( tolower ) %>% 
          filter( str_detect( description, fixed('10-k', ignore_case=TRUE)) | 
                  str_detect( type, fixed('10-k', ignore_case=TRUE ))) %>% 
          select( document ) %>% 
          flatten_chr 
      ) %>% 
  str_extract( ".*[^[:blank:]+iXBRL]") 
  
secData <- 
  tibble( header=filings, address=documentURL ) %>% 
  transmute( url=glue::glue("{header}{address}") ) %>%  
  pull() %>% 
  map( ~ .x %>% 
          read_html() %>% 
          html_node('table') )  
  #read_html() %>%
  #html_node('table') %>% 
  #html_table()

#  map( ~ .x %>% 
#          as_tibble( .name_repair="unique") ) %>% 
#  discard(function(x) nrow(x) == 0) %>%  
#  discard(function(x) nrow(x) == 1 ) %>% 
#  map( ~ slice(.x,1))
