library( tidyverse )
library( rvest ) 
library(edgarWebR) 


#regex for toc ~ str_extract( table.of.content, '\\"(.*?)\\"' ) %>% str_replace_all( '\\"', "" ) )
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
  transmute( url=glue::glue("{header}{address}")) %>% 
  mutate( toc = map(url,  ~ .x %>% 
                              read_html() %>% 
                              html_nodes("table") %>% 
                              html_nodes("tr") %>% 
                              html_nodes( "a" ) %>% 
                              keep( ~ str_detect( .x, "Financial Data") ) %>% 
                              as.character %>% 
                              str_extract(  'href=\\"(.*?)\\"' ) %>% 
                              str_replace_all( '\\"', "" ) %>% 
                              str_replace_all("href=", "" ) )) %>% 
  unnest( toc ) %>% 
  mutate( financialStatementURL = glue::glue("{url}{toc}" ) ) %>% 
  mutate( statement = map( financialStatementURL, ~ read_html( .x ) %>% 
                                    html_nodes("table") %>% 
                                    html_table(fill=TRUE) %>% 
                                    as_tibble(.name_repair="unique" ) ) ) 
