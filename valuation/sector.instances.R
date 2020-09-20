source("valuation/benGraham.R") 

realEstate <- 
  tq_index("SP500") %>% 
  filter( sector == "Real Estate" ) %>% 
  select( symbol ) %>% 
  pull %>% 
  map( ~ .x %>% companyFinancialDocumentsQuaterly())
