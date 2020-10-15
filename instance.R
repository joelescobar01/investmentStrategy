source("analysis/marketWatchWeb/financialStatementInteface.R")
source("analysis/marketWatchWeb/marketWatchHTTP.R")
source("data.transfer.lib.R")

financialDocuments <-  tq_index("SP500") %>% filter( sector == "Utilities" ) %>% select( symbol ) %>% mutate( incomeStatementURL = incomeStatementURL( symbol ) ) %>% mutate( income.statement = map( incomeStatementURL, ~ .x %>% createHTMLSession() %>% fetchTable() %>% combineTables() )) %>% mutate( balanceSheetURL = balanceSheetURL(symbol) ) %>% mutate( balance.sheet = map( balanceSheetURL, ~ .x %>% createHTMLSession() %>% fetchTable() %>% combineTables() )) %>% mutate( cashFlowURL = cashFlowURL( symbol ) ) %>% mutate( cash.flow = map( cashFlowURL, ~ .x %>% createHTMLSession() %>% fetchTable() %>% combineTables() ) ) %>% select( symbol, income.statement, balance.sheet, cash.flow )

income.Statement.Analysis <- 
  tq_index("SP500") %>% 
  filter( sector == "Utilities" ) %>% 
  select( symbol ) %>% 
  mutate( incomeStatementURL = incomeStatementURL( symbol ) ) %>% 
  mutate( raw.income.statement = map( incomeStatementURL, 
                                     ~ .x %>% 
                                       createHTMLSession() %>% 
                                       fetchTable() %>% 
                                       combineTables() ) ) %>% 
  mutate( income.statement = map( raw.income.statement, 
                                 ~ .x %>% 
                                   reshapeTable() %>% 
                                   removeDashes() %>% 
                                   removePercentage() %>% 
                                   convertFinanceFormat() %>% 
                                   cleanTable() %>% 
                                   select_if( ~ !is.numeric(.) || sum(.) != 0 ) ) ) %>% 
  mutate( common.income.statement = map( incomeStatementURL, 
                                        ~ .x %>% 
                                          financialStatement %>% 
                                          commonSizeStatement() )  ) %>% 
  select( - incomeStatementURL )
