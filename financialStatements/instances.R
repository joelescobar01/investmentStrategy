source("financialStatements/tables.R")


industrialSp500CommonSizeIncome <- 
    tq_index("SP500") %>% 
    filter( sector == "Industrials") %>% 
    select( symbol, company ) %>% 
    filter( symbol != "IR") %>% 
    filter( symbol != "RTX")  %>% 
    group_by( symbol )  %>% 
    mutate( common.size.income = map( symbol, ~ .x %>% 
                                     commonSizeIncomeStatementTable( ) ) ) %>% 
    unnest() %>% 
    filter( income.statement == "gross.income" ) %>% 
    mutate( mean.gross.income = mean( c(year1, year2, year3, year4, year5) , na.rm=TRUE) )  %>%  
    mutate( median.gross.income = median( c( year1, year2, year3, year4, year5), na.rm=TRUE)) %>% 
    mutate( stdev = sd( c( year1, year2, year3, year4, year5), na.rm=TRUE )) %>% 
    mutate( avg.annual.growth =  sum( c((year2-year1), (year3-year2) ,(year4 - year3) , (year5-year4)), na.rm=TRUE ) )
