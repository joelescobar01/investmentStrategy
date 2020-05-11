source("analysis/fundamentalAnalysis.R" )

financeReportName <- "analysis/financialAnalysis/MaterialSector_SP500.csv" 
generateMaterialFinancialReport <- function( symbols=basic_symbols() ){
  loadingSize <- 
    length( symbols ) 
  for(ii in seq_along(symbols)){
    print( paste("Starting Report on:", symbols[ii]) )
       duPontModel( symbols[ii] ) %>% 
        write.table( financeReportName, sep=',', append=TRUE ) 
    status <- 
      paste( (ii/loadingSize)*100 , "% complete", sep="")
    print( status ) 
  }

}

