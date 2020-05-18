source("../../analysis/fundamentalAnalysis.R")
context("Fundamental Analysis")

library(tidyverse)
library(rvest) 

#incomeStatementTable
load("marketWatchIncomeStatement.RData" ) 

test_that("marketWatchTableClean", {
    #mwTable <- marketWatchTableClean( incomeStatementTable )
    #expect_true( is_tibble( mwTable ) )


})

test_that( "incomeTable is valid", {
  incomeStatement <-
    incomeStatementURL("AA") %>% 
    createHTMLSession  %>%
    marketWatchTableClass %>% 
    marketWatchTableClean2( n=2 )

    expect_true( is_tibble( incomeStatement ) )
    incomeStatement <- 
      incomeStatement %>% 
      marketWatchNetIncomeTable 
    expect_true( is_tibble( incomeStatement ) )
})


test_that( "asset Table  is valid", {
  assetStatement <-
    balanceSheetURL("AA") %>% 
    createHTMLSession  %>%
    marketWatchTableClass %>% 
    marketWatchTableClean2( n=2 )

    expect_true( is_tibble( assetStatement ) )
    names( assetStatement )
})
