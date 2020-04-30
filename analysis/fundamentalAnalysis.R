library(rvest)
library(dplyr)
source("utils.R") 

cnbcWebPageURL <- "https://www.cnbc.com/quotes/?symbol=AGFS&qsearchterm=AGFS&tab=financials"


marketWatchWebPageURL <- "https://www.marketwatch.com" 
marketWatchWebPagePath <- "investing/stock" 
marketWatchWebPageQuery <-"financials" 
# webPageUrl + webPagePath + lowercase stock Symbol + webPageQuery 
marketWatchWebPageTableClass <- ".crDataTable"
marketWatchWebPageBalanceSheet <- "balance-sheet" 
marketWatchWebPageCashFlowSheet <- "cash-flow"

#incomeStatementRetrieval <- function( symbol, header=TRUE ){
retrieveRevenueTable <- function( symbol ){
    urlPath <- paste( marketWatchWebPageURL, marketWatchWebPagePath, tolower(symbol), marketWatchWebPageQuery, sep="/")
    incomeStatement <- list() 
    financialTable <- data.frame() 
    incomeStatement <-
      read_html(urlPath) %>% 
      html_table( ".crDataTable", header=TRUE )
    revenueTable <- incomeStatement[[1]][,1:ncol(incomeStatement[[1]])-1]  #%>% # remove 5-year trend 
    colnames(revenueTable) <- c("", "2015", "2016", "2017", "2018", "2019") # remove first column header ONLY 
    #revenueTable <- apply(revenueTable, 2, function(x) gsub("-$", NA, x))  #remove Emtpy cells 
    revenueTable <- as.data.frame( apply( revenueTable[, 1:6] ,2, function(x) gsub("\\r\\s+ ", "", x) ) ) #Clean data 
    revenueTable <- as.data.frame(apply(revenueTable,2,function(x)gsub('\\s+', '',x)))
    filename <- paste( "~/Documents/stocks/research/w4_26_Report/", toupper(symbol), "/RevenueTable.csv", sep="" )
    print(revenueTable)
    write.csv(revenueTable, filename , row.names = FALSE)
}
retrieveSGATable <- function( symbol ){
  urlPath <- paste( marketWatchWebPageURL, marketWatchWebPagePath, tolower(symbol), marketWatchWebPageQuery, sep="/")
  incomeStatement <- list() 
  financialTable <- data.frame() 
  incomeStatement <-
    read_html(urlPath) %>% 
    html_table( ".crDataTable", header=TRUE )
  sgaTable <- incomeStatement[[2]][,1:ncol(incomeStatement[[1]])-1]  #%>% # remove 5-year trend 
  colnames(sgaTable) <- c("", "2015", "2016", "2017", "2018", "2019") # remove first column header ONLY 
  filename <- paste( "~/Documents/stocks/research/w4_26_Report/", toupper(symbol), "/SGnATable.csv", sep="")
  print(sgaTable)
  write.csv(sgaTable, filename, row.names = FALSE)
  
}

retrieveRevenueTable <- function( symbol ){
  urlPath <- paste( marketWatchWebPageURL, marketWatchWebPagePath, tolower(symbol), marketWatchWebPageQuery, sep="/")
  incomeStatement <- list() 
  financialTable <- data.frame() 
  incomeStatement <-
    read_html(urlPath) %>% 
    html_table( ".crDataTable", header=TRUE )
  revenueTable <- incomeStatement[[1]][,1:ncol(incomeStatement[[1]])-1]  #%>% # remove 5-year trend 
  colnames(revenueTable) <- c("", "2015", "2016", "2017", "2018", "2019") # remove first column header ONLY 
  #revenueTable <- apply(revenueTable, 2, function(x) gsub("-$", NA, x))  #remove Emtpy cells 
  revenueTable <- as.data.frame( apply( revenueTable[, 1:6] ,2, function(x) gsub("\\r\\s+ ", "", x) ) ) #Clean data 
  revenueTable <- as.data.frame(apply(revenueTable,2,function(x)gsub('\\s+', '',x)))
  filename <- paste( "~/Documents/stocks/research/w4_26_Report/", toupper(symbol), "/RevenueTable.csv", sep="" )
  print(revenueTable)
  write.csv(revenueTable, filename , row.names = FALSE)
}

#retrievetotalAssetTable <- function(symbol){
retrievetotalAssetTable <- function( symbol ){
    urlPath <- paste( marketWatchWebPageURL, marketWatchWebPagePath, tolower(symbol), marketWatchWebPageQuery, marketWatchWebPageBalanceSheet, sep="/")
    incomeStatement <- list() 
    financialTable <- data.frame() 
    incomeStatement <-
      read_html(urlPath) %>% 
      html_table( ".crDataTable", header=TRUE )
    totalAssetTable <- incomeStatement[[1]][,1:ncol(incomeStatement[[1]])-1]  #%>% # remove 5-year trend 
    colnames(totalAssetTable) <- c("", "2015", "2016", "2017", "2018", "2019") # remove first column header ONLY 
    #totalAssetTable <- apply(totalAssetTable, 2, function(x) gsub("-$", NA, x))  #remove Emtpy cells 
    totalAssetTable <- as.data.frame( apply( totalAssetTable[, 1:6] ,2, function(x) gsub("\\r\\s+ ", "", x) ) ) #Clean data 
    totalAssetTable <- as.data.frame(apply(totalAssetTable,2,function(x)gsub('\\s+', '',x)))
    filename <- paste( "~/Documents/stocks/research/w4_26_Report/", toupper(symbol), "/totalAssetsTable.csv", sep="" )
    print(totalAssetTable)
    write.csv(totalAssetTable, filename , row.names = FALSE)
}

#totaltotalAssets
retrieveTotalAssetTable <- function( symbol ){
  urlPath <- paste( marketWatchWebPageURL, marketWatchWebPagePath, tolower('asix'), marketWatchWebPageQuery, marketWatchWebPageBalanceSheet, sep="/")
  incomeStatement <- list() 
  financialTable <- data.frame() 
  incomeStatement <-
    read_html(urlPath) %>% 
    html_table( ".crDataTable", header=TRUE )
  totalAssetTable <- incomeStatement[[2]][,1:ncol(incomeStatement[[1]])-1]  #%>% # remove 5-year trend 
  colnames(totalAssetTable) <- c("", "2015", "2016", "2017", "2018", "2019") # remove first column header ONLY 
  #totalAssetTable <- apply(totalAssetTable, 2, function(x) gsub("-$", NA, x))  #remove Emtpy cells 
  totalAssetTable <- as.data.frame( apply( totalAssetTable[, 1:6] ,2, function(x) gsub("\\r\\s+ ", "", x) ) ) #Clean data 
  totalAssetTable <- as.data.frame(apply(totalAssetTable,2,function(x)gsub('\\s+', '',x)))
  filename <- paste( "~/Documents/stocks/research/w4_26_Report/", toupper('asix'), "/totalAssetsTable.csv", sep="" )
  print(totalAssetTable)
  write.csv(totalAssetTable, filename , row.names = FALSE)
}

retrievecashFlowTable <- function( symbol, colNum ){
  urlPath <- paste( marketWatchWebPageURL, marketWatchWebPagePath, tolower(symbol), marketWatchWebPageQuery, marketWatchWebPageBalanceSheet, sep="/")
  incomeStatement <- list() 
  financialTable <- data.frame() 
  incomeStatement <-
    read_html(urlPath) %>% 
    html_table( ".crDataTable", header=TRUE )
  cashFlowTable <- incomeStatement[[colNum]][,1:ncol(incomeStatement[[1]])-1]  #%>% # remove 5-year trend 
  colnames(cashFlowTable) <- c("", "2015", "2016", "2017", "2018", "2019") # remove first column header ONLY 
  #cashFlowTable <- apply(cashFlowTable, 2, function(x) gsub("-$", NA, x))  #remove Emtpy cells 
  cashFlowTable <- as.data.frame( apply( cashFlowTable[, 1:6] ,2, function(x) gsub("\\r\\s+ ", "", x) ) ) #Clean data 
  cashFlowTable <- as.data.frame(apply(cashFlowTable,2,function(x)gsub('\\s+', '',x)))
  filename <- paste( "~/Documents/stocks/research/w4_26_Report/", toupper(symbol), "/cashFlowsTable.csv", sep="" )
  print(cashFlowTable)
  write.csv(cashFlowTable, filename , row.names = FALSE)
}

retrieveCashFlowTable <- function( symbol, colNum, file ){
  urlPath <- paste( marketWatchWebPageURL, marketWatchWebPagePath, tolower(symbol), marketWatchWebPageQuery, marketWatchWebPageCashFlowSheet, sep="/")
  incomeStatement <- list() 
  financialTable <- data.frame() 
  incomeStatement <-
    read_html(urlPath) %>% 
    html_table( ".crDataTable", header=TRUE )
  cashFlowTable <- incomeStatement[[colNum]][,1:ncol(incomeStatement[[1]])-1]  #%>% # remove 5-year trend 
  colnames(cashFlowTable) <- c("", "2015", "2016", "2017", "2018", "2019") # remove first column header ONLY 
  #cashFlowTable <- apply(cashFlowTable, 2, function(x) gsub("-$", NA, x))  #remove Emtpy cells 
  cashFlowTable <- as.data.frame( apply( cashFlowTable[, 1:6] ,2, function(x) gsub("\\r\\s+ ", "", x) ) ) #Clean data 
  cashFlowTable <- as.data.frame(apply(cashFlowTable,2,function(x)gsub('\\s+', '',x)))
  filename <- paste( "~/Documents/stocks/research/w4_26_Report/", toupper(symbol), "/", file ,".csv", sep="" )
  print(cashFlowTable)
  write.csv(cashFlowTable, filename , row.names = FALSE)
}

