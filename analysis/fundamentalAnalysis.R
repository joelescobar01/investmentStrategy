source("analysis/marketWatchWebScrape.R") 
source("analysis/financialRatios.R")
library( ggpubr ) 




parseStockMarketTable <- function( stockMarketTbbl ){
  
  t1 <- 
    stockMarketTbbl %>% 
    pmap( 
         function(symbol, name, last.sale, 
                  market.cap, ipo.year, sector, 
                  industry, ...) 
           incomeStatementURL(symbol) %>% 
             createHTMLSession() %>% 
             marketWatchTableClass() %>% 
             marketWatchTableClean() %>% 
             publishTable(ticker=symbol, 
                          companyName=name, 
                          marketCap=market.cap, 
                          ipoYear=ipo.year, 
                          Sector=sector, 
                          Industry=industry ) 
             )
  return(t1)
}


incomeStatementRatios <- function( incomeStatement ){
  profitability <- 
    incomeStatement %>% 
    calculateGrossProfit() %>%  
    calculateOperatingMargin() %>% 
    calculateNetProfitMargin() %>%
    calculateReturnOnSales() %>%
    calculateInterestCoverageRatio() %>% 
    select( year, Gross.Profit.Margin, 
           	Operating.Margin, Net.Profit.Margin, 
           	Return.On.Sales, Interest.Coverage,
					 	gross.income, revenue, 
						EBITDA, interest.expenses, 
						net.income )
  return(profitability)
}

publishTable <- function( plotTable, ticker="",companyName="", 
                                          marketCap="", ipoYear="",
                                          Sector="", Industry=""){
  bottomText <- 
    glue::glue("Sector: {Sector}\nIndustry: {Industry}\nIPO Year: {ipoYear}\nMarketCap: {marketCap}")
  
  p1 <- 
    plotTable %>% 
    mutate_if( is.numeric, round, 4 ) %>% 
    mutate_if( is.numeric, format, scientific=F ) %>% 
    ggtexttable( rows=NULL ) %>% 
    annotate_figure( top = text_grob(companyName, color = "red", face = "bold", size = 14 ),
                    bottom = text_grob(bottomText, color = "blue",
                                      hjust = 1, x = 1, face = "bold", size = 14),
                    fig.lab = ticker, fig.lab.face = "bold" ) 


  return(p1) 
}

publishIncomeStatementRatios <- function( incomeRatios, ticker="",companyName="", 
                                          marketCap="", ipoYear="",
                                          Sector="", Industry=""){
  ratiosDefinitions <- 
    paste(  
					"Gross Profit Margin: amount remaining after deducting the cost of goods sold not including indirect expenses\n",  
          "Operating Margin: profit on a dollar of sales, after paying for variable costs of production, but before paying interest or tax.\n", 
          "Net profit margin: percentage of revenue remaining after all operating expenses; exclude common stock dividends, have been deducted from the total revenue.\n",
          "Interest Coverage: how easily a company can pay interest on its outstanding debt.\n",  
          "Return on Sales: how efficiently a company is able to generate operating profit from its revenue, the revenue that results in profit rather than operating cost.\n", sep="", collapse="") %>% text_grob() %>% as_ggplot() 
	
  table1 <-
		incomeRatios %>% 
		select( year, Gross.Profit.Margin, 
						Operating.Margin, Net.Profit.Margin, 
						Return.On.Sales, Interest.Coverage ) %>% 
		mutate_if( is.numeric, round, 4 ) %>%  
		ggtexttable( rows=NULL ) 

	table2 <-
		incomeRatios %>% 
		select( year, gross.income, revenue, EBITDA, interest.expenses, net.income ) %>% 
		mutate_if( is.numeric, round, 4 ) %>%
		mutate_if( is.numeric, format, scientific=F ) %>% 
		ggtexttable( rows=NULL )
  
  gp1 <- 
	  ggarrange( table2, table1, ratiosDefinitions, nrow=3, ncol=1, heights = c(1,1, 0.5) ) 

  bottomText <- 
    glue::glue("Sector: {Sector}\nIndustry: {Industry}\nIPO Year: {ipoYear}\nMarketCap: {marketCap}")
  gp1 <-
    annotate_figure(  gp1, 
                    top = text_grob(companyName, color = "red", face = "bold", size = 14 ),
                    bottom = text_grob(bottomText, color = "blue",
                                      hjust = 1, x = 1, face = "italic", size = 10),
                    fig.lab = ticker, fig.lab.face = "bold" ) 
  return(gp1) 
} 
