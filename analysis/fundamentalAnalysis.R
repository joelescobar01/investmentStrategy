source("analysis/marketWatchWebScrape.R")
source("analysis/financialRatios.R")

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

publishIncomeStatementRatios <- function( incomeRatios ){
  ratiosDefinitions <- 
    paste( 
					"Gross Profit Margin: amount remaining after deducting the cost of goods sold not including indirect expenses\n",  
          "Operating Margin: profit on a dollar of sales, after paying for variable costs of production, but before paying interest or tax.\n", 
          "Net profit margin: percentage of revenue remaining after all operating expenses; exclude common stock dividends, have been deducted from the total revenue.\n",
          "Interest Coverage: how easily a company can pay interest on its outstanding debt.\n",  
          "Return on Sales: how efficiently a company is able to generate operating profit from its revenue, the revenue that results in profit rather than operating cost.\n", sep="" ) %>% text_grob() %>% as_ggplot() 
	
	
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
	ggarrange( table2, table1, ratiosDefinitions, nrow=3, ncol=1, heights = c(1,1, 0.5) ) 
} 
