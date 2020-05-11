priceSimulationMC <- function( stock ){
  # Parameters
  stock <- 
    stock %>% 
    adjustOHLC(use.Adjusted=TRUE )
  
  price_init <- last(stock) 
  dailyLogReturns <- stockDailyLogReturns(stock)  
  mean_log_returns <- mean(dailyLogReturns, na.rm = TRUE)
  sd_log_returns <- sd(dailyLogReturns, na.rm = TRUE)
  
  N     <- 252 # Number of Stock Price Simulations
  M     <- 250  # Number of Monte Carlo Simulations
  mu    <- mean_log_returns
  sigma <- sd_log_returns
  day <- 1:N
  # Simulate prices
  #price_init <- stock$CAT.Adjusted[[nrow(stock$CAT.Adjusted)]]
  set.seed(123)
  monte_carlo_mat <- matrix(nrow = N, ncol = M)
  for (j in 1:M) {
    monte_carlo_mat[[1, j]] <- price_init
    for(i in 2:N) {
      monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
    }
  }
 
  # Format and organize data frame
  price_sim <- cbind(day, monte_carlo_mat) %>%
    as_tibble() 
  nm <- str_c("Sim.", seq(1, M))
  nm <- c("Day", nm)
  names(price_sim) <- nm
  price_sim <- price_sim %>%
    gather(key = "Simulation", value = "Stock.Price", -(Day))
  end_stock_prices <- price_sim %>% 
    filter(Day == max(Day))
  probs <- c(.005, .025, .25, .5, .75, .975, .995)
  dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
  dist_end_stock_prices %>% round(2)
  
  # Inputs
  N_hist          <- nrow(stock) / 252
  p_start_hist    <- first(Ad(stock))
  p_end_hist      <-last(Ad(stock))
  N_sim           <- N / 252
  p_start_sim     <- p_end_hist
  p_end_sim       <- dist_end_stock_prices[[4]]
  # CAGR calculations
  CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1
  CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1
   
  # Format and organize data frame
  price_sim <- cbind(day, monte_carlo_mat) %>%
    as_tibble()
  nm <- str_c("Sim.", seq(1, M))
  nm <- c("Day", nm)
  names(price_sim) <- nm
  price_sim <- price_sim %>%
    gather(key = "Simulation", value = "Stock.Price", -(Day))
  
  startDate <- paste( "Using data since", first( index(stock), "to simulate" ))
  capt <- paste("Historical Annual Growth: ", CAGR_historical, "% Simulated Annual Growth: ", CAGR_sim, "%" )
  # Visualize simulation
  p <- chartPriceSimulationMC(price_sim, caption=capt) +
    labs( caption=capt, subtitle = startDate, y="Stock Price" )
  
  plot(p)
  
  return(price_sim)   
}
