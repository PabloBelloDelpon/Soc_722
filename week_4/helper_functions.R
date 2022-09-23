###--- Grid  Approximation Function
require(tidyverse)

grid_approx <- function(W, L, grid_n, plot = TRUE, prior_f = NULL) {
  # Define Grid
  p_grid <- seq(from=0, to=1, length.out = grid_n)
  
  # Define Prior
  prior <- rep(1, grid_n)
  
  # If I don't want a uniform then take them as an input 
  if(is.null(prior_f) == FALSE) {
    prior <- ifelse(p_grid < 0.5, 0, 1) 
  }
  
  # Compute Likelihood at each value ingrid
  likelihood <- dbinom(W, size= W + L, prob = p_grid)
  
  # Compute Product of Likelihood and Prior
  unstd_posterior <- likelihood * prior
  
  # Standardize The posterior, so it sums to 1
  posterior <- unstd_posterior / sum(unstd_posterior)
  
  # Put params and posterior together and return it
  tbl <- tibble(p_water = p_grid, posterior_probability = posterior) 
  
  if(plot == TRUE) {
    p <- tbl |> 
      ggplot(aes(p_water, posterior_probability)) +
      geom_line() +
      geom_point() +
      labs(x = "Probability of Water",
           y = "Posterior Probability",
           title = paste0("Posterior for W = ",W,", L =", L)) +
      theme_pubr(border = TRUE)
    
    return(p)
  }
  
  else{
    return(tbl)
  }
}