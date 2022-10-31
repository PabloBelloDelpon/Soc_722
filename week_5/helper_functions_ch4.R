

predict_function <- function(N = 10, data, model_fit) {
  
  
  ###--- Create a vector of x values to predict mean of Y and plot lines 
  posterior_predicted <- 
    as_tibble(data) |> 
    mutate(id = row_number()) |> 
    uncount(N) |> 
    bind_cols(tidy_draws(model_fit, n = N*nrow(data))) |> 
    select(- .chain, - .iteration) |> 
    rowwise() |> 
    mutate(
      mu = alpha + beta1*weight_s + beta2*weight_s2,
      pred = rnorm(n = 1, 
                        mean = mu,
                        sd = sigma)) |>  
    group_by(id) |> 
    summarise(
      lo_bound = HPDI(pred)[1],
      up_bound = HPDI(pred)[2],
      w = unique(weight_s),
      h = unique(height),
      beta1 = beta1[1],
      beta2 = beta2[1],
      sigma = sigma[1],
      alpha = alpha[1]
    )
  
  
  
  # Plot posterior predictions over data 

  plot <- 
    posterior_predicted |> 
    ggplot(aes(w,h)) +
    geom_point(color = "lightblue", alpha = .8) +
    geom_ribbon(aes(ymax = up_bound,
                    ymin = lo_bound),
                alpha = .2) 
  
  
  
res <- 
  apply(posterior_predicted[1:100,],1, function(x){
  
  alpha <- x["alpha"]
  beta1 <- x["beta1"]
  beta2 <- x["beta2"]
  
  plot <<- 
    plot +
    stat_function(fun = function(x) alpha + beta1*x + beta2*x^2,alpha = .01)
    
  return()
  
})





