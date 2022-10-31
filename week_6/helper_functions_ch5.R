

###--- Make Posterior predictions

make_post_pred <- function(model_fit, data, n_preds, vars) {
  
  
  post_pred <- tidy_draws(model_fit, n = n_preds*nrow(data))  
 
  params <- colnames(post_pred)
  params <- params[!startsWith(params,".")]
  beta <- params[str_which(params, "beta")]
  
  
  posterior_predicted <- 
    data |> 
    mutate(id = row_number()) |> 
    uncount(n_preds) |> 
    bind_cols(post_pred) |> 
    select(!starts_with(".")) |> 
    rowwise() |> 
    mutate(pred = rnorm(n = 1, 
                        mean = alpha + !!sym(beta) * predictor,
                        sd = sigma)) |> 
    group_by(id) |> 
    summarise(
      lo_bound = HPDI(pred)[1],
      up_bound = HPDI(pred)[2],
      outcome = unique(outcome),
      predictor = unique(predictor),
      beta = sample(!!sym(beta),size = 1),
      alpha = sample(alpha,size = 1)
    )
  
  
  ###--- Plot Posterior Predictions
  plot_post <-
    posterior_predicted |> 
    ggplot(aes(predictor,outcome)) +
    geom_point(color = "lightblue", alpha = .8) +
    geom_abline(data = posterior_predicted |> 
                  slice_sample(n = 100,replace = TRUE),
                aes(intercept = alpha, slope = beta), alpha = .1) +
    geom_ribbon(aes(ymax = up_bound,
                    ymin = lo_bound),
                alpha = .2) +
    labs(x = vars["predictor"],
         y = vars["outcome"],
         title = "Posterior Predictions over Data")
  
  return(list(tibble = posterior_predicted,plot = plot_post))
  
}



###################
###--- Coefficient plot

coef_plot <- function(models) {
  
  # Function to extract coefficients
  coef_tbl <- 
    lapply(1:length(models),function(x){
      
      tbl <- precis(models[[x]]) # Mean and compatibility intervals for coef
      model <- names(models)[x] # The name of the model the coefs are coming from
      
      # Put it together and return it
      tbl |> 
        as_tibble() |> 
        mutate(coef = rownames(tbl),
               model = model) |> 
        relocate(model,coef)
      
    }) |> 
    bind_rows()
  
  # Plot the beta coefficients
  coef_tbl |> 
    filter(str_detect(coef,"beta") == TRUE) |> 
    ggplot(aes(mean,model, color = coef)) +
    geom_point() +
    geom_linerange(aes(xmin = `5.5%`, xmax = `94.5%`)) +
    geom_vline(aes(xintercept = 0), color = "red") +
    facet_wrap(~ coef,nrow = 2) +
    theme(legend.position = "none")
}

