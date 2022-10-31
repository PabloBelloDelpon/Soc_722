
###--- Statistical Rethinking Chapter 4
library(tidyverse)
library(rethinking)
library(ggpubr)
library(tidybayes)
library(tidybayes.rethinking)
library(patchwork)

theme_set(theme_pubr(border = TRUE))

###--- Gaussian from multiplication
big <-replicate(10000,prod(1+runif(12,0,0.5)))
small <-replicate(10000,prod(1+runif(12,0,0.01)))

hist(big)
hist(small)


log_big <- replicate(10000,log(prod(1+runif(12,0,0.5))))
hist(log_big)


######--- Gaussian Model of Height ---######
  data(Howell1)
  tbl <- Howell1
  tbl_2 <- tbl |> filter(age > 18) |> slice_sample(n = 20)
  
  
  
  ###--- Prior predictive for the height gaussian model
  sample_mu <-rnorm(1e4,178,20)
  sample_sigma <-runif(1e4,0,50)
  prior_h <-rnorm(1e4,sample_mu,sample_sigma)
  dens( prior_h)
  
  
  
  ###--- Posterior distribution
  
  # Paramter values
  mu_list <- seq(from = 150, to = 160, length.out = 100)
  sigma_list <-seq(from = 7, to = 9, length.out = 100)
  
  # Grid of parameters
  post <- expand.grid(mu = mu_list, sigma = sigma_list)
  
  # Likelihood p(mu,sigma|h_i)
  post$"likelihood" <- sapply(1:nrow(post), function(i) {
    
    sum(dnorm(tbl_2$height, post$mu[i], post$sigma[i], log=TRUE))
    
    })

  post$"prod" <- 
    post$"likelihood" + 
    dnorm(post$mu,178,20,TRUE) + # Prior mu p(mu)
    dunif( post$sigma,0,50,TRUE) # Prior sigma p(sigma)
  
  post$prob <- exp(post$prod - max(post$prod)) # Standardize
  
  
  # Visualize posterior dist
  contour_xyz(post$mu,post$sigma,post$prob)
  image_xyz(post$mu,post$sigma,post$prob)  
  
  
  # Sampling from the posterior
  post_sample <- 
    as_tibble(post) |> 
    slice_sample(n = 1e4, replace = TRUE, weight_by = prob)

  post_sample |> 
    ggplot(aes(mu,sigma)) +
    geom_point(alpha = .01, color = "blue") +
    theme_pubr(border = TRUE)
   
  # Summarizing the samples from the posterior
  
    # Marginal posterior densities of mu and sigma 
    # The jargon “marginal” here means “averaging over the other parameters.”
    post_sample |> 
      pivot_longer(cols = c(mu,sigma), names_to = "param", values_to = "value") |> 
      ggplot(aes(value)) +
      geom_density() +
      facet_wrap(~ param,scales = "free") +
      theme_pubr(border = TRUE)
    
    PI(post_sample$mu)
    PI(post_sample$sigma)
    
###--- Posterior Distribution with Quadratic Approximation (quap)
    
    # Load the data again
    tbl <- Howell1
    tbl_2 <- 
      as_tibble(tbl) |> 
      filter(age >= 18) 
    
    # Define the model
    flist <- list(
      height ~ dnorm(mu,sigma), 
      mu ~ dnorm(178,20),
      sigma ~ dunif(0,50)
    )
    
    # Fit the model to the data
    m4_1 <- quap(flist, data= tbl_2)
    
    # Gaussian approximation for each parameter's 
    # marginal posterior distribution
    precis(m4_1)
    
    # Sampling from a Quadratic Approximation (quap)
    
      # Variance-Covariance Matrix
      vcov(m4_1)
      
      # Variance
      diag(vcov(m4_1)) 
      
      # Correlation
      cov2cor(vcov(m4_1))     
      
      # Sample the (two-dimensional) posterior
      post <- extract.samples(m4_1, n=1e4)
      precis(m4_1)
      
      
      
######---- 4.4. Linear Prediction ------######
      
      ###--- Add weight as a predictor to the model of height
      
      # Mean center the variables
      tbl_2 <- 
        tbl_2 |> 
        mutate(height_c = height - mean(height),
               weight_c = weight - mean(weight))
      
      
      # Look at the data
      (p1 <- 
        tbl_2 |> 
        ggplot(aes(weight_c,height_c)) +
        geom_point() +
        theme_pubr(margin = TRUE))
      
      
      ###--- Prior Predictive Distribution
      set.seed(2971)
      N <-12 # N of lines
     
      priors <- 
        tibble(
          id = factor(1:N),
          alpha = rnorm(N,0,2), # Prior for the intercept
          beta = rlnorm(N,0,1),
          sigma = runif(N,0,10)) # Prior for the weight parameter

      

      ggplot() +
        xlim(-10,10) +
        ylim(-100,100) +
        geom_abline(data = priors, aes(intercept = alpha, slope = beta, color = id)) +
        theme_pubr() +
        theme_pubr(border = TRUE,
                   legend = "none") 
        facet_wrap(~ id)
        
      
      ###--- Add predictions with sigma
      obs_sim <- 100
      
      ###--- Prior predictive simulations
      prior_predictive_tbl <- 
        priors |> 
        uncount(obs_sim) |> 
        mutate(weight = sample(x = tbl_2$weight_c, 
                               size =  max(row_number()), 
                               replace = TRUE),
               height = rnorm(n = max(row_number()),
                              mean = alpha + beta*weight,
                              sd = sigma
                              ),
               id = paste("alpha =",round(alpha,2),
                          "beta =",round(beta,2),
                          "sigma =",round(sigma,2)))
      
      ###--- Plot them 
      prior_predictive_tbl |> 
        ggplot(aes(weight,height, color = id)) +
        geom_point() +
        geom_abline(aes(intercept = alpha, slope = beta, color = id)) +
        facet_wrap(~ id) +
        theme_pubr(legend = "none",
                   border = TRUE) 
      
      
        
      ###--- Define the model
      model <- alist(
        height_c ~ dnorm(mu,sigma),
        mu <- beta * weight_c,
        beta ~ dlnorm(0,1),
        sigma ~ dunif(0,30)
       # alpha is not defined because both variables are mean centered
      )
      
      ###--- Fit the model
      model_fit <- 
        quap(model,
          data = tbl_2)
      
      ###--- Summarise
      precis(model_fit) # Posterior mean and HPDIs
      round(vcov(model_fit),3) # Variance covariance
      pairs(model_fit) # Covariance and marginal posterior dist. 
      
      ###--- Another approach to inspecting the model
      # Draw samples from the posterior
      posterior_draws <- tidy_draws(model_fit, n = 1000)

    # Marginal posterior distribution for beta and sigma
    (posterior_plot <- 
        posterior_draws |>
        pivot_longer(cols = c(beta,sigma),
                     names_to = "parameter",
                     values_to = "value") |> 
        ggplot(aes(value, color = parameter)) +
        geom_density() +
        theme(legend.position = "none") +
        facet_wrap(~ parameter,scales = "free"))
      
      # covariance between beta and sigma
      (posterior_cov <- 
        posterior_draws |> 
        ggplot(aes(beta,sigma)) +
        geom_point(color = "blue", alpha = .2) +
        theme(aspect.ratio = 1))
      
      # Plot them together
        posterior_plot + 
        posterior_cov 


  ###--- Plotting posterior inference against the data
        
      # First just beta
      p1 +
        geom_abline(data = posterior_draws |> 
                      slice_sample(n =  100), # Draw only 100 lines
                    aes(intercept = 0, slope = beta), alpha = .1)
      
      # Now with sigma
      ppsims <- predicted_draws(model_fit,
                                newdata = tbl_2,
                                draws = 100)
      
      
     
      N <- 1000
      
      posterior_predicted <- 
        tbl_2 |> 
        mutate(id = row_number()) |> 
        uncount(N) |> 
        bind_cols(tidy_draws(model_fit, n = N*nrow(tbl_2))) |> 
        select(- .chain, - .iteration) |> 
        rowwise() |> 
        mutate(pred = rnorm(n = 1, 
                            mean = beta*weight_c,
                            sd = sigma)) |> 
        group_by(id) |> 
        summarise(
               lo_bound = HPDI(pred)[1],
               up_bound = HPDI(pred)[2],
               w = unique(weight_c),
               h = unique(height_c),
               beta = sample(beta,size = 1)
               )
      
      
      # Plot posterior predictions over data 
      posterior_predicted |> 
        ggplot(aes(w,h)) +
        geom_point(color = "lightblue", alpha = .8) +
        geom_abline(data = posterior_predicted |> 
                      slice_sample(n = 100),
                    aes(intercept = 0, slope = beta), alpha = .1) +
        geom_ribbon(aes(ymax = up_bound,
                        ymin = lo_bound),
                    alpha = .2) 
      
###--- Curves from lines

  tbl <- Howell1
  
  tbl |> 
    ggplot(aes(weight,height)) +
    geom_point()
  
  ###--- Polynomial Regression 
  
  # Model of height on weight and weight^2
  
    # First Standardize weight and include the polynomial
    tbl <- 
      tbl |> 
      mutate(weight_s = (weight - mean(weight))/sd(weight),
             weight_s2 = weight_s^2)
    
    # Define the model
    model_2 <- alist(
      height ~ dnorm(mu,sigma),
      mu <- alpha + beta1*weight_s + beta2*weight_s2,
      alpha ~ dnorm(178,20),
      beta1 ~ dlnorm(0,1),
      beta2 ~ dnorm(0,1),
      sigma ~ dunif(0,50)
    ) 

    # Fit the model
    model_fit2 <- quap(model_2,
                       data = tbl)
    
    # Summarise
    precis(model_fit2)
    
    # Predict
    
    
  ###--- Splines 
    # Cherry Blossoms data
    data(cherry_blossoms)
    tbl_cherry <- as_tibble(cherry_blossoms) |> drop_na()
    precis(tbl_cherry)

    # Day of blossom by year
    
    (plot_1 <- 
      tbl_cherry |> 
      ggplot(aes(year,doy)) +
      geom_point(color = "#fcc9b9", alpha = .6) +
      labs(y = "Day in Year",
           x = "Year",
           title = "Day of cherry blossom by year"))
    
    # Step 1. Choose the Knots
    # The knots are just values of year (the predictor) that serve as pivots for our spline.
    # Place the knots at different evenly-spaced quantiles of the predictor variable
    n_knots <- 10
    
    knot_list <- quantile(tbl_cherry$year,
                          probs = seq(0,1,length.out= n_knots))

    
   # Choose the plynomial degree
    library(splines)
    basis <- bs(tbl_cherry$year,
            knots = knot_list[-c(1,n_knots)] ,
           degree = 3 ,
           intercept = TRUE)
      
    # Plot the splines
    
      # Prepare the data
    
      tbl_splines <- 
        as_tibble(basis) |> 
        mutate(year = row_number()) |> 
        left_join(tbl_cherry |> 
                    arrange(year) |> 
                    select(year1 = year ) |>
                    mutate(year = row_number())) |> 
        select(-year) |> 
        pivot_longer(cols = - year1,
                     names_to = "knot",
                     values_to = "basis_value")
      
      # Plot splines
      tbl_splines |> 
        ggplot(aes(year1,basis_value, group = knot)) +
        geom_line()
      
    # Define and fit the model
     cherry_model <-
        alist(
          D ~ dnorm(mu,sigma), 
          mu <- a + B %*% w, 
          a ~ dnorm(100,10),
          w ~ dnorm(0,10),
          sigma ~ dexp(1)
        )

      # Fit the model
     cherry_model_fit <- 
       quap(cherry_model,
       data = list(D = tbl_cherry$doy, B = basis),
       start = list(w = rep(0, ncol(basis))))
     
     # Plot posterior predictions
     # cherry_predict <- predicted_draws(cherry_model_fit,
     #                                   newdata = list(D = tbl_cherry$doy, B = basis),
     #                                   draws = 1000)
     
     
     
     post_samples <- extract.samples(cherry_model_fit, n = 1e5)
     post_samples_tbl <- 
       as_tibble(post_samples$w) |> 
       rename_all( ~str_remove(.,"V"))
    
     weights <- colSums(post_samples_tbl)/nrow(post_samples_tbl)
     weights_tbl <- tibble(knot = names(weights), 
                         weight = weights)
   
   
  (splines_plot_post <- 
    tbl_splines |> 
    left_join(weights_tbl) |> 
     ggplot(aes(x = year1, y = basis_value*weight , group = knot)) +
     geom_line(color = "#ffb7c5", alpha = 1/2, size = 1.5))
     
     
     mu <- link(cherry_model_fit)
     mu_PI <- apply(mu,2,PI,0.97)
     mu_mean <- colMeans(mu)
     mu <- 
       as_tibble(t(mu_PI)) |> 
       rename(low = `2%`,high = `98%`) |> 
       bind_cols(mean = mu_mean)
       
      
    tbl_cherry |> 
      bind_cols(mu) |> 
      ggplot(aes(year,doy)) +
      geom_point(color = "#fcc9b9", alpha = .6) +
      geom_line(aes(x = year, y = mean)) +
      geom_ribbon(aes(ymax = low,
                     ymin = high), alpha = .5) +
         labs(y = "Day in Year",
              x = "Year",
              title = "Day of cherry blossom by year") 
     
      

      