library(truncnorm)

fit_norm_conditional <- function(Q, size, prob_rating, max_p = 128) {
  
  m <- Q[1] # current mean
  sd <- Q[2] # current sd
  
  # get predicted probability ratings from normal distribution
  pred <- NULL
  for(pump in size) {
    pred <- c(pred,
              dnorm((pump + 1), mean = m, sd = sd) /
                sum(dnorm((pump + 1):max_p, mean = m, sd = sd)))
  }
  
  if (is.na(mean((prob_rating - pred) ** 2))) return(1)
  # calculate mean squared error
  mean((prob_rating - pred) ** 2)
  
}

fit_unif_conditional <- function(Q, size, prob_rating, max_p = 128) {
  
  minimum <- Q[1] # current minimum
  
  if (128 <= minimum) {
    # return maximum error if parameters are chosen such that the maximum is smaller
    # than the minimum to avoid NAs
    return(1)
  }
  
  # get predicted probability ratings from normal distribution that is truncated at 1
  pred <- NULL
  for(pump in size) {
    pred <- c(pred,
              dunif((pump + 1), min = minimum, max = max_p) /
                sum(dunif((pump + 1):max_p, min = minimum, max = max_p)))
  }
  
  # calculate mean squared error
  mean((prob_rating - pred) ** 2)
  
}


fit_norm_cdf <- function(Q, size, prob_rating) {
  
  m <- Q[1] # current mean
  sd <- Q[2] # current sd
  
  # get predicted probability ratings from normal distribution that is truncated at 1
  #pred <- ptruncnorm(size, mean = m, sd = sd, a = 1)
  pred <- pnorm(size, mean = m, sd = sd)
  
  # calculate mean squared error
  mean((prob_rating - pred) ** 2)
  
}

fit_unif_cdf <- function(Q, size, prob_rating) {
  
  minimum <- Q[1] # current minimum
  maximum <- Q[2] # current maximum
  
  if (maximum <= minimum) {
    # return maximum error if parameters are chosen such that the maximum is smaller
    # than the minimum to avoid NAs
    return(1)
  }
  
  # get predicted probability ratings from uniform distribution
  pred <- punif(size, min = minimum, max = maximum)
  
  # calculate mean squared error
  mean((prob_rating - pred) ** 2)
  
}

fit_norm <- function(size, prob_rating, out = "mse", dist = "CDF", max_p = 128) {
  
  m_start <- seq(1, 100, 5)
  sd_start <- seq(1, 40, 2.5)
  
  startp_model <- as.matrix(expand.grid(m_start, 
                                        sd_start))
  
  # boundaries for the optim function
  low_model <- c(-Inf, .01)
  up_model  <- c(Inf, Inf)
  
  if (dist == "CDF") {
    
    # grid search to identify a good startpoint
    grid_model <- apply(startp_model, 1, fit_norm_cdf,
                        size = size,
                        prob_rating = prob_rating)
    
    # best parameter by grid search
    #startp_model[which.min(grid_model),]
    
    # fit parameters using optim
    fit_model <- optim(startp_model[which.min(grid_model),],
                       fit_norm_cdf,
                       method = "L-BFGS-B",
                       lower = low_model,
                       upper = up_model,
                       size = size,
                       prob_rating = prob_rating)
    
  } else if (dist == "conditional") {
    
    # grid search to identify a good startpoint
    grid_model <- apply(startp_model, 1, fit_norm_conditional,
                        size = size,
                        prob_rating = prob_rating,
                        max_p = max_p)
    
    # best parameter by grid search
    # startp_model[which.min(grid_model),]
    
    # fit parameters using optim
    fit_model <- optim(startp_model[which.min(grid_model),],
                       fit_norm_conditional,
                       method = "L-BFGS-B",
                       lower = low_model,
                       upper = up_model,
                       size = size,
                       prob_rating = prob_rating,
                       max_p = max_p)
    
  }
  
  
  if (out == "mse") {
    # return mse
    return(fit_model$val)
  }
  
  if (out == "par") {
    # return parameter estimates
    return(fit_model$par)
  }
  
}


fit_unif <- function(size, prob_rating, out = "mse", dist = "CDF", max_p = 128) {
  
  min_start <- seq(1, 100, 5)
  max_start <- seq(2, max_p, 5)
  
  startp_model <- as.matrix(expand.grid(min_start, 
                                        max_start))
  
  startp_model <- startp_model[startp_model[, 1] < startp_model[, 2],]
  
  if (dist == "CDF") {
    
    # boundaries for the optim function
    low_model <- c(-Inf, -Inf)
    up_model  <- c(max_p - 1, Inf)
    
    # grid search to identify a good startpoint
    grid_model <- apply(startp_model, 1, fit_unif_cdf,
                        size = size,
                        prob_rating = prob_rating)
    
    
    # fit parameters using optim
    fit_model <- optim(startp_model[which.min(grid_model),],
                       fit_unif_cdf,
                       method = "L-BFGS-B",
                       lower = low_model,
                       upper = up_model,
                       size = size,
                       prob_rating = prob_rating)
    
  } else if (dist == "conditional") {
    
    # boundaries for the optim function
    low_model <- c(-Inf)
    up_model  <- c(max_p - 1)
    
    # grid search to identify a good startpoint
    grid_model <- apply(startp_model, 1, fit_unif_conditional,
                        size = size,
                        prob_rating = prob_rating,
                        max_p = max_p)
    
    
    # fit parameters using optim
    fit_model <- optim(startp_model[which.min(grid_model),],
                       fit_unif_conditional,
                       method = "L-BFGS-B",
                       lower = low_model,
                       upper = up_model,
                       size = size,
                       prob_rating = prob_rating,
                       max_p = max_p)
    
  }
  

  
  if (out == "mse") {
    # return mse
    return(fit_model$val)
  }
  
  if (out == "par") {
    # return parameter estimates
    return(fit_model$par)
  }
  
}
