# code to create sequences for the two bart conditions
library(moments)
library(truncnorm)
library(tidyverse)

set.seed(111)
t1 <- Sys.time()
d_m <- 32 # distribution mean
d_sd <- 10 # distribution standard deviation

### Function definitions =======================================================
get_uniform <- function(d_m) {
  
  # uniform condition
  uniform <- round(runif(n = 30, min = 4, max = 64))
  
  while(length(unique(uniform)) < 30 | mean(uniform) > d_m + .2 | 
        mean(uniform) < d_m - .2 | sum(uniform >= 32) <= 14 |
        sum(uniform >= 32) >= 16){
    uniform <- round(runif(n = 30, min = 1, max = 64))
    
  }
  
  iter <- 1
  while(mean(uniform[1:10]) > 32.25 | mean(uniform[1:10]) < 31.25 |
        mean(uniform[11:20]) > 32.25 | mean(uniform[11:20]) < 31.25 |
        mean(uniform[21:30]) > 32.25 | mean(uniform[21:30]) < 31.25 |
        any(uniform[1:3] <= 10) | any(uniform[1:3] >= 54)) {
    uniform <- sample(uniform)
    iter <- iter + 1
    if (iter > 10000) stop("no sequence produced")
  }
  
  return(uniform)
  
}




get_normal <- function(d_m, d_sd, kurt_thresh = .1) {
  
  
  # normal condition
  normal <-  round(rtruncnorm(n = 30, mean = d_m, sd = d_sd, a = 1, b = 64))
  while(mean(normal) > d_m + .2 | mean(normal) < d_m - .2 |
        sd(normal) > d_sd + .2 | sd(normal) < d_sd - .2 |
        sum(normal >= 32) <= 14 | sum(normal >= 32) >= 16 |
        skewness(normal) > .1 | skewness(normal) < -.1 |
        kurtosis(normal) > (3 + kurt_thresh) | kurtosis(normal) < (3 - kurt_thresh) |
        any(normal[1:3] <= 10) | any(normal[1:3] >= 54) |
        any(normal < 1) | any(normal > 64)){
    
    normal <- round(rtruncnorm(n = 30, mean = d_m, sd = d_sd, a = 1, b = 64))
    
  }
  
  iter <- 1
  while(mean(normal[1:10]) > 32.25 | mean(normal[1:10]) < 31.25 |
        mean(normal[11:20]) > 32.25 | mean(normal[11:20]) < 31.25 |
        mean(normal[21:30]) > 32.25 | mean(normal[21:30]) < 31.25 |
        any(normal[1:3] <= 10) | any(normal[1:3] >= 54)) {
    normal <- sample(normal)
    iter <- iter + 1
    if (iter > 10000) stop("no sequence produced")
  }
  
  return(normal)
  
  
}


### create distributions

## U(1, 64)
uniform <- try(get_uniform(d_m), silent = TRUE)

while(class(uniform) == "try-error") {
  
  uniform <- try(get_uniform(d_m), silent = TRUE)
  
}


## N(32, 6)
normal_low <- try(get_normal(d_m, 6), silent = TRUE)

while(class(normal_low) == "try-error") {
  
  normal_low <- try(get_normal(d_m, 6), silent = TRUE)
  
}



## N(32, 12)
normal_moderate <- try(get_normal(d_m, 12), silent = TRUE)

while(class(normal_moderate) == "try-error") {
  
  normal_moderate <- try(get_normal(d_m, 12), silent = TRUE)
  
}



## N(32, 18)
normal_high <- try(get_normal(d_m, 18, 1), silent = TRUE)

while(class(normal_high) == "try-error") {
  
  normal_high <- try(get_normal(d_m, 18, 1), silent = TRUE)
  
}


dists <- data.frame(uniform = uniform,
              normal_low = normal_low,
              normal_moderate = normal_moderate,
              normal_high = normal_high)

saveRDS(dists, "data/experiment/bart_distributions.RDS")

t2 <- Sys.time()
t2 - t1

paste0(uniform, collapse = ", ")
paste0(normal_low, collapse = ", ")
paste0(normal_moderate, collapse = ", ")
paste0(normal_high, collapse = ", ")

dists %>%
  pivot_longer(cols = everything(),
               names_to = "distribution") %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ distribution) + 
  theme_bw()
