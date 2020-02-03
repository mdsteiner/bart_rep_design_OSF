# Helper functions, mostly to print stuff out more nicely

# cohen's d function
cohens_d <- function(x, y) {
  
  n1 <- length(x)
  n2 <- length(y)
  m1 <- mean(x)
  m2 <- mean(y)
  s1 <- var(x)
  s2 <- var(y)
  
  # calculate pooled standard deviation with pooled variances
  s <- sqrt(((n1 - 1)*s1 + (n2 - 1)*s2) / (n1 + n2 -2))
  
  # cohen's d
  d <- (m1 - m2) / s
  
  # return cohen's d
  d
}

cv <- function(x, y = NULL) {
  if(is.null(y)) {
    sd(x) / mean(x)
  } else {
    mean(c(sd(x) / mean(x), sd(y) / mean(y)))
  }
  
}

get_rel <- function(x, y) {
  rel_i <- correlationBF(x, y, iterations = 10000)
  cv_i <- cv(x, y)
  post_i <- describe_posterior(rel_i, rope_range = c(-.05, .05), ci = .95,
                     rope_ci = 1)
  out = list(
    full = rel_i,
    cv = cv_i,
    corr = post_i$Median,
    ci_low = post_i$CI_low,
    ci_high = post_i$CI_high,
    pROPE = post_i$ROPE_Percentage
  )
  
  cat("Test-Retest Reliability is:\n")
  cat("r = ", numformat(out$corr, 2), ", [", numformat(out$ci_low, 2), ", ",
      numformat(out$ci_high, 2), "], pROPE = ", numformat(out$pROPE), sep = "")
  cat("\nCV is:\n", round(out$cv, 2), "\n", sep = "")
  cat("For Table:\n")
  cat(numformat(out$corr, 2), ", [", numformat(out$ci_low, 2), ", ",
      numformat(out$ci_high, 2), "] & ", round(out$cv, 2), "\n" , sep = "")
  
  
  return(out)
}

compare_cors <- function(x, y, iter = 10000, raw = FALSE) {
  tt <- get_parameters(x, iterations = iter) -
    get_parameters(y, iterations = iter)
  post <- describe_posterior(tt, rope_range = c(-.05, .05), ci = .95,
                             rope_ci = 1)
  
  if (isFALSE(raw)) {
    cat("Difference in r (positive = first r larger):\n")
    cat(round(post$Median, 2), " [", round(post$CI_low, 2), ", ",
        round(post$CI_high, 2), "], pROPE = ", numformat(post$ROPE_Percentage),
        "\n", sep = "")
    cat("For Table:\n")
    cat(ifelse(post$ROPE_Percentage < .025, "\\textbf{", ""), numformat(post$Median, 2),
        ifelse(post$ROPE_Percentage < .025, "}", ""), ", [", numformat(post$CI_low, 2), ", ",
        numformat(post$CI_high, 2), "]\n", sep = "")
  } else {
    post
  }
  
}

compare_diffs <- function(x, y, do = "both") {
  tt <- tibble(
    delta_m = x$mu - y$mu,
    delta_sd = x$sigma - y$sigma
  )
  
  post1 <- describe_posterior(tt$delta_m, test = c("rope"), ci = .95,
                              rope_ci = 1, rope_range = c(-.1, .1) * sd(tt$delta_m))
  
  if (do == "both") {
    post2 <- describe_posterior(tt$delta_sd, test = c("rope"), ci = .95,
                                rope_ci = 1, rope_range = c(-.1, .1) * sd(tt$delta_sd))
  }
  
  
  cat("Difference in Mean (positive = first Mean larger):\n")
  cat(round(post1$Median, 2), " [", round(post1$CI_low, 2), ", ",
      round(post1$CI_high, 2), "], pROPE = ", numformat(post1$ROPE_Percentage), "\n",
      sep = "")
  if (do == "both") {
  cat("\n")
  cat("Difference in SD (positive = first SD larger):\n")
  cat(round(post2$Median, 2), " [", round(post2$CI_low, 2), ", ",
      round(post2$CI_high, 2), "], pROPE = ", numformat(post2$ROPE_Percentage), "\n",
      sep = "")
  }
  
}

numformat <- function(x, digits = 3) {
    ncode <- paste0("%.", digits, "f")
    sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}
