lambda_true <- 10  
sample_size <- 50  
num_simulations <- 10000  
compute_T <- function(sample, lambda_true) {
  lambda_mle <- mean(sample)
  # True Log-likelihood
  log_likelihood_true <- sum(dpois(sample, lambda = lambda_true, log = TRUE))
  # Log-likelihood for MLE
  log_likelihood_mle <- sum(dpois(sample, lambda = lambda_mle, log = TRUE))
  # Calculate T
  T <- 2 * (log_likelihood_mle - log_likelihood_true)
  return(T)
}
# Simulate and collect t
T_stats <- numeric(num_simulations)
for (i in 1:num_simulations) {
  sample <- rpois(sample_size, lambda = lambda_true)
  T_stats[i] <- compute_T(sample, lambda_true)
}
# Plot the distribution of T
hist(T_stats, breaks = 50, probability = TRUE, main = "The distribution of T", xlab = "Value of T")
# Plot the curve of chi-square with random one
curve(dchisq(x, df = 1), col = "red", add = TRUE, lwd = 2)
mean(T_stats)
sd(T_stats)