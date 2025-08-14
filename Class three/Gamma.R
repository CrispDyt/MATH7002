# Input data
sample_data <- c(32.39, 30.45, 80.07)
# Define log-likelihood function
log_likelihood <- function(params, data) {
  # alpha is the shape parameter
  alpha <- params[1]
  # beta is the scale parameter
  beta <- params[2]
  n <- length(data)
  # Calculate the log-likelihood function
  log_lik <- n * alpha * log(beta) - n * lgamma(alpha) + 
    (alpha - 1) * sum(log(data)) - beta * sum(data)
  return(-log_lik)
}
# Initial Parameters
initial_params <- c(alpha = 2, beta = 0.05)
# Optimization
result <- optim(par = initial_params, 
                fn = log_likelihood, 
                data = sample_data,
                method = "L-BFGS-B",
                lower = c(0.001, 0.001))

# Optimal Parameters 
alpha_hat <- result$par[1]
beta_hat <- result$par[2]

# Maximal value of Log-likelihood 
max_log_lik <- result$value

# Draw figures
library(graphics)

# Sequence for alpha, beta 
alpha_seq <- seq(0.5, 5, length.out = 50)
beta_seq <- seq(0.01, 0.2, length.out = 50)
grid <- expand.grid(alpha = alpha_seq, beta = beta_seq)

# Calculate the value of each log-likelihood function
log_lik_values <- apply(grid, 1, function(params) {
  log_likelihood(params, sample_data)
})

log_lik_matrix <- matrix(log_lik_values, nrow = length(alpha_seq), ncol = length(beta_seq))

contour(alpha_seq, beta_seq, log_lik_matrix, 
        xlab = "α (shape)", ylab = "β (rate)", 
        main = "Log-Likelihood Contour Plot")
points(alpha_hat, beta_hat, col = "red", pch = 19, cex = 1.5)
legend("topright", legend = "MLE estimate", col = "red", pch = 19)


