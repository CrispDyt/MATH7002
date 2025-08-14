sample_data <- c(32.39, 30.45, 80.07)
gradient_log_likelihood <- function(alpha, data) {
  n <- length(data)
  x_bar <- mean(data)
  # ∂l(α)/∂α = n*log(α) - n*log(x̄) - n*(∂/∂α)log Γ(α) + Σlog(xi)
  # where (∂/∂α)log Γ(α) = digamma(α)
  gradient <- n * log(alpha) - n * log(x_bar) - n * digamma(alpha) + sum(log(data))
  return(gradient)
}
# Parameters  
data <- sample_data
initial_alpha <- 1
gamma <- 0.1
max_iter <- 10000
tol <- 1e-6
# Initial value
alpha <- initial_alpha
# Gradient ascent iteration
for (i in 1:max_iter) {
  grad <- gradient_log_likelihood(alpha, data)
  # Update alpha using gradient ascent
  alpha_new <- alpha + gamma * grad
  # Check for convergence
  # If the change of alpha or the gradient is almost to zero
  if (abs(alpha_new - alpha) < tol || abs(grad) < tol) {
    cat("Converged after", i, "iterations\n")
    break
  }
  alpha <- alpha_new
}
cat("Maximum likelihood estimate of α:", alpha, "\n")