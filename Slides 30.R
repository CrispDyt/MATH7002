library(CircNNTSR)
data(Turtles)

gaussian_kernel_density <- function(data, h, point) {
  n <- length(data)
  kernel_values <- dnorm((point - data) / h)  
  return(sum(kernel_values) / (n * h))
}

leave_one_out_density <- function(data, h, point) {
  n <- length(data)
  densities <- numeric(n)
  # Loop, train n models
  for (i in 1:n) {
    # The trained density function after leaving the i-th point
    densities[i] <- gaussian_kernel_density(data[-i], h, point)
  }
  return(densities)
}

# Cost Function M(h)
cost_function <- function(data, h) {
  n <- length(data)
  densities <- numeric(n)
  # Loop all n models
  for (i in 1:n) {
    densities[i] <- gaussian_kernel_density(data[-i], h, data[i])
  }
  M_h <- sum(densities^2) - (2 / n) * sum(densities)
  return(M_h)
}

# Find the optimal bandwidth
find_best_bandwidth <- function(data) {
  optimal_result <- optim(par = 1,  # Initial value
                          fn = cost_function,  # cost function 
                          data = data,  # Data you need to minimize 
                          method = "Brent",  # Optimization method 
                          lower = 0.1,  #  Lower bound
                          upper = 76) # Upper bound
  best_h <- optimal_result$par  # best result 
  return(best_h)
}

turtle_data <- Turtles

best_bandwidth <- find_best_bandwidth(turtle_data)
print(paste("Best Bandwidth: ", best_bandwidth))
