## Density Estimation 
## Autor: Chris 

sample_data <- c(1, 2, 3, 5)

local_averaging <- function(x, sample_data, delta_x) {
  n <- length(sample_data)
  density_estimate <- (1 / (n * delta_x)) * sum(abs(x - sample_data) < delta_x / 2)
  return(density_estimate)
}
# Interval
delta_x <- 3

density_at_1 <- local_averaging(1, sample_data, delta_x)
density_at_2 <- local_averaging(2, sample_data, delta_x)
density_at_3 <- local_averaging(3, sample_data, delta_x)

# Print the results
print(paste("Density at x = 1:", density_at_1))
print(paste("Density at x = 2:", density_at_2))
print(paste("Density at x = 3:", density_at_3))