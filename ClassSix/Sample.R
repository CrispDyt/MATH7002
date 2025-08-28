library(mvtnorm)
mean_vector <- c(0, 0) 
cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)  
x_vals <- seq(-3, 3, length.out = 100)
y_vals <- seq(-3, 3, length.out = 100)
grid <- expand.grid(x = x_vals, y = y_vals)
z_vals <- dmvnorm(grid, mean = mean_vector, sigma = cov_matrix)
contour(x_vals, y_vals, matrix(z_vals, nrow = 100), 
        main = "Correlation = 0.5", 
        xlab = "X", ylab = "Y", 
        col = "black", 
        lwd = 2)