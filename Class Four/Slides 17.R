## Try different Kernels 
## Autor: Chris

library(CircNNTSR)
data(Turtles)

par(mfrow = c(2, 2)) 

# Rectangular
dens_rectangular <- density(Turtles, kernel = "rectangular", width = 30)
plot(dens_rectangular, main = "Rectangular Kernel", xlab = "Turtle Data", ylab = "Density")

# Triangular
dens_triangular <- density(Turtles, kernel = "triangular", width = 30)
plot(dens_triangular, main = "Triangular Kernel", xlab = "Turtle Data", ylab = "Density")

# Epanechnikov
dens_epanechnikov <- density(Turtles, kernel = "epanechnikov", width = 30)
plot(dens_epanechnikov, main = "Epanechnikov Kernel", xlab = "Turtle Data", ylab = "Density")

# Gaussian kernel
dens_gaussian <- density(Turtles, kernel = "gaussian", width = 30)
plot(dens_gaussian, main = "Gaussian Kernel", xlab = "Turtle Data", ylab = "Density")