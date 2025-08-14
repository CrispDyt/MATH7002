library(CircNNTSR)
data(Turtles)
par(mfrow = c(1, 2)) 


dens_30 <- density(Turtles, kernel="rectangular", width=80)
plot(dens_30, main="Local average estimate (width=80)", xlab="Turtle data", 
     ylab="Density")


dens_10 <- density(Turtles, kernel="rectangular", width=10)
plot(dens_10, main="Local average estimate (width=10)", xlab="Turtle data", 
     ylab="Density")

