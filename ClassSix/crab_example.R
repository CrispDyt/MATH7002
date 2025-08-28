library(MASS)
data(crabs)
RW <- crabs$RW
CL <- crabs$CL
plot(RW, CL, 
     main = "Carapace Length vs Rear Width",
     xlab = "Rear Width (RW)", 
     ylab = "Carapace Length (CL)", pch = 19,  col = "darkblue",   cex = 0.8)          
model <- lm(CL ~ RW)
abline(model, col = "red", lwd = 2)
summary(model)
cat("Regression equation: CL =", round(coef(model)[1], 3), "+", 
    round(coef(model)[2], 3), "* RW\n")
# Print R-squared
cat("R-squared:", round(summary(model)$r.squared, 4), "\n")
# Print correlation coefficient
cat("Correlation:", round(cor(RW, CL), 4), "\n")