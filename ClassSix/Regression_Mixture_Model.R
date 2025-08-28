library(mixtools)  

# Fit the mixture model
fit.mix = with(crabs, regmixEM(CL, RW, arbvar = FALSE, k = 2))
summary(fit.mix)

# Extract the posterior probabilities for each component
gamma <- fit.mix$posterior[, 1]  

data <- data.frame(gamma = gamma, gender = crabs$sex)

# Create a boxplot to visualize the gamma values by gender
boxplot(gamma ~ gender, data = data, col = c("orange", "lightblue"), 
        main = "Boxplot of Gamma by Gender", 
        ylab = "Gamma", 
        xlab = "Gender")
