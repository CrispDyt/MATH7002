# Mixture Distribution Example
# Autor: Chris

set.seed(123)

# Parameters for the two populations
male_mean <- 175
male_sd <- 7
female_mean <- 155
female_sd <- 6

# Sample sizes
n_males <- 500
n_females <- 500
n_total <- n_males + n_females

# Generate data from both populations 
male_heights <- rnorm(n_males, mean = male_mean, sd = male_sd)
female_heights <- rnorm(n_females, mean = female_mean, sd = female_sd)

# Combine the data (this creates the mixture)
all_heights <- c(male_heights, female_heights)

# Create labels for visualization
gender <- c(rep("Male", n_males), rep("Female", n_females))

# Define Negative Loglikelihood function
neg_log_likelihood <- function(params, data) {
  mu <- params[1]
  sigma <- params[2]
  -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# Initialize Parameters
init_params <- c(mean(all_heights), sd(all_heights))

mle_result <- optim(
  par = init_params,
  fn = neg_log_likelihood,
  data = all_heights,
  method = "L-BFGS-B",
  lower = 0.001
)

# Extract results
mle_mean <- mle_result$par[1]
mle_sd <- mle_result$par[2]

cat(sprintf("Estimated mean: %.4f\n", mle_mean))
cat(sprintf("Estimated SD: %.4f\n", mle_sd))

par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
plot(all_heights, type = "n", xlim = c(140, 200), ylim = c(0, 0.1), 
     xlab = "Height (cm)", ylab = "Density", main = "All Distributions Combined")
hist(male_heights, breaks = 30, probability = TRUE, col = rgb(0, 0, 1, 0.5), add = TRUE)
curve(dnorm(x, male_mean, male_sd), add = TRUE, col = "blue", lwd = 2)
hist(female_heights, breaks = 30, probability = TRUE, col = rgb(1, 0, 0, 0.5), add = TRUE)
curve(dnorm(x, female_mean, female_sd), add = TRUE, col = "red", lwd = 2)
hist(all_heights, breaks = 50, probability = TRUE, col = "gray", add = TRUE)
curve(dnorm(x, mle_mean, mle_sd), add = TRUE, col = "green", lwd = 3)
x_vals <- seq(140, 200, 0.1)
true_mixture <- 0.5 * dnorm(x_vals, male_mean, male_sd) + 
  0.5 * dnorm(x_vals, female_mean, female_sd)
lines(x_vals, true_mixture, col = "purple", lwd = 2, lty = 2)
legend("topright", 
       legend = c("Male Distribution", "Female Distribution", "Single (Wrong) Distribution", "True Mixture Distribution"),
       col = c("blue", "red", "green", "purple"), 
       lty = c(1, 1, 1, 2), lwd = c(2, 2, 3, 2), cex = 0.8)



