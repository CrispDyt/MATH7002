set.seed(123)
male   <- rnorm(500, mean = 175, sd = 7)
female <- rnorm(500, mean = 155, sd = 6)
x <- c(male, female)

em_gmm2 <- function(x, init) { 
  lambda <- init$lambda
  mu     <- init$mu
  sigma  <- pmax(init$sigma)  
  repeat {
    oldp <- c(lambda, mu, sigma)
    gamma  <- lambda * dnorm(x, mu[1], sigma[1])
    gamma  <- gamma/(gamma + (1 - lambda) * dnorm(x, mu[2], sigma[2]))
    # M step: Update the Q function 
    mu[1]    <- sum(gamma * x) / sum(gamma)
    mu[2]    <- sum((1 - gamma) * x) / sum(1-gamma)
    sigma[1] <- sqrt(sum(gamma * (x - mu[1])^2) / sum(gamma))
    sigma[2] <- sqrt(sum((1 - gamma) * (x - mu[2])^2) / sum(1-gamma))
    lambda   <- sum(gamma) / length(x)
    diff <- sum(abs(c(lambda, mu, sigma) - oldp))
    if (diff < 0.001) break
  }
  return(list(lambda = lambda, mu = mu, sigma = sigma))
} 
# Still test three different initial points 
init_good <- list(lambda = 0.50, mu = c(155, 175), sigma = c(6, 7))      
init_bad1 <- list(lambda = 0.50, mu = c(162, 163), sigma = c(30, 30))     
init_bad2 <- list(lambda = 0.90, mu = c(170, 171), sigma = c(12, 12))     

fit_good <- em_gmm2(x, init_good)
fit_b1   <- em_gmm2(x, init_bad1)
fit_b2   <- em_gmm2(x, init_bad2)

report <- function(tag, fit) {
  cat(sprintf("[%s] pi1=%.3f | mu1=%.2f | sd1=%.2f | mu2=%.2f | sd2=%.2f | iters=%d\n",
              tag, fit$lambda, fit$mu[1], fit$sigma[1], fit$mu[2], fit$sigma[2], fit$iters))
}
report("good init", fit_good)
report("bad  init 1", fit_b1)
report("bad  init 2", fit_b2)


hist(x, breaks = 40, probability = TRUE, col = "grey85", border = "white",
     main = "EM for Mixture distribution", xlab = "Height (cm)")

xs <- seq(min(x)-15, max(x)+15, length.out = 1000)
add_mix <- function(fit, col, lwd = 2, lty = 1) {
  dens <- fit$lambda * dnorm(xs, fit$mu[1], fit$sigma[1]) +
    (1 - fit$lambda) * dnorm(xs, fit$mu[2], fit$sigma[2])
  lines(xs, dens, col = col, lwd = lwd, lty = lty)
}
add_mix(fit_good, "blue", 2, 1)
add_mix(fit_b1,   "red",  2, 2)
add_mix(fit_b2,   "darkgreen", 2, 3)

true_mix <- 0.5*dnorm(xs, 155, 6) + 0.5*dnorm(xs, 175, 7)
lines(xs, true_mix, col = "purple", lwd = 2, lty = 4)

legend("topright",
       legend = c("Good init (EM)", "Bad init 1 (EM)", "Bad init 2 (EM)", "True mixture"),
       col = c("blue", "red", "darkgreen", "purple"),
       lty = c(1, 2, 3, 4), lwd = 2, bty = "n")
