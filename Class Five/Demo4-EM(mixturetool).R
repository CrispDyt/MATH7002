set.seed(123)
male   <- rnorm(500, mean = 175, sd = 7)
female <- rnorm(500, mean = 155, sd = 6)
x <- c(male, female)

init_good <- list(lambda = 0.50, mu = c(155, 175), sigma = c(6, 7))
init_bad1 <- list(lambda = 0.50, mu = c(162, 163), sigma = c(30, 30))
init_bad2 <- list(lambda = 0.90, mu = c(170, 171), sigma = c(12, 12))

library(mixtools)

fit_good <- normalmixEM(
  x, k = 2,
  lambda = init_good$lambda,
  mu     = init_good$mu,
  sigma  = init_good$sigma,
  arbvar = TRUE,        
  verb   = FALSE
)

fit_b1 <- normalmixEM(
  x, k = 2,
  lambda = init_bad1$lambda,
  mu     = init_bad1$mu,
  sigma  = init_bad1$sigma,
  arbvar = TRUE,
  verb   = FALSE
)

fit_b2 <- normalmixEM(
  x, k = 2,
  lambda = init_bad2$lambda,
  mu     = init_bad2$mu,
  sigma  = init_bad2$sigma,
  arbvar = TRUE,
  verb   = FALSE
)

report <- function(tag, fit) {
  cat(sprintf("[%s] logLik=%.1f | pi1=%.3f | mu1=%.2f | sd1=%.2f | mu2=%.2f | sd2=%.2f | iter=%d\n",
              tag, fit$loglik, fit$lambda[1], fit$mu[1], fit$sigma[1],
              fit$mu[2], fit$sigma[2], fit$iter))
}
report("good init (mixtools)", fit_good)
report("bad  init 1 (mixtools)", fit_b1)
report("bad  init 2 (mixtools)", fit_b2)

hist(x, breaks = 40, probability = TRUE, col = "grey85", border = "white",
     main = "EM (mixtools) for Mixture distribution", xlab = "Height (cm)")

xs <- seq(min(x)-15, max(x)+15, length.out = 1000)
add_mix <- function(fit, col, lwd = 2, lty = 1) {
  dens <- fit$lambda[1] * dnorm(xs, fit$mu[1], fit$sigma[1]) +
    (1 - fit$lambda[1]) * dnorm(xs, fit$mu[2], fit$sigma[2])
  lines(xs, dens, col = col, lwd = lwd, lty = lty)
}
add_mix(fit_good, "blue", 2, 1)
add_mix(fit_b1,   "red",  2, 2)
add_mix(fit_b2,   "darkgreen", 2, 3)

true_mix <- 0.5*dnorm(xs, 155, 6) + 0.5*dnorm(xs, 175, 7)
lines(xs, true_mix, col = "purple", lwd = 2, lty = 4)

legend("topright",
       legend = c("Good init (mixtools)", "Bad init 1", "Bad init 2", "True mixture"),
       col = c("blue", "red", "darkgreen", "purple"),
       lty = c(1, 2, 3, 4), lwd = 2, bty = "n")
