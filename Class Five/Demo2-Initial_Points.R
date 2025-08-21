set.seed(123)
male  <- rnorm(500, mean = 175, sd = 7)
female<- rnorm(500, mean = 155, sd = 6)
x <- c(male, female)

nll_gmm2 <- function(params, x, eps = 1e-9) {
  pi1 <- params[1]; mu1 <- params[2]; sd1 <- params[3]
  mu2 <- params[4]; sd2 <- params[5]
  pi1 <- min(max(pi1, eps), 1 - eps)
  sd1 <- max(sd1, eps); sd2 <- max(sd2, eps)
  dens <- pi1 * dnorm(x, mu1, sd1) + (1 - pi1) * dnorm(x, mu2, sd2)
  -sum(log(pmax(dens, eps)))
}

fit_once <- function(x, init, maxit = 500) {
  lower <- c(1e-4, -Inf, 1e-3, -Inf, 1e-3)
  upper <- c(1-1e-4,  Inf,  Inf,  Inf,  Inf)
  opt <- optim(par = init, fn = nll_gmm2, x = x,
               method = "L-BFGS-B", lower = lower, upper = upper,
               control = list(maxit = maxit))
  par <- opt$par
  if (par[2] > par[4]) par <- c(1 - par[1], par[4], par[5], par[2], par[3])
  list(par = par, loglik = -opt$value)
}

# The initial point is just a little bit different
init_good <- c(0.50, 155, 6, 175, 7)    
init_bad1 <- c(0.50, 162, 30, 163, 30)  
init_bad2 <- c(0.90, 170, 12, 171, 12)  

fit_good <- fit_once(x, init_good)
fit_b1   <- fit_once(x, init_bad1)
fit_b2   <- fit_once(x, init_bad2)

report <- function(tag, fit) {
  p <- fit$par
  cat(sprintf(
    "[%s] pi1 = %.3f | mu1 = %.2f | sd1 = %.2f | mu2 = %.2f | sd2 = %.2f\n",
    tag, p[1], p[2], p[3], p[4], p[5]
  ))
}
report("good init", fit_good)
report("bad  init 1", fit_b1)
report("bad  init 2", fit_b2)

hist(x, breaks = 40, probability = TRUE, col = "grey85",
     border = "white", main = "Effect of initial points", xlab = "Height (cm)")

xs <- seq(min(x)-15, max(x)+15, length.out = 1000)
add_mix <- function(fit, col, lwd = 2, lty = 1) {
  p <- fit$par
  dens <- p[1]*dnorm(xs, p[2], p[3]) + (1 - p[1])*dnorm(xs, p[4], p[5])
  lines(xs, dens, col = col, lwd = lwd, lty = lty)
}
add_mix(fit_good, "blue", 2, 1)
add_mix(fit_b1,   "red",  2, 2)
add_mix(fit_b2,   "darkgreen", 2, 3)

true_mix <- 0.5*dnorm(xs, 155, 6) + 0.5*dnorm(xs, 175, 7)
lines(xs, true_mix, col = "purple", lwd = 2, lty = 4)

legend("topright",
       legend = c("Good init", "Bad init 1", "Bad init 2", "True"),
       col    = c("blue", "red", "darkgreen", "purple"),
       lty    = c(1, 2, 3, 4), lwd = 2, bty = "n")
