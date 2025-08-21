set.seed(123)
male   <- rnorm(500, mean = 175, sd = 7)
female <- rnorm(500, mean = 155, sd = 6)
x <- c(male, female)
init <- list(lambda = 0.50, mu = c(155, 175), sigma = c(6, 7))
loglikeli1 = sum(dnorm(x, mean(x), sd(x), log= TRUE))
library(mixtools)
fit1 <- normalmixEM(
  x, k = 2,
  lambda = init$lambda,
  mu     = init$mu,
  sigma  = init$sigma,       
  verb   = FALSE
)
fit2 <- normalmixEM(
  x, k = 3,
  lambda = init$lambda,
  mu     = init$mu,
  sigma  = init$sigma,677
  arbvar = TRUE,        
  verb   = FALSE
)
n = length(x)
aic = c(-2*loglikeli1+2*2, -2*fit1$loglik+2*(3*2-1), -2*fit2$loglik+2*(3*3-1))
bic = c(-2*loglikeli1+log(n)*2, -2*fit1$loglik+log(n)*(3*2-1), -2*fit2$loglik+log(n)*(3*3-1))
print(aic)
print(bic)