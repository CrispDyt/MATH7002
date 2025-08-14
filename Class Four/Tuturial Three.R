# # Question two:
library(faraway)
clms = motorins$perd
saveRDS(clms, file = "clms_data.rds")
# Negative log-likelihood function of Gamma distribution
nloglik2 = function(alpha, beta)
  -sum(dgamma(clms, shape=alpha, scale=beta, log=TRUE))
# Sometimes, if the initial value is not appropriate, the optimization process can not converge.
fit2 = mle(nloglik2, start=list(alpha=5, beta=3000),
            nobs=length(clms), lower = 0.01)
# Using @details to check the convergent condition
fit2@details

# In case of non-convergence we can restart from the values we go so far
fit3 = mle(nloglik2, start=as.list(coef(fit2)), nobs=length(clms))

# Question three
library(faraway)
clms = motorins$perd
nloglik3 = function(mu, sigma)
  -sum(dlnorm(clms, mu, sigma, log=TRUE))
fit4 = mle(nloglik3, start=list(mu=8, sigma=1),
           nobs=length(clms),lower = c(mu = -Inf, sigma = 1e-6))

# cat("mu: ", fit4@coef["mu"], "\n")
# cat("sigma: ", fit4@coef["sigma"], "\n")
# cat("Mean of log(clms): ", mean(log(clms)), "\n")
# cat("Standard Deviation of log(clms): ", sd(log(clms)), "\n")

# Question Four 

fit5 = mle(nloglik2, start=list(alpha=1, beta=3000),
             nobs=length(clms), method="Nelder-Mead") 

fit5@details

confint(fit5)

# Question Five 
 
nloglik2 = function(alpha, beta)
  -sum(dgamma(clms, shape=alpha, scale=beta, log=TRUE))

# Here we define the constrained negative log-likelihood function based on the negative
# loglikelihood function.  
nloglik.null = function(beta)
  nloglik2(2, beta)
fit.null = mle(nloglik.null, start=list(beta=2400), 
               nobs=length(clms))
T1 = as.numeric(2*(logLik(fit3) - logLik(fit.null)))
p.val = 1 - pchisq(T1,1)
cat("T1 statistic: ", T1, "\n")
cat("p-value: ", p.val, "\n") 





