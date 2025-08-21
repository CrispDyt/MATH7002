mu1 <- 10
mu2 <- 30
sd1 <- 5
sd2 <- 10
lambda <- 0.3 
n <- 100
# Create latent labels because we don' know which distribution the data point comes from.
z <- rbinom(n, size = 1, prob = lambda)
# Here we say if the latent label for the data point is one, then it comes from the X distribution 
x <- ifelse(z == 1, rnorm(n, mu1, sd1), rnorm(n, mu2, sd2))

library(ggplot2)
# grid for densities
xs <- seq(min(x) - 10, max(x) + 10, length.out = 1000)

# true densities (mixture and components)
mix_dens <- lambda * dnorm(xs, mu1, sd1) + (1 - lambda) * dnorm(xs, mu2, sd2)
comp1    <- lambda * dnorm(xs, mu1, sd1)
comp2    <- (1 - lambda) * dnorm(xs, mu2, sd2)

frame <- data.frame(x = x, comp = factor(ifelse(z == 1, "X", "Y")))
df_mix <- data.frame(xs = xs, mix_dens = mix_dens)
df_c1  <- data.frame(xs = xs, y = comp1)
df_c2  <- data.frame(xs = xs, y = comp2)

p <- ggplot(frame, aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "grey70", color = "white") +
  geom_line(data = df_mix, aes(x = xs, y = mix_dens), linewidth = 1.2, inherit.aes = FALSE) +
  geom_line(data = df_c1,  aes(x = xs, y = y), linetype = 2, inherit.aes = FALSE) +
  geom_line(data = df_c2,  aes(x = xs, y = y), linetype = 3, inherit.aes = FALSE) +
  labs(
    title = "Sampling from a Two-Component Gaussian Mixture",
    subtitle = paste0("λ = ", lambda, ",  n = ", n,
                      "   (solid: mixture, dashed/dotted: components)"),
    x = "Value", y = "Density"
  ) +
  theme_minimal(base_size = 13)

print(p)
