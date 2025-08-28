library(mixtools)
data = faithful
Waiting = data$waiting
out = boot.comp(Waiting, max.comp = 3, mix.type = "normalmix", , hist = TRUE)
hist(out, cex = 0.5)