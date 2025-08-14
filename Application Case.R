# Download the data from vUWS
SAheart <- read.csv("SAheart.data") 
library(ggplot2)
# ggplot(data, aes(x = factor(chd), y = sbp)) +
#   geom_boxplot() +
#   labs(x = "CHD", y = "SBP") +
#   theme_minimal() +
#   ggtitle("Boxplot of SBP by CHD")
lo = 101
hi = 218
f = density(SAheart$sbp, from=lo, to=hi)
f0 = density(SAheart$sbp[SAheart$chd==0],from=lo, to=hi)
f1 = density(SAheart$sbp[SAheart$chd==1],from=lo, to=hi)
p = mean(SAheart$chd)
plot(f$x, f1$y*p/(p*f1$y+(1-p)*f0$y) ,type="l", xlab="sbp", ylab="P(chd|sbp)")
