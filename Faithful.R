# 2D faithful data
library(CircNNTSR)
library(MASS)
dens = kde2d(faithful$waiting, faithful$eruptions)
par(mar=c(4,4,1,1)+.1, lwd=1, cex=0.7)
contour(dens) 