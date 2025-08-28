library(mixtools)
FC = read.csv("Flow.csv")
X = FC[sample(1:nrow(FC), size=1000),]
m2 = mvnormalmixEM(X, k=3)
summary(m2)