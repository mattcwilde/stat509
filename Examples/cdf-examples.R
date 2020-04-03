#Some examples with constructing CDFs:

## The function plot.ecdf constructs an "empirical CDF":

# Here is a simple example:
plot.ecdf(c(1,2,2),main="Simple example")

# This is the CDF for a discrete random variable such that
# P(X=1) = 1/3, P(X=2)=1/3
# Note that the function is leaving out the unfilled circles 

# Let's do a slightly more complex example - a fair die:

plot.ecdf(1:6,main="A fair die")

# Doing the same thing by simulation:

nsims <- 1000
sims <- sample(1:6,size=nsims,replace=TRUE)

table(sims)
table(sims)/nsims

plot.ecdf(sims, main="Empirical CDF")

#Comparison 

par(mfrow=c(1,2))
plot.ecdf(1:6,main="Theoretical CDF")
plot.ecdf(sims, main="Empirical CDF")

# Now repeat with different values of nsims