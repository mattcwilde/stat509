# Matt Wilde
# HW3

# problem 5
# a)
X = rnorm(1000, mean = 3, sd = 1)

# b)
V = exp(0.01*X)
E_V = mean(V)
print(paste("E[V] = ", mean(V)))

# c)
W = exp(-0.01*X)
E_W = mean(W)
print(paste("E[W] = ", mean(W)))

# d)
# since delta ~ 0.01
delta = 0.00001
deriv = (mean(exp(delta*X)) - mean(exp(-delta*X)))/(2 * delta)
# deriv = (E_V - E_W)/(2 * 0.01)
print(paste("derivative = ", deriv))

# this is what we expect since the first derivative of the 
# moment generating funciton gives the expected value which is 
# 3 from the problem.

