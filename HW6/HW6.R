# HW 6 
# Matt Wilde

# Problem 4f):
print(paste("P(lambda  < 0.5 | x0 = 4) =", pgamma(0.5, 5, 3)))

print(paste("P(lambda  < 0.5 | x0 = 0) =", pgamma(0.5, 1, 3)))
print(paste("P(lambda  < 0.5 | x0 = 0) from (d) =", 1 - exp(-3/2)))

# 4f
p = pgamma(0:5/10, 5, 3)

plot(p, qgamma(p, 5, 3))
med = qgamma(0.5, 5, 3)
low = qgamma(0.025, 5, 3)
high = qgamma(0.975, 5, 3)

# interval = quantile(qgamma(0.5, 5, 3), c(0.025, 0.975))

print(paste("posterior median = ", med))
print(paste("95% creditable interval: ", low, "-", high))
# print(interval)



# Problem 5)
# a)
x = 0:25/100
f = (1/4 - x)^(-1/2)
plot(x, f, type = "o")