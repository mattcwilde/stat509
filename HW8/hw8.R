# HW 8 
# Matthew Wilde

### Problem 1)

### b) 
p = 0.9
n = 16

sampleMean = 0
for(i in 1:10000){
  sampleMean[i] = mean(rbinom(n,1,p))
}

hist(sampleMean)
# hist(rbinom(n,1,p))


### c)
print(c("mean of sampleMean:", mean(sampleMean)))
print(c("theoretical mean of sampleMean:", p))
print(c("var of sampleMean:", var(sampleMean)))
print(c("theoretical var of sampleMean:", p*(1-p)/n))


### d)
MSE = 0
for(i in 1:10000){
  Xhat = mean(rbinom(n,1,p))
  MSE[i] = (Xhat - p)^2
}
print(paste("avererage MSE:", mean(MSE)))



### Problem 2)
### b)

lam = 4
n = 25

sampleMean = 0
for(i in 1:10000){
  sampleMean[i] = mean(rpois(n,lam))
}

hist(sampleMean, main = "Problem 2b) Poisson")

### c)
print("problem 2c:")
print(c("mean of sampleMean:", mean(sampleMean)))
print(c("theoretical mean of sampleMean:", lam))
print(c("var of sampleMean:", var(sampleMean)))
print(c("theoretical var of sampleMean:", lam/n))

### d)
MSE = 0
for(i in 1:10000){
  Xhat = mean(rpois(n,lam))
  MSE[i] = (Xhat - lam)^2
}
print(paste("avererage MSE:", mean(MSE)))


### 5) 
### a) 
3*1.64
m_0 = 0
m_a = 0.3
sig = 3
n = 36
z = qnorm(0.95)
power = 1 - pnorm(sqrt(n)*(m_0 - m_a)/sig + z)
print(paste("power =", power))

### c)
for (n in 100:1000){
  power = 1 - pnorm(sqrt(n)*(m_0 - m_a)/sig + z)
  if (power > 0.8){
    break
  }
}
print(paste("power =", power))
print(paste("n =", n))