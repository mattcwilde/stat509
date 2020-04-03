# HW 9
# Matt Wilde

# Problem 6
n = 70
p = 0.15
counter = 0
for (i in 1:10000){
  X = rbinom(n, size=1, p=p)
  phat = mean(X)
  SE = sqrt(phat*(1-phat)/n)
  cl = 1.96*SE
  # print(c(phat, SE + cl, SE - cl))
  if ((p > phat-cl) & (p < phat+cl)){
    counter = counter + 1 
  }

}
print(paste("proportion of intervals which contain the true value of p:", 
            counter/10000))
