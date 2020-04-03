# STAT509 HW1
# Matthew Wilde
# 10/6/17

###################################################
# problem 5
###################################################

# 5b + c
n <-10000
k <-150
for (m in (n-k):1){
  numerator <- lfactorial(n - k) + lfactorial(n-m)
  denominator <- lfactorial(n) + lfactorial(n-k-m)
  p <- 1 - exp(numerator - denominator)
  if (abs(p - 0.8) < 0.01) print(paste(p, m))
}

# 5d
# binomial dist
for (m in (n-k):1){
  p <- 1 - pbinom(0, size=m, prob=(k/n))
  if (abs(p - 0.8) < 0.01) print(paste(p, m))
}


