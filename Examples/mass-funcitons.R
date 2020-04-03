### Some examples of probability mass functions

# (1) Bernoulli

p <- 0.1

dbinom(1,1,p) # P(X=1) = p 
dbinom(0,1,p) # P(X=0) = 1-p 

# here we use the fact that Bernoulli(p) 
# is the same as Binomial(n=1, p)

# "silly" values
dbinom(2,1,p) # P(X=2) = 0 
dbinom(-1,1,p) # P(X=-1) = 0
dbinom(0.5,1,p) # P(X=0.5) = 0

#Plot of the mass function

# values
xseq <- -2:4  # including values other than 0 and 1 
# is unnecessary
yseq <- dbinom(xseq,1,p)
plot(xseq,yseq,xlab="Values of x",ylim=c(0,1), ylab="P(X=x)",main=paste("pmf for Bernoulli distribution with p=",p))


# (2) Binomial distribution

p <- 0.4
n <- 8

dbinom(8,n,p) # P(X=8) = p^n
## let's check:
p^n

dbinom(0,n,p) # P(X=0) = (1-p)^n
# check:
(1-p)^n

xseq <- 0:n

yseq <- dbinom(xseq,n,p) 
sum(yseq) # check

#Plot of the mass function

plot(xseq,yseq,xlab="Values of x",ylim=c(0,1), ylab="P(X=x)",main=paste("pmf for Binomial distribution with n=",n," and p=",p))

# (3) Geometric distribution

p <- 0.4

## R defines the Geometric distribution 
## slightly differently from Goldberger and lecture notes
## In R it is the number of tosses *before* the first head
## whereas Goldberger and others define it as the number
## of tosses up to *and including* the first head.

## we address this by defining our own function:
dmygeom <- function(x,p){dgeom(x-1,p)}

dmygeom(1,p) # P(X=1) = p
#check:
p

dmygeom(2,p) # P(X=1) = p(1-p)
# check:
p*(1-p)


xseq <- 0:10  # 10 is arbitrary here
yseq <- dmygeom(xseq,p) 
sum(yseq) # Why is this < 1?

#Plot of the mass function

plot(xseq,yseq,xlab="Values of x",ylim=c(0,1), ylab="P(X=x)",main=paste("pmf for Geometric distribution with  p=",p))



# (4) Discrete Uniform

## R doesn't have a function for this. 
## Let's build our own:

# First attempt:
ddiscunif <- function(x,n){1/n}
#
ddiscunif(1,5)
ddiscunif(2,5)
# looks ok so far... but
ddiscunif(6,5) # not so good : should be 0
ddiscunif(-1,5) # not so good : should be 0

# second attempt
ddiscunif <- function(x,n){
  sum(rep(x,n)==(1:n))/n
}
# How does this work?

#
ddiscunif(1,5)
ddiscunif(2,5)
# looks ok so far
ddiscunif(6,5) # better
ddiscunif(-1,5) # better

# but... what about if we feed in a vector:
ddiscunif(c(0,1,2),5)
# not so good. 
# we want 0, 0.2,0.2

# third attempt:
# safe for all inputs and can take vectors

ddiscunif <- function(x,n){
  out <- rep(NA,length(x))
  for(i in 1:length(x)){
    out[i] <- sum(rep(x[i],n)==(1:n))/n
  }
  return(out)
}
## Can you see how this is working?

ddiscunif(1,5)
ddiscunif(2,5)
# looks ok so far
ddiscunif(6,5) # better
ddiscunif(-1,5) # better

# but... what about if we feed in a vector:
ddiscunif(c(0,1,2),5)

### Best approach using
### built "%in%" function (due to Danping)

ddiscunif <- function(x,n){
  index <- x%in%(1:n)
  # match each element of x in the set {1,2,...,n}
  # return logical values, i.e. TRUE or FALSE.
  out <- rep(0, length(x))
  out[index] <- 1/n
  # "legitimate" points of x receive the point mass 1, others get 0.
  return(out)
}


n <- 36
xseq <- -1:38
yseq <- ddiscunif(xseq,n)

plot(xseq,yseq,xlab="Values of x",ylim=c(0,1), ylab="P(X=x)",main=paste("pmf for Discrete Uniform distribution with  n=",n))

# (5) Poisson Distribution

lambda <- 1

dpois(0,lambda)
# check:
lambda^0*exp(-lambda)/factorial(0)

dpois(1,lambda)
# check:
lambda^1*exp(-lambda)/factorial(1)

## Why is this the same? (Hint - try a different lambda)

dpois(2,lambda)
# check:
lambda^2*exp(-lambda)/factorial(2)

## Some silly values:
dpois(-1,lambda)
dpois(1.5,lambda) # gives a warning



xseq <- 0:10
yseq <- dpois(xseq,lambda)
sum(yseq)
## Is it really 1?
sum(yseq)-1  # How do we explain this? Rounding error

options(digits=15) #This increases the number of digits printed
sum(yseq)

plot(xseq,yseq,xlab="Values of x",ylim=c(0,1), ylab="P(X=x)",main=paste("pmf for Poisson distribution with  lambda=",lambda))

# Try some other lambdas:
lambda <- 1

xseq <- 0:10
yseq <- dpois(xseq,lambda)
plot(xseq,yseq,xlab="Values of x",ylim=c(0,1), ylab="P(X=x)",main=paste("pmf for Poisson distribution with  lambda=",lambda))


lambda <- 2

xseq <- 0:10
yseq <- dpois(xseq,lambda)
plot(xseq,yseq,xlab="Values of x",ylim=c(0,1), ylab="P(X=x)",main=paste("pmf for Poisson distribution with  lambda=",lambda))