# data = read.table('http://www.stat.washington.edu/tsr/s509/examples/caschool.csv',header=TRUE,sep=",")

data = read.table('caschool.csv', header=TRUE, sep=",")

# see column names
names(data)

summary(data$enrl_tot)
summary(data$teachers)

students = data$enrl_tot
teachers = data$teachers
testscr = data$testscr

# stRatio = teachers/students
stRatio = students/teachers
hist(testscr)
hist(stRatio)

plot(stRatio,testscr, xlab="Student Teacher Ratio", ylab="Test Score", 
     main="Relating Student Teacher Ratio and Test Scores")


#let's fit a linear model

fit = lm(testscr ~ stRatio)
fit # just the coefficients
# Note that an intercept term is included by default

summary(fit) # more information on the fit
fit$coef # accessing the intercept and slope
cor(testscr,stRatio) #find the correlation
cor(testscr,stRatio)^2 #find the squared correlation

plot(stRatio,testscr, xlab="Student Teacher Ratio", ylab="Test Score", 
     main="Relating Student Teacher Ratio and Test Scores")
abline(fit, col="red") #put the regression line on the plot

print(paste("a) The best linear predictor:"))
print(summary(fit))

# b) The approximate conditional expectation function 
#   E[Test Score | Student Teacher Ratio] via binning 
#   Student Teacher Ratio.

x = stRatio
y = testscr

hx <- hist(x)
abline(v=hx$breaks,col="red")


plot(x,y,main="Scatterplot for data")
abline(v=hx$breaks,col="red")

z = rep(0,length(x))
bin.no = rep(0,length(x))
for (i in 1:(length(hx$breaks)-1)){
  z <- z + rep(hx$mid[i],length(x))*((x > hx$breaks[i]) & (x < hx$breaks[i+1]))
  bin.no <- bin.no + rep(i,length(x))*((x > hx$breaks[i]) & (x < hx$breaks[i+1]))
}

### Let's look at some of these subgroups
boxplot(y[z==hx$mid[1]],main="Y vals for smallest X bin",ylab="Y") # Y values for observations in smallest X bin
boxplot(y[bin.no==1],main="Y vals for smallest X bin",ylab="Y") # Y values for observations in smallest X bin; same thing

boxplot(y[z==hx$mid[2]],main="Y vals for sec. smallst X bin",ylab="Y") # Y values for obs. in second smallest X bin
boxplot(y[z==hx$mid[length(hx$breaks)-1]],main="Y vals for largest X bin",ylab="Y") # Y values for obs. in largest X bin

boxplot(y~z,main="Boxplots for each bin") # all at once!

plot(x,y,main="Scatterplot for data")
abline(v=hx$mid,col="red") # just to give the picture of what we are doing


mean.y.given.x = rep(NA,length(hx$mid))
sd.y.given.x = rep(NA,length(hx$mid))
var.y.given.x = rep(NA,length(hx$mid))
for(i in 1:length(hx$mid)){
  if(hx$counts[i]>0){
    mean.y.given.x[i] = mean(y[z==hx$mid[i]])
    sd.y.given.x[i] = sd(y[z==hx$mid[i]])
    var.y.given.x[i] = sd.y.given.x[i]^2
  }}

mean.y.given.x
# how to plot both at same time
plot(x, y)
plot(hx$mid,mean.y.given.x,type="l",xlab="student teacher ratio",ylab="test score",
     col="blue",
     xlim=c(min(x),max(x)),ylim=c(min(y),max(y)))
abline(fit, col="red")

loess.fit = loess(y ~ x)
points(sort(x),predict(loess.fit,data.frame(x=sort(x))),col="green",type="l")
points(x,y,type="p")#,pch="+")
legend('topright',c("linear","binned E(Y|X)","Loess"),
       col=c('red','blue','green'), lty = c(1,1,1))

