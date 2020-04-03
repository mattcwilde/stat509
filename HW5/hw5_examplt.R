###################################
# anything after a "#" on the same line is ignored
# in other words it is a 'comment'
###################################



# Adapted from Ernst R. Berndt, "The Practice of Econometrics: #Classic   and Contemporary" (Reading, MA: Addison-Wesley, 1991),  
#  pp. 193-194. @
#     ed = education (in years)                                 
#     wage = hourly wage (in dollars)                             


# educ <- read.table('http://www.stat.washington.edu/tsr/s509/examples/caschool.csv',header=TRUE,sep=",")
# this assumes that you are connected to the internet

# Alternative
educ <- read.table('caschool.csv',header=TRUE,sep=",")

names(educ)      #what are vars
educ$ed[1]    # what is the education
# for first study subject
educ$wage[1]    # what is the wage
# for first study subject
educ[1,]       #both
educ$ed[1:5]
summary(educ$wage)
summary(educ$ed)

ed <- educ$ed    #make new vars to save typing the whole name
wage <- educ$wage

plot(ed,wage,xlab="Education in Years", ylab="Hourly Wage in Dollars",main="Relating Education and Wages")

# let's look at the wage variable
mean(wage)
summary(wage)
hist(wage)

# let's look at the education variable
mean(ed)
summary(ed)
hist(ed)

#let's fit a linear model

fit <- lm(wage ~ ed)
fit # just the coefficients
# Note that an intercept term is included by default

summary(fit) # more information on the fit
fit$coef # accessing the intercept and slope
cor(ed,wage) #find the correlation
cor(ed,wage)^2 #find the squared correlation

plot(ed,wage,xlab="Education in Years", ylab="Hourly Wage in Dollars",main="Relating Education and Wages")

abline(fit,col="red") #put the regression line on the plot

predict(fit,data.frame(ed=8)) # fitted value for ed=8
as.numeric(fit$coef[1] + fit$coef[2]*8)  #check
predict(fit) # fitted values for the 528 observed ed values
predict(fit,data.frame(ed)) # same thing
points(ed,predict(fit),col="purple",pch="+") #add fitted values to plot

# let's look at some residual plots
# don't worry if you don't understand all of these
plot(fit)

# let's do something a bit more fancy
loess.fit <- loess(wage  ~ ed)
help(loess)

plot(ed,wage,xlab="Education in Years", ylab="Hourly Wage in Dollars",main="Relating Education and Wages")

predict(loess.fit,data.frame(ed=8)) # fitted value for ed=8
predict(loess.fit,data.frame(ed))
points(sort(ed),predict(loess.fit,data.frame(ed=sort(ed))),col="green",type="l")

#q2: what does type="l" do here; what if we leave it out
#q3: why do we need to do a sort?

abline(fit,col="red")  #put back in our the linear regression line
#for comparison



hx <- hist(x)
abline(v=hx$breaks,col="red")


plot(x,y,main="Scatterplot for data")
abline(v=hx$breaks,col="red")

z <- rep(0,length(x))
bin.no <- rep(0,length(x))
for (i in 1:(length(hx$breaks)-1)){
  z <- z + rep(hx$mid[i],length(x))*((x > hx$breaks[i]) & (x < hx$breaks[i+1]))
  bin.no <- bin.no + rep(i,length(x))*((x > hx$breaks[i]) & (x < hx$breaks[i+1]))
}