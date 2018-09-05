# Purpose is to demonstrate simpe linear regression on real world dataset in R. 
library(ISLR) #calls datasets from Introduction to Statistical Learning Textbook
library(MASS) #calls datatsets from MASS dattabase

names(Boston) #provides names of all variables in the Boston dataset
?Boston #gives more detailed information on each variable

plot(medv~lstat, Boston)  #plots medv variable as a function of lstat
fit1 = lm(medv~lstat, data=Boston) #creates linear fit for medv(lstat)
names(fit1) #gives names of variables in linear model such as coefficients and residuals
summary(fitl) #gives residuals, t-values, and p-values for fit1
abline(fit1,col="red") #adds red line on fit1
confint(fit1) #gives confidence intervals for intercept and lstat
predict(fit1,data.frame(lstat=c(5,10,15)),interval = "confidence") #predicts value if lstat=5,10,and 15 with confidence intervals

fit2 = lm(medv~lstat+age, data = Boston) #creates fit for medv as a function of lstat and age
summary(fit2) #creates summary of fit2, giving residuals, t-values, and p-values
fit3 = lm(medv~.,Boston) #fits a fit for medv as a function of all the variables in the dataset, excluding medv
par(mfrow(c(2,2)) #creates 2x2 layer for plot
plot(fit3) #when used with previous line creates several plots that give information about linear fit
    
fit4 = update(fit3,~.-age-indus) #same response as fit 3, removes age and indus variables
summmary(fit4) 
fit5 = lm(medv~lstat*age, Boston) #linear model for medv with lstat, age, and interactoin w/lstat+age
summary(fit5)
fit6 = lm(medv~lstat+I(lstat^2), Boston)#linear fit for medv, accounting for quadratic function of lstat (sometimes can tell us something non-quadratic can't, or create a better fit)
summary(fit6)
    
attach(Boston)  #attaches Boston data, eliminates need to call Boston for every lm function
plot(medv~lstat)  #no need to call Boston
points(lstat,fitted(fit6),col="red",pch=20) #uses pounts() instead of abline() because fit 6 is not straight line, fitted creates points for fit 6, 
fit7 = lm(medv~ploy(lstat,4)) #fits polynomial of degree 4,(simpler way)
plot(1:20,1:20,pch=1:20,cex=2) #plots all plotting character, maginifies characters by 2
fix() #opens up editor to edit dataframe

fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats) #plots all variables + interactions between income & advertising and age & price in Carseats dataset
contrasts(Carseats$ShelveLoc)  #shows how R will code that variable when put in a linear model (qualitative factors)
    
regplot = function(x,y){ #function to plot linear model of y as a function of x
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
    
attatch (Carseats)
regplot(Price, Sales) #demonstration of earlier function
    
regplot = function(x,y,...){  #same function but allows arguemnets for labels, color, pch, etc in plot
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
    
#created with the aid of Robert Tribshirani and Trevor Hastie, from ISLR course
