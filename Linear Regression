library() #calls library of data

plot(medv~lstat, Boston)
fit1 = lm(med~lstat, data=Boston) #creates linear fit
summary(fill) #gives residuals, t-values, and p-values
abline(fit1,col="red") #adds red line on fit1
confint(fit1) #gives confidence intervals for intercept and lstat
predict(fit1,data.frame(lstat=c(5,10,15)),interval = "confidence") #predicts value if lstat=5,10, and 15 with confidence intervals

fit2 = lm(medv~lstat+age, data = Boston) #fit for 2 variables
fit3 = lm(medv~.,Boston) #fits medv for all predictors (except medv)
par(mfrow(c(2,2)) #creates 2x2 layer for plot
plot(fit3) #creates various views of linear model
fit4 = update(fit3,~.-age-indus) #same response, removes age and indus variables
fit5 = lm(medv~lstat*age, Boston) #linear model with lstat, age, and interactoin w/lstat+age
fit6 = lm(medv~lstat+I(lstat^2), Boston) #Identity function so literal square of lstat is put in
attach(Boston)
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20) #uses pounts() instead of abline() because not straight line
fit7 = lm(medv~ploy(lstat,4)) #fits polynomial of degree 4,(simpler way)
plot(1:20,1:20,pch=1:20,cex=2) #plots all plottin character, maginifies characters by 2
fix() #opens up editor

fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats) #plots all variables + interactions between income & advertising and age & price
contrasts(Carseats$ShelveLoc)  #shows how R will code that variable when put in a linear model (qualitative factors)
regplot = function(x,y){ #function to plot
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attatch (Carseats)
regplot(Price, Sales)
regplot = function(x,y,...){  #same function but allows arguemnets for labels, color, pch, etc in plot
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
