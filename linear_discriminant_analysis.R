require(ISLR)
require(MASS) #call libraries

lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset = Year<2005) #create lda fit

lda.fit #view fit

plot(lda.fit) #view plot of fit

Smarket.2005=subset(Smarket,Year==2005) #create training data

lda.pred=predict(lda.fit,Smarket.2005) #create predictions on trainng data
  
lda.pred[1:5] #view first 5 predictions
  
class(lda.pred) #view class
  
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction) #view table of predictions vs actual
mean(lda.pred$class==Smarket.2005$Direction) #mean percentage correct
