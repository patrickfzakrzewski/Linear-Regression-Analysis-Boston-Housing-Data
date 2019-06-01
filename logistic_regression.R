#Using Smarket data set with variables Lag(1-6), Direction, and Volume
require() #similar to library
library(ISLR) #callISLR

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+volume,data=Smarket,family=binomial) #creates logistic regression fit

glm.probs=predict(glm.fit,type="response")  #predicts proabability

glm.pred=ifelse(glm.probs>0.5,"Up","Down") #classifies up vs down based on probability
attach(Smarket) # attaches variables
table(glm.predict,Direction)  #creates prediction table

mean(glm.pred==Direction) #compares prediction to actual direction

train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.prob=predict(glm.fit,newdata=Smarket[!train,], type="response")
#Divides data into training set(Years<2005) and test set (Years>2005)

Direction.2005=Smarket$Direction[!train]
#compares pred to actual, when fitting only for response variables Lag1 and Lag2, mean was found to be .56
