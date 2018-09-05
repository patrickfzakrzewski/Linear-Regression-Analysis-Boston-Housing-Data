#Using Smarket data set with variables Lag(1-6), Direction, and Volume
require() #similar to library

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+volume,data=Smarket,family=binomial)

glm.probs=predict(glm.fit,type="response")  #predicts proabability

glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.predict,Direction)  #creates table

mean(glm.pred==Direction) #cases where glm.pred is = to Direction, gives probability

train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.prob=predict(glm.fit,newdata=Smarket[!train,], type="response")
#Divides data into training set(Years<2005) and test set (Years>2005)

Direction.2005=Smarket$Direction[!train]
#in this case mean was <0., possible overfitting, when fitting only for response variables Lag1 and Lag2, mean was found to be .56
