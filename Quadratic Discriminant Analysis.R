#Continuatino of LDA, some variables are declared there
qda.fit=qda(Direction∼Lag1+Lag2 ,data=Smarket ,subset=train)
qda.fit
qda.class=predict (qda.fit ,Smarket.2005)$class
table(qda.class,Smarket.2005$Direction)
mean(qda.class==Smarket.2005$Direction)
