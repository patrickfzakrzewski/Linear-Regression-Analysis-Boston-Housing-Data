library(class)
attach(Smarket)
Xlag=cbind(Lag1,Lag2) #creates matric with 2 columns; lag1 and lag2
train = Year<2005
knn.pred=knn(Xlag[train,]Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
