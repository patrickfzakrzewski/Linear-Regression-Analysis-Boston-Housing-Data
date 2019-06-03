##Bootstrap
##Minimnum risk investment

#function to determine optimal investment strategy between two assets so that risk (variance) is minimized
alpha=function(x,y){ 
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx-vy-2*cxy)
 }
 
alpha(Portfolio$X,Portfolio$Y)


alpha.fn=function(data,index){
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE) #of rows 1 to 100, going to take 100 random samples with replace=true and apply alpha function, gives bootstrap of sample size 1

boot.out=boot(Portfolio,alphafn,R=1000)
boot.out
plot(boot.out)
