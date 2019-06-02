require(ISLR) #like using library but will return True or False if package does or does not exist
require(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)

##LOOCV
glm.fit=glm(mpg~horsepower,data=Auto)
cv.glm(Auto,glm.fit)$delta #delta is a vector component of cv.glm that gives cv and adjusted cv error

#writing function for LOOCV

loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2) #formula 5.2 from ISLR stat textbook
}

loocv(glm.fit)

cv.error=rep(0.5) #create vector for collecting the errors
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error[d]=loocv(glm.fit)
}

plot(degree,cv.error,type='b')

#10 fold CV

cv.error10=rep(0.5)
for(d in degree){
  glm.fit=glm(mpgpoly(horsepower,d),data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type='b',col='red')
