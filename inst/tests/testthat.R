library(testthat)
library(MesFonctions)

#test_check("MesFonctions")

x<-runif(1:20)
y<-2+3*x+rnorm(20,0,5)
data<-data.frame(y,x)
fit<-lm(y~x,data=data)
RSS(fit)
PRESS(fit)
