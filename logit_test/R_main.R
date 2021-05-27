rm(list = ls())
setwd("/home/ruiqliu/SGD_LEC/logit_test")
library(MASS)
args <- commandArgs(TRUE)
parameters = as.numeric(args)

 

n = parameters[1]
r = parameters[2]
RandomSeed = parameters[3]
set.seed(RandomSeed)


p=4 
x=rnorm(n*p)
x=matrix(x,nrow = n)
x=cbind(x)
beta=c(1,-2,-2+r,1.5)
y=x%*%beta+rlogis(n)
y=y>0
cf=glm(y[1:1000]~x[1:1000,]-1,family = binomial)$coef
lmod=glm(y~x-1,family = binomial)
ocf=lmod$coef
y=(y==1)-(y==0)

dl=function(y,x,b){
  s=1+exp(y*sum(x*b))
  s=-y*x/s
  return(s)
}
ddl=function(y,x,b){
  s=1+exp(y*sum(x*b))
  s=exp(y*sum(x*b))/(s^2)
  s=s*x%*%t(x)
  return(s)
}
B=rep(0,p)
B[2]=1
B[3]=-1
B=t(B)
P=diag(rep(1,p))-t(B)%*%solve(B%*%t(B))%*%B
I=diag(rep(1,p))

b=rnorm(p,sd=0.1)
sb=b 
bI=b
sbI=sb
Ghat=0
Shat=0
count=0

for(i in 1:n){
  if (i<1){
    tau=0.5
  }else{
    tau=1*i^(-0.505)
  }
  b=b-tau*dl(y[i],x[i,],b) 
  b=P%*%b
  sb=sb+b
  
  bI=bI-tau*dl(y[i],x[i,],bI)
  sbI=sbI+bI
  Ghat=Ghat+ddl(y[i],x[i,],sbI/i) 
  Shat=Shat+dl(y[i],x[i,],sbI/i)%*%t(dl(y[i],x[i,],sbI/i)) 
}
Ghat=Ghat/n
Shat=Shat/n
What=(I-P)%*%ginv(Ghat)%*%Shat%*%ginv(Ghat)%*%(I-P)

sb=sb/n
sbI=sbI/n
kappa=n*t(sb-sbI)%*%ginv(What)%*%(sb-sbI)





result=data.frame(kappa=kappa, test=kappa>qchisq(0.95,df=1),n=n, r=r)

print(result)
filename=paste("data/n",n, ".txt", sep = "")

write.table(result, file = filename, append = TRUE, quote = TRUE, sep = " ", col.names = FALSE)