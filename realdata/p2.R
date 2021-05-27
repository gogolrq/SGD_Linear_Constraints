rm(list=ls())
library(MASS)
library(higrad)
setwd("D:/Dropbox/SGD_Linear_Constraint/realdata")
set.seed(1)
df=read.csv("CASP.csv")
for (i in 2:10){
  mu=mean(df[,i])
  sigma=sd(df[,i])
  df[,i]=(df[,i]-mu)/sigma
}
n=dim(df)[1]
p=9
x=as.matrix(df[,2:(2+p-1)])
x=cbind(rep(1,n),x)
y=as.numeric(df[,1])

dl=function(y,x,b){
  s=-(y-sum(b*x))*x
  return(s)
}
ddl=function(y,x,b){
  s=x%*%t(x)
  return(s)
}
B=rep(0,(p+1)*1)
B=matrix(B,ncol=p+1)

#B[1,2]=1
#B[2,6]=1
#B[3,8]=1
B[1,10]=1

P=diag(rep(1,p+1))-t(B)%*%solve(B%*%t(B))%*%B
I=diag(rep(1,p+1))
#P=diag(rep(1,p+1))
#b=lm(y[1:10000]~x[1:10000,]-1)$coef
b=c(0,rep(0,p))
sb=c(0,rep(0,p))
bI=b
sbI=sb
Ghat=0
Shat=0
n0=1
for(i in 1:n){
  if (i<=n0){
    tau=0.1
  }else{
    tau=0.5*(i-n0)^(-0.501)
    # print(t(b))
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
Vhat=ginv(Ghat)%*%Shat%*%ginv(Ghat)
std=sqrt(diag(Vhat/n))
sb=sb/n
sbI=sbI/n
What=(I-P)%*%ginv(Ghat)%*%Shat%*%ginv(Ghat)%*%(I-P)
kappa=n*t(sb-sbI)%*%ginv(What)%*%(sb-sbI)

cf1=sb
cf0=sbI
cf2=higrad(x,y,nsplits =1, nthreads = 1)$coefficients
cf3=lm(y~x-1)$coef
print(cbind(cf1,cf0,cf2,cf3))



pv=2*pnorm(-abs(cf1/std))
pv=round(pv,3)
