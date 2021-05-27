rm(list = ls())
setwd("/home/ruiqliu/SGD_LEC/logit")
library(MASS)
args <- commandArgs(TRUE)
parameters = as.numeric(args)

 

n = parameters[1]
RandomSeed = parameters[2]
set.seed(RandomSeed)


p=4 
x=rnorm(n*p)
x=matrix(x,nrow = n)
x=cbind(x)
beta=c(1,-2,-2,1.5)
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
  
  err=mean((sb/i-beta)^2)
  
  bI=bI-tau*dl(y[i],x[i,],bI)
  sbI=sbI+bI
  
  Ghat=Ghat+ddl(y[i],x[i,],sb/i) 
  Shat=Shat+dl(y[i],x[i,],sb/i)%*%t(dl(y[i],x[i,],sb/i)) 
}
count=n
Ghat=Ghat/count
Shat=Shat/count

Vhat=ginv(P%*%Ghat%*%P)%*%Shat%*%ginv(P%*%Ghat%*%P)
std=sqrt(diag(Vhat/n))
se=std
sb=sb/n
sbI=sbI/n




result=data.frame(sb=sb,sbI=sbI,std=std,beta=beta)

print(result)
filename=paste("data/n",n, ".txt", sep = "")

write.table(result, file = filename, append = TRUE, quote = TRUE, sep = " ", col.names = FALSE)