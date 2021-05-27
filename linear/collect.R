rm(list = ls())
setwd("/home/ruiqliu/SGD_LEC/linear")
options(width = 200)
n.sim = c(50000,100000,150000,200000,250000)

n.sim=c(10000, 20000, 50000, 100000)
n.sim=c(50000,100000)
n.sim=c(100000, 200000,500000,1000000)
#n.sim = c(50000)
output = c()
result = c()
for (n in n.sim) {
    ep.sim=c()
    ei.sim=c()
    cover.sim=c()
        
        filename = paste("data/n",
                         n,
                         ".txt",
                         sep = "")
        
        if (file.exists(filename)) {
          tryCatch({
        #    print(c(n,i))
            
            df = read.table(filename, sep = " ")
         #   errorP=abs(df[,1]-df[,4])
        #    errorP=errorP
        #    errorI=abs(df[,2]-df[,4])
        #    errorI=errorI
          #  cover=(df[,1]+1.645*df[,3]>df[,4])*(df[,1]-1.645*df[,3]<df[,4])
            pcover=(df[,2]+1.96*df[,4]>df[,5])*(df[,2]-1.96*df[,4]<df[,5])
            cover=(df[,3]+1.96*df[,4]>df[,5])*(df[,3]-1.96*df[,4]<df[,5])
            df$pcover=pcover
            df$cover=cover
            df$n=n
            df$perror=abs(df[,2]-df[,5])
            df$error=abs(df[,3]-df[,5])
            print(paste(n," ",dim(df)[1],sep=""))
            for (fac in c(1,2,3,4)){
              tempdf=df[which(df$V1==fac),]
              output=rbind(output,colMeans(tempdf))
            }
          }, error = function(e) {
            
          })
        }


  
}


#output=c(output$V1,output$cover, output$n, output$perror, output$error)
output=output[,c(1,6:10)]
colnames(output)[1]="beta"
print(output)
