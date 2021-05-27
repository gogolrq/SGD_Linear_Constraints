rm(list = ls())
setwd("/home/ruiqliu/SGD_LEC/logit_test")
options(width = 200)
n.sim = c(50000,100000,150000,200000,250000)

n.sim=c(10000, 20000, 50000, 100000)
n.sim=c(50000,100000)
n.sim=c(100000,200000,500000,1000000)
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
      
            print(paste(n," ",dim(df)[1],sep=""))
            for (fac in c(0, 0.005, 0.01, 0.015, 0.02, 0.025)){
              tempdf=df[which(df$V5==fac),]
              output=rbind(output,colMeans(tempdf))
            }
          }, error = function(e) {
            
          })
        }


  
}

output=output[,3:5]
colnames(output)=c("rp","n","r")
print(output)