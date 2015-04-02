test <- read.csv("test.csv", header=T)
train <- read.csv("train.csv", header=T)
library(reshape)
classes<- c("Class_1","Class_2", "Class_3", "Class_4" , "Class_5" ,"Class_6","Class_7","Class_8","Class_9")
split <- sort_df(data.frame(train, randnum = runif(nrow(train))),vars=96)

validation <- sort_df(split[1:12376,1:95],1)
vtrain <- sort_df(split[12377:61878,1:95],1)

all <- data.frame(rbind(train[,-95],test))

sall <- scale(all)
best <- c(1,10000)
clusters <- 10:10000
for(clust in clusters){
  fit<-kmeans(sall,clust, iter.max=100)
  
  compare<-data.frame(cbind(fit$cluster[vtrain[,1]],vtrain[,95]))
  
  m <- matrix(0, ncol = 9, nrow = clust)
  
  for(i in 1:9){
    for (j in 1:clust){
      m[j,i] <-length(which(compare[,1]==j & compare[,2]==i ))
    }
  }
  
  m<-m[,1:9]/rowSums(m[,1:9])
  
  cv<- data.frame(validation[,1], m[fit$cluster[validation[,1]],])
  
  logloss<-0
  
  for(i in 1:nrow(validation)){
    for(j in 2:10){
      
      if( cv[i,j]<10^-15){ 
        cv[i,j]<-10^-15 
      } else if(cv[i,j]>(1-10^-15)){
        cv[i,j]<- 1-10^-15
      }
      
      if( classes[j-1]==validation[i,95]){
        logloss <- logloss - log(cv[i,j])/nrow(validation)
      }
    }
  }
  if(best[2]>logloss){
    best[1]<-clust
    best[2]<-logloss
  }
}
