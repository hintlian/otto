test <- read.csv("test.csv", header=T)
train <- read.csv("train.csv", header=T)

count<-0
crit <- .05
classes<- c("Class_1","Class_2", "Class_3", "Class_4" , "Class_5" ,"Class_6","Class_7","Class_8","Class_9")
#classes <-c("Class_1")
ccount <-0
pred<-data.frame(id=test[,1])
for(C in classes){
  ccount <- ccount + 1
  tempmax<- 1
  tempdata <- data.frame(train[,-95] , class=ifelse(train$target==C,1,0))
  while (tempmax > crit){
    g <- glm(class~.-id ,data=tempdata, family = "binomial")
    summary(g)
    tempvalue <- summary(g)$coefficients[,4]
    tempmax <-max(tempvalue)
    for (i in 2:93 ){
      if(tempmax == summary(g)$coefficients[i,4]) {
        tempdata <- tempdata[,-i]
        break
      }
    }
  }  
  summary(g)
  pred <- cbind(pred,C=predict(g,test,type="response"))
}
