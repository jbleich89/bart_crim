setwd("~/Criminology_Research/BART/bart_crim") ##work
setwd("~/Documents/Research/Crim/BART") #home

load("~/Criminology_Research/BART/working2.rdata")
source("aux_functions2.R")
source("bart_crim_fns.R")

##Check for NAs
temp=apply(work2,2,is.na)
apply(temp,2,sum) ##only AFirstChargeAge has NAs
##remove Na rows
work3=work2[-which(is.na(work2$AFirstChargeAge)),]
work4=work3[,-c(1,2,4,30)] ##get rid of some columns

hashNames=c("bartTrain","bartTest","rfTrain","rfTest")
##currently specific to our data set
##should take work3




work=work4;i=1
train_size=1000
test_size=900
p.fail=.48;p.nofail=1-p.fail
  

train=work[1:1500,]
test=work[1501:2500,]
resp=train[,1]
testclass=test[,1]
table(resp)
table(testclass)
rtrain=ifelse(resp=="fail",1,0)
rtest=ifelse(testclass=="fail",1,0)
table(rtrain)
table(rtest)

dim(train)
##Train and get confusion table
bart.mod=bart(x.train=train[,-1],y.train=rtrain,x.test=test[,-1],ntree=200,nskip=200,ndpost=1000)
plot(bart.mod)
probs=pnorm(bart.mod$yhat.train)
mean.probs=apply(probs,2,mean)
hist(mean.probs)

class=ifelse(mean.probs>=.115,"Fail","No Fail")
trainTab=conf_table_stored_print(train[,1],class)
impliedCost=as.numeric(trainTab[[4]][2])
print(impliedCost)

#BART Test
probs=pnorm(bart.mod$yhat.test)
test.mean.probs=apply(probs,2,mean)
hist(test.mean.probs)

predclass=ifelse(test.mean.probs>=.115,"Fail","No Fail")
trainTab=conf_table_stored_print(test[,1],predclass)



library(randomForest)
rf=randomForest(x=train[,-1],y=train[,1],ntree=500,sampsize=c(70,85))
conf_table(train[,1],rf$predicted)
testpreds=predict(rf,test,type="class")
conf_table(test[,1],testpreds)


plotRB(clean6,"bartTest","rfTest","overallErr",pch=16,xlab="BART Test Data",ylab="RF Test Data"
       ,main="Overall Error")
