setwd("~/Criminology_Research/BART/bart_crim") ##work
setwd("~/Documents/Research/Crim/BART") #home

load("~/Criminology_Research/BART/working2.rdata")
source("aux_functions2.R")
source("bart_prob_fns.R")
source("cv_loop_fns.R")
library(BayesTree);library(randomForest)

##Check for NAs
temp=apply(work2,2,is.na)
apply(temp,2,sum) ##only AFirstChargeAge has NAs
##remove Na rows
work3=work2[-which(is.na(work2$AFirstChargeAge)),]
work4=work3[,-c(1,2,4,30)] ##get rid of some columns

hashNames=c("bartTrain","bartTest","rfTrain","rfTest")
##currently specific to our data set
##should take work3h

nsim = 10
train_size = 1000; cv_size = 999; test_size = 998
id_list = gen_data(data = work4,train_size = 1000,cv_size = 999,test_size = 998,nsim = 10)
seed_list =sample.int(10000, nsim)
out10 = bartVsRf_CVProbsLoop(data = work4,idx_list = id_list,seed_list = seed_list,nsim = nsim,
                             train_size = train_size, cv_size = cv_size, 
                             test_size = test_size, cost_ratio = 5, ntree=200,ndpost=200)


out30=bartVsRf_LoopProbs(data=work4,nsim=30,train_size=1200,test_size=1000,loss_cut=1/6,cost_ratio=5,nskip=500,ndpost=1000)
hashnames=c("cv_bis","test_bis","rf_train","rf_test")
results=getResultMatrix(calcList=out10,hashnames)
plotRB(data=results,"train_bis","test_bis","impliedCost",pch=16)
results




