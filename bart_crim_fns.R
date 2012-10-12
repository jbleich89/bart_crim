##BART CRIM FUNCTIONS

bartVsRf_Loop=function(data,nsim,train_size,test_size,p.fail,p.nofail,...){
  require(BayesTree)
  require(randomForest)
  require(sampling)
  work=data
  storage=list()
  for(i in 1:nsim){
    storage[[i]]=list()
    num.rows=sample(x=1:nrow(work),(train_size+test_size),replace=F) ##draw rows
    
    train.rows=num.rows[1:train_size]
    test.rows=num.rows[(train_size+1):length(num.rows)] #get disjoint set
    #Set test/train split-disjoint  
    samp=work[train.rows,] #sample data
    samp[1,]=work[1,] ##include him for stratify to not get screwed up. 
    test=work[test.rows,] #test data
    print(dim(test))
    
    ##change of measure
    alt.rows=alter_prior(samp,p.nofail,p.fail,"FailSerious") #alter prior
    train.data=samp[alt.rows,] ##training data
    response=train.data[,1] ##set response in training set with altered prior
    print(table(response))
    
    
    ##Train and get confusion table
    bart.mod=bart(x.train=train.data[,-1],y.train=response,x.test=samp[,-1],...)
    probs=pnorm(bart.mod$yhat.test)
    mean.probs=apply(probs,2,mean)
    class=ifelse(mean.probs>=.5,"No Fail","Fail")
    print("Train Table")
    trainTab=conf_table_stored_print(samp[,1],class)
    impliedCost=as.numeric(trainTab[[4]][2])
    storage[[i]][[1]]=trainTab
    print(impliedCost)
    
    ##Test (not identical trees)
    bart.test=bart(x.train=train.data[-1],y.train=response,x.test=test[,-1],...) ##get predictions for original and build table
    test.probs=pnorm(bart.test$yhat.test)
    mean.test.probs=apply(test.probs,2,mean)
    test.class=ifelse(mean.test.probs>=.5,"No Fail","Fail")
    test.response=test[,1]
    print("Test Table")
    testTab=conf_table_stored_print(test.response,test.class)
    storage[[i]][[2]]=testTab
    
    ##get 2/3 fail class for RFs
    rf.resp=samp[,1]
    rf_fail=ceiling(2/3*length(which(rf.resp=="fail"))) ##set 2/3 of fails to be sampled
    rf_nofail=ceiling(rf_fail*1.25) ##set 1.25 no fail to start and adjust
    rfCost=-1
    while(abs(rfCost-impliedCost)>.5){ #check if cost within .5
      rf=randomForest(x=train.data[,-1],y=rf.resp,ntree=500,sampsize=c(rf_fail,rf_nofail))
      rfTrainTab=conf_table_stored_print(rf.resp,rf$predicted)
      rfCost=as.numeric(rfTrainTab[[4]][2])
      rf_nofail=rf_nofail-2
      if(rf_nofail<=0) break 
    }
    storage[[i]][[3]]=rfTrainTab
    test.classes=predict(rf,newdata=test,type="class")
    print("Test Table")
    rfTestTab=conf_table_stored_print(actual=test[,1],test.classes)
    storage[[i]][[4]]=rfTestTab
    print(i )
  }
  return(storage)
}

#save(item,file="bartRFList.rdata")


alter_prior=function(data,p.fail,p.nofail,response_name){
  num_fail=ceiling(nrow(data)*p.fail)
  num_nofail=ceiling(nrow(data)*p.nofail)
  strat.samp=strata(data=data, stratanames=response_name,
                    size=c(num_fail,num_nofail),method="srswr")[,"ID_unit"]
  return(strat.samp)
}


bartVsRf_Loop_seeded=function(idx_list,data,seed.list,nsim,train_size,test_size,p.fail,p.nofail,...){
  require(BayesTree)
  require(randomForest)
  require(sampling)
  work=data
  storage=list()
  for(i in 1:nsim){
    storage[[i]]=list()
    train.rows=idx_list[["train"]][,i]
    test.rows=idx_list[["test"]][,i]
    #Set test/train split-disjoint  
    samp=work[train.rows,] #sample data
    samp[1,]=work[1,] ##include him for stratify to not get screwed up. 
    test=work[test.rows,] #test data
    print(dim(test))
    
    ##change of measure
    alt.rows=alter_prior(samp,p.nofail,p.fail,"FailSerious") #alter prior
    train.data=samp[alt.rows,] ##training data
    response=train.data[,1] ##set response in training set with altered prior
    print(table(response))
    
    set.seed(seed.list[i])  
    ##Train and get confusion table
    bart.mod=bart(x.train=train.data[,-1],y.train=response,x.test=samp[,-1],...)
    probs=pnorm(bart.mod$yhat.test)
    mean.probs=apply(probs,2,mean)
    class=ifelse(mean.probs>=.5,"No Fail","Fail")
    print("Train Table")
    trainTab=conf_table_stored_print(samp[,1],class)
    impliedCost=as.numeric(trainTab[[4]][2])
    storage[[i]][[1]]=trainTab
    print(impliedCost)
    
    ##Test (not identical trees)
    set.seed(seed.list[i])
    bart.test=bart(x.train=train.data[-1],y.train=response,x.test=test[,-1],...) ##get predictions for original and build table
    test.probs=pnorm(bart.test$yhat.test)
    mean.test.probs=apply(test.probs,2,mean)
    test.class=ifelse(mean.test.probs>=.5,"No Fail","Fail")
    test.response=test[,1]
    print("Test Table")
    testTab=conf_table_stored_print(test.response,test.class)
    storage[[i]][[2]]=testTab
    
    ##get 2/3 fail class for RFs
    rf.resp=samp[,1]
    rf_fail=ceiling(2/3*length(which(rf.resp=="fail"))) ##set 2/3 of fails to be sampled
    rf_nofail=ceiling(rf_fail*1.25) ##set 1.25 no fail to start and adjust
    rfCost=-1
    while(abs(rfCost-impliedCost)>.5){ #check if cost within .5
      rf=randomForest(x=train.data[,-1],y=rf.resp,ntree=500,sampsize=c(rf_fail,rf_nofail))
      rfTrainTab=conf_table_stored_print(rf.resp,rf$predicted)
      rfCost=as.numeric(rfTrainTab[[4]][2])
      rf_nofail=rf_nofail-2
      if(rf_nofail<=0) break 
    }
    storage[[i]][[3]]=rfTrainTab
    test.classes=predict(rf,newdata=test,type="class")
    print("Test Table")
    rfTestTab=conf_table_stored_print(actual=test[,1],test.classes)
    storage[[i]][[4]]=rfTestTab
    print(i )
  }
  return(storage)
}


getResultMatrix=function(calcList,hashNames){            
  temp=list() 
  output=list()
  for(j in 1:4){
    mat=matrix(nrow=length(calcList),ncol=6)
    for(i in 1:length(calcList)){            
      
      mat[i,1]=calcList[[i]][[j]][[1]][[2]][[1]] ##modelErrorFail
      mat[i,2]=calcList[[i]][[j]][[1]][[2]][[2]]  ##model error NoFail
      
      mat[i,3]=calcList[[i]][[j]][[2]][[2]][[1]] ##UseErrorFail
      mat[i,4]=calcList[[i]][[j]][[2]][[2]][[2]]  ##Use error NoFail
      
      mat[i,5]=calcList[[i]][[j]][[3]][[2]] #overall error
      mat[i,6]=calcList[[i]][[j]][[4]][[2]] ##implied cost
    }
    colnames(mat)=c("modErrFail","modErrNoFail","useErrFail","useErrNoFail","overallErr","impliedCost")
    temp[[hashNames[j]]]=mat
    
    ##clean out for RFs that didn't work
  }
  exc=which(temp[["rfTest"]][,"impliedCost"]==Inf)
  if(length(exc!=0)){
    for(i in 1:4){
      output[[hashNames[i]]]=temp[[i]][-exc,]
    }
  }
  else {output=temp}
  return(output)
}  

plotRB=function(data,xName,yName,metric,...){
  par(mgp=c(1.8,.5,0), mar=c(3,3,2,1))
  xdata=data[[xName]][,metric]
  ydata=data[[yName]][,metric]
  lower=min(min(xdata),min(ydata))
  upper=max(max(xdata),max(ydata))
  plot(xdata,ydata,xlim=c(lower,upper),ylim=c(lower,upper),...)
  abline(0,1,col="red")
}