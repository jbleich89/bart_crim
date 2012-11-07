##BART CRIM FUNCTIONS

bartVsRf_LoopProbs=function(data,nsim,train_size,test_size,loss_cut,cost_ratio,...){
  require(BayesTree) ##setup
  require(randomForest) ##setup
  require(modeest)
  start=Sys.time()
  work=data
  storage=list()
  for(i in 1:nsim){
    storage[[i]]=list()
    num.rows=sample(x=1:nrow(work),(train_size+test_size),replace=F) ##draw rows
    
    train.rows=num.rows[1:train_size] ##size of training
    test.rows=num.rows[(train_size+1):length(num.rows)] #get disjoint set
    #Set test/train split-disjoint  
    samp=work[train.rows,] #sample data 
    test=work[test.rows,] #test data
    print(dim(test))

    ##Train/test BART and get confusion table
    resp=ifelse(samp[,1]=="fail",1,0)
    bart.mod=bart(x.train=samp[,-1],y.train=resp,x.test=test[,-1],...) ##get bart model
    #Training
    probs=pnorm(bart.mod$yhat.train) ##get probs on training set 
    test.probs=pnorm(bart.mod$yhat.test) #get test probs
    #mean.probs=apply(probs,2,mean)
    #mean.probs.test=apply(probs,2,mean)
    probs.list=apply(probs,2,mlv,method="parzen") ##density mode
    mlv.probs=sapply(1:nrow(samp), function(s) probs.list[[s]]$M)
    probs.test.list=apply(test.probs,2,mlv,method="parzen") #density mode
    mlv.probs.test=sapply(1:nrow(test), function(s) probs.test.list[[s]]$M)
    ##A priori cut off 
    class=ifelse(mlv.probs>=loss_cut,"Fail","No Fail") ##classify based on what you input
    print("Train Table A Priori Cut") 
    trainTab=conf_table_stored_print(samp[,1],class) ##create table
    impliedCost=as.numeric(trainTab[[4]][2]) #useless
    storage[[i]][["train_naive"]]=trainTab # save it
    print(impliedCost)
    
    #Test Naive
    test.class=ifelse(mlv.probs.test>=loss_cut,"Fail","No Fail") ##classify test naively
    print("Test Table A Priori Cut") 
    testTab=conf_table_stored_print(test[,1],test.class) ##create table
    storage[[i]][["test_naive"]]=testTab ##store it

    ##Bisection based empirical cut off -TODO
    bis_cut=bisect_loss(samp=samp,probs=mlv.probs,cost_ratio=cost_ratio) ##get bisection value based off training
    print(bis_cut)
    class=ifelse(mlv.probs>=bis_cut,"Fail","No Fail")## change
    print("Train Table- Empirical Cutoff")
    trainTab=conf_table_stored_print(samp[,1],class)
    impliedCost=as.numeric(trainTab[[4]][2])
    storage[[i]][["train_bis"]]=trainTab
    print(impliedCost)
    
    #Test for bisect
    test.class=ifelse(mlv.probs.test>=bis_cut,"Fail","No Fail")
    print("Test Table Empirical Cut")
    testTab=conf_table_stored_print(test[,1],test.class)
    storage[[i]][["test_bis"]]=testTab
    
  
    ##RF Work
    ##get 2/3 fail class for RFs
    rf.resp=samp[,1]
    rf_fail=ceiling(2/3*length(which(rf.resp=="fail"))) ##set 2/3 of fails to be sampled
    rf_nofail=ceiling(rf_fail*1.25) ##set 1.25 no fail to start and adjust
    rfCost=-1
    while(abs(rfCost-cost_ratio)>.25){ #check if cost within .25 of 5
      rf=randomForest(x=samp[,-1],y=rf.resp,ntree=500,sampsize=c(rf_fail,rf_nofail))
      rfTrainTab=conf_table_stored(rf.resp,rf$predicted)
      rfCost=as.numeric(rfTrainTab[[4]][2])
      rf_nofail=rf_nofail-1
      if(rf_nofail<=0) break 
    }
    storage[[i]][["rf_train"]]=rfTrainTab
    test.classes=predict(rf,newdata=test,type="class")
    print("Test Table")
    rfTestTab=conf_table_stored_print(actual=test[,1],test.classes)
    storage[[i]][["rf_test"]]=rfTestTab
    print(i)
  }
  print(Sys.time()-start)
  return(storage)
}


bisect_loss=function(samp,probs,cost_ratio,xl=.05,xr=.2,ntry=200,tol=.2){
  count=0
  impliedCost=0
  while(count<=ntry & .5*abs(impliedCost-cost_ratio)>tol){
    xmid=(xl+xr)/2
    tryclass=ifelse(probs>=xmid,"Fail","No Fail")
    trainTab=conf_table_stored_print(samp[,1],tryclass)
    impliedCost=as.numeric(trainTab[[4]][2])
    #print(impliedCost)
    if(impliedCost==cost_ratio) {
      print(impliedCost)
      return(xmid)
      }
    leftclass=ifelse(probs>=xl,"Fail","No Fail")
    trainTab=conf_table_stored(samp[,1],leftclass)
    leftCost=as.numeric(trainTab[[4]][2])
    
    if(sign(leftCost-cost_ratio)==sign(impliedCost-cost_ratio)){
      xl=xmid
    }
    else xr=xmid
    count=count+1
  }
  print(impliedCost)
  return(xmid) 
  
}

hashNames=hashnames
calcList=out
out[[1]]
getResultMatrix(out,hashnames)
getResultMatrix=function(calcList,hashNames){            
  temp=list() 
  output=list()
  for(j in hashNames){
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
    temp[[j]]=mat
    
    ##clean out for RFs that didn't work
  }
  exc=which(temp[["rfTest"]][,"impliedCost"]==Inf)
  if(length(exc!=0)){
    for(i in hashNames){
      output[[i]]=temp[[i]][-exc,]
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