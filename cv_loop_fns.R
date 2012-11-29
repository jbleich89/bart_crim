##BART CRIM FUNCTIONS


gen_data = function(data,train_size, cv_size,test_size, nsim){
  out = list() ##will return row indices
  out[["train"]] = matrix(nrow = train_size,ncol = nsim)
  out[["cv"]] = matrix(nrow = cv_size,ncol = nsim)
  out[["test"]] = matrix(nrow = test_size,ncol = nsim)
  for(i in 1:nsim){
    num_rows = sample(1:nrow(data),(train_size + test_size + cv_size),replace=F)
    out[["train"]][,i] = num_rows[1 : train_size]
    out[["cv"]][,i] = num_rows[(train_size + 1) : (cv_size + train_size)]
    out[["test"]][,i] = num_rows[(train_size + cv_size+1) : length(num_rows)]
  }
  out
}


bartVsRf_CVProbsLoop = function(data,idx_list,seed_list,nsim,train_size,cv_size,test_size,cost_ratio,...){
  require(BayesTree) ##setup
  require(randomForest) ##setup
  require(modeest)
  start = Sys.time()
  work = data
  storage = list()
  for(i in 1:nsim){
    storage[[i]]=list()
    #get rows
    train.rows=idx_list[["train"]][,i]
    cv.rows = idx_list[["cv"]][,i]
    test.rows=idx_list[["test"]][,i]
    #Set test/train split-disjoint  
    train = work[train.rows,] #sample data
    train[1,]=work[1,] ##include him for stratify to not get screwed up. 
    cv = work[cv.rows, ] ##CV data
    test = work[test.rows,] #test data
    print(dim(cv))
    print(dim(test))

    ##Train/test BART and get confusion table
    train.resp = ifelse(train[,1]=="fail",1,0)
    
    
    set.seed(seed_list[i]) ##set first run seed 
    print(seed_list[i])
    bart.mod = bart(x.train=train[,-1],y.train=train.resp,x.test=cv[,-1],...) ##get bart model
    #Training
    probs = pnorm(bart.mod$yhat.train) ##get probs on training set 
    cv.probs = pnorm(bart.mod$yhat.test) #get test probs
    #mean.probs=apply(probs,2,mean)
    #mean.probs.test=apply(probs,2,mean)
    #probs.list = apply(probs,2,mlv,method="parzen") ##density mode
    #mlv.probs.train = sapply(1:nrow(train), function(s) probs.list[[s]]$M)
    probs.cv.list = apply(cv.probs,2,mlv,method="parzen") #density mode
    mlv.probs.cv = sapply(1:nrow(cv), function(s) probs.cv.list[[s]]$M)
    
    ##Bisection based empirical cut off 
    bis_cut = bisect_loss(response = cv[,1], probs = mlv.probs.cv, cost_ratio = 5)

    print(bis_cut)
    cv.class = ifelse(mlv.probs.cv >= bis_cut,"Fail","No Fail")## check against train
    print("Train Table- Empirical Cutoff")
    cvTab = conf_table_stored_print(cv[,1],cv.class)
    impliedCost = as.numeric(cvTab[[4]][2])
    storage[[i]][["cv_bis"]] = cvTab
    print(impliedCost)
    
    #Test for bisect
    set.seed(seed_list[i]) ##set first run seed 
    print(seed_list[i])
    bart.mod = bart(x.train=train[,-1],y.train=train.resp,x.test=test[,-1],...) ##get bart model
    test.probs = pnorm(bart.mod$yhat.test) #get test probs
    #mean.probs=apply(probs,2,mean)
    #mean.probs.test=apply(probs,2,mean)
    probs.test.list = apply(test.probs,2,mlv,method="parzen") #density mode
    mlv.probs.test = sapply(1:nrow(test), function(s) probs.test.list[[s]]$M)
    test.class = ifelse(mlv.probs.test >= bis_cut,"Fail","No Fail")## check against train
    print("Test Table Empirical Cut")
    testTab=conf_table_stored_print(test[,1],test.class)
    storage[[i]][["test_bis"]]=testTab
    
  
    ##RF Work
    ##get 2/3 fail class for RFs
    rf.resp=train[,1]
    rf_fail=ceiling(2/3*length(which(rf.resp=="fail"))) ##set 2/3 of fails to be sampled
    rf_nofail=ceiling(rf_fail*1.25) ##set 1.25 no fail to start and adjust
    rfCost=-1
    while(abs(rfCost-cost_ratio)>.25){ #check if cost within .25 of 5
      rf=randomForest(x=train[,-1],y=rf.resp,ntree=500,sampsize=c(rf_fail,rf_nofail))
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


bisect_loss=function(response,probs,cost_ratio,xl=.05,xr=.2,ntry=200,tol=.2){
  count=0
  impliedCost=0
  while(count<=ntry & .5*abs(impliedCost-cost_ratio)>tol){
    xmid=(xl+xr)/2
    tryclass=ifelse(probs>=xmid,"Fail","No Fail")
    trainTab=conf_table_stored_print(response,tryclass)
    impliedCost=as.numeric(trainTab[[4]][2])
    #print(impliedCost)
    if(impliedCost==cost_ratio) {
      print(impliedCost)
      return(xmid)
    }
    leftclass=ifelse(probs>=xl,"Fail","No Fail")
    trainTab=conf_table_stored(response,leftclass)
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