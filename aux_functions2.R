##1. Set-up
##creates initial data with dob variable-must run other 2 functions outside to have usability
setup=function(){
  require(rpart)
  load("working2.rdata")
  
##Date Functions-data set specific
  date2jul=function(dates){
    j.dates=julian(dates,origin=min(dates))
    }

  jul2date=function(j.dates){
    dates=as.Date(j.dates,min(Birth)) ##not robust because of Birth- data set specific-could change with     
    }                                 ##additional input 


  dob=date2jul(work2$Birth)
  work3=data.frame(work2,dob)
  return(work3)
}

##2. Get training and test set
splitsamp=function(n,splitsize,data){
  idx=sample(1:n,size=n*splitsize,replace=F)
  train=data[idx,]
  test=data[-idx,]
  z=list()
  z[[1]]=train
  z[[2]]=test
  return(z)
  }


##Create formulas
f2=FailSerious~dob+PriorJailDays+PriorJailStays+ASeriousPriors+JSeriousPriors+AFirstChargeAge+JFirstChargeAge+ADrugPriors+JDrugPriors+AMurderPriors
f4=FailSerious~dob+PriorJailDays+PriorJailStays+ASeriousPriors+JSeriousPriors+AFirstChargeAge+JFirstChargeAge+AAllPriors+JAllPriors
f5=FailSerious~dob+PriorJailDays+PriorJailStays+ASeriousPriors+JSeriousPriors+AFirstChargeAge+JFirstChargeAge+ADrugPriors


##Tree plotting
plottree <- function(tree)
{
  par(xpd=NA) # otherwise on some devices the text is clipped
  plot(tree) # plot the tree
  text(tree, use.n=TRUE) # display labels
}

##Confusion Table Functions
conf_table=function(actual,predicted){
  tab=table(actual, predicted)
  print(tab)
  cat(c("\nModel Error \n",round(tab[1,2]/sum(tab[1,]),3),"\n",round(tab[2,1]/sum(tab[2,]),3),"\n\n"))
  cat(c("Use Error \n",round(tab[2,1]/sum(tab[,1]),3),round(tab[1,2]/sum(tab[,2]),3),"\n\n"))
    cat(c("Overall Error \n",round((tab[1,2]+tab[2,1])/(sum(tab[1,])+sum(tab[2,])),3),"\n\n"))
    cat(c("Implied Cost Ratio \n",round(tab[2,1]/tab[1,2],3)))
 }

conf_table_stored_print=function(actual,predicted){
   data=list()
    tab=table(actual, predicted)
  print(tab)
  cat(c("\nModel Error \n",round(tab[1,2]/sum(tab[1,]),3),"\n",round(tab[2,1]/sum(tab[2,]),3),"\n\n"))
  cat(c("Use Error \n",round(tab[2,1]/sum(tab[,1]),3),round(tab[1,2]/sum(tab[,2]),3),"\n\n"))
    cat(c("Overall Error \n",round((tab[1,2]+tab[2,1])/(sum(tab[1,])+sum(tab[2,])),3),"\n\n"))
    cat(c("Implied Cost Ratio \n",round(tab[2,1]/tab[1,2],3)))
    data[[1]]=list();data[[2]]=list();data[[3]]=list();data[[4]]=list()
    data[[1]][[1]]="Model Error"
    data[[1]][[2]]=c(round(tab[1,2]/sum(tab[1,]),3),round(tab[2,1]/sum(tab[2,]),3))
    data[[2]][1]="Use Error"
    data[[2]][[2]]=c(round(tab[2,1]/sum(tab[,1]),3),round(tab[1,2]/sum(tab[,2]),3))
    data[[3]][1]="Overall Error"
    data[[3]][2]=round((tab[1,2]+tab[2,1])/(sum(tab[1,])+sum(tab[2,])),3)
    data[[4]][1]="Implied Cost Ratio"
    data[[4]][2]=round(tab[2,1]/tab[1,2],3)
    return(data)          
  }

conf_table_stored=function(actual,predicted){
   data=list()
    tab=table(actual, predicted)
    data[[1]]=list();data[[2]]=list();data[[3]]=list();data[[4]]=list()
    data[[1]][[1]]="Model Error"
    data[[1]][[2]]=c(round(tab[1,2]/sum(tab[1,]),3),round(tab[2,1]/sum(tab[2,]),3))
    data[[2]][1]="Use Error"
    data[[2]][[2]]=c(round(tab[2,1]/sum(tab[,1]),3),round(tab[1,2]/sum(tab[,2]),3))
    data[[3]][1]="Overall Error"
    data[[3]][2]=round((tab[1,2]+tab[2,1])/(sum(tab[1,])+sum(tab[2,])),3)
    data[[4]][1]="Implied Cost Ratio"
    data[[4]][2]=round(tab[2,1]/tab[1,2],3)
    return(data)          
  }



##Search Algorithm
#modelvec=c(f1,f2,f3,f4,f5)
#cp_vec=(seq(.001,.005,by=.0005))
#loss_vec=seq(4,10,by=.5)
#searches across different cross tabulations and cp and loss 
search=function(model,cp_vec,loss_vec,dataset){
  main=list()
  loc=1
    for(cp in 1:length(cp_vec)){
      for(loss in 1:length(loss_vec)){
       main[[loc]]=list()
       l=matrix(c(0,loss_vec[loss],1,0),nrow=2)
       tree=rpart(formula=model,control=rpart.control(minsplit=1,cp=cp_vec[cp]),parms=list(loss=l),data=dataset) ##not robust over here-train set
      class<-predict(tree,type="class")
       ##stores cp used, loss used, actual rpart tree, and predicted classes
       main[[loc]][1]=cp_vec[cp]
        main[[loc]][2]=loss_vec[loss]
        main[[loc]][[3]]=tree
        main[[loc]][[4]]=class
        loc=loc+1
       print(loc)
      }
    }
    return(main)
}


##Function to find number of terminal nodes in a given tree
get_num_term_nodes=function(tree){
  nodes=which(tree$frame[,1]=="<leaf>")
  return(length(nodes))
}

##Function for plotting bootstrapped mean consistency proportions against number of terminal nodes 
#(for now in  original model)
plot_termnode_prop=function(rpart.list,propmeans){
  num.term=sapply(rpart.list,get_num_term_nodes) #list of terminal node sizes
  plot(num.term,propmeans,pch=16,main="Stable Proportion vs. Number Terminal Nodes",xlab="Number of Terminal Nodes",ylab="Proportion")
}

smooth_termnode_prop=function(rpart.list,propmeans,tuning){
    num.term=sapply(rpart.list,get_num_term_nodes) #list of terminal node sizes
  scatter.smooth(num.term,propmeans,span=tuning,pch=16,main="Stable Proportion vs. Number Terminal Nodes",xlab="Number of Terminal Nodes",ylab="Proportion")
}


#######################################################################################################
##Functions for Tree Checking
##Functions#################################################################################
##Algorithm to check 
good_tree_check=function(model.list,actual,lower=4.5,upper=10.5){
  n=length(model.list)
  idx=numeric(1)
  for(i in 1:n){
    predicted=model.list[[i]][[4]]
    conf=conf_table_stored_print(actual,predicted)
    cost_ratio=conf[[4]][2]
    if(cost_ratio>=lower & cost_ratio<=upper){
      idx=c(idx,i)
    }
    if(i%%10==0) print(i)
  }
  return(idx[2:length(idx)])
}

##Bootstrap Functions
##Main call  
####Function to carry out bootstrap and give proportions

gen_prop_mat=function(ntrees,model.list,cp.vec,loss.vec,minsplit,dataset){ ##must all line up
  prop.matrix=matrix(nrow=choose(ntrees,2),ncol=length(cp.vec))
  for(i in 1:length(cp.vec)){
    model.choice=model.list[[i]]
    cp.choice=cp.vec[i]
    loss.choice=matrix(c(0,loss.vec[i],1,0),nrow=2)
    ##above sets up the model
    b.idx=get_boot_samples(data=dataset,num_trees=ntrees,dataset=dataset) ##list of bootstrapped data set
    boot.trees=get_boot_trees(boot_list=b.idx,model=model.choice,cp=cp.choice,loss_matrix=loss.choice,minsplit=minsplit) ##rpart object for each data set
    comps=tree_comp(tree_list=boot.trees)
    props=prop_calc(comps)
    prop.matrix[,i]=props
    #colnames(prop.matrix)[i]=paste("props",i,sep="")
    print(i)
  }
  return(prop.matrix)
}  

##Helper functions 
##Get samples stored as a list
get_boot_samples=function(data,num_trees,dataset){
    ##Get index of sampled rows
  boot_idx=function(n){
    idx=sample(1:n,size=n,replace=T)
    return(idx)
  }

  ##Get actual rows rows
  gen_boot=function(idx,data){
    b.data=data[idx,]
    return(b.data)
  }
  
  
  N=nrow(data)
  locs=lapply(rep(N,times=num_trees),boot_idx)
  samps=lapply(locs,gen_boot,data=dataset)
  return(samps)
}  

  
#Obvious
tree_build=function(model,data,cp,loss_matrix,minsplit){
  tree=rpart(formula=model,control=rpart.control(minsplit=minsplit,cp=cp),parms=list(loss=loss_matrix),data=data)
  return(tree)
}

  
##Get list of all trees for each boostrapped sample  
get_boot_trees=function(boot_list,model,cp,loss_matrix,minsplit){
  trees=lapply(boot_list,tree_build,model=model,cp=cp,loss_matrix=loss_matrix,minsplit=minsplit)  
  return(trees)
}

##Function that computes all of the cross-tabulations between trees 
tree_comp=function(tree_list){
  comp_list=list()
  loc=1
  predicted=lapply(tree_list,predict,type="class") ##returns list of predictions for each tree
  for(i in 1:length(predicted)){
    for(j in 1:i){
      if(j!=i){ ##check over all groupings
        comp_list[[loc]]=list()
        r1=names(predicted[[i]])
        r2=names(predicted[[j]])
        common=intersect(r1,r2) ##find common row names 
        r1.com=predicted[[i]][common] ##take common predictions from first 
        r2.com=predicted[[j]][common] ##take common predictions from second
        tab=table(r1.com,r2.com) ##Next 3 lines tabulate quantities of interest
        shared_class=sum(diag(tab))
        total_shared=sum(tab)
        comp_list[[loc]][[1]]=shared_class ##number shared and correct
        comp_list[[loc]][[2]]=total_shared  ##total common
        comp_list[[loc]][[3]]=c("tree_1"=i) #tree 1 name
        comp_list[[loc]][[4]]=c("tree_2"=j) #tree 2 name
        loc=loc+1
      }
    }
  }
  return(comp_list)
}

        
        
prop_calc=function(comp_list){
  n=length(comp_list)  
props=sapply(1:n,FUN=function(i) comp_list[[i]][[1]]/comp_list[[i]][[2]])
}
