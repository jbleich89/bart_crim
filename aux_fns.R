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

  
  