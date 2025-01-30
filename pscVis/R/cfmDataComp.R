
cfmDataComp   <- function(cfm,data,plot=F){
  ### Setting - Data Summary
  dset <- cfm$data$m

  ## removing first and last column (outcome and weight)
  dset <- dset[,-c(1,ncol(dset))]

  ## Getting classes
  cls <- lapply(dset,class)

  ## Getting initial data vis
  cfmVis <- cfmDataVis(cfm,plot=F)

  ## Comparisons of DC to m
  gglist.compare <- list()

  for(i in 1:ncol(dset)){

    x <- data[,which(names(data)%in%names(dset)[i])];x

    if(cls[i]%in%c("factor","character")){
      if(!any(unique(dset[,i])%in%unique(x))) warning("factor levels may not match")
      gglist.compare[[i]] <- facVisComp(cfmVis[[i]],x)
    }


    if(cls[i]%in%c("numeric","integer")){
      gglist.compare[[i]] <- numVisComp(cfmVis[[i]],x)
    }
  }

  cls

  if(!plot) return(gglist.compare)

  if(plot){
    ## plotting results
    grid.arrange(grobs=gglist.compare,ncol=2)
  }


}
