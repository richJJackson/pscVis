cfmDataVis <- function(cfm,plot=T){

  ### Setting - Data Summary
  dset <- cfm$data$m

  ## removing first and last column (outcome and weight)
  dset <- dset[,-c(1,ncol(dset))]

  ## Getting classes
  cls <- lapply(dset,class)

  ## Creating a list of grobs
  gglist <- list()

  for(i in 1:ncol(dset)){

    x <- dset[,i];x
    nm <- names(dset)[i]
    if(cls[i]%in%c("factor","character")){
      gglist[[i]] <- fac.vis(x,nm)
    }

    if(cls[i]%in%c("numeric","integer")){
      gglist[[i]] <- num.vis(x,nm)
    }

  }


  return(gglist)

  if(plot){
    ## plotting results
    grid.arrange(grobs=gglist,ncol=2)
  }



}
