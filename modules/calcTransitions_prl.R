#
#
#
calcTransitions_prl <- function(r1, r2, year0, year1) {
  
  stack.tmp.t0.t1 <- stack(r1, r2) 
  
  print(paste0("Working on  _transition_123_",year0,"_",year1,".tif "))
  
  fun.t0.t1=function(x){ 
    if (any(!is.na(x)) ) {
      if (x[1]==0 & x[2]==1){
        return(1)
      }else if (x[1]==1 & x[2]==1){
        return(0)
      }else{
        return(NA)
      }	    
    }else{
      return(NA)
    }
  }
  
  
  fun.t0.t2=function(x){ 
    if (any(!is.na(x)) ) {
      if (x[1]==0 & x[2]==1){
        return(1)
      }else{
        return(0)
      }	    
    }else{
      return(NA)
    }
  }
  
  fun.t0.t3=function(x){ 
    if (any(!is.na(x)) ) {
      if (x[1]==0 & x[2]==1){
        return(1)
      }else if (x[1]==1 & x[2]==1){
        return(2)
      }else if (x[1]==0 & x[2]==0){
        return(3)
      }else{
        return(NA)
      }	    
    }else{
      return(NA)
    }
  }
  
  
  if(!file.exists(paste0(bsgm.output.path.countries.tmp,
                         bsgm.countries.tag,"_transition_123_",year0,"_",year1,".tif")) ){
    
    transition_raster_123 <- wpRasterStackCalc(stack.tmp.t0.t1,
                                               fun=fun.t0.t3,
                                               filename=paste0(bsgm.output.path.countries.tmp,
                                                               bsgm.countries.tag,"_transition_123_",
                                                               year0,"_",year1,".tif"),
                                               NAflag=255,
                                               datatype='INT2U',
                                               overwrite=TRUE,
                                               cores=bsgm.cluster_workers,
                                               cblk=1,
                                               silent=F)
  }else{
    transition_raster_123 <- raster(paste0(bsgm.output.path.countries.tmp,
                                           bsgm.countries.tag,"_transition_123_",year0,"_",year1,".tif"))
  }				
  
  
  print(paste0("Working on  _transition_",year0,"_",year1,".tif "))
  
  if(!file.exists(paste0(bsgm.output.path.countries.tmp,
                         bsgm.countries.tag,"_transition_",year0,"_",year1,".tif")) ){
    
    transition_raster <- wpRasterStackCalc(stack.tmp.t0.t1,
                                           fun=fun.t0.t2,
                                           filename=paste0(bsgm.output.path.countries.tmp,
                                                           bsgm.countries.tag,"_transition_",year0,
                                                           "_",year1,".tif"),
                                           NAflag=255,
                                           datatype='INT2U',
                                           overwrite=TRUE,
                                           cores=bsgm.cluster_workers,
                                           cblk=1,
                                           silent=F)
    
  }else{
    transition_raster <- raster(paste0(bsgm.output.path.countries.tmp,
                                       bsgm.countries.tag,"_transition_",year0,"_",year1,".tif"))
  }
  
  
  return(transition_raster)
  
}  