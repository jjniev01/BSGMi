#
#
#
check_region_mask_to_NA <- function(r1, r2) {
  
  if(!file.exists(paste0(bsgm.output.path.countries.tmp,
                         "stack_tmp_region_mask_water.tif"))) { 
    
  print(paste0("Working on  check_region_mask_to_NA "))
  
  stack.tmp <- stack(r1, r2)
  
  fun = function(x) {
    if (any(!is.na(x))) {
      if (x[2] == 1 & !is.na(x[1])) {
        return(NA)
      } else{
        return(1)
      }
    } else{
      return(NA)
    }
  }

  
  region_mask <- wpRasterStackCalc(stack.tmp,
                                   fun = fun,
                                   filename = paste0(bsgm.output.path.countries.tmp,
                                                     "stack_tmp_region_mask_water.tif"),
                                   NAflag = 255,
                                   datatype = 'INT2U',
                                   overwrite = TRUE,
                                   cores = bsgm.cluster_workers,
                                   cblk = 5,
                                   silent = F)
  
  }else{
    
    region_mask <-raster(paste0(bsgm.output.path.countries.tmp,"stack_tmp_region_mask_water.tif"))
    
  }
  
  return(region_mask)
  
}  