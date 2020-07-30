checkPopRasterInterp <- function(){
  ##  Create a holder for the return names:
  r_fn <- vector(mode = "list", length = length(bsgm.input.countries))
  names(r_fn)<-bsgm.input.countries
  ##  If the mode is interpolation we will be checking for two appropriately 
  ##  named population rasters in every country specific data directory:
  
  ##  For every input country:
  for(i in bsgm.input.countries){
    ##  Construct the names of the population rasters which should be there:
    tif_names <- c(paste0(root_path,"/data/",i,
                          "/Pop/ppp_prj_",observed_years,"_",i, ".tif"))
    
    
    ##   Retrieve a list of all .tifs within the proper folder:
    foo_tifs <- Sys.glob(paste0(root_path,"/data/",i,"/Pop/*.tif"))
    
    ##  Check that those filenames are in the returned glob:
    fn_test <- tif_names %in% foo_tifs
    ##  If both are there:
    if(sum(fn_test) == length(observed_years)){
      r_fn[[i]] <- vector(mode = "list", length = length(observed_years))
      names(r_fn[[i]]) <- as.character(observed_years)
      for(t in as.character(observed_years)){
        ##  Store their names for retrieval:
        r_fn[[i]][[t]] <- tif_names[endsWith(tif_names,paste0(t,"_",i,".tif"))]
      }
    }
    else{
      stop(paste0("ERROR: ",length(observed_years)," population tifs are required. Double check population folder for country ", i))
    }
  }
  ##  Return the country list of file name lists.
  return(r_fn)
}
