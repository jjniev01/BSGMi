##  Authors:  Maksym Bondarenko & Jeremiah J. Nieves

.rasterInfoGDType <- function(filename){
  ##  This function utilizes GDAL based operations to retrieve raster 
  ##  information and return it in a data.frame.
  ####
  ##
  ##  Retrieve info from the input file using a call to gdalinfo:
  gdalinfo <- try(rgdal::GDALinfo(filename,  
                                  returnCategoryNames=TRUE,
                                  returnStats=TRUE ,
                                  silent=TRUE))
  
  ##  Extract info which came from gdalinfo and format it:
  nc <- as.integer(gdalinfo[["columns"]])
  nr <- as.integer(gdalinfo[["rows"]])
  
  xn <- gdalinfo[["ll.x"]]
  xn <- round(xn, digits=9)
  xx <- xn + gdalinfo[["res.x"]] * nc
  xx <- round(xx, digits=9)
  
  yn <- gdalinfo[["ll.y"]]
  yn <- round(yn, digits=9)
  yx <- yn + gdalinfo[["res.y"]] * nr
  yx <- round(yx, digits=9)  
  
  bi <- attr(gdalinfo, 'df')
  GDType <- as.character(bi[['GDType']])
  hasNoDataValues <- bi[['hasNoDataValue']]
  NoDataValue <- bi[['NoDataValue']]
  Bmin <- bi[['Bmin']]
  Bmax <- bi[['Bmax']]
  Bmean <- bi[['Bmean']]  
  
  driver <- attr(gdalinfo, 'driver')
  projection <- attr(gdalinfo, 'projection')
  resX <- gdalinfo[["res.x"]]
  resY <- gdalinfo[["res.y"]]
  lower_left_originX <- gdalinfo[[4]]
  lower_left_originY <- gdalinfo[[5]]
  
  ##  Place that extracted information into a dataframe:
  df <- data.frame( "nrow" = nr,
                    "ncol" = nc,
                    "xmin" = xn,
                    "xmax" = xx,
                    "ymin" = yn,
                    "ymax" = yx,
                    "Bmin" = Bmin,
                    "Bmax" = Bmax,
                    "Bmean" = Bmean,
                    "NoDataValue" = NoDataValue,
                    "hasNoDataValues" = hasNoDataValues,
                    "driver" = driver,
                    "GDType" = GDType,
                    "projection" = projection,
                    "resX" = resX,
                    "resY" =resY,
                    "lower_left_originX" = lower_left_originX,
                    "lower_left_originY" = lower_left_originY)
  
  ##  Return that dataframe:
  return(df)
}

###############################################################################



.rasterInfo<- function(filename){
  ##  This function retrieves raster information based upon GDAL information
  ##  and returns it in a data.frame. This function is similar to the function
  ##  '.rasterInfoGDType,' also defined in this module, but this is limited to 
  ##  information on extent, raster dimensions, and NA values.
  ####
  ##
  ##  Retrieve info from the input file using a call to gdalinfo:
  gdalinfo <- try ( rgdal::GDALinfo(filename,
                                    returnCategoryNames=TRUE,
                                    returnStats=TRUE,
                                    silent=TRUE) 
                    )
  
  ##  Extract info which came from gdalinfo and format it:
  nc <- as.integer(gdalinfo[["columns"]])
  nr <- as.integer(gdalinfo[["rows"]])
  
  xn <- gdalinfo[["ll.x"]]
  xn <- round(xn, digits=9)
  xx <- xn + gdalinfo[["res.x"]] * nc
  xx <- round(xx, digits=9)
  
  yn <- gdalinfo[["ll.y"]]
  yn <- round(yn, digits=9)
  yx <- yn + gdalinfo[["res.y"]] * nr
  yx <- round(yx, digits=9)  
  
  bi <- attr(gdalinfo, 'df')
  GDType <- as.character(bi[['GDType']])
  hasNoDataValues <- bi[['hasNoDataValue']]
  NoDataValue <- bi[['NoDataValue']]
  Bmin <- bi[['Bmin']]
  Bmax <- bi[['Bmax']]
  Bmean <- bi[['Bmean']]  
  
  ##  Place that extracted information into a dataframe:
  df <- data.frame(nr, nc, xn,
                   xx, yn, yx,
                   Bmin, Bmax, Bmean,
                   NoDataValue, hasNoDataValues)
  colnames(df) <- c("nrow","ncol","xmin",
                    "xmax","ymin","ymax",
                    "Bmin","Bmax","Bmean",
                    "NoDataValue","hasNoDataValues")
  
  ##  Return that dataframe:
  return(df)
}

###############################################################################



tmDiff <- function(start, end, frm="hms") {
  ##  Function which takes time objects and calculates the difference between 
  ##  the start and end time point. Returned is the formatted time.
  dsec <- as.numeric(difftime(end, start, units = c("secs")))
  hours <- floor(dsec / 3600)
  
  if (frm == "hms" ){
    minutes <- floor((dsec - 3600 * hours) / 60)
    seconds <- dsec - 3600*hours - 60*minutes
    
    out=paste0(
      sapply(c(hours, minutes, seconds), function(x) {
        formatC(x, width = 2, format = "d", flag = "0")
      }), collapse = ":")
    
    return(out)
  }else{
    return(hours)
  }
}

###############################################################################



check.df.has.na<- function(df){
  ##  Check if there are NAs in the input dataframe and return a TRUE or FALSE
  ##  value:
  M <- colSums(is.na(df))

  if (length(M[M == 1]) > 0)
    return(TRUE)
  else
    return(FALSE)
  }

###############################################################################



get.covariates.var.names <- function(){
  ##  This function retrieves variable names from the given covariates.
  ##  Create a vector to hold the covariate variable names:
  covariates.var.names <- c()
  
  ##  For every covariate in the covariates object:
  for ( icvritm in 1:length(covariates) ) { 
    ##  Retrieve that covariate object:
    covariate <- covariates[[icvritm]]
    ##  Retrieve the dataset_name attribute:
    var_name_class <- covariate[['dataset_name']]
    ##  Append that variable name to the covariates.var.names vector:
    covariates.var.names <- c(covariates.var.names, var_name_class)
  }    
  ##  Sort those names:
  sort(covariates.var.names)
  ##  Return the names vector:
  return(covariates.var.names)
}


###############################################################################



transY <- function(x, inverse=FALSE) {
  ##	The default is to log transform the x argument:
  if (!inverse) {
    return( log(x) )
  } else {
    ##  Otherwise we backtransform it by exponentiating it:
    return( exp(x) )
  }
}


###############################################################################



get_minblocks_rf_prd_need <- function() {
  ##  Determine how many blocks woud be optimal for the RF prediction phase.
  ##  If there is a minimum of blocks specified, return that minimum:
  if (!is.null(rfg.minblocks)) {
    return(rfg.minblocks)
  }else{
    ##  Return the number of blocks determined to be optimal as decided by the 
    ##  function wpGetBlocksNeed() which is housed in the wpUtilities package:
    return(wpGetBlocksNeed(covariate_stack, rfg.cluster_workers, n=1))
    
    # mem.avail <- (0.5 * (memory.size(NA)-memory.size(FALSE)))/rfg.cluster_workers
    # stack.cells <- ncell(census_mask) +2
    # stack.nlayers <- length(popfit_final$forest$xlevels)
    # 
    # maxmemory.need <- ( round( 10 * 8 * stack.cells * stack.nlayers) )/ (1024 * 1024)
    # rf.minblocks <- 1
    # 
    # while ((maxmemory.need > mem.avail))
    # {
    #   stack.cells <- stack.cells/rf.minblocks
    #   maxmemory.need <- ( round( 10 * 8 * stack.cells * stack.nlayers) )/ (1024 * 1024)
    #   rf.minblocks <- rf.minblocks + 1
    # }   
    # 
    # if (rf.minblocks < rfg.cluster_workers){
    #   rf.minblocks <- rfg.cluster_workers
    # }
    ##  And return the minimum number of blocks:
    return(rf.minblocks)
  }  
  
}

###############################################################################



##  Create a raster stack from all covariates from popfit and census_mask 
##  and water_raster:
creat_raster_stack <- function() {
  ##  Create an empty list to hold the rasters:
  list_ras <- list()

  ##  For every raster name in the list of names used in the RF:
  for (i in 1:length(names(covariates))){
    ##  Retrieve the name:
    var_name <- names(covariates)[i]
    ##  Assign that raster path to the varname:
    assign(var_name, raster( covariates[[var_name]]$path ), envir = .GlobalEnv)
    ##  Put the raster path in the list:
    list_ras[[i]] <-  get(var_name, envir = .GlobalEnv)
  }
  
  ##  Stack all the rasters we just retrieved:
  ras_stack <- stack(list_ras)

  ##  Return the raster stack object:
  return(ras_stack)
}

create_final_raster_stack <- function() {
  ##  Create an empty list to hold the rasters:
  list_ras <- list()
  
  ##  For every raster name in the list of names used in the RF:
  for (i in 1:length(names(covariates))){
    ##  Retrieve the name:
    var_name <- names(covariates)[i]
    ##  Assign that raster path to the varname:
    assign(var_name, raster( covariates[[var_name]]$path ), envir = .GlobalEnv)
    ##  Put the raster path in the list:
    list_ras[[i]] <-  get(var_name, envir = .GlobalEnv)
  }
  ##  Append the census mask and the water mask to that list:
  list_ras[[length(list_ras)+1]] <- get("region_mask")
  #list_ras[[length(list_ras)+1]] <- get("water_raster")
  
  ##  Stack all the rasters we just retrieved:
  ras_stack <- stack(list_ras)
  
  ##  Return the raster stack object:
  return(ras_stack)
}
###############################################################################



gcQuiet <- function(quiet = TRUE, ...) {
  ##  Use gc (i.e. garbage collection) to clear memory and report about the 
  ##  usage.
  if(quiet){ 
    invisible(gc())
  }else{
    print(paste('R is using', memory.size(), 'MB out of limit', memory.limit(), 'MB'))
    gc(...)
    print(paste('R is using', memory.size(), 'after using garbige collection'))
  }  
}

###############################################################################


checkRetrieveExtents <- function(){
  ##  This function is typically called under the extrapolation mode to ensure 
  ##  that a folder called "Extents" exists in the country's /Data/ folder,
  ##  that said folder has the correct number of rasters in it, that those 
  ##  rasters have the proper file name structure which is retrievable, and then
  ##  returns a named list of those raster paths.
  ##  If the directory exists:
  if(dir.exists(paste0(bsgm.data.path.countries,"Extents/"))){
    ##  Preallocate an empty list to hold paths:
    bsgm.bs.extents.path.list <- list()
    
    ##  For every declared year:
    for( yy in 1:length(bsgm.interp.year.seq)){
      ##  Retrieve what should be a valid file:
      foo_path <- Sys.glob(paste0(bsgm.data.path.countries,
                                  "Extents/BSGM_Extents*",
                                  bsgm.interp.year.seq[yy],".tif"))
      ##  As long as it is not an empty file path:
      if(foo_path != ""){
        ##  For every year, place the corresponding  year-specific extent tif:
        bsgm.bs.extents.path.list[[as.character(bsgm.interp.year.seq[yy])]] <- foo_path
      }
    }
    
    ##  If the number of those retrieved file names is equal to the sequence 
    ##  declared by the user:
    if(length(bsgm.bs.extents.path.list)==length(bsgm.interp.year.seq)){
      ##  Return the named list of file paths retrieved:
      return(bsgm.bs.extents.path.list)
    }else{
      stop(paste0("ERROR: The number of files found in the Extents Folder does not match the number declared by the user in input.R.",
                  " Please check the contents of the folder for the correct number of files, that the files match the expected file name pattern, and that the user specified number of files is correct."))
    }
  }else{stop(paste0("ERROR: Manually create the missing folder ",
                    bsgm.data.path.countries,
                    "Extents/ and populate it with the ",
                    length(bsgm.interp.year.seq), 
                    " corresponding extents rasters."))
  }
}