##  This function checks for common, but not all, errors which could occur in 
##  the input.R file where user supplied parameters are stored and passed to the
##  rest of the program.
check_config_input <- function(){
  
  logdebug('Start checking an input file')
  ##  Check if the number of declared poptables matches the number of declared 
  ##  countries:
  if (!is.null(bsgm.input.poptables)) {
    if ( as.integer(length(bsgm.input.poptables)) != bsgm.nb.countries){
      warning("Error: Number of local poptables is not equal total numbers of countries")
      return(FALSE)
    }
  }
  ##  Check if the number of admin ID vectors matches the number of declared 
  ##  countries:
  if (!is.null(bsgm.input.adminids)) {
    if ( as.integer(length(bsgm.input.adminids)) != bsgm.nb.countries){
      warning("Error: Number of admunitid is not equal total numbers of countries")
      return(FALSE)
    }
  }  
  
  ##  Check if the user is trying to use both the admin ID subset and the 
  ##  shapefile subset options at the same time:
  if (!is.null(bsgm.input.adminids) & !is.null(bsgm.input.shp)) {
      warning("Error: bsgm.input.adminids and bsgm.input.shp parameters in input.R file can not be used both at the same time. One of them should be NULL")
      return(FALSE)
  }    
  
  ##  Check that the number of supplied shapefiles for subsetting matches the 
  ##  number of declared countries:
  if (!is.null(bsgm.input.shp)) {
    if ( as.integer(length(bsgm.input.shp)) != bsgm.nb.countries){
      warning("Error: Number of shape files is not equal total numbers of countries")
      return(FALSE)
    }
    ##  For those supplied shapefiles, make sure they actually exist before 
    ##  starting:
    for ( icountry in bsgm.input.countries )  {
      if(!file.exists(as.character(bsgm.input.shp[icountry])) ){
        warning(paste0("Error: Shape files does not exist ",as.character(bsgm.input.shp[icountry]) ))
        return(FALSE)        
      }  
    }  
  }    
  

  if (bsgm.DologDEBUG) 
    show.output.on.console=TRUE
  
  # cheking if python installed
  logdebug('checking python')
  py_out <- system(paste(bsgm.python_path,"-V",spr=" "), show.output.on.console=show.output.on.console)
  if (py_out != 0){
    warning("ERROR:  There's an error somewhere in your configuration! The system path to Python does not exist.")
    return(FALSE)
  }



  logdebug('checking gdal_merge')

  result = tryCatch({
    gdal_merge.version <- system( paste(bsgm.gdal_merge,"--version",spr=" "), show.output.on.console=show.output.on.console)
  }, warning = function(w) {
    warning(w)
    warning("Could not get a version of gdal_merge. Check config.R file")
    return(FALSE)
  }, error = function(e) {
    warning(w)
    warning("Could not get a version of gdal_merge. Check config.R file")
    return(FALSE)
  }, finally = {
    #cat(gdal_merge.version)
  }
  )


# Chekc if gdal_calc is avalible
# 
  logdebug('checking gdal_calc')
  
  result = tryCatch({
    gdal_calc.version <- system( paste(bsgm.gdal_calc,"-h",spr=" "), show.output.on.console=FALSE)
  }, warning = function(w) {
    warning(w)
    warning("Could not get gdal_calc. Check config.R file")
    return(FALSE)
  }, error = function(e) {
    warning(e)
    warning("Error: Could not get gdal_calc. Check config.R file")
    return(FALSE)
  }, finally = {
    #cat(gdal_calc.version)
  }
  )


return(TRUE)
}