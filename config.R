##	The version of the scripts used to produce the mapping products, and
##		which will match the "_v" portion of the filename outputs:
bsgm.version = "1.1a"

bsgm.ftp.username <-"wpftp"
bsgm.ftp.password <-"rt45te3gZQ"

bsgm.DologDEBUG <- TRUE

##  Calculate Zonal stats using R default if FALSE or Python if kPythonZonal is 
##  TRUE
bsgm.kPythonZonal <- FALSE

##  Calculate Zonal stats using R default if FALSE or Python if kPythonZonal is
##  TRUE
bsgm.saveZonalStats <- TRUE

##	Set the path to the system variable which points to the python.exe file:
bsgm.python_path <- "python"

#if zonal stats was calculated before then overwrite it if TRUE
bsgm.overwrite.compiled.covariates <- FALSE

## var name of the water mask
bsgm.water.mask <- "level1_water"

## var name of the water mask
bsgm.ccidadminl1 <- "ccidadminl1"

##  var name of the pixel area raster:
bsgm.pxarea <- "px_area"

##  Declare if we want the RF quant output:
bsgm.input.quant.output <- FALSE

##  Specify a path to gdal_merge.py
##  EXAMPLE: On some system you should do 
##           gdal_merge_path <- "python  /local/software/gdal/1.10.1/gcc/bin/gdal_merge.py "
##  NOTE:  These paths hsould only need to be changed for first time setup on a
##         machine.
bsgm.gdal_merge_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_merge.py\"",spr="")
bsgm.gdal_calc_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_calc.py\"",spr="")
bsgm.gdal_polygonize_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_polygonize.py\"",spr="")
# bsgm.gdal_merge_path <- paste("/local/software/gdal/1.10.1/gcc/bin/gdal_merge.py",spr="")
# bsgm.gdal_calc_path <- paste("/local/software/gdal/1.10.1/gcc/bin/gdal_calc.py",spr="")
# bsgm.gdal_polygonize_path <- paste("/local/software/gdal/1.10.1/gcc/bin/gdal_polygonize.py",spr="")

bsgm.pkgs <- c("rgdal",
               "plyr",
               "dplyr",
               "raster", 
               "randomForest", 
               "quantregForest", 
               "foreign",
               "MASS",
               "snow",
               "doParallel",
               "gdalUtils",
               "jsonlite",
               "logging",
			         "doSNOW",
			         "RCurl",
			         "data.table"
			         )
##  Declare a constant used in determining the number of blocks to break large
##  rasters up into. This is determined in an algorithm within the wpUtilities 
##  package:
#nmb <- 50
args <- commandArgs(TRUE)
nmb <- as.numeric(eval(parse(text = args[5])))
######
##	BEGIN:	RANDOM FOREST CONFIGURATION
##
##	Configuration options for RandomForest modeling and prediction.
##  If we are using a set of covariates from another country set the 
##		fixed_set variable to specify which will then be used to fix the 
##		covariate list to an existing randomForest object, otherwise, 
##		use the full set from the covariate metadata if it is NULL.
##
##    Note that the fixed_set flag also changes the way that the 
##		randomForest object is created below by eliminating the variable 
##		elimination routine:
#fixed_set <- "VNM"
#fixed_set <- c("VNM", "KHM")
#fixed_set <- NULL
if (!exists("bsgm.fixed_set")) {
  bsgm.fixed_set <- NULL
}

##	You can control whether you want covariates re-aggregated/summarized
##		by setting this flag to TRUE or FALSE:
bsgm.overwrite_compiled_covariates <- FALSE

##	You can control whether or not we should estimate or combine Random
##		Forest models (init_popfit, popfit, popfit_combined, popfit_final, and
##		the quantile output) by setting this flag to TRUE or FALSE.  This
##		is useful when we don't want to run the RF code at all for new
##		countries, but instead just want to use an existing popfit_final.RData
##		and popfit_quant.RData that we copied into the /output/XXX/tmp folder
##		for the current country:
bsgm.estimate_RF <- TRUE
##
##	END:	RandomForest configuration
#####

##	Fixed parameters and defaults:
bsgm.proj4str_gcs_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

bsgm.cores = parallel:::detectCores()
##  Set the number of cores to use in parallel operations:
#bsgm.cluster_workers <- bsgm.cores
bsgm.cluster_workers <- bsgm.cores[1]-1
#bsgm.cluster_workers <- bsgm.cores[1]-5

# if bsgm.minblocks  <- NULL then minblocks for cluster prediction parallesation 
# will be calculated based on aval memory 
# see function get_minblocks_rf_prd in internal_function.R file
#
bsgm.minblocks  <- NULL

######
##  NOTICE:  IN PRACTICE NOTHING BELOW THIS LINE SHOULD NEED TO BE REGULARLY 
##           EDITED UNLESS VERY SPECIFIC DETAILS NEED TO BE CHANGED ABOUT THE 
##           MODELING PROCESS.
######
bsgm.gdal_merge <- paste(bsgm.python_path, bsgm.gdal_merge_path,spr=" ")
bsgm.gdal_calc <- paste(bsgm.python_path, bsgm.gdal_calc_path,spr=" ")
bsgm.gdal_polygonize <- paste(bsgm.python_path, bsgm.gdal_polygonize_path,spr=" ")

logindicator = TRUE