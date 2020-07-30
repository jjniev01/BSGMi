##  TITLE:  BUILT-SETTLEMENT GROWTH MODEL (BSGM) 1i
##  AUTHOR:  JEREMIAH J. NIEVES & MAKSYM BONDARENKO
##  DATE:  2018-9-04
##  LICENSE:  MIT
##  NOTES:  THE BSGM 1A REQUIRES ONLY THIS SCRIPT BE RUN IN ORDER TO CARRY OUT 
##          THE ENTIRE MODELING PROCESS. THIS IS POSSIBLE DUE TO THE SOURCING OF
##          FUNCTIONS FROM SEVERAL "MODULES" INCLUDED WITHIN A MODULE FOLDER OF 
##          THE SAME DIRECTORY AS THIS SCRIPT. MANY OF THE SCRIPTS WITHIN SOME 
##          OF THE MODULES PERTAINING TO INTERACTION WITH THE WORLDPOP DATA BASE
##          (WPDB) WERE ORIGINALLY WRITTEN BY MAKSYM BONDARENKO AND WERE ADOPTED
##          AND OR ADAPTED FOR THE BSGM TO INTERACT WITH THE WPDB AND HANDLE 
##          COVARIATE FILES RETRIEVED FROM THAT DATABASE.
##          
##          THIS FUNCTION CARRIES OUT EITHER INTERPOLATION OR EXTRAPOLATION OF 
##          BUILT-SETTLEMENT (BS) EXTENTS ON AN ANNUAL BASIS BASED UPON CROSS-
##          SECTIONAL OBSERVED BS EXTENTS DATA AND TEMPORALLY AND SPATIALLY
##          CORRESPONDING COVARIATES IN CONJUNCTION WITH ANNUAL SUB-NATIONAL
##          SPATIAL POPULATION COUNTS PROVIDED BY THE CENTER FOR INTERNATIONAL
##          EARTH SCIENCE INFORMATION NETWORK (CIESIN).
##
##          THE USER IS REQUIRED TO DEFINE SOME PARAMETERS AT THE BEGINNING OF 
##          THIS SCRIPT AS WELL AS IN THE INPUT_PARAMETER.JSON FILE FOUND WITHIN 
##          THE SAME DIRECTORY AS THIS SCRIPT. OTHER THAN THAT, THE MAJORITY OF 
##          THE SCRIPT SHOULD NOT NEED TO BE CHANGED FOR REGULAR PRODUCTION 
##          RUNS. FOR MORE DETAILS ON HOW TO SET UP AND RUN THE SCRIPT, PLEASE 
##          SEE THE ACCOMPANYING DOCUMENTATION LOCATED WITHIN THE ".../DOC/" 
##          FOLDER.
##  
##          WARNING: IN VERSION 1A, THE USER MUST MANUALLY ENSURE THAT THE 
##                   COUNTRY SPECIFIC POPULATION RASTERS FOR t0, AND t1 IF 
##                   INTERPOLATING, ARE LOCATED IN EACH ./data/<ISO>/Pop/ 
##                   DIRECTORY
##
##  ------------------------------------------------------------------------  ##




######
##  BEGIN:  GENERAL SETTINGS AND CONFIGURATIONS
##
##  General Settings  -------------------------
##  Remove all objects from memory to have a clean run:
rm(list=ls(all=TRUE)) 

##  Define the root path; this should be set to the folder containing the 
##  primary directory tree of the model folder hierarchy:
root_path <- "E:\\Research\\BSGMi1a"


######
##  NOTICE:  IN PRACTICE NOTHING BELOW THIS LINE SHOULD NEED TO BE REGULARLY 
##           EDITED UNLESS VERY SPECIFIC DETAILS NEED TO BE CHANGED ABOUT THE 
##           MODELING PROCESS.
######


##  Declare a factor by which to break up the tasks to be completed in the 
##  parallel version of the interpolation and weighting functions of the BSGM:
##    NOTE:  Used in - interpolateURR_prl(), interpolateBSPDBAR_prl(), 
##           wpDFcalcBS_POP(), wpDFcalcBS_CNT(), and weightChanges_prl()
cblk <- 1

##  Load the model input script and get an idea about what countries we are 
##  working with:
source(paste0(root_path,"/input.R"))
#bsgm.input.countries <- as.list(bsgm.input.countries)
bsgm.nb.countries <- as.integer(length(bsgm.input.countries))

##  Determine how many periods we need to weight, i.e. dasymetric periods:
period_list <- vector(mode = "list", 
                      length = {length(observed_years)-1})
for(o in 2:length(observed_years)){
  ##  Populate with the observed period endpoints:
  period_list[[{o-1}]] <- c(observed_years[{o-1}], observed_years[o])
}

##  Assign the proper covariate name for retrieval in primary script similar 
##  to how we download the watermask and the level 1 rasters:
# bsgm.t1.extents <- bsgm.obs.ext.cvr.names[[as.character(t1)]]
# bsgm.input.cvr <- c(bsgm.input.cvr, bsgm.t1.extents)
#print(bsgm.input.cvr)

##  Set the random seed:
set.seed(2011)

##  Load all our other "modules" which do the heavy lifting for this script:
source(paste0(root_path,"/config.R"))
source(paste0(root_path,"/modules/load_Packages.R"))
source(paste0(root_path,"/modules/bsTableTools.R"))
##  TODO:  COMBINE NECESSARY FUNCTIONS FROM INTERNAL INTO INTERNAL_FUNCTIONS
source(paste0(root_path,"/modules/internal.R"))
source(paste0(root_path,"/modules/rfProbabilityBSGM.R"))
source(paste0(root_path,"/modules/samplingBSGM.R"))
source(paste0(root_path,"/modules/transitionTools.R"))
source(paste0(root_path,"/modules/internal_functions.R"))  
source(paste0(root_path,"/modules/check_config_input.R"))
source(paste0(root_path,"/modules/create_dirs_for_prj.R"))
source(paste0(root_path,"/modules/download_covariates.R"))
source(paste0(root_path,"/modules/prep_rst_cvr_adminid.R"))
source(paste0(root_path,"/modules/calculate_zonal_stats.R")) 
source(paste0(root_path,"/modules/load_pop_table_and_farea.R"))
source(paste0(root_path,"/modules/mrg_rst_cvr_countries.R"))  
source(paste0(root_path,"/modules/checkPopRasters.R"))
source(paste0(root_path,"/modules/LAN_functions.R"))
source(paste0(root_path,"/modules/cluster_predict.R"))
source(paste0(root_path,"/modules/rf_functions.R"))
source(paste0(root_path,"/modules/wpTmp.R"))
source(paste0(root_path,"/modules/interpolateBSPOP.R"))
source(paste0(root_path,"/modules/interpolateBSPDBAR.R"))

if (!load.Packages()){
  stop("There was an error when loading R packages")
  }

##  Check the configuration options:
##    Function is sourced from check_config_input.R
if (!check_config_input()){
  stop("There was an error in your input or configuration!")
  }

##  Determine our sequence of years we are modeling with (in numeric form):
year_seq <- seq(as.numeric(t0), as.numeric(t1), by = 1)

##  Time frame the model will represent (in string form):
bsgm.timeframe <- paste(c(t0,setdiff(observed_years,c(t0,t1)),t1), sep="-", collapse = '-')
##
##  END:  GENERAL SETTINGS AND CONFIGURATIONS
######




######
##  BEGIN:  FOLDER STRUCTURE SETUP
##
##  Create all the necessary directories for the model based upon the user 
##  input:
##    Function is sourced from create_dirs_for_prj.R
glPaths <- create_dirs_for_prj()  

##  Get the paths to the countries' data:
bsgm.data.path.countries <- glPaths$data
##  Declare where we are outputting things:
bsgm.output.path.countries <- glPaths$output
##  Declare where our temporary path is:
bsgm.output.path.countries.tmp <- paste0(bsgm.output.path.countries, "/tmp/")
##  Retrieve the country tag:
##  EXAMPLE:  "prj_2000-2012_ARM_AZE_GEO"
bsgm.countries.tag <- glPaths$countries_tag

##  Remove unnecessary items:
rm(glPaths)
##
##  END:  FOLDER STRUCTURE SETUP
######




######
##  BEGIN:  LOADING COVARIATES FROM WPFTP TO LOCAL FOLDER
##
##  This section retrieves covariates based upon the parameters of the 
##  input.R file.
##  Load module for dealing with variable names:
source(paste0(root_path,"/modules/variable_names.R")) 

##  Pre allocate a list to hold all possible covariate names we will be dealing 
##  with:
covariates <- list()
covariates.var.names <- list()

##  If the census_data_<countries_tag>.Rdata file already exist locally:
if (file.exists(paste0(bsgm.output.path.countries.tmp, 
                       bsgm.countries.fln.Rdata))) {
  ##  Load them:
  load(file=paste0(bsgm.output.path.countries.tmp, bsgm.countries.fln.Rdata))
  
  ##  Also begin downloading the required ppp population rasters from the FTP 
  ##  using the function DownloadFileFromWPFTP:
  #DownloadPopRasterInterp()
}else{
  ##  Begin the download and check of covariates from database to /data/ folder 
  ##  using a function called from internal_functions.R module:
  covariates <- Download_Covariates()
  
  ##  Downloading ppp population from FTP using function DownloadFileFromWPFTP:
  #DownloadPopRasterInterp()
  
  ##  Nonstandard insertion:
  for(v in bsgm.nonstand.cvr){
    covariates[[bsgm.input.countries[[1]]]][[v]] <- list("dataset_folder" = paste0(root_path, "/data/",bsgm.input.countries[[1]],"/"), 
                                                        "dataset_filename" = paste0(tolower(bsgm.input.countries[[1]]),
                                                                                    "_grid_100m_",v,".tif"),
                                                        "dataset_description" = v,
                                                        "dataset_summary" = "mean",
                                                        "dataset_country" = bsgm.input.countries[[1]],
                                                        "dataset_class" = v,
                                                        "path" = paste0(root_path, "/data/",bsgm.input.countries[[1]],"/",
                                                                        tolower(bsgm.input.countries[[1]]),"_grid_100m_",v,".tif"))
  }
  
  
  ## If bsgm.input.adminids is not NULL in input file then we 
  ## will crop all downloaded rasters to ID(s) mentioned in input parameters:
  if (!is.null(bsgm.input.adminids)){
    ##  If the input admin IDs are not null, subset the covariate rasters:
    prep_rst_cvr_adminid()
  }
  
  if (!is.null(bsgm.input.shp)){
    ##  If the input shapefile is not null, subset the covariate rasters by it:
    bsgm.input.adminids <- prep_rst_cvr_shp()
  }  
}

for(v in bsgm.nonstand.cvr){bsgm.input.cvr[[ {length(bsgm.input.cvr) + 1} ]] <- v}


##  Download the corresponding LAN data if we are using it in reweighting:
##    Sourced from the downloadLAN.R file.
##  JJN 2018-07-23:  Will assumme there is a ./LAN/derived/ folder that has the required 
##        norm lagged data. Error will get thrown later if it does not exist.
if(bsgm.LAN.weighting){
   DownloadLANRaster()
   }

##  Ensure the population rasters are where they should be using the 
##  checkPopRasters.R module and return a list of each country's(ies') time 
##  specific population raster(s):
bsgm.popras.list <- checkPopRasterInterp()


##
##  END:  LOADING COVARIATES FROM WPFTP TO LOCAL FOLDERS
######




######
##  BEGIN:  RETRIEVAL AND FORMATTING OF POPULATION TABLES
##
##  Create a matrix to hold data of ALL declared countries:
census_data <- matrix(nrow=0, ncol = 23) 

##  For every one of our input countries:
for ( icountry in 1:length(bsgm.input.countries) ){
  ##  Declare the temporary output path for the individual country:
  output.country.tmp <- paste0(root_path,"/","output/", 
                               bsgm.input.countries[icountry] , "/tmp/")
  
  ##  Retrieve the country specific population table:
  census_data.country <- as.data.frame(load.pop.table())
  ##  JJN Put this in twice because for some reason the function fails the 
  ##  first time and then tosses everything off. Haven't been bothered to 
  ##  figure it out  fully with a full step by step traceback().
  census_data.country <- as.data.frame(load.pop.table())
  
  ##  Append the country specific data to the all country poptable container:
  census_data <- rbind(census_data, census_data.country)
  #print(head(census_data))
  ##  Modify the column names to an expected format:
  colnames(census_data) <- c("ISO","GID","UNP2000","UNP2001","UNP2002","UNP2003",
                             "UNP2004","UNP2005","UNP2006","UNP2007","UNP2008",
                             "UNP2009","UNP2010","UNP2011","UNP2012","UNP2013",
                             "UNP2014","UNP2015","UNP2016","UNP2017","UNP2018",
                             "UNP2019","UNP2020")
}

loginfo("Saving census_data...")

##  Convert that data to a dataframe for continued use:
census_data <- as.data.frame(census_data)

##  Tidy up the tabular population data for use:
pop_df <- tidyCIESIN(census_data)

##  Save the compiled pop data as a new file in the temporary output 
##  folder:
save(pop_df, 
     file=paste(bsgm.output.path.countries.tmp, 
                bsgm.census.data.fln.Rdata, 
                sep="")) 


##
##  END:  RETRIEVAL AND FORMATTING OF POPULATION TABLES
######




######
##  BEGIN:  COVARIATE COMPILATION AND STUDY AREA CONFIRMATION
##
##  Create a list of covariates for main RF subrouting function called 
##  from mrg_rst_cvr_countries.R file:
covariates <- create_covariates_list_for_RF()

loginfo("Saving covariates for RF...")
##  Save the covariates as an RData file:
save(covariates, 
     file=paste(bsgm.output.path.countries.tmp, 
                bsgm.covariates.RF.Rdata, 
                sep="")) 

##  Make sure the correct covariates object is loaded:
load(paste0(bsgm.output.path.countries.tmp, 
            bsgm.covariates.RF.Rdata))
                
##  Retrieve the paths for the watermask, the census mask, the t0 extents, and
##  the t1 extents (if applicable):
watermaskPathFileName <- covariates[[bsgm.water.mask]]$path
censusmaskPathFileName <- covariates[[bsgm.ccidadminl1]]$path
t0extentsPathFileName <- paste0(root_path, "/data/",bsgm.input.countries[1],"/",
                                tolower(bsgm.input.countries[1]),"_grid_100m_",
                                bsgm.t0.extents,".tif")
pxareaPathFileName <- covariates[["px_area"]]$path

if(!is.null(bsgm.other.extents)){
  print(paste0("Intermediate Extents: ", bsgm.other.extents))
  ##  Extent file names between the t0 and t1 times:
  interextentsPathFileNames <- paste0(root_path,
                                      "/data/",
                                      bsgm.input.countries[1],
                                      "/",
                                      tolower(bsgm.input.countries[1]),
                                      "_grid_100m_",
                                      bsgm.other.extents,".tif")
}

print(paste0("BSGM.t1.extents: ", bsgm.t1.extents))
#print(str(covariates))
t1extentsPathFileName <- paste0(root_path, "/data/",bsgm.input.countries[1],"/",
                                tolower(bsgm.input.countries[1]),"_grid_100m_",
                                bsgm.t1.extents,".tif")
if(is.null(bsgm.other.extents)){
  bsgm.extent.list <- vector(mode = "list", length = 2)
  names(bsgm.extent.list) <- as.character(observed_years)
  bsgm.extent.list[[as.character(t0)]] <- t0extentsPathFileName
  bsgm.extent.list[[as.character(t1)]] <- t1extentsPathFileName
}else{
  bsgm.extent.list <- vector(mode = "list", length = length(observed_years))
  names(bsgm.extent.list) <- as.character(observed_years)
  for(o in 1:length(observed_years)){
    if(o==1){
      bsgm.extent.list[[as.character(observed_years[o])]] <- t0extentsPathFileName
    }
    if(o==length(observed_years)){
      bsgm.extent.list[[as.character(observed_years[o])]] <- t1extentsPathFileName
    }
    if(o != 1 & o != length(observed_years)){
      bsgm.extent.list[[as.character(observed_years[o])]] <- interextentsPathFileNames[endsWith(interextentsPathFileNames,
                                                                                  paste0("_",observed_years[o],".tif"))]
    }
  }
}


##  Remove AdminId, Watermask, t0 extents, t1 extents, and px_area info from 
##  prepared covariates list:
if(bsgm.water.mask %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == bsgm.water.mask)]
}
if(bsgm.ccidadminl1 %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == bsgm.ccidadminl1)]
}
if(bsgm.t0.extents %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == bsgm.t0.extents)]
}
if("px_area" %in% names(covariates)){
   covariates <- covariates[ - which(names(covariates) == "px_area")]
}
# if(bsgm.other.extents %in% names(covariates)){
#   covariates <- covariates[ - which(names(covariates) == bsgm.other.extents)]
# }
if(bsgm.t1.extents %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == bsgm.t1.extents)]
}


##  Prepare a single 'region_mask' which is an subtraction of the watermask from
##  the census mask so we only have to worry about applying one mask moving 
##  forward:
##  Bring in the region raster mask which is aligned with all our covariates:
region_mask <- raster(censusmaskPathFileName)


######
##  Check if we can carry out the entire processing and modelling in memory or 
##  if we need to break this down in parallel:
bsgm.prl <- ! canProcessInMemory(region_mask, 3)

##  If the region raster is not already all NA values or 1s, make it so:
if (bsgm.prl==FALSE){
  region_mask[!is.na(region_mask)] <- 1
}else{
  region_mask <- wpSetAllValuesTo(x=region_mask, v=1, cores=bsgm.cluster_workers, silent=F)
}

##	We need to ensure the water mask is included in covariate stack:
water_raster <- raster(watermaskPathFileName)

##  Go ahead and turn inland water areas within the region mask to NA so we 
##  can just have one mask we work with:
##    If we are running in memory:
if (bsgm.prl==FALSE){
  region_mask[which(getValues(water_raster)==1)] <- NA
}else{
  ##    If we are running it in parallel, source the functions to make it happen
  ##    more efficiently and change those values to NA:
  source(paste0(root_path,"/modules/check_region_mask_to_NA.R"))
  region_mask <- check_region_mask_to_NA(region_mask, water_raster)
}

##  Set up a template raster for prediction:
prediction_raster <- region_mask

##  Remove the water raster from memory:
rm("water_raster")

##
##  END:  COVARIATE COMPILATION AND STUDY AREA CONFIRMATION
######




######
##  BEGIN:  COVARIATE STACKING
##
##  Create the raster stack; sourced from internal_functions.R:
covariate_sample_stack <- creat_raster_stack()
##  Transfer the names of those covariates to the layer names of the stack:
names(covariate_sample_stack) <- names(covariates)


##  END:  COVARIATE STACKING
######




######
##  BEGIN: TRANSITION IDENTIFICATION AND SAMPLING
##
##  Determine what cells transitioned; see samplingBSGM.R:
##  Print out what paths are being used for t0 and t1 extents:
##  Declare the expected transition rasters that should be in existence or 
##  created; first declare an empty preallocated list to hold these:
transition_raster_names <- vector(mode = "list", 
                                  length = {length(observed_years) - 1})
for(o in 2:length(observed_years)){
  ##  If the transition raster does not exist already:
  if(!file.exists(paste0(bsgm.output.path.countries.tmp,
                         bsgm.countries.tag,"_transition_",
                         observed_years[o-1],"_", 
                         observed_years[o],".tif"))){
    ##  If we are running things locally:
    if (bsgm.prl==FALSE){
      ##  Calculate the transitions locally
      transition_raster <- calcTransitions(raster(bsgm.extent.list[[as.character(observed_years[o-1])]]),
                                           raster(bsgm.extent.list[[as.character(observed_years[o])]]),
                                           year0 = observed_years[o-1],
                                           year1 = observed_years[o])
      transition_raster_names[[{o-1}]] <- paste0(bsgm.output.path.countries.tmp,
                                                 bsgm.countries.tag,"_transition_",
                                                 observed_years[o-1],"_", 
                                                 observed_years[o],".tif")
    }
    ##  Else, if we are running things in parallel:
    else{
      ##  Source the needed parallel tools for calculating transitions:
      source(paste0(root_path,"/modules/calcTransitions_prl.R"))
      ##  Determine the transitions and write out the transition raster 
      ##  in parallel:
      transition_raster <- calcTransitions_prl(raster(bsgm.extent.list[[as.character(observed_years[o-1])]]),
                                               raster(bsgm.extent.list[[as.character(observed_years[o])]]),
                                               year0 = observed_years[o-1],
                                               year1 = observed_years[o])
      transition_raster_names[[{o-1}]] <- paste0(bsgm.output.path.countries.tmp,
                                                 bsgm.countries.tag,"_transition_",
                                                 observed_years[o-1],"_", 
                                                 observed_years[o],".tif")
    }
  }
  else{
    ##  If the transition raster exists already, put their paths into a list:
    transition_raster_names[[{o-1}]] <- paste0(bsgm.output.path.countries.tmp,
                                           bsgm.countries.tag,"_transition_",
                                           observed_years[o-1],"_", 
                                           observed_years[o],".tif")
  }
}

##  If we are working with more than two observed points, we need to create a
##  transition raster which spans the entire temporal extent for RF training 
##  purposes
if(length(observed_years > 2)){
  if(!file.exists(paste0(bsgm.output.path.countries.tmp,
                         bsgm.countries.tag,"_transition_",
                         observed_years[1],"_", 
                         observed_years[length(observed_years)],".tif"))){
    ##  If we are running things locally:
    if (bsgm.prl==FALSE){
      ##  Calculate the transitions locally
      transition_raster <- calcTransitions(raster(bsgm.extent.list[[as.character(observed_years[1])]]),
                                           raster(bsgm.extent.list[[as.character(observed_years[length(observed_years)])]]),
                                           year0 = observed_years[1],
                                           year1 = observed_years[length(observed_years)])
    }
    ##  Else, if we are running things in parallel:
    else{
      ##  Source the needed parallel tools for calculating transitions:
      source(paste0(root_path,"/modules/calcTransitions_prl.R"))
      ##  Determine the transitions and write out the transition raster 
      ##  in parallel:
      transition_raster <- calcTransitions_prl(raster(bsgm.extent.list[[as.character(observed_years[1])]]),
                                               raster(bsgm.extent.list[[as.character(observed_years[length(observed_years)])]]),
                                               year0 = observed_years[1],
                                               year1 = observed_years[length(observed_years)])
    }
  }
}
##  
##  END:  TRANSITION IDENTIFICATION AND SAMPLING
######




######
##  BEGIN: RF PREDICITON OF TRANSITION PROBABILITY
##
##  If the prediction of transition probabilities already exists:
if(file.exists(paste0(bsgm.output.path.countries,
                      "predict_transition_base_rf_pred_",
                      bsgm.countries.tag, ".tif")) ){
  ##  Load in the raster:
  prob_ras <- raster(paste0(bsgm.output.path.countries,
                            "predict_transition_base_rf_pred_",
                            bsgm.countries.tag, ".tif"))
}else{##  If the transition probability raster does not exist:
  ##  If we are processing things locally:
  if (bsgm.prl==FALSE){
    ##  Gather country wide samples across ALL prediction time periods and the
    ##  corresponding covariate values locally to train the raster:
    bsgm.table <- overunderSample(transition_raster,
                                  cov_stack = covariate_sample_stack,
                                  cov_names = names(covariate_sample_stack),
                                  year0 = observed_years[1],
                                  year1 = observed_years[length(observed_years)])
    ##  Get a rasterstack of covariates:
    covariate_stack <- create_final_raster_stack()
    ##  Modify the stack layer names:
    names(covariate_stack) <- c(names(covariates), "region_mask")
    
    ##  Get the fixed predictors, if there are any from another model declared 
    ##  in fixed set parameters; if there are no fixed predictors defined from 
    ##  other models, then the ones in the covariate stack are used:
    fixed_predictors <- get_fixed_predictors()
    
    ##  Retrieve the corresponding covariate values using those fixed 
    ##  predictor names and removing any NAs that may have gotten in with 
    ##  custom datasets:
    x_data <- bsgm.table[complete.cases(bsgm.table),fixed_predictors, with = FALSE]
    
    ##  Extract a vector of values of the sampled pixel level transition status:
    y_data <- bsgm.table[complete.cases(bsgm.table), ]$Transition
    ##  Transform them into factors
    y_data <- factor(y_data, levels = c(0,1), labels = c("0","1"))
    
    ##  Estimate the initial model fit, removing any insignificant covariates in
    ##  the iterative process:
    bsgmfit <- get_bsgmfit()
    ##  Get the final model with the selected set of covariates from the 
    ##  initial fit:
    bsgmfit_final <- get_bsgmfit_final()
    ##  Set the fixed set to the current set up:
    set_fixed_set_to_existing_countries()
    ##  Remove the initial fit and the proximity attribute as it creates an 
    ##  exceptinally large matrix which wil cause large runs to fail:
    rm(bsgmfit)
    bsgmfit_final$proximity <- NULL
    
    ##  Start the parallel prediction of the transition probability surface 
    ##  using the trained BSGM RF:
    beginCluster(n = bsgm.cluster_workers)
    prob_ras <- cluster_predict(prediction_raster, 
                                quant_output=FALSE, 
                                nmb=nmb)
    endCluster()
    ##  Clean up memory:
    gcQuiet(quiet = F)  
  }else{
    ##  If we are processing things in parallel, load the required functions:
    source(paste0(root_path,"/modules/calcPredictTransition_prl.R"))
    
    ##  Determine transitions and create the transition raster in parallel:
    ##    NOTE:  This one function takes care of the sampling procedure and the 
    ##           table construction which is more explicitly declared in this 
    ##           script above for the localized version; 
    ##           see module ' calcPredictTransition_prl.R'
    prob_ras <- calcPredictTransition_prl(covariate_sample_stack,region_mask)
    
    ##  Create the final covariate stack:
    covariate_stack <- create_final_raster_stack()
    ##  Rename the layers to the shortened covariate names:
    names(covariate_stack) <- c(names(covariates), "region_mask")    
    
    ##  Start the cluster
    beginCluster(n = bsgm.cluster_workers)
    
    ##  Predict for the transition probabilities:
    prob_ras <- cluster_predict(prediction_raster, 
                                quant_output=FALSE,
                                nmb=nmb)
    
    ##  Terminate the cluster:
    endCluster()
    ##  Take out the trash:
    gcQuiet(quiet = F)
  }
  
  ##  Remove the covariate stack as we no longer need it now:
  rm(covariate_sample_stack)
  gc()
}


##
##  END:  RF PREDICTION OF TRANSITION PROBABILITY
######




######
##  BEGIN:  BS POPULATION EXTRACTION AND TRANSITION MAGNITUDE ESTIMATION
##
##  NOTES:  At this point I expect the following structure in the population 
##          dataframe, pop_df. with the fields:
##
##          GID | YEAR | POP 
##      
##          This section will add to that resulting in fields:
##      
##          GID|YEAR|POP|BS.CNT|BS.POP|BS.POP.DENS|RUR|PBS|
##          
##          BS.CNT - number of cells in subnational unit which are BS
##          BS.POP - Population located in BS in the given subnational unit
##          BS.POP.DENS - Population density of BS areas in the given 
##                        subnational unit
##          RUR - Urban Rural Ratio or the ratio of the 'urban' population to 
##                the 'rural' population where urban and rural are defined as BS 
##                and non-BS, respectively.
##          PBS - The proportion of the subnational unit population which is 
##                located in built-settlement
##  ---------------------------------------------------------------------  ##
##
admin_ras <- raster(censusmaskPathFileName)

##  Carryout BS related extractions to the pop_df; sourced from 
##  bstableTools.R:
pop_df <- getBSPopulation(population_path_list = bsgm.popras.list,
                          built_settlement_path_list = bsgm.extent.list,
                          admin_zonal_raster = admin_ras,
                          pop_df_obj = pop_df,
                          process_years = observed_years,
                          prl = bsgm.prl,
                          silent=T)

##  Classify every record as to what period it is in:
pop_df <- as.data.table(pop_df)
pop_df[,PERIOD := 0]

##  Determine what period every record is in and label it:
for(p in 1:length(period_list)){
  if(p==length(period_list)){
    pop_df[YEAR >= period_list[[p]][1] & YEAR <= period_list[[p]][2], PERIOD := p]
  }
  if(p != length(period_list)){
    pop_df[YEAR >= period_list[[p]][1] & YEAR < period_list[[p]][2], PERIOD :=p]
  }
}


##  Create a temporary copy of the population dataframe:
pop_df.tmp <- copy(pop_df)

##  Get a list of the unique GIDS which do not have only zeros or zero population:
gid_list <- unique(pop_df[pop_df$YEAR %in% observed_years & pop_df$BS.POP != 0 & pop_df$BS.POP != 0,]$GID)

##  Determine the number of rows we need in our predicted data.frame:
rows <- length(gid_list)*{length(year_seq)-length(observed_years)}
predict_length <-{length(year_seq)-length(observed_years)}
##  Set up a data.frame to hold the predictions:
pred_df <- data.frame("GID" = numeric(length = rows),
                      "YEAR" = numeric(length = rows),
                      "t" = numeric(length = rows),
                      "BS.POP"=numeric(length = rows),
                      "PGR" = numeric(length = rows),
                      "C.VALUE" = numeric(length = rows),
                      stringsAsFactors = F)

##  Determine the currently unknown BS.POP values::
print("Interpolating BSPOP values...")
#pop_df <- interpolateURR(pop_df, t0, t1)
beginCluster(bsgm.cluster_workers)
bspop_predict <- cluster_BSPOP(pred_df, observed_years)
endCluster()
rm(pred_df)
gcQuiet()


##  Merge them with the primary population table
pop_df$PGR <- 0
pop_df$C.VALUE <- 0


##  Change names of the dataframe prior to rbinding:
bspop_predict <- as.data.table(bspop_predict)[,t := NULL]
pop_df <- as.data.table(pop_df)
setkey(bspop_predict, GID, YEAR)
setkey(pop_df, GID, YEAR)

##  For every GID and year of the prediction dataframe:
##  Transfer the values
##  NOTE:  data.table solution based upon below post: 
##         https://stackoverflow.com/questions/42537520/data-table-replace-data-using-values-from-another-data-table-conditionally
pop_df[bspop_predict, names(bspop_predict)[3:5] := bspop_predict[,.SD,.SDcols = 3:5]]

rm(bspop_predict)
gcQuiet()


##  Get a list of the unique GIDS which do not have only zeros or zero population:
gid_list <- unique(pop_df[pop_df$YEAR %in% observed_years & pop_df$BS.PDBAR != 0 & pop_df$BS.PDBAR != 0,]$GID)

##  Determine the number of rows we need in our predicted data.frame:
rows <- length(gid_list)*{length(year_seq)-length(observed_years)}
predict_length <-{length(year_seq)-length(observed_years)}
##  Set up a data.frame to hold the predictions:
pred_df <- data.frame("GID" = numeric(length = rows),
                      "YEAR" = numeric(length = rows),
                      "BS.PDBAR"=numeric(length = rows),
                      stringsAsFactors = F)

##  Determine the currently unknown BS.POP values::
print("Interpolating BS.PDBAR values...")
#pop_df <- interpolateURR(pop_df, t0, t1)
beginCluster(bsgm.cluster_workers)
bspdbar_predict <- cluster_BSPDBAR(pred_df, observed_years)
endCluster()
rm(pred_df)
gcQuiet()


##  Change names of the dataframe prior to rbinding:
pop_df <- as.data.table(pop_df)
bspdbar_predict <- as.data.table(bspdbar_predict)
setkey(bspdbar_predict, GID, YEAR)
setkey(pop_df, GID, YEAR)

##  For every GID and year of the prediction dataframe:
##  Transfer the values
##  NOTE:  data.table solution based upon below post: 
##         https://stackoverflow.com/questions/42537520/data-table-replace-data-using-values-from-another-data-table-conditionally
pop_df[bspdbar_predict, names(bspdbar_predict)[2:3] := bspdbar_predict[, .SD, .SDcols = 2:3]]

rm(bspdbar_predict)
gcQuiet()

pop_df <- data.table(pop_df)
print("Calculating estimated built settlement population...")
print("Calculating estimated number of built settlement by year and admin unit...")
pop_df[!(YEAR %in% observed_years), BS.CNT := BS.POP/BS.PDBAR]

##  Calculate the weighted demand for BS cells, perform the 'anchor check', 
##  and handle negative values.
##    See  bsTableTools.R for details.
##  If we are running things locally:
##  Weight the changes:
pop_df <- weightChanges(pop_df)


##
##  END:  BS POPULATION EXTRACTION AND TRANSITION MAGNITUDE ESTIMATION
######




######
##  BEGIN:  TRANSITION DISAGGREGATION AND PREDICTED EXTENT MAPPING
##
##  ----  Pre Task List Creation:  One-time Processes  ----
##

for(p in 1:length(period_list)){
  ##  Determine the sequence of years we are transitioning for:
  year_seq <- seq(period_list[[p]][1],period_list[[p]][2])
  ##  Determine what transition raster we need to be using:
  transition_raster_path <- Sys.glob(paste0(bsgm.output.path.countries.tmp,
                                            "*_transition_",period_list[[p]][1],
                                            "_",period_list[[p]][2],".tif"))
  transition_raster <- raster(transition_raster_path)
  ##  Retrieve the cell indices of cells which have transitioned during the 
  ##  time period of interest, i.e. cell value == '1'
  print("Retrieving Transition indices...")
  ##  If we are running things locally:
  if (!bsgm.prl) { 
    ##  Retrieve those cell indices:
    trans_ind <- which(values(transition_raster)==1)
  }else{##  If we are running in parallel:
    ##  Retrieve those indices in parallel:
    trans_ind <- wpGetindexesWhichValues(x=transition_raster, 
                                       v=1, 
                                       cores=bsgm.cluster_workers)  
  }
  
  ##  Set up a processing loop which calls upon the parallelized function to 
  ##  determine which cells transition
  ##  For every year in the sequence which needs to be predicted:
  for(t in 2:(length(year_seq)-1)){
    start_time <- proc.time()[3]
    ##  Retrieve the proper year:
    y <- year_seq[t]
    ##  If we are using the Weighted LAN values to adjust the probabilities:
    if(bsgm.LAN.weighting){
      ##  Retrieve the corresponding pattern we'll be using for retrieving the 
      ##  year specific LAN derived data:
      if(y <= "2012"){
        lan_pattern <- "_dmsp_"}
      else{lan_pattern <- "_viirs_"}
    }
    ##  Determine output name for this year:
    out_name <- paste0("BSGM_",bsgm.version,"_Extents_",bsgm.countries.tag,"_",y,".tif")
    
    ##  Retrieve the year prior's built settlement extent raster path:
    if((y-1)==t0){
      ##  If prior year is base year, get the initial extent path:
      bsip <- t0extentsPathFileName
    }
    if((y-1) %in% observed_years[2:{length(observed_years)-1}]){
      ##  If the prior year is an observed year, get the initial extent path: 
      bsip <- bsgm.extent.list[[as.character(y-1)]]
    }
    if((y-1) != t0 & !( (y-1) %in% observed_years[2:{length(observed_years)-1}])){
      ##  Retrieve the output from the year prior's predictions:
      bsip <- paste0(bsgm.output.path.countries,"BSGM_",bsgm.version,"_Extents_",
                     bsgm.countries.tag,"_",(y-1),".tif")
    }
    
    ##  ----  Pre Task List Creation:  Annual Processes  ----
    ##  Retrieve the unique admin ids, store those IDs as the name in a list,
    ##  and store the corresponding cell indices as a numeric vector under that
    ##  the corresponding name in the list as long as the index is listed as 
    ##  having transitioned and was not already built:
    ##
    ##  Retrieve the indices of the cells which are built settlement, i.e. have a
    ##  value of '1'
    start_time <- proc.time()[3]
    print("Retrieving initial BS inidices...")
    init_built_ras <- raster(bsip)
    ##  If we are running locally:
    if (!bsgm.prl) { 
      ##  Determine the indices of the BS extents at the initial time points:
      init_built_ind <- which(values(init_built_ras)==1)
    }else{##  If we are running in parallel:
      ##  Determine the indices of the BS extents at the intiial time point in
      ##  parallel:
      init_built_ind <- wpGetindexesWhichValues(x=init_built_ras, 
                                                v=1, 
                                                cores=bsgm.cluster_workers)  
    } 
    
    ##  Remove the intial built settlement raster from memory:
    rm(init_built_ras)
    gc()
    
    ##  From those transition indices, remove cells which have already been 
    ##  transitioned in previous steps:
    print("     Differencing those indices...")
    trans_ind_diff <- setdiff(trans_ind, init_built_ind)
    admin_ind <- trans_ind_diff
    
    ##  Filter our growth dataframe to the records for a specific year and only 
    ##  the records for non-zero growth admin units
    print("Filtering data specific to year...")
    grow_info <- copy(pop_df[YEAR == y & EST.CHANGE != 0,])
    print("     Pulling GIDs...")
    ##  If processing locally
    if (!bsgm.prl) { 
      ##  Retrieve the GIDs of those identified cells:
      admin_gid <- getValues(admin_ras)[admin_ind]
    }else{##  If we are running in parallel:
      ##  Retrieve the GIDs of those identified cells in parallel:
      admin_gid <- wpGetValuesbyInds(x=admin_ras,
                                     v=admin_ind,
                                     cores=bsgm.cluster_workers)  
    }      
    print("     Creating dataframe and filtering...")
    admin_df <- data.frame("IND" = admin_ind, "GID" = admin_gid)
    
    ##  Make sure the only records we have are those which have growth observed
    ##  for the given year:
    admin_sub_df <- admin_df[admin_df$GID %in% unique(grow_info$GID),]
    
  ###  JJN  2018-08-09: Set in this if statement because it was crashing when 
    ##  there were no transitions to be made in a year.
    if(nrow(admin_sub_df) != 0){
      if(bsgm.LAN.weighting){
        ##  Get the path of the year specific LAN based data:
        if(y == 2012){
          ##  We reuse the last dmsp year when there is an issue of DMSP versus 
          ##  viirs so we don't have to try and homogenize them.
          lan_path <- Sys.glob(paste0(bsgm.data.path.countries, 
                                      "/LAN/derived/*",lan_pattern,
                                      "2010_normlag_2011-2010.tif"))[1]
        }
        if(y > 2016){
          ##  We reuse the last observed if past our observed LAN data:
          lan_path <- Sys.glob(paste0(bsgm.data.path.countries, 
                                      "/LAN/derived/*",lan_pattern,
                                      "2015_normlag_2016-2015.tif"))[1]
        }
        if(y != 2012 & y <= 2016){
          lan_path <- Sys.glob(paste0(bsgm.data.path.countries, 
                                      "/LAN/derived/*",lan_pattern,{y-1},
                                      "*.tif"))[1]
        }
        print(paste0("     Weighting probabilities with ", basename(lan_path)))
        ##  Get the LAN weights from the corresponding processed LAN raster band:
        ##  If we are running locally:
        if (!bsgm.prl) { 
          ##  Weight the probabilities by the year's LAN data:
          admin_sub_df$WEIGHT <- getValues(raster(lan_path))[admin_sub_df$IND]
        }else{##  If we are running in parallel:
          ##  Weight the probabilities by the year's LAN data in parallel:
          admin_sub_df$WEIGHT <- wpGetValuesbyInds(x=raster(lan_path), 
                                                   v=admin_sub_df$IND, 
                                                   cores=bsgm.cluster_workers)  
        }       
        
        
        ##  Get the base prob values so we don't have to get them again:
        ##  If we are running locally:
        if (!bsgm.prl) { 
          ##  Retrieve the probability values:
          admin_sub_df$BASEPROB <- getValues(prob_ras)[admin_sub_df$IND]
        }else{##  If we are running in parallel:
          ##  Retrieve the probability values in parallel:
          admin_sub_df$BASEPROB <- wpGetValuesbyInds(x=prob_ras,
                                                     v=admin_sub_df$IND,
                                                     cores=bsgm.cluster_workers)  
        }        
        ##  Calculate the weighted probability of transition:
        admin_sub_df$PROB <- admin_sub_df$WEIGHT * admin_sub_df$BASEPROB
      }
      ##  If we are not using LAN weighting of transition probabilities:
      if(!bsgm.LAN.weighting){
        ##  Get the prob values so we don't have to get them again:
        if (!bsgm.prl) { 
          admin_sub_df$PROB <- getValues(prob_ras)[admin_sub_df$IND]
        }else{
          admin_sub_df$PROB <- wpGetValuesbyInds(x=prob_ras,
                                                 v=admin_sub_df$IND,
                                                 cores=bsgm.cluster_workers)  
        }       
      }
      
      ##  There is potential for the probability surface to have NA values so 
      ##  we'll remove the 'incomplete' rows of the dataframe:
      admin_sub_df <- admin_sub_df[complete.cases(admin_sub_df),]
      
      ##  For every admin unit which has a non-zero amount of growth for the year:
      gid_list <- unique(admin_sub_df$GID)
      print(paste0("Creating task list of ", 
                   as.character(length(gid_list)),
                   ":"))
      
      gc()
      
      ##  ----  Task List Creation  ----
      ##  Create an empty preallocated_list of the correct size:
      task_list <- vector(mode="list", length = length(gid_list))
      
      ##  Start up the cluster and carry out the transition index extraction:
      ##  NOTE:  This is a modified clusterPredict is from the transitionTools.R
      beginCluster(n = bsgm.cluster_workers)
      master_task <- clusterTasker(task_list)
      endCluster()
      rm(task_list)
      
      ##  Create the cell vector to transition cells:
      ##  NOTE:  bsInterpolate is from transitionTools.R
      transitions_ty_list <- sapply(master_task, bsInterpolate, simplify = TRUE)
      ##  Unlist and compile into a single vector of cell indices:
      transitions_ty <- unlist(transitions_ty_list, use.names = FALSE)
      transitions <- unique(transitions_ty[!is.na(transitions_ty)])
      
      ##  Carry out the transition mapping.
      ##  If we are running locally:
      if (bsgm.prl==FALSE){
        ##  Carry out the transition mapping:
        bsTransitionMap(bsip, 
                        transitions, 
                        out_name)
      }else{##  If we are running in parallel:
        ##  Carry out the transition mapping in parallel:
        wpSetValueWhichindexes(x=raster(bsip),
                               y=transitions,
                               v=1,
                               filename = paste0(bsgm.output.path.countries, out_name),
                               cores=bsgm.cluster_workers)
      }
      
      rm(master_task, transitions_ty)
      gc()
    }
    if(nrow(admin_sub_df) == 0){
      writeRaster(raster(bsip),
                  file = paste0(bsgm.output.path.countries, out_name),
                  format = "GTiff",
                  overwrite = T,
                  options = c("COMPRESS = LZW"))
    }
  }
}
  ##
  ##  END:  TRANSITION DISBURSEMENT AND PREDICTED EXTENT MAPPING
  ######