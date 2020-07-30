##  AUTHORS:  JEREMIAH J. NIEVES
##  DATE OF LAST MODIFICATION: MARCH 20 2018
##  TITLE:  INTERPOLATION ANALYSES
##  DESC:
##    THESE CODE BLOCKS CARRYOUT PRE- AND POST-PROCESSING OF COMPARISON DATASETS
##    AND ANALYSES UTILIZED FOR THE INITIAL APPRAISAL OF THE BUILT-SETTLEMENT
##    GROWTH MODEL (BSGM). THIS SCRIPT IS DEPENDENT UPON FUNCTIONS PLACED IN THE
##    SCRIPT 'VALIDATIONFUNCTIONS.R' AS WELL AS AN EXPECTED FOLDER HIERARCHY 
##    WHERE SPECIFIC DATASETS ARE LOCATED. SEE THE CODE AND COMMENTS BELOW 
##    REGARDING SUCH STRUCTURE.
##  
##  ----------------------------------------------------------------------------

##  Declare the root directory we are working with:
root <- "D:/"


##  Source the required functions from ValidationFunctions.R; ensure that all 
##  packages required in the functions are installed prior to running this line:
source(paste0(root, "Research/BSGMi/accessories/ValidationFunctions.R"))

#####  MAUPP PREPROCESSING  ----------------------------------------------------
##  Below block was used for resampling the MAUPP data and rectifying its 
##  alignment and extents to match the modeled data spatial parameters for 
##  further comparison
##
##  Retrieve the original MAUPP files using a regular expression to pick the 
##  correct ones:
maupp_files <- paste0(root,"Research\\BSGMGeneral\\ValidationData\\MAUPP\\original\\",
                      list.files(paste0(root,
                                        "Research\\BSGMGeneral\\ValidationData\\MAUPP\\original\\"),
                                 recursive = TRUE, 
                                 pattern = "classes_[0-9]{4}[.]tif$"))
##  Ensure that the output directory exists and if it doesn't already, 
##  create it:
outdir <- ensure_dir(paste0(root, 
                            "Research/BSGMGeneral/ValidationData/MAUPP/Derived/"))
##  For every one of those retrieved MAUPP files:
for(m in maupp_files){
  ##  Retrieve the ISO country code, the city name, and the descriptor of the 
  ##  file:
  iso <- strsplit(strsplit(m,"\\\\")[[1]][length(strsplit(m,"\\\\")[[1]])],"/")[[1]][1]
  city <- strsplit(strsplit(m,"\\\\")[[1]][length(strsplit(m,"\\\\")[[1]])],"/")[[1]][2]
  desc <- strsplit(strsplit(strsplit(m,"\\\\")[[1]][length(strsplit(m,"\\\\")[[1]])],"/")[[1]][3],"\\.")[[1]][1]
  
  ##  Construct the output file name using that retrieved information:
  outpath <- paste0("MAUPP_",iso,"_",city,"_",desc,".shp")
  
  ##  Give some processing output to console
  print(paste0("ISO: ", iso))
  print(paste0("CITY: ", city))
  
  ##  Carry out the actual preprocessing:
  preprocess.MAUPP(m, outdir, outpath, overwrite = TRUE)
}




#####  ESA SUBSETTING PREPROCESSING  -------------------------------------------
##  Below block was used for cutting out the country specific ESA annual 
##  classified extents, subset to make a binary representation of class 190 
##  "urban areas," to make the comparison work more efficient, i.e. not always 
##  working with global layers for 8 countries across 13 years.
##
##  Declare the name tag we want to use later on in file path creation:
nt <- "ESA_Binary_190"
##  For every year between 2000 and 2012:
for(y in seq(2000,2012)){
  ##  Print what year we are processing:
  print(paste0("Year: ",y))
  ##  Retrieve the year specific ESA global raster for the year we are 
  ##  processing:
  esa <- raster(Sys.glob(paste0(root,
                                "Research\\BSGMGeneral\\ValidationData\\ESA CCI ANNUAL CLS 190\\ESACCI_LC*",
                                y,"*.tif"))[1])
  ##  For every country we are wanting to extract:
  for(i in c("CRI", "VNM","NGA","NPL","PAK","NGA","MDG","PER","KHM")){
    ##  Print the country we are currently processing:
    print(i)
    ##  Retrieve our Lvl1 raster of the country we are processing in order to 
    ##  use it as a mask for cutting out its extents from the global ESA 
    ##  dataset:
    lvl1_path <- Sys.glob(paste0(root, 
                                 "Research\\BSGM\\data\\",i,
                                 "\\*adminl1.tif"))[1]
    ##  Carry out the extraction, writing the extraction out to file:
    extractCovByCountry(lvl1_path, 
                        esa,
                        paste0(root,
                               "Research\\BSGMGeneral\\ValidationData\\ESA CCI ANNUAL CLS 190\\"),
                        i,
                        nt,
                        y)
  }
}


####  RF VALIDATION  -----------------------------------------------------------
##
##  Below block validates every RF object for the 2000-2012 models
##
##  Declare which countries we are carrying out the validation of the RF on:
iso_list <- c("CRI","NPL","PAK","PER","NGA","MDG","VNM","KHM")
##  Declare the number of samples which should be taken; see the 'rfValidate' 
##  function to understand just how this parameter operates, particularly within 
##  the context of the biased sampling to create psuedo validation sets with a 
##  fixed prevalence:
sampvol <- 1000
##  Declare the output directory:
outdir_user <- paste0(root,"Research/BSGMGeneral/ValidationOutput/")
##  For every iso given:
for(i in iso_list){
  ##  Do a validation for every prevalence level from 0.005 to 0.03 by intervals 
  ##  of 0.005:
  for(p in seq(0.005, 0.03, by = 0.005)){
    ##  Print some processing output to console:
    print(paste0("Validating ",i,
                 " with a sample containing transition prevalence of: ",
                 p))
    
    ##  Carry out the psuedo validations with fixed prevalences; note this fails 
    ##  for 0.005 in NGA I believe because the actual prevalence in the whole 
    ##  country is larger than 0.005.
    rfValidate(root,
               iso = i,
               sampsize = sampvol,
               outdir = outdir_user, 
               prevalence = p)
  }
  ##  Do a truely random sample of the validation set which should have a 
  ##  validation prevalence of transitions equal to 0.2t/(n-1.8t) where n is 
  ##  all cells and t is the total observed transition cells, i.e. the 20 
  ##  percent of transitions which were not used in training:
  print("Validating using a representative prevalence of validation set...")
  rfValidate(root,iso = i, 
             sampsize = 100000,
             outdir = outdir_user, 
             prevalence = NULL)
  
  ##  Carry out a validation for a sample which is representative for all admin 
  ##  units and therefore allows construction of valid local contingency tables:
  ##    Note: See the function 'getRandSampByGID' for details on the sampling 
  ##          on different sized subnational units.
  admin_samp <- getRandSampByGID(paste0(root,
                                        "Research/BSGM/data/",
                                        i,"/",tolower(i),
                                        "_grid_100m_ccidadminl1.tif"),
                                 simplify = TRUE)
  ##  Print processing output to console:
  print(paste0("Using ", length(admin_samp), " units."))
  print("Validating using a sample allowing for admin unit level contingencies to be constructed...")
  ##  Carry out the RF validation using the subnational unit representative 
  ##  sample:
  rfValidate(root,
             iso = i, 
             sample_order = admin_samp,
             outdir = outdir_user)
}


####  DTE EXTRACTION  ---------------------------------------------------------
##
##  Below Block extracts and calculates metrics for the 2000-2012 model derived 
##  and comparison dataset DTE analyses:
iso_list <- c("EGY","NGA","VNM")
#c("CRI","NPL","PAK","PER","NGA","MDG","VNM","KHM")

##  Declare the output directory:
outdir_user <- paste0(root,"Research/BSGMiGeneral/ValidationOutput/")

## Size for random samples:
sample_size <- 100000

years <-c(2010) #2001:2011
##  NOTE: compare_name 'MAUPP' is only valid for years 2005 and 2010 whereas 
##        'ESACCI' can use values from 2000 to 2012

##  Declare the comparison dataset we want to compare DTE with:
##    NOTE:  the MAUPP option can only be used for iso_list <- c("MDG","NGA"). 
##           ESACCI can be used with all study areas countries. Also, 
##           
compare_name <- "GUF+" #"MAUPP" "ESA" "GUF+"
##  For every declared country:
for( i in iso_list){
  ##  Print some processing output to the console:
  print(paste0("Processing ", i, "..."))
  print(Sys.time())
  ##  Ensure the output directory exists and if not, create it:
  ensure_dir(paste0(outdir_user,i,"/"))
  
  if(compare_name == "GUF+"){
    ##  Get the country Lvl1 raster path and load it:
    regpath <- paste0(root,
                      "Research\\BSGMi\\data\\",i,"_GUF+\\",
                      tolower(i),"_grid_100m_ccidadminl1.tif")
  }
  if(compare_name =="MAUPP"){}  
  if(compare_name == "ESA"){}
  
  region_ras <- raster(regpath)
  
  ##  If we are looking at the GUF+ data,
  if(compare_name == "GUF+"){
    
  }
  
  
  ##  If the points we wish to sample already exist as a shapefile, load it:
  if(file.exists(paste0(outdir_user, i,"/",i,"_DTE_admin_sample_points.shp"))){
    print("Admin indices already exist. Loading...")
    admin_samp <- readOGR(paste0(outdir_user,i,"/",i,"_DTE_admin_sample_points.shp"),
                          paste0(i,"_DTE_admin_sample_points"))
  }
  ##  But if it doesn't exist:
  if(!file.exists(paste0(outdir_user,i,"/",i,"_DTE_admin_sample_points.shp"))){
    print("Getting Admin Unit Representative Samples...")
    
    ##  Get a subnational unit representative sample, storing the Cell Index:
    admin_samp <- getRandSampByGID(paste0(root,"Research/BSGM/data/",
                                          i,"/",tolower(i),
                                          "_grid_100m_ccidadminl1.tif"),
                                   simplify = TRUE)
    ##  Change those Cell Indicies to spatial points:
    coords <- xyFromCell(raster(paste0(root,"Research/BSGM/data/",
                                       i,"/",tolower(i),
                                       "_grid_100m_ccidadminl1.tif")),
                         admin_samp,
                         spatial = TRUE)
    ##  Turn into Spatial Points Data Frame with the Cell Index (CID) stored for
    ##  reference:
    ##  NOTE: The CID is relative to the Lvl1 subnational unit raster:
    adminshp <- SpatialPointsDataFrame(coords,
                                       data.frame("CID"=admin_samp),
                                       proj4string = CRS(as.character("+proj=longlat +datum=wgs84 +no_defs +ellps=wgs84 +towgs84=0,0,0")))
    ##  Save the SPDF as a shapefile for replicability:
    writeOGR(adminshp, 
             paste0(outdir_user,i,"/",i,"_DTE_admin_sample_points.shp"),
             driver = "ESRI Shapefile",
             layer = paste0(i,"_DTE_admin_sample_points"),
             overwrite_layer = TRUE)
    
    ##  Read it back in:
    admin_samp <- readOGR(paste0(outdir_user,i,"/",i,"_DTE_admin_sample_points.shp"),
                          paste0(i,"_DTE_admin_sample_points"))
    ##  Memory cleaning
    rm(adminshp,coords)
    gc()
  }
  
  ##  If a shapefile of random samples from across the ocuntry exists, 
  ##  read it in:
  if(file.exists(paste0(outdir_user,i,"/",i,"_DTE_random_sample_points.shp"))){
    print("Admin indices already exist. Loading...")
    rand_samp <- readOGR(paste0(outdir_user,i,"/",i,"_DTE_random_sample_points.shp"))
  }
  ##  Otherwise, if it doesn't exist:
  if(!file.exists(paste0(outdir_user,i,"/",i,"_DTE_random_sample_points.shp"))){
    print("Getting random samples from across the study area...")
    
    ##  Get all potential sample cell indices which are not NA or water (0):
    poten_ind <- which(!is.na(values(region_ras)) & values(region_ras) != 0)
    
    ##  Sample them randomly without replacement:
    samp_ind <- sample(poten_ind, sample_size, replace = FALSE)
    
    ##  Create a points object from those cell indices:
    coords <- xyFromCell(region_ras,
                         samp_ind,
                         spatial = TRUE)
    ##  Turn those points into a SPDF storing the CID for reference:
    samp_points <- SpatialPointsDataFrame(coords, 
                                          data.frame("CID"=samp_ind),
                                          proj4string = CRS(as.character("+proj=longlat +datum=wgs84 +no_defs +ellps=wgs84 +towgs84=0,0,0")))
    ##  Save it for replicability:
    writeOGR(samp_points, 
             paste0(outdir_user,i,"/",i,"_DTE_random_sample_points.shp"),
             driver = "ESRI Shapefile",
             layer = paste0(i,"_DTE_random_sample_points"),
             overwrite_layer = TRUE)
    
    ##  Memory cleaning:
    rm(samp_ind,coords,samp_points)
    gc()
    
    ##  Read it back in for use:
    rand_samp <- readOGR(paste0(outdir_user,i,"/",i,
                                "_DTE_random_sample_points.shp"),
                         paste0(i,"_DTE_random_sample_points"))
  }
  
  ##  For every defined year:
  for(y in years){
    print(paste0("    Working on year ", y,"..."))
    
    ##  Retrieve the year-specific BSGM-derived DTE raster path:
    ##  NOTE: This requires preprocessing the BSGM annual output using the DTE 
    ##        python script (DTE_v_batch_regex.py) which is dependent on ArcGIS
    ##        but could equally be done in QGIS or any other GIS with distance 
    ##        to and raster math capabilities. ArcGIS was used because the 
    ##        script had been written for a previous project.
    ##  Declare the path to the "predicted" DTE raster:
    if(compare_name == "GUF+"){
      predpath <- paste0(root,"Research\\BSGMi\\output\\prj_2000-2015_",i,
                         "_GUF+\\derived\\BSGM_Extentsprj_2000-2015_",i,"_",y,"_DTE.tif")
    }    
    ##  If we are using the ESA for comparison:
    if(compare_name == "ESACCI"){
      ##  Retrieve the country and year specific ESA DTE raster path:
      obspath <- paste0(root,
                        "Research\\BSGMGeneral\\DTEOutput\\",i,
                        "_ESA_Binary_190_",y,"_DTE_WGS84.tif")
      if(i == "MDG"){
        obspath <- paste0(root,"Research\\BSGMGeneral\\DTEOutput\\",i,
                          "_",y,"ESA_DTE_WGS84.tif")
      }
      ##  Otherwise process things as for a country wide sample:
      print("     Processing the global sample...")
      print(Sys.time())
      ##  Pull the random sample DTE values from both datsets for comparison:
      count_samp<- dteCompareExtract(predpath,
                                     obspath,
                                     regpath,
                                     samp_points = rand_samp)
      ##  Save the values to an RDS data object:
      saveRDS(count_samp, paste0(outdir_user,i,"/",i,"_DTE_extract_rand_comp_",
                                 y,"_",compare_name,".rds"))
      print("     Processing the admin representative sample...")
      print(Sys.time())
      
      ##  Pull the subnational unit representative sample DTE values from both 
      ##  datasets for comparison:
      count_samp<- dteCompareExtract(predpath,
                                     obspath,
                                     regpath,
                                     samp_points = admin_samp)
      
      ##  Save the values to an RDS data object:
      saveRDS(count_samp, paste0(outdir_user,i,"/",i,"_DTE_extract_admin_comp_",
                                 y,"_",compare_name,".rds"))
    }
    ##  If we are comparing to MAUPP:
    if(compare_name=="MAUPP"){
      ##  Retrieve the paths of all city-based year-specific rasters for which 
      ##  we are to compare:
      obspath <- Sys.glob(paste0(root,
                                 "Research\\BSGMGeneral\\DTEOutput\\MAUPP_",
                                 i,"*",y,"_DTE_WGS84.tif"))
      ##  We are working with multiple files per country in the MAUPP case as it
      ##  only has city coverage, therefore we need to process each one 
      ##  individually and then remove the NAs as the points used for sampling 
      ##  will cover an area larger than the cities.
      cities <- obspath
      ##  For every city based dataset:
      for(city in cities){
        print(paste0("     Processing the global sample for city file ",
                     city,"..."))
        obspath <- city
        
        ##  Pull the city name:
        cityname <- sub(".*MAUPP_[A-Z]{3}(.*)[0-9]{4}.*","\\1",city)
        
        ##  Create the city sample points which are exhaustive since the cities 
        ##  are so small.
        
        print("Getting Extraction Points...")
        ##  Pull in the cty raster:
        cityras <- raster(city)
        
        ##  Change every cell in the raster it into points:
        coords <- xyFromCell(cityras,
                             1:ncell(cityras),
                             spatial = TRUE)
        ##  Turn into SPDF containing the CID of the original raster:
        extshp <- SpatialPointsDataFrame(coords,
                                         data.frame("CID"=1:ncell(cityras)),
                                         proj4string = CRS(as.character("+proj=longlat +datum=wgs84 +no_defs +ellps=wgs84 +towgs84=0,0,0")))
        
        rm(coords,cityras)
        gc()
        
        print(paste0("Working with ",nrow(extshp@data), " points for extraction."))
        print(Sys.time())
        
        ##  Pull the DTE values from the corresponding DTE raster:
        count_samp<- dteCompareExtract(predpath,
                                       obspath,
                                       regpath,
                                       samp_points = extshp)
        ##  Remove the NAs:
        count_samp <- count_samp[complete.cases(count_samp),]
        ##  Write the extracted data to an RDS object:
        saveRDS(count_samp, paste0(outdir_user,i,"/",i,"_DTE_extract_rand_comp_",cityname,"_",
                                   y,"_",compare_name,".rds"))
        print(Sys.time())
      }
    }
  }
}





####  RF VALIDATION CONTINGENCY TABLES  ---------------------------------------
##
##  This block calculates the True Positive (TP), False Positive (FP),
##  True Negative (TN), False Negative (FN), associated metrics, null models, 
##  and outputs tables using the htmlTable package. This is not for the admin 
##  representative statistics as those will need to be processed by group. This 
##  handles the global samples.

root <- "E:/"
prj_root <- "Research/BSGMGeneral/ValidationOutput/"
isolist <- c("PAK","NGA","MDG","KHM","VNM","PER", "CRI","NPL")
##  Calculate the random null model?
calcnull = TRUE
##  Calculate and display the positive predictor model?
calcpos = TRUE
##  Write the output to a file?
writefile <- TRUE
##  Resave table?
resave <- TRUE
## For every country:
for( i in isolist){
  print(paste0("Working on ",i,"..."))
  ##  Retrieve the basic tables:
  flist <- as.vector(Sys.glob(paste0(root,prj_root,i,
                                     "/validation_datatable_",i,"_*.rds")))
  ##  Refine which files we use so we arent improperly using the admin unit 
  ##  representative tables:
  m <- regexpr(".*/.*_([0-9]{4}|[0-9]{4}_prevalence.*)\\.rds", 
               flist, 
               perl = TRUE)
  flist <- regmatches(flist,m)
  ##  Declare the output directory:
  outdir <- ensure_dir(paste0(root,prj_root,i,"/ContingencyTables/"))
  
  ##  Set the table themes to minimal:
  tt <- ttheme_minimal()
  ##  For every type of validation/comparison file:
  for(f in flist){
    print(paste0("     Processing ",f," ..."))
    ##  Determine any output file tags we want to use:
    if(length(sub(".*(validation_datatable).*","\\1",f,perl=TRUE))>0){
      reg <- ".*/validation_datatable_(.*)\\.rds"
      tag <- sub(reg,"\\1",f,perl=TRUE)
    }
    ##  Load the data and ensure it is a data.table:
    indat <- as.data.table(readRDS(f))
    
    ##  Determine the TP,FP,FN,TN based upon default 50 percent threshold:
    indat <- indat[PRED==1 & OBS==1, TP:=1]
    indat <- indat[PRED==1 & OBS==0, FP:=1]
    indat <- indat[PRED==0 & OBS==1, FN:=1]
    indat <- indat[PRED==0 & OBS==0, TN:=1]
    
    ##  If we are calculating for the null model:
    if(calcnull){
      ##  Calculate the NULL Model if Desired:
      indat <- calcContingency(indat,null_model=TRUE)
    }
    ##  If we are calculating for the always positive model:
    if(calcpos){
      ##  Calculate the always Positive Model if Desired:
      indat <- calcContingency(indat,pos_model = TRUE)
    }
    ##  If we are saving the output file:
    if(resave){
      saveRDS(indat, file = f)
    }
    
    ##  Create a matrix to hold the derisered TP, FP, FN, TN, and marginal values
    footbl <- matrix(ncol = 3, nrow = 3)
    
    ##  Add in the desired data to construct the basic contingency table:
    footbl[1,1] <- sum(indat$TP)
    footbl[1,2] <- sum(indat$FP)
    footbl[1,3] <- sum(indat$PRED)
    footbl[2,1] <- sum(indat$FN)
    footbl[2,2] <- sum(indat$TN)
    footbl[2,3] <- {sum(indat$FN)+sum(indat$TN)}
    footbl[3,1] <- sum(indat$OBS)
    footbl[3,2] <- {sum(indat$FP)+sum(indat$TN)}
    footbl[3,3] <- nrow(indat)
    
    ##  Create the table grob object:
    foo_gtable <- tableGrob(footbl, 
                            rows= c("Transition","NonTransition","Total"),
                            cols = c("Transition","NonTransition","Total"),
                            theme = tt)
    ##  Add aesthetic borders:
    foo_gtable <- gtable_add_grob(foo_gtable,
                                  grobs = rectGrob(gp=gpar(fill = NA, lwd=2)),
                                  t=2, l=2, b = {nrow(foo_gtable)-1},r=3)
    
    if(calcnull){
      ##  Calculate the null contingency table:
      nulltbl <- matrix(ncol = 3, nrow = 3)
      nulltbl[1,1] <- sum(indat$TPN)
      nulltbl[1,2] <- sum(indat$FPN)
      nulltbl[1,3] <- sum(indat$NULL.PRED)
      nulltbl[2,1] <- sum(indat$FNN)
      nulltbl[2,2] <- sum(indat$TNN)
      nulltbl[2,3] <- {sum(indat$FNN)+sum(indat$TNN)}
      nulltbl[3,1] <- sum(indat$OBS)
      nulltbl[3,2] <- {sum(indat$FPN)+sum(indat$TNN)}
      nulltbl[3,3] <- nrow(indat)
      null_gtable <- tableGrob(nulltbl, 
                               rows= c("Transition","NonTransition","Total"),
                               cols = c("Transition","NonTransition","Total"),
                               theme = tt)
      ##  Add aesthetic borders:
      null_gtable <- gtable_add_grob(null_gtable,
                                     grobs = rectGrob(gp=gpar(fill = NA, lwd=2)),
                                     t=2, l=2, b = {nrow(null_gtable)-1},r=3)
    }
    if(calcpos){
      ##  Calculate the null contingency table:
      postbl <- matrix(ncol = 3, nrow = 3)
      postbl[1,1] <- sum(indat$TPP)
      postbl[1,2] <- sum(indat$FPP)
      postbl[1,3] <- sum(indat$POS.PRED)
      postbl[2,1] <- sum(indat$FNP)
      postbl[2,2] <- sum(indat$TNP)
      postbl[2,3] <- {sum(indat$FNP)+sum(indat$TNP)}
      postbl[3,1] <- sum(indat$OBS)
      postbl[3,2] <- {sum(indat$FPP)+sum(indat$TNP)}
      postbl[3,3] <- nrow(indat)
      pos_gtable <- tableGrob(postbl, 
                              rows= c("Transition","NonTransition","Total"),
                              cols = c("Transition","NonTransition","Total"),
                              theme = tt)
      ##  Add aesthetic borders:
      pos_gtable <- gtable_add_grob(pos_gtable,
                                    grobs = rectGrob(gp=gpar(fill = NA, lwd=2)),
                                    t=2, l=2, b = {nrow(pos_gtable)-1},r=3)
    }
    
    ##  Calculate desired metrics for the contingency table:
    prevalence <- round(sum(indat$OBS)/nrow(indat),digits=4)
    recall <- round(sum(indat$TP)/sum(indat$OBS),digits = 3)
    precision <- round(sum(indat$TP)/sum(indat$PRED),digits=3)
    fscore <- round(2/{1/recall + 1/precision},digits=3)
    specificity <- round(sum(indat$TN)/{sum(indat$FP)+sum(indat$TN)},digits=3)
    
    ##  If we are doing the null model:
    if(calcnull){
      ##  Calculate metrics
      nullprevalence <- round(sum(indat$OBS)/nrow(indat),digits=4)
      nullrecall <- round(sum(indat$TPN)/sum(indat$OBS),digits = 3)
      nullprecision <- round(sum(indat$TPN)/sum(indat$NULL.PRED),digits = 3)
      nullfscore <- round(2/{1/nullrecall + 1/nullprecision},digits = 3)
      nullspecificity <- round(sum(indat$TNN)/{sum(indat$FPN)+sum(indat$TNN)},digits = 3)
    }
    
    ##  If we are doing the always positive model:
    if(calcpos){
      ##  Calculate metrics:
      posprevalence <- round(sum(indat$OBS)/nrow(indat),digits=4)
      posrecall <- round(sum(indat$TPP)/sum(indat$OBS),digits = 3)
      posprecision <- round(sum(indat$TPP)/sum(indat$POS.PRED),digits = 3)
      posfscore <- round(2/{1/posrecall + 1/posprecision},digits = 3)
      posspecificity <- round(sum(indat$TNP)/{sum(indat$FPP)+sum(indat$TNP)},digits = 3)
    }
    
    ##  Create the corresponding table for the summary metrics:
    metrictbl <- tableGrob(matrix(c(c(prevalence,recall,precision,specificity,fscore),
                                    if(calcnull){c(nullprevalence,nullrecall,nullprecision,nullspecificity,nullfscore)},
                                    if(calcpos){c(posprevalence,posrecall,posprecision,posspecificity,posfscore)}),
                                  ncol = {1+calcpos+calcnull}, 
                                  nrow = 5,
                                  byrow = FALSE),
                           rows= c("Prevalence","Recall","Precision","Specificity","Fscore"),
                           cols= c("BSGM",
                                   if(calcnull){"Null Model"},
                                   if(calcpos){"PositivePredictor"}),
                           theme = tt)
    
    ##  Create the arranged figure(s) depending on what the input parameters 
    ##  were:
    outfig <- grid.arrange(arrangeGrob(foo_gtable,metrictbl,
                                       ncol=1,
                                       top = textGrob(f),
                                       bottom = textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.")))
    
    if(calcpos){outfig <- grid.arrange(arrangeGrob(foo_gtable, 
                                                   pos_gtable, 
                                                   metrictbl, 
                                                   textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.\nLeft-top table is the BSGM model, top-right is the Always Positive model."),
                                                   ncol=2),
                                       top = textGrob(f)) 
    }
    if(calcnull){outfig <- grid.arrange(arrangeGrob(foo_gtable,
                                                    null_gtable,
                                                    metrictbl, 
                                                    textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.\nLeft-top table is the BSGM model, top-right is the Null model."),
                                                    ncol=2),
                                        top = textGrob(f))
    }
    if(calcpos & calcnull){outfig <- grid.arrange(arrangeGrob(foo_gtable, 
                                                              null_gtable, 
                                                              pos_gtable,
                                                              metrictbl,
                                                              textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.\nLeft-top table is the BSGM model, top-right is the Null model, bottom-left is the Always Positive model."),
                                                              ncol=2),
                                                  top = textGrob(f))
    }
    ##  Send the output figure to console:
    print(outfig)
    ##  If we are writing the file to output:
    if(writefile){
      ggsave(paste0(outdir,"ContingencyTable_",tag,".pdf"),
             outfig)
    }
  }
}




####
##  TRANSITION PREVALENCE CALCULATION  -----------------------------------------
##
##  This block calculates the prevalence of transitions by subnational unit 
##  based upon the BSGM-inputs and the total transitionable areaat the beginning
##  of the modelling period:

prj_root <- "Research/BSGM/data/"
isolist <- c("KHM","VNM","PER", "CRI","NPL", "MDG","NGA","PAK")
##  For every country:
for( i in isolist){
  ##  Bring in the zonal raster (i.e. Lvl1 GID raster of the country):
  zonras <- raster(paste0(root, prj_root, i,"/",tolower(i),
                          "_grid_100m_ccidadminl1.tif"))
  
  ##  Create a polygonized version of the zonal raster using the function 
  ##  created by Maksym that is GDAL based and write it to file:
  gdal_polygonizeR(zonras, 
                   outshape = paste0(root, prj_root, i,"/",tolower(i),
                                     "_ccidadminl1.shp")) 
  
  ##  Read in the polygon we created:
  fooshp <- readOGR(paste0(root, prj_root, i,"/",tolower(i),"_ccidadminl1.shp"),
                    layer = paste0(tolower(i),"_ccidadminl1"))
  ##  Rename its ID column:
  names(fooshp) <- c("GID")
  
  ##  Bring in the transition raster created during the BSGM run:
  transras <- raster(paste0(root,"Research/BSGM/output/prj_2000-2012_",i,
                            "/tmp/prj_2000-2012_",i,"_transition.tif"))
  
  
  
  ##  GID pixel count  -----
  ##  Create a datatable of the GID values:
  dat <- data.table("GID" = zonras[])
  ##  Remove all NA records:
  dat <- dat[!is.na(GID),]
  ##  Get a count of each GID:
  dat[,COUNT := .N, by = list(GID)]
  ##  Set the value key:
  setkeyv(dat,c("GID"))
  ##  Remove duplicate records so were left with one record per GID:
  dat <- subset(unique(dat))
  
  
  ##  Transition pixel count  -----
  transstat <- wpZonalStatistics(transras, zonras, "sum")
  names(transstat) <- c("GID", "N_TRANS")
  
  ##  Combine data with the shapefile:
  fooshp <- merge(fooshp, dat, by="GID", all.x=TRUE)
  fooshp <- merge(fooshp, transstat, by="GID", all.x=TRUE)
  
  ##  Resave the shapefile:
  writeOGR(fooshp,
           dsn=paste0(root,"Research/BSGM/data/",i,"/",
                      tolower(i),"_ccidadminl1.shp"),
           layer = paste0(tolower(i),"_ccidadminl1"),
           driver = "ESRI Shapefile", 
           overwrite_layer = TRUE)
  
  ##  NOTE:  AFTER THIS THE GID == 0, i.e. WATER BODIES, WERE REMOVED AND ALL 
  ##         GID POLYGONS WERE DISSOLVED AND THEN SAVED AS A SHAPEFILE MATCHING
  ##         THE ABOVE NAMING PATTERN BUT WITH A '_dissolve' PRECEDING THE .SHP.
  ##         THESE STEPS WERE CARRIED OUT MANUALLY IN QGIS.
}


####  DTE COMPARISON GRAPHS  --------------------------------------------------
##
##  This block creates the 1:1 scatter plots for the DTE extraction comparisons.
prj_root <- "Research/BSGMGeneral/ValidationOutput/"
outroot <- "Research/BSGMGeneral/Figures/"

##  Declare whether, for the MAUPP comparisons, we want to only plot the points 
##  which the BSGM-inputs declare as transitioning within the period:
tran_only = TRUE

##  Declare the countries to create plots for:
##  NOTE:  Only NGA and MDG can be run using the 'MAUPP Random' input for the 
##         'comparison_names' parameter.

isolist <-"NGA"#c("PAK","NGA","MDG","KHM","VNM","PER", "CRI","NPL")

comparison_names <- "MAUPP Random"#c("ESA Admin","ESA Random")
#"MAUPP Random","ESA Admin","ESA Random",

##  For all the specified comparison data:
for(comparison_name in comparison_names){
  print(paste0("Working on the ", comparison_name," datasets..."))
  ##  For every country:
  for(i in isolist){
    print(paste0("   Working on ", i))
    ##  Retrieve the DTE random comparisons file pattern:
    if(comparison_name == "ESA Random"){
      reg <- ".*DTE_extract_rand_.*ESACCI\\.rds"
    }
    if(comparison_name == "ESA Admin"){
      reg <- ".*DTE_extract_admin_.*ESACCI\\.rds"
    }
    if(comparison_name == "MAUPP Random"){
      reg <- ".*DTE_extract_rand_.*MAUPP\\.rds"
    }
    ##  Retrieve the extraction data files using the pattern:
    flist <- paste0(root,prj_root,i,"/",
                    list.files(paste0(root,prj_root,i,"/"),
                               pattern = reg))
    
    
    ##  For every file which was retrieved:
    for(f in flist){
      print(paste0("     Processing file: ",f))
      ##  Read in the data:
      indat <- readRDS(f)
      
      ##  Remove any NAs which might have been acquired during the point value 
      ##  extraction because of projection-reprojection misalignement of edge 
      ##  areas which will never be eliminated:
      indat <- indat[complete.cases(indat),]
      
      ##  Set our color palette for the contours:
      cols <- c("#AC0C0C","#D95B0D","#19C25D","#1441D5")
      
      ##  If comparing to ESA across a random sample of points from the entire 
      ##  country:
      if(comparison_name == "ESA Random"){
        ##    Create the plot  ----
        
        ##  Get the paths needed to show the .9,.75,.5, and .25 density lines:
        ##    Solution derived from this great stackoverflow post
        ##  https://stackoverflow.com/questions/19329318/how-to-correctly-interpret-ggplots-stat-density2d
        ##    HPDregionplots returns a list of contour lines; multiple if they 
        ##    happen to cross any axis. This multiple was an issue until I 
        ##    unlisted the list and combined the x and y vectors.
        df <- indat[,c("OBS.DTE","PRED.DTE")]
        L90 <- getLevel(df,0.1)
        L75 <- getLevel(df, 0.25)
        L50 <- getLevel(df, 0.5)
        L25<- getLevel(df, 0.75)
        
        ##  Remove the unnecessary dataframe:
        rm(df)
        gc()
        
        ##  Determine the min and max of the plots based upon the country data 
        ##  so we have fixed axes between plots. Each vector gives the ordered 
        ##  limits as xmin, xmax, ymin, ymax. This took a lot of adjusting...
        limlist <- list("MDG" = c(-1000,50000,-1000,40000),
                        "CRI" = c(-5000,37500,-5000,30000),
                        "VNM" = c(-5000,60000,-5000,30000),
                        "KHM" = c(-5000,100000,-1000,50000),
                        "NGA" = c(-10000,80000,-5000,50000),
                        "NPL" = c(-1000,75000,-5000,57500),
                        "PAK" = c(-5000,130000,-5000,80000),
                        "PER" = c(-5000,150000,-5000,185000))
        
        ##  Create the 1:1 plot with no overlay of other data:
        plot11 <- ggplot(indat, aes(x = OBS.DTE, y= PRED.DTE))+
          geom_point(alpha=0.1) + 
          theme_bw() + 
          stat_smooth(method="lm", fill=NA, colour="red", alpha=0.6) + 
          geom_abline(slope=1, intercept=0, linetype="dotted", 
                      colour="black", size =1, show.legend = TRUE) +
          scale_x_continuous(limits=c(limlist[[i]][1],limlist[[i]][2])) + 
          scale_y_continuous(limits=c(limlist[[i]][3],limlist[[i]][4])) +
          labs(x=paste0(comparison_name," DTE (meters)"), y="BSGM DTE (meters)")+
          geom_hline(yintercept = 0)+
          geom_vline(xintercept = 0)+
          geom_path(data = L90, aes(x=x,y=y,colour = "1"),
                    show.legend = TRUE)+
          geom_path(data = L75, aes(x=x,y=y,colour = "2"),
                    show.legend = TRUE)+
          geom_path(data = L50, aes(x=x,y=y,colour = "3"),
                    show.legend = TRUE)+
          geom_path(data = L25, aes(x=x,y=y,colour = "4"),
                    show.legend = TRUE)+
          scale_color_manual(values = rev(cols),
                             labels = c("0.90","0.75","0.50","0.25"))+
          guides(colour=guide_legend(title="Point Density\nPercentile"))
        
        ##  Declare the output directory:
        outdir <- ensure_dir(paste0(root, outroot,i,"/"))
        ##  Get the year out of the file name
        y <- sub(".*/.*([0-9]{4}).*\\.rds","\\1",f)
        ##  Inititalize the output graphic device
        tiff(file = paste0(outdir, i,"_1_1Plots_",comparison_name,"_",y,".tiff"),
             res = 150, compression = "lzw",
             width = 120,height=100,units = "mm")
        grid.arrange(plot11,
                     top = paste0(country_list[[i]]," ",comparison_name, " ", y))
        dev.off()
      }
      
      if(comparison_name == "MAUPP Random"){
        ##  Load the transition raster:
        tran <- raster(Sys.glob(paste0(root,
                                       "Research/BSGM/output/prj_2000-2012_", i,
                                       "/tmp/*transition.tif"))[1])
        ##  Load the correct MAUPP raster:
        ##     Jesus I should have done all this in the preprocessing...
        ##  Retrieve the city name based upon the expected file name:
        city <- sub(".*_DTE_extract_rand_comp_(.*)_[0-9]{4}.*[.]rds", "\\1",
                    basename(f), perl = TRUE) 
        ##  Retrieve the year from the expected file name:
        year <- sub(".*_([0-9]{4}).*[.]rds", "\\1",
                    basename(f), perl = TRUE) 
        ##  Retrieve and load the MAUPP raster:
        mauppras <- raster(paste0(root, 
                                  "Research/BSGMGeneral/ValidationData/MAUPP/Derived/",
                                  "MAUPP_",i,"_",city,"_classes_", year, ".tif"))
        ##  Get points of nonNA cells and convert to spatial points:
        maupppoint <- rasterToPoints(mauppras, 
                                     fun=function(x){!is.na(x)},
                                     spatial = TRUE,
                                     progress="text")
        ##  Get cell ID of those points:
        maupppoint <- extract(mauppras,maupppoint, method = "simple",
                              cellnumbers = TRUE, sp = TRUE)
        ##  Modify the column names:
        names(maupppoint) <- c("VAL1","CELLID", "VAL2")
        ##  Remove the raster from memory:
        rm(mauppras)
        
        ##  Extract transition values using those points:
        maupppoint <- extract(tran,maupppoint, method = "simple",
                              cellnumbers = FALSE, sp = TRUE)
        ##  Modify the names of columns again:
        names(maupppoint) <- c("VAL1","CELLID", "VAL2", "TRAN")
        ##  Removew the transition raster:
        rm(tran)
        ##  Remove columns which we are not interested in:
        maupppoint <- maupppoint[,-c(1,3)]
        
        ##  Merge the values based on the MAUPP raster CELLID:
        indat <- dplyr::left_join(indat,
                                  as.data.frame(maupppoint)[,c(1,2)],
                                  by = "CELLID")
        ##  Make sure it is a data.table:
        indat <- as.data.table(indat)
        ##  Remove records which have NA in TRAN as those are water masked in 
        ##  the BSGM (but not in the MAUPP):
        indat <- indat[complete.cases(indat),]
        
        ##  If we want to look only at transition areas as defined by the inputs 
        ##  to BSGM:
        if(tran_only){
          ##  Keep only those cells which were defined to have transitioned 
          ##  during the modelling period:
          indat <- indat %>% 
            filter(TRAN == 1) %>% as.data.table(indat)
        }
        
        ##  ----
        
        
        ##    Create the plots  ----
        ##  Get the city name we are working with:
        cityname <- sub(".*_rand_comp_(.*)_[0-9]{4}.*","\\1",f)
        ##  Get the paths needed to show the .9,.75,.5, and .25 density lines:
        ##    Solution derived from this great stackoverflow post
        ##  https://stackoverflow.com/questions/19329318/how-to-correctly-interpret-ggplots-stat-density2d
        ##    HPDregionplots returns a list of contour lines; multiple if they 
        ##    happen to cross any axis. This multiple was an issue until I 
        ##    unlisted the list and combined the x and y vectors.
        df <- indat[,c("OBS.DTE","PRED.DTE")]
        L90 <- getLevel(df,0.9)
        L75 <- getLevel(df, 0.75)
        L50 <- getLevel(df, 0.5)
        L25<- getLevel(df, 0.25)
        ##  Remove the no longer necessary dataframe:
        rm(df)
        gc()
        
        
        ##  Create the 1:1 plot:
        plot11 <- ggplot(indat, aes(x = OBS.DTE, y= PRED.DTE))+
          geom_point(alpha=0.3, size = 2) + 
          theme_bw() + 
          geom_hline(yintercept = 0)+
          geom_vline(xintercept = 0)+
          stat_smooth(method="lm", fill=NA, colour="red", alpha=0.6) + 
          geom_abline(slope=1, intercept=0, linetype="dotted", 
                      colour="black", size =1, show.legend = TRUE) +
          scale_x_continuous(limits=c(min(indat$OBS.DTE),max(indat$OBS.DTE))) + 
          scale_y_continuous(limits=c(min(indat$PRED.DTE),max(indat$PRED.DTE))) +
          labs(x=paste0(comparison_name," DTE (meters)"), y="BSGM DTE (meters)")+
          geom_path(data = L90, aes(x=x,y=y,colour = "1"),
                    show.legend = TRUE)+
          geom_path(data = L75, aes(x=x,y=y,colour = "2"),
                    show.legend = TRUE)+
          geom_path(data = L50, aes(x=x,y=y,colour = "3"),
                    show.legend = TRUE)+
          geom_path(data = L25, aes(x=x,y=y,colour = "4"),
                    show.legend = TRUE)+
          scale_color_manual(values = rev(cols),
                             labels = c("0.90","0.75","0.50","0.25"))+
          guides(colour=guide_legend(title="Point Density\nPercentile"))
        
        
        ##  Declare the output directory:
        outdir <- ensure_dir(paste0(root, outroot,i,"/"))
        ##  Get the year out of the file name:
        y <- sub(".*/.*([0-9]{4}).*\\.rds","\\1",f)
        ##  Arrange the two plots in a grid and output to file:
        ##  Initialize the output graphics:1
        tiff(file = paste0(outdir, i,"_1_1Plots_",comparison_name,"_",
                           cityname,"_",y,".tiff"),
             res = 300, compression = "lzw",
             width = 90,height=60,units = "mm")
        grid.arrange(plot11,
                     top = paste0(country_list[[i]]," ",comparison_name, " \n",
                                  cityname," ", y))
        dev.off()
        
      }
      
      ##  If this is subnational unit representative sample of the country, plot 
      ##  the mean of every unit in order to understand how different 
      ##  things going into the RF are:
      if(comparison_name == "ESA Admin"){
        ##  Aggregate the table by GID  ----
        aggdat <- indat %>% group_by(GID) %>% 
          mutate(AVG.OBS.DTE = mean(OBS.DTE, na.rm = TRUE),
                 AVG.PRED.DTE = mean(PRED.DTE, na.rm = TRUE)) %>%
          dplyr::summarize(AVG.OBS.DTE = mean(OBS.DTE, na.rm = TRUE),
                           AVG.PRED.DTE = mean(PRED.DTE, na.rm = TRUE))
        
        
        ##    Create the plots  ----
        ##  Get the paths needed to show the .9,.75,.5, and .25 density lines:
        ##    Solution derived from this great stackoverflow post
        ##  https://stackoverflow.com/questions/19329318/how-to-correctly-interpret-ggplots-stat-density2d
        df <- aggdat[,c("AVG.OBS.DTE","AVG.PRED.DTE")]
        L90 <- getLevel(df,0.9)
        L75 <- getLevel(df, 0.75)
        L50 <- getLevel(df, 0.5)
        L25<- getLevel(df, 0.25)
        rm(df)
        gc()
        
        ##  Determine the min and max of the plots based upon the country data 
        ##  so we have fixed axes between plots. Each vector gives the ordered 
        ##  limits as xmin, xmax, ymin, ymax.
        limlist <- list("MDG" = c(0,45000,0,25000),
                        "CRI" = c(-5000,30000,-2000,15000),
                        "VNM" = c(-5000,60000,-1000,15000),
                        "KHM" = c(-5000,90000,-5000,32000),
                        "NGA" = c(-7500,50000,-5000,25000),
                        "NPL" = c(-5000,75000,-2000,50000),
                        "PAK" = c(-5000, 100000,-2000,40000),
                        "PER" = c(-5000,120000,-2000,82000))
        
        ##  Create the 1:1 plot:
        plot11 <- ggplot(aggdat, aes(x = AVG.OBS.DTE, y= AVG.PRED.DTE))+
          geom_point(alpha=0.6) + 
          theme_bw() + 
          stat_smooth(method="lm", fill=NA, colour="red", alpha=0.6) + 
          geom_abline(slope=1, intercept=0, linetype="dotted", 
                      colour="black", size =1, show.legend = TRUE) +
          scale_x_continuous(limits=c(limlist[[i]][1],limlist[[i]][2])) + 
          scale_y_continuous(limits=c(limlist[[i]][3],limlist[[i]][4])) +
          labs(x=paste0(comparison_name," DTE (meters)"), y="BSGM DTE (meters)")+
          geom_hline(yintercept = 0)+
          geom_vline(xintercept = 0)+
          geom_path(data = L90, aes(x=x,y=y,colour = "1"),
                    show.legend = TRUE)+
          geom_path(data = L75, aes(x=x,y=y,colour = "2"),
                    show.legend = TRUE)+
          geom_path(data = L50, aes(x=x,y=y,colour = "3"),
                    show.legend = TRUE)+
          geom_path(data = L25, aes(x=x,y=y,colour = "4"),
                    show.legend = TRUE)+
          scale_color_manual(values = rev(cols),
                             labels = c("0.90","0.75","0.50","0.25"))+
          guides(colour=guide_legend(title="Point Density\nPercentile"))
        
        
        
        
        ##  Declare the output directory: 
        outdir <- ensure_dir(paste0(root, outroot,i,"/"))
        
        ##  Get the year out of the file name
        y <- sub(".*/.*([0-9]{4}).*\\.rds","\\1",f)
        ##  Arrange the two plots in a grid and output to file:
        tiff(file = paste0(outdir, i,"_1_1Plots_",comparison_name,"_",y,".tiff"),
             res = 150,compression='lzw',
             width = 120,height=100,units = "mm")
        grid.arrange(plot11,
                     top = paste0(country_list[[i]]," ",comparison_name, " ", y))
        dev.off()
      }
    }
  }
}






#### ESA COMPARISON CONTINGENCY TABLES BY ADMIN UNIT SAMPLE  ------------------
##
##  This block calculates the TP, FP, TN, FN, associted metrics, null models,and 
##  outputs tables using the htmlTable package.  
##  This is for the admin representative statistics as those will need to be
##  processed by group.
prj_root <- "Research/BSGMGeneral/ValidationOutput/"
isolist <- c("PAK","NGA","MDG","KHM","VNM","PER", "CRI","NPL")
##  Calculate the random null model?
calcnull = TRUE
##  Calculate and display the positive predictor model?
calcpos = TRUE
##  Write the output to a file?
writefile <- TRUE
##  Resave table?
resave <- TRUE
## For every country:
for( i in isolist){
  print(paste0("Working on ",i,"..."))
  ##  Retrieve the basic tables:
  flist <- as.vector(Sys.glob(paste0(root,prj_root,i,
                                     "/validation_datatable_",i,"_*.rds")))
  ##  Refine it some so we arent utilizing incorrect tables:
  m <- regexpr(".*/.*_([0-9]{4}|[0-9]{4}_prevalence.*)\\.rds", 
               flist, 
               perl = TRUE)
  flist <- regmatches(flist,m)
  ##  Ensure a contingency table folder exists and make that the new output 
  ##  directory:
  outdir <- ensure_dir(paste0(root,prj_root,i,"/ContingencyTables/"))
  
  ##  Set the table themes to minimal:
  tt <- ttheme_minimal()
  ##  For retrieved validation/comparison file:
  for(f in flist){
    print(paste0("     Processing ",f," ..."))
    ##  Determine any output file tags we want to use:
    if(length(sub(".*(validation_datatable).*","\\1",f,perl=TRUE))>0){
      reg <- ".*/validation_datatable_(.*)\\.rds"
      tag <- sub(reg,"\\1",f,perl=TRUE)
    }
    ##  Load the data and ensure it is a data.table:
    indat <- as.data.table(readRDS(f))
    
    ##  Determine the TP,FP,FN,TN based upon the default Random Forest 50 
    ##  percent threshold:
    indat <- indat[PRED==1 & OBS==1, TP:=1]
    indat <- indat[PRED==1 & OBS==0, FP:=1]
    indat <- indat[PRED==0 & OBS==1, FN:=1]
    indat <- indat[PRED==0 & OBS==0, TN:=1]
    
    ##  If we are calculating the null model:
    if(calcnull){
      ##  Calculate the NULL Model if Desired:
      indat <- calcContingency(indat,null_model=TRUE)
    }
    ##  If we are calculating the always positive model:
    if(calcpos){
      ##  Calculate the always Positive Model if Desired:
      indat <- calcContingency(indat,pos_model = TRUE)
    }
    ##  If we are saving the ourput, save it:
    if(resave){
      saveRDS(indat, file = f)
    }
    
    ##  Create a matrix to hold the TP, FP, FN, TN, and marginal values
    ##  in the contingency table:
    footbl <- matrix(ncol = 3, nrow = 3)
    
    ##  Add in the corresponding data to construct the basic contingency table:
    footbl[1,1] <- sum(indat$TP)
    footbl[1,2] <- sum(indat$FP)
    footbl[1,3] <- sum(indat$PRED)
    footbl[2,1] <- sum(indat$FN)
    footbl[2,2] <- sum(indat$TN)
    footbl[2,3] <- {sum(indat$FN)+sum(indat$TN)}
    footbl[3,1] <- sum(indat$OBS)
    footbl[3,2] <- {sum(indat$FP)+sum(indat$TN)}
    footbl[3,3] <- nrow(indat)
    
    ##  Create the table grob object to display the contingency table:
    foo_gtable <- tableGrob(footbl, 
                            rows= c("Transition","NonTransition","Total"),
                            cols = c("Transition","NonTransition","Total"),
                            theme = tt)
    ##  Add aesthetic borders:
    foo_gtable <- gtable_add_grob(foo_gtable,
                                  grobs = rectGrob(gp=gpar(fill = NA, lwd=2)),
                                  t=2, l=2, b = {nrow(foo_gtable)-1},r=3)
    
    ##  If we are calculating the null model:
    if(calcnull){
      ##  Calculate the null contingency table:
      nulltbl <- matrix(ncol = 3, nrow = 3)
      nulltbl[1,1] <- sum(indat$TPN)
      nulltbl[1,2] <- sum(indat$FPN)
      nulltbl[1,3] <- sum(indat$NULL.PRED)
      nulltbl[2,1] <- sum(indat$FNN)
      nulltbl[2,2] <- sum(indat$TNN)
      nulltbl[2,3] <- {sum(indat$FNN)+sum(indat$TNN)}
      nulltbl[3,1] <- sum(indat$OBS)
      nulltbl[3,2] <- {sum(indat$FPN)+sum(indat$TNN)}
      nulltbl[3,3] <- nrow(indat)
      null_gtable <- tableGrob(nulltbl, 
                               rows= c("Transition","NonTransition","Total"),
                               cols = c("Transition","NonTransition","Total"),
                               theme = tt)
      ##  Add aesthetic borders:
      null_gtable <- gtable_add_grob(null_gtable,
                                     grobs = rectGrob(gp=gpar(fill = NA, lwd=2)),
                                     t=2, l=2, b = {nrow(null_gtable)-1},r=3)
    }
    ##  If we are calculating the always positive model:
    if(calcpos){
      ##  Calculate the null contingency table:
      postbl <- matrix(ncol = 3, nrow = 3)
      postbl[1,1] <- sum(indat$TPP)
      postbl[1,2] <- sum(indat$FPP)
      postbl[1,3] <- sum(indat$POS.PRED)
      postbl[2,1] <- sum(indat$FNP)
      postbl[2,2] <- sum(indat$TNP)
      postbl[2,3] <- {sum(indat$FNP)+sum(indat$TNP)}
      postbl[3,1] <- sum(indat$OBS)
      postbl[3,2] <- {sum(indat$FPP)+sum(indat$TNP)}
      postbl[3,3] <- nrow(indat)
      pos_gtable <- tableGrob(postbl, 
                              rows= c("Transition","NonTransition","Total"),
                              cols = c("Transition","NonTransition","Total"),
                              theme = tt)
      ##  Add aesthetic borders:
      pos_gtable <- gtable_add_grob(pos_gtable,
                                    grobs = rectGrob(gp=gpar(fill = NA, lwd=2)),
                                    t=2, l=2, b = {nrow(pos_gtable)-1},r=3)
    }
    
    ##  Calculate defined metrics for the contingency table:
    prevalence <- round(sum(indat$OBS)/nrow(indat),digits=4)
    recall <- round(sum(indat$TP)/sum(indat$OBS),digits = 3)
    precision <- round(sum(indat$TP)/sum(indat$PRED),digits=3)
    fscore <- round(2/{1/recall + 1/precision},digits=3)
    specificity <- round(sum(indat$TN)/{sum(indat$FP)+sum(indat$TN)},digits=3)
    
    
    if(calcnull){
      ##  Calculate metrics
      nullprevalence <- round(sum(indat$OBS)/nrow(indat),digits=4)
      nullrecall <- round(sum(indat$TPN)/sum(indat$OBS),digits = 3)
      nullprecision <- round(sum(indat$TPN)/sum(indat$NULL.PRED),digits = 3)
      nullfscore <- round(2/{1/nullrecall + 1/nullprecision},digits = 3)
      nullspecificity <- round(sum(indat$TNN)/{sum(indat$FPN)+sum(indat$TNN)},digits = 3)
    }
    if(calcpos){
      ##  Calculate metrics:
      posprevalence <- round(sum(indat$OBS)/nrow(indat),digits=4)
      posrecall <- round(sum(indat$TPP)/sum(indat$OBS),digits = 3)
      posprecision <- round(sum(indat$TPP)/sum(indat$POS.PRED),digits = 3)
      posfscore <- round(2/{1/posrecall + 1/posprecision},digits = 3)
      posspecificity <- round(sum(indat$TNP)/{sum(indat$FPP)+sum(indat$TNP)},digits = 3)
    }
    
    ##  Create the corresponding table for the summary metrics:
    metrictbl <- tableGrob(matrix(c(c(prevalence,recall,precision,specificity,fscore),
                                    if(calcnull){c(nullprevalence,nullrecall,nullprecision,nullspecificity,nullfscore)},
                                    if(calcpos){c(posprevalence,posrecall,posprecision,posspecificity,posfscore)}),
                                  ncol = {1+calcpos+calcnull}, 
                                  nrow = 5,
                                  byrow = FALSE),
                           rows= c("Prevalence","Recall","Precision","Specificity","Fscore"),
                           cols= c("BSGM",
                                   if(calcnull){"Null Model"},
                                   if(calcpos){"PositivePredictor"}),
                           theme = tt)
    
    ##  Create the arranged figure(s) depending on what the input parameters 
    ##  were:
    outfig <- grid.arrange(arrangeGrob(foo_gtable,metrictbl,
                                       ncol=1,
                                       top = textGrob(f),
                                       bottom = textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.")))
    
    if(calcpos){outfig <- grid.arrange(arrangeGrob(foo_gtable, 
                                                   pos_gtable, 
                                                   metrictbl, 
                                                   textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.\nLeft-top table is the BSGM model, top-right is the Always Positive model."),
                                                   ncol=2),
                                       top = textGrob(f)) 
    }
    if(calcnull){outfig <- grid.arrange(arrangeGrob(foo_gtable,
                                                    null_gtable,
                                                    metrictbl, 
                                                    textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.\nLeft-top table is the BSGM model, top-right is the Null model."),
                                                    ncol=2),
                                        top = textGrob(f))
    }
    if(calcpos & calcnull){outfig <- grid.arrange(arrangeGrob(foo_gtable, 
                                                              null_gtable, 
                                                              pos_gtable,
                                                              metrictbl,
                                                              textGrob("Left side of contingency tables are the BSGM output.\nTop side are the observations of the validation set.\nLeft-top table is the BSGM model, top-right is the Null model, bottom-left is the Always Positive model."),
                                                              ncol=2),
                                                  top = textGrob(f))
    }
    ##  Send the figure to console:
    print(outfig)
    ##  SAve the file if we indicated to do so:
    if(writefile){
      ggsave(paste0(outdir,"ContingencyTable_",tag,".pdf"),outfig)
    }
  }
}




#####  PLOTTING ADMIN AGGREGATE AGREEMENT METRICS  -----------------------------
##  
##  THIS SECTION CALCULATES THE AGGREGATE AGREEMENT METRICS BASED UPON THE 
##  SUBNATIONAL UNIT REPRESENTATIVE SAMPLES AND DATA EXTRACTIONS AND THEN CREATE 
##  CHOROPLETH PLOTS OF THE METRICS IN ADDITION TO A FULL TABLE PRINT OUT OF THE 
##  TABLES FOR EVERY ADMIN UNIT.
##
####
prj_root <- "Research/BSGMGeneral/ValidationOutput/"
isolist <- c("KHM","NGA","MDG","VNM","PER", "CRI","NPL","PAK")

##  Declare extents for the insets to focus on as well as their relative 
##  position in the larger maps created:
insetparams <- list("KHM" = list(data.frame(xmin=104.85, xmax=105,
                                            ymin=11.4, ymax=11.65),
                                 c(0.4,0.4,0.8,0.25)),
                    "PAK" = list(data.frame(xmin = 67.5, xmax = 69.0,
                                            ymin = 27, ymax = 28.5),
                                 c(0.3,0.3,0.2,0.75)),
                    "NGA" = list(data.frame(xmin=5.5,xmax=7.5,
                                            ymin=4.25, ymax=5.75),
                                 c(0.25,0.25,0.75,0.25)),
                    "MDG" = list(data.frame(xmin=47.25, xmax=47.75, 
                                            ymin=-19.25, ymax=-18.5),
                                 c(0.3,0.3,0.7,0.25)),
                    "VNM" = list(data.frame(xmin=105, xmax=106.25, 
                                            ymin=20.5, ymax=21.5),
                                 c(0.35,0.35,0.25,0.5)),
                    "PER" = list(data.frame(xmin=-77.5, xmax=-76.5, 
                                            ymin=-12.5, ymax=-11.75),
                                 c(0.3,0.3,0.25,0.22)),
                    "CRI" = list(data.frame(xmin=-84.5, xmax=-83.75,
                                            ymin=9.75, ymax=10.25),
                                 c(0.4,0.3,0.3,0.25)),
                    "NPL" = list(data.frame(xmin=85.125, xmax=85.625,
                                            ymin=27.5, ymax=27.8),
                                 c(0.3,0.3,0.7,0.75)))

##  For every country:
for(i in isolist){
  
  ##  If the file does not already exist, perform the extractions necessary:
  if(!file.exists(paste0(root,prj_root,i,
                         "/AdminContingencyTables/",i,
                         "_extract_admin_comp_ESACCI_2000_2012.rds"))){
    getAnnualAdminExtract(root="E:/",
                          iso = i,
                          years = 2000:2012)
  }
  
  ##  Bring in the subnational unit representative sample of the BSGM 
  ##  predictions and the corresponding ESA equivalent:
  adtable <- readRDS(file = paste0(root,prj_root,i,
                                   "/AdminContingencyTables/",i,
                                   "_extract_admin_comp_ESACCI_2000_2012.rds"))
  
  ##  Calculate the metrics of interest by GID:
  adtable <- adtable %>% 
    group_by(GID, YEAR) %>%
    mutate(RECALL = TP/(TP+FN),
           PRECISION = TP/(TP+FP),
           SPECIFICITY = TN/(TN+FP),
           FSCORE = 2*(((TP/(TP+FP))*(TP/(TP+FN)))/((TP/(TP+FP))+(TP/(TP+FN)))))
  
  ##  Bring in the shapefile we want to use with it:
  ##    NOTE:  Should be premade.
  ishp <- readOGR(dsn = Sys.glob(paste0(root,"Research/BSGM/data/",i,"/*_dissolve.shp")),
                  layer = strsplit(basename(Sys.glob(paste0(root,
                                                            "Research/BSGM/data/",i,
                                                            "/*_dissolve.shp"))),
                                   ".shp")[[1]][1])
  
  ##  For every year of mapping we would like to do:
  for(y in 2000:2012){
    ##  Subset the data to the year of current interest:
    seltab <- adtable %>% dplyr::filter(YEAR == y)
    ##  Merge it with the shapefile:
    mshp <- merge(ishp,seltab,by = "GID",all.x = TRUE)
    
    ##  Prep the shapefile for plotting by converting to dataframe defining 
    ##  regions for the polygons:
    mshp@data$id <- rownames(mshp@data)
    mshppoints <- fortify(mshp, region = "id")
    mshp_df <- join(mshppoints,mshp@data, by = "id")
    
    
    ##  Create maps for that year  ----
    ##    RECALL MAP
    ##  Declare the breaks to be used; we just use equal interval here. The 
    ##  plots in the main article use Jenk's Natural Breaks.
    mshp_df$disc <- cut(mshp_df$RECALL, breaks = c(-0.001,seq(0.2,1.0,by=0.2)))
    ##  NOTE:  We only actually have values between zero and 1, but have to 
    ##  construct the breaks in the above manner to ensure we get all the values
    ##  so we will rename the factor levels to avoid confusing any readers of 
    ##  the maps.
    
    ##  Rename the label for that facotr level so it is not confusing to 
    ##  readers:
    levels(mshp_df$disc)[1] <- c("[0.0,0.2]")
    ##  Create the recall map:
    recallmap <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, aes(long,lat,
                                     group=group, 
                                     fill=disc), 
                   colour = "grey20",
                   size=0.1)+
      scale_fill_brewer(type = "seq",
                        palette = "YlGnBu",
                        na.value = 'cornsilk3')+
      geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                 ymin=ymin,ymax=ymax),
                alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
      labs(fill="Recall",x="Longitude",y="Latitude")+
      guides(fill = guide_legend(reverse = TRUE))+
      annotate("text", 
               x = {mean(mshp_df$long)},
               y = {max(mshp_df$lat)},
               label = y,
               size = 6)
    ##  If Costa Rica, we have a special extent override we need to do because 
    ##  of a far outlying island, which makes it zoom out super far to try and 
    ##  include the small island; we exclude it to make everywhere else visible.
    if(i == "CRI"){
      recallmap <- ggplot()+
        theme_bw()+
        geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                       fill=disc), colour = "grey20",size=0.1)+
        scale_fill_brewer(type = "seq",
                          palette = "YlGnBu",
                          na.value = 'cornsilk3')+
        xlim(ifelse(i=="CRI",-86,NA),ifelse(i=="CRI",-82.5,NA))+
        ylim(ifelse(i=="CRI",8,NA),ifelse(i=="CRI",11.5,NA))+
        geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                   ymin=ymin,ymax=ymax),
                  alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
        labs(fill="Recall",x="Longitude",y="Latitude")+
        guides(fill = guide_legend(reverse = TRUE))+
        annotate("text", 
                 x = {mean(mshp_df$long)},
                 y = {max(mshp_df$lat)},
                 label = y,
                 size = 6)
      }
    
    ##  Create the inset map for recall:
    recallinset <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                     fill=disc), colour = "grey20",size=0.1)+
      scale_fill_brewer(type = "seq",
                        palette = "YlGnBu",
                        na.value = 'cornsilk3')+
      coord_map(xlim = c(insetparams[[i]][[1]]$xmin,
                         insetparams[[i]][[1]]$xmax),
                ylim = c(insetparams[[i]][[1]]$ymin,
                         insetparams[[i]][[1]]$ymax))+
      labs(x=NULL,y=NULL)+
      guides(fill = FALSE)+
      theme(axis.text.x =element_blank(),
            axis.text.y= element_blank(), 
            axis.ticks=element_blank(),
            axis.title.x =element_blank(),
            axis.title.y= element_blank(),
            panel.border = element_rect(size = 1))
    
    ##  Set up the viewports for plotting:
    v1 <- viewport(width = 1, height = 1, x = 0.5, y= 0.5)
    v2 <- viewport(width = insetparams[[i]][[2]][1],
                   height = insetparams[[i]][[2]][2],
                   x = insetparams[[i]][[2]][3],
                   y = insetparams[[i]][[2]][4])
    
    ##  Plot the recall map to file:
    tiff(filename = paste0(root,"Research/BSGMGeneral/Figures/",i,"/",
                           i,"_Admin_Comparison_ESA_RECALL_",y,".tif"),
         width = ifelse({i=="MDG"|i=="VNM"},21.0,29.7),
         height=ifelse({i=="MDG"|i=="VNM"},29.7,21.0),
         res= 150,
         units="cm",
         bg="white")
    print(recallmap,vp=v1)
    print(recallinset,vp=v2)
    dev.off()
    
    
    ##  PRECISION MAP
    ##  Declare the breaks to be used; we just use equal interval here. The 
    ##  plots in the main article use Jenk's Natural Breaks: 
    mshp_df$disc <- cut(mshp_df$PRECISION, breaks = c(-0.001,seq(0.2,1.0,by=0.2)))
    levels(mshp_df$disc)[1] <- c("[0.0,0.2]")
    ##  Create the precision maps:
    precisionmap <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                     fill=disc), colour = "grey20",size=0.1)+
      geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                 ymin=ymin,ymax=ymax),
                alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
      scale_fill_brewer(type = "seq",
                        palette = "OrRd",
                        na.value = 'cornsilk3')+
      labs(fill="Precision",x="Longitude",y="Latitude")+
      guides(fill = guide_legend(reverse = TRUE))+
      annotate("text", 
               x = {mean(mshp_df$long)},
               y = {max(mshp_df$lat)},
               label = y,
               size = 6)
    ##  Again, CRI plotting considerations:
    if(i=="CRI"){
      precisionmap <- ggplot()+
        theme_bw()+
        geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                       fill=disc), colour = "grey20",size=0.1)+
        xlim(ifelse(i=="CRI",-86,NA),ifelse(i=="CRI",-82.5,NA))+
        ylim(ifelse(i=="CRI",8,NA),ifelse(i=="CRI",11.5,NA))+
        geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                   ymin=ymin,ymax=ymax),
                  alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
        scale_fill_brewer(type = "seq",
                          palette = "OrRd",
                          na.value = 'cornsilk3')+
        labs(fill="Precision",x="Longitude",y="Latitude")+
        guides(fill = guide_legend(reverse = TRUE))+
        annotate("text", 
                 x = {mean(mshp_df$long)},
                 y = {max(mshp_df$lat)},
                 label = y,
                 size = 6)
    }
    
    ##  Create the inset map:
    precisioninset <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, 
                   aes(long,lat,group=group, 
                       fill=disc), 
                   colour = "grey20",
                   size=0.1)+
      scale_fill_brewer(type = "seq",
                        palette = "OrRd",
                        na.value = 'cornsilk3')+
      coord_map(xlim = c(insetparams[[i]][[1]]$xmin,
                         insetparams[[i]][[1]]$xmax),
                ylim = c(insetparams[[i]][[1]]$ymin,
                         insetparams[[i]][[1]]$ymax))+
      labs(x=NULL,y=NULL)+
      guides(fill = FALSE)+
      theme(axis.text.x =element_blank(),
            axis.text.y= element_blank(), 
            axis.ticks=element_blank(),
            axis.title.x =element_blank(),
            axis.title.y= element_blank(),
            panel.border = element_rect(size = 1))
    ##  Set up the viewports:
    v1 <- viewport(width = 1, height = 1, x = 0.5, y= 0.5)
    v2 <- viewport(width = insetparams[[i]][[2]][1],
                   height = insetparams[[i]][[2]][2],
                   x = insetparams[[i]][[2]][3],
                   y = insetparams[[i]][[2]][4])
    
    ##  Plot the precision map to file:
    tiff(filename = paste0(root,"Research/BSGMGeneral/Figures/",i,"/",
                           i,"_Admin_Comparison_ESA_PRECISION_",y,".tif"),
         width = ifelse({i=="MDG"|i=="VNM"},21.0,29.7),
         height=ifelse({i=="MDG"|i=="VNM"},29.7,21.0),
         res= 150,
         units="cm",
         bg="white")
    print(precisionmap,vp=v1)
    print(precisioninset,vp=v2)
    dev.off()
    
    ##  SPECIFICITY MAP
    ##  Same as previous maps for recall and precision:
    mshp_df$disc <- cut(mshp_df$SPECIFICITY, breaks = c(-0.001,seq(0.2,1.0,by=0.2)))
    levels(mshp_df$disc)[1] <- c("[0.0,0.2]")
    specificitymap <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, 
                   aes(long,lat,group=group, 
                       fill=disc), 
                   colour = "grey20",
                   size=0.1)+
      scale_fill_brewer(type = "seq",
                        palette = "GnBu",
                        na.value = 'cornsilk3')+
      labs(fill="Specificity",x="Longitude",y="Latitude")+
      geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                 ymin=ymin,ymax=ymax),
                alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
      guides(fill = guide_legend(reverse = TRUE))+
      annotate("text", 
               x = {mean(mshp_df$long)},
               y = {max(mshp_df$lat)},
               label = y,
               size = 6)
    
    if(i=="CRI"){
      specificitymap <- ggplot()+
        theme_bw()+
        geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                       fill=disc), colour = "grey20",size=0.1)+
        scale_fill_brewer(type = "seq",
                          palette = "GnBu",
                          na.value = 'cornsilk3')+
        xlim(ifelse(i=="CRI",-86,NA),ifelse(i=="CRI",-82.5,NA))+
        ylim(ifelse(i=="CRI",8,NA),ifelse(i=="CRI",11.5,NA))+
        labs(fill="Specificity",x="Longitude",y="Latitude")+
        geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                   ymin=ymin,ymax=ymax),
                  alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
        guides(fill = guide_legend(reverse = TRUE))+
        annotate("text", 
                 x = {mean(mshp_df$long)},
                 y = {max(mshp_df$lat)},
                 label = y,
                 size = 6)
    }
    
    specificityinset <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                     fill=disc), colour = "grey20",size=0.1)+
      scale_fill_brewer(type = "seq",
                        palette = "GnBu",
                        na.value = 'cornsilk3')+
      coord_map(xlim = c(insetparams[[i]][[1]]$xmin,
                         insetparams[[i]][[1]]$xmax),
                ylim = c(insetparams[[i]][[1]]$ymin,
                         insetparams[[i]][[1]]$ymax))+
      labs(x=NULL,y=NULL)+
      guides(fill = FALSE)+
      theme(axis.text.x =element_blank(),
            axis.text.y= element_blank(), 
            axis.ticks=element_blank(),
            axis.title.x =element_blank(),
            axis.title.y= element_blank(),
            panel.border = element_rect(size = 1))
    ##  Set up the viewports:
    v1 <- viewport(width = 1, height = 1, x = 0.5, y= 0.5)
    v2 <- viewport(width = insetparams[[i]][[2]][1],
                   height = insetparams[[i]][[2]][2],
                   x = insetparams[[i]][[2]][3],
                   y = insetparams[[i]][[2]][4])
    
    ##  Plot the recall map to file:
    tiff(filename = paste0(root,"Research/BSGMGeneral/Figures/",i,"/",
                           i,"_Admin_Comparison_ESA_SPECIFICITY_",y,".tif"),
         width = ifelse({i=="MDG"|i=="VNM"},21.0,29.7),
         height=ifelse({i=="MDG"|i=="VNM"},29.7,21.0),
         res= 150,
         units="cm",
         bg="white")
    print(specificitymap,vp=v1)
    print(specificityinset,vp=v2)
    dev.off()
    
    ##  FScore Map
    mshp_df$disc <- cut(mshp_df$FSCORE, breaks = c(-0.001,seq(0.2,1.0,by=0.2)))
    levels(mshp_df$disc)[1] <- c("[0.0,0.2]")
    fscoremap <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                     fill=disc), colour = "grey20",size=0.1)+
      scale_fill_brewer(type = "seq",
                        palette = "Greens",
                        na.value = 'cornsilk3')+
      geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                 ymin=ymin,ymax=ymax),
                alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
      labs(fill="F-Score",x="Longitude",y="Latitude")+
      guides(fill = guide_legend(reverse = TRUE))+
      annotate("text", 
               x = {mean(mshp_df$long)},
               y = {max(mshp_df$lat)},
               label = y,
               size = 6)
    
    if(i=="CRI"){
      fscoremap <- ggplot()+
        theme_bw()+
        geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                       fill=disc), colour = "grey20",size=0.1)+
        scale_fill_brewer(type = "seq",
                          palette = "Greens",
                          na.value = 'cornsilk3')+
        xlim(ifelse(i=="CRI",-86,NA),ifelse(i=="CRI",-82.5,NA))+
        ylim(ifelse(i=="CRI",8,NA),ifelse(i=="CRI",11.5,NA))+
        geom_rect(data = insetparams[[i]][[1]],aes(xmin=xmin,xmax=xmax,
                                                   ymin=ymin,ymax=ymax),
                  alpha = 0, colour = "black", size = 1.0, linetype = "solid")+
        labs(fill="F-Score",x="Longitude",y="Latitude")+
        guides(fill = guide_legend(reverse = TRUE))+
        annotate("text", 
                 x = {mean(mshp_df$long)},
                 y = {max(mshp_df$lat)},
                 label = y,
                 size = 6)
    }
    
    fscoreinset <- ggplot()+
      theme_bw()+
      geom_polygon(data=mshp_df, aes(long,lat,group=group, 
                                     fill=disc), colour = "grey20",size=0.1)+
      scale_fill_brewer(type = "seq",
                        palette = "Greens",
                        na.value = 'cornsilk3')+
      coord_map(xlim = c(insetparams[[i]][[1]]$xmin,
                         insetparams[[i]][[1]]$xmax),
                ylim = c(insetparams[[i]][[1]]$ymin,
                         insetparams[[i]][[1]]$ymax))+
      labs(x=NULL,y=NULL)+
      guides(fill = FALSE)+
      theme(axis.text.x =element_blank(),
            axis.text.y= element_blank(), 
            axis.ticks=element_blank(),
            axis.title.x =element_blank(),
            axis.title.y= element_blank(),
            panel.border = element_rect(size = 1))
    ##  Set up the viewports:
    v1 <- viewport(width = 1, height = 1, x = 0.5, y= 0.5)
    v2 <- viewport(width = insetparams[[i]][[2]][1],
                   height = insetparams[[i]][[2]][2],
                   x = insetparams[[i]][[2]][3],
                   y = insetparams[[i]][[2]][4])
    
    ##  Plot the recall map to file:
    tiff(filename = paste0(root,"Research/BSGMGeneral/Figures/",i,"/",
                           i,"_Admin_Comparison_ESA_FSCORE_",y,".tif"),
         width = ifelse({i=="MDG"|i=="VNM"},21.0,29.7),
         height=ifelse({i=="MDG"|i=="VNM"},29.7,21.0),
         res= 150,
         units="cm",
         bg="white")
    print(fscoremap,vp=v1)
    print(fscoreinset,vp=v2)
    dev.off()
  }
  
}

##  CREATE PLOT FOR AIDING INTERPRETATION OF DTE PLOTS  ------------------------
##  This is a one-off so no fancy pathing.
pdf(file = "E:/Research/BSGMGeneral/Figures/DTE_InterpretationGuide.pdf",
    paper = "a4r", bg = "white")
##  Plot it:
diagram <- ggplot()+
  geom_hline(yintercept = 0, size = 1)+
  geom_vline(xintercept = 0, size = 1)+
  theme_bw() + 
  geom_abline(slope=1, intercept=0, linetype="solid", 
              colour="black", size =1) +
  geom_abline(slope=-1, intercept=0, linetype="solid", 
              colour="black", size =1) +
  scale_x_continuous(limits=c(-5,5)) + 
  scale_y_continuous(limits=c(-5,5)) +
  labs(x="COMPARISON DTE", y="BSGM DTE")+
  annotate("text", x = 1.5, y = 3.5, label ="A", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = 3.5, y = 1.5, label ="B", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = 3.5, y = -1.5, label ="C", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = 1.5, y = -3.5, label ="D", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = -1.5, y = -3.5, label ="E", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = -3.5, y = -1.5, label ="F", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = -3.5, y = 1.5, label ="G", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = -1.5, y = 3.5, label ="H", size = 5, color = "black",
           fontface = "bold")+
  annotate("text", x = -0.25, y = -0.65, label ="0", size = 5, color = "black",
           fontface = "bold")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
##  Describe the regions:
dtable <- data.frame("REGION" = c("A","B","C","D","E","F","G","H"),
                     "BSGM DTE" = c("+","+","-","-","-","-","+","+"),
                     "COMPARISON DTE"= c("+","+","+","+","-","-","-","-"),
                     "|BSGM| v. |COMP|" = c(">","<","<",">",">","<","<",">"))
desctable <- tableGrob(dtable, rows = NULL,
                       cols = c("REGION"," BSGM DTE", "COMPARISON DTE",
                                "|BSGM| v. |COMP|"),
                       theme = ttheme_minimal(core = list(bg_params = list(fill="white")),
                                              colhead=list(fg_params = list(fontface="plain"),
                                                           bg_params = list(fill="white"))))
##  Add lines to the table:
desctable <- gtable_add_grob(desctable, 
                             grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                  y0 = unit(0,"npc"),
                                                  x1 = unit(1,"npc"),
                                                  y1 = unit(0,"npc"),
                                                  gp = gpar(lwd = 2.0)),
                             t = 1, b = 1, l =1, r = ncol(desctable))
desctable <- gtable_add_grob(desctable, 
                             grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                  y0 = unit(0,"npc"),
                                                  x1 = unit(1,"npc"),
                                                  y1 = unit(0,"npc"),
                                                  gp = gpar(lwd = 2.0)),
                             t = 1, b = nrow(desctable), l =1, r = ncol(desctable))
lay <- rbind(c(3,1,1,3),
             c(2,2,2,2))
grid.arrange(grobs = list(diagram, desctable), layout_matrix = lay)

dev.off()

######
##  RF PERFORMANCE ACROSS PREVALENCE  ----
##
##  Create the dataframe to hold the values I am transcribing from the 
##  contingency tables that were produced above:
##    Group numbers are as follows:
##      1 - NGA 2 - MDG
foodat <- data.frame("GROUP" = as.factor(c("Nigeria","Nigeria","Nigeria",
                                           "Nigeria","Nigeria","Nigeria",
                                           "Madagascar","Madagascar",
                                           "Madagascar","Madagascar",
                                           "Madagascar","Madagascar",
                                           "Madagascar")),
                     "PREV" = c(0.009,0.01,0.015,0.020,0.025,0.03,
                                0.0003,0.005,0.01,0.015,0.020,0.025,0.03),
                     "RECALL" = c(0.805,0.836,0.833,0.828,0.826,0.826,
                                  0.804,0.806,0.807,0.821,0.816,0.816,0.818),
                     "PREC" = c(0.073,0.082,0.118,0.15,0.181,0.209,
                                0.002,0.037,0.071,0.105,0.134,0.163,0.189),
                     "SPEC" = c(0.908,0.906,0.906,0.907,0.907,0.906,
                                0.895,0.895,0.895,0.895,0.895,0.895,0.895))
##  Make the plot:
tiff(filename = "E:/Research/BSGMGeneral/Figures/Publication Ready/RFValidation_Across_Prevalence.tif",
     width = 140, height = 100, units = "mm", compression = "lzw", res =500)
ggplot()+
  geom_line(data = foodat, aes(x=PREV, y = RECALL, linetype = GROUP), size = 0.8)+
  geom_line(data = foodat, aes(x=PREV, y = SPEC, linetype = GROUP), size =0.8)+
  geom_line(data = foodat, aes(x=PREV, y = PREC, linetype = GROUP), size =0.8)+
  theme_bw()+
  ylim(0,1)+
  geom_vline(xintercept=0.009,colour = "grey50", linetype = "dotdash", size = 0.5)+
  geom_vline(xintercept=0.0003,colour = "grey50", linetype = "dotdash", size = 0.5)+
  annotate("text", x = 0.0005, y = 0.5, 
           label = "Madagascar\nActual\nPrevalence",
           hjust = 0, size = 2.5)+
  annotate("text", x = 0.00925, y = 0.5, 
           label = "Nigeria\nActual\nPrevalence",
           hjust = 0, size = 2.5)+
  annotate("text", x = 0.0005, y = 0.95, 
           label = "Recall", size = 2.5,
           hjust = 0, fontface = "bold")+
  annotate("text", x = 0.0005, y = 0.85, 
           label = "Specificity",size = 2.5,
           hjust = 0, fontface = "bold")+
  annotate("text", x = 0.0005, y = 0.05, 
           label = "Precision",size = 2.5,
           hjust = 0, angle = 8, fontface = "bold")+
  labs(x = "Countrywide Transition Prevalence",
       y = "Metric Value",
       linetype = "Country")
dev.off()


####  DTE COMPARISON GRAPHS FOR PUBLICATION  ----------------------------------
##
##  This block creates scatter plots against a 1:1 line for the DTE comparison 
##  extractions in a gridded format more conducive for the publication format.
##  NOTE:  See the original DTE Comparison Graphs block above for the description of what is going on
prj_root <- "Research/BSGMGeneral/ValidationOutput/"
outroot <- "Research/BSGMGeneral/Figures/Publication Ready/"
tran_only = TRUE
country_list <- list("VNM" = "Vietnam",
                     "PER" = "Peru",
                     "PAK" = "Pakistan",
                     "NPL" = "Nepal",
                     "NGA" = "Nigeria",
                     "MDG" = "Madagascar",
                     "KHM" = "Cambodia",
                     "CRI" = "Costa Rica")

isolist <- "NGA"#c("KHM","VNM","PER", "CRI","NPL","PAK")#c("NGA","MDG","KHM","VNM","PER", "CRI","NPL","PAK")#
#c("PAK","NGA","MDG","KHM","VNM","PER", "CRI","NPL")
comparison_names <- c("MAUPP Random")#,"MAUPP Random","ESA Admin","ESA Random",
##  For all the specified data extraction types:
for(comparison_name in comparison_names){
  print(paste0("Working on the ", comparison_name," datasets..."))
  ##  For every country:
  for(i in isolist){
    print(paste0("   Working on ", i))
    ##  Global(study area representative) sample  ------
    ##  Retrieve the DTE random comparisons:
    if(comparison_name == "ESA Random"){
      reg <- ".*DTE_extract_rand_.*ESACCI\\.rds"
    }
    if(comparison_name == "ESA Admin"){
      reg <- ".*DTE_extract_admin_.*ESACCI\\.rds"
    }
    if(comparison_name == "MAUPP Random"){
      reg <- ".*DTE_extract_rand_.*MAUPP\\.rds"
    }
    flist <- paste0(root,prj_root,i,"/",
                    list.files(paste0(root,prj_root,i,"/"),
                               pattern = reg))
    ##  Retrieve the shapefile which has info regarding the prevalence of 
    ##  initial, end BS, and transitions:
    # shppath <- Sys.glob(paste0(root,"Research/BSGM/data/",i,"/*_dissolve.shp"))
    # inshp <- readOGR(dsn = shppath,
    #                  layer = strsplit(basename(shppath),".shp")[[1]][1],
    #                  stringsAsFactors = FALSE)
    # ##  Make sure that inshp$GID is a numeric:
    # inshp$GID <- as.numeric(inshp$GID)
    # inshp$FIRST_COUN <- as.numeric(inshp$FIRST_COUN)
    
    ##  Set our color palette for the contours:
    cols <- c("#AC0C0C","#D95B0D","#19C25D","#1441D5")
    
    if(comparison_name == "MAUPP Random"){
      ##  Create a list to hold all of the plots desired:
      plot_list <- list()
      ##  NOTE THIS SECTION ASSUMES WE HAVE AN EXTRACTION FOR 2005 and 2010.
      ##  Also, in order to have the most efficient use of space, given the 
      ##  limits of the grid package and ggplot2 I will have to add the plot 
      ##  legends to the overall figure using Inkscape or something.
      years <- c(2005,2010)
      
      ##  Load the transition raster:
      tran <- raster(Sys.glob(paste0(root,
                                     "Research/BSGM/output/prj_2000-2012_", i,
                                     "/tmp/*transition.tif"))[1])
      
      ##  Get a list of the city names we are working with:
      citynames <- c()
      for(f in flist){
        ##  Retrieve the city name and add it to the vector:
        citynames <- c(citynames,sub(".*_comp_(.*)_[0-9]{4}_MAUPP\\.rds","\\1",
                                     basename(f), perl = TRUE))
      }
      citynames <- unique(citynames)
      
      ##  For every unique city we will make and output a plot
      for(cn in citynames){
        for(y in years){
          ##  Declare which file we're working with:
          f <- flist[grepl(paste0(".*comp_",cn,"_",y,".*"),flist)]
          print(paste0("     Processing file: ",f))
          ##  Read in the data:
          indat <- readRDS(f)
          ##  Remove any NAs which might have been acquired during the point value 
          ##  extraction because of projection-reprojection misalignement which will
          ##  never be eliminated:
          indat <- indat[complete.cases(indat),]
          
          ##  Load the correct MAUPP raster:
          ##     Jesus I should have done this in the preprocessing...
          mauppras <- raster(paste0(root, 
                                    "Research/BSGMGeneral/ValidationData/MAUPP/Derived/",
                                    "MAUPP_",i,"_",cn,"_classes_", y, ".tif"))
          ##  Get points of cells:
          maupppoint <- rasterToPoints(mauppras, 
                                       fun=function(x){!is.na(x)},
                                       spatial = TRUE,
                                       progress="text")
          ##  Get cell ID:
          maupppoint <- extract(mauppras,maupppoint, method = "simple",
                                cellnumbers = TRUE, sp = TRUE)
          names(maupppoint) <- c("VAL1","CELLID", "VAL2")
          rm(mauppras)
          
          ##  Extract transition values
          maupppoint <- extract(tran,maupppoint, method = "simple",
                                cellnumbers = FALSE, sp = TRUE)
          names(maupppoint) <- c("VAL1","CELLID", "VAL2", "TRAN")
          maupppoint <- maupppoint[,-c(1,3)]
          
          ##  Merge the values based on the MAUPP raster CELLID:
          indat <- dplyr::left_join(indat,
                                    as.data.frame(maupppoint)[,c(1,2)],
                                    by = "CELLID")
          indat <- as.data.table(indat)
          ##  Remove records which have NA in TRAN as those are water masked in 
          ##  the BSGM (but not in the MAUPP):
          indat <- indat[complete.cases(indat),]
          
          ##  If we want to look only at transition areas as defined by the inputs 
          ##  to BSGM:
          if(tran_only){
            indat <- indat %>% filter(TRAN == 1) %>% as.data.table(indat)
          }
          
          ##  ----
          
          # ##  Merge the desired data from the shapefile to the data.table:
          # indat <- as.data.table(merge(indat,as.data.table(inshp), by="GID"))
          # 
          # ##  Quickly calculate prevalence of transitions :
          # indat[,TRAN.PREV := FIRST_N_TR/FIRST_COUN]
          
          ##    Create the plot  ----
          
          ##  Get the paths needed to show the .9,.75,.5, and .25 density lines:
          ##    Solution derived from this great stackoverflow post
          ##  https://stackoverflow.com/questions/19329318/how-to-correctly-interpret-ggplots-stat-density2d
          ##    HPDregionplots returns a list of contour lines; multiple if they 
          ##    happen to cross any axis. This multiple was an issue until I 
          ##    unlisted the list and combined the x and y vectors.
          df <- indat[,c("OBS.DTE","PRED.DTE")]
          L90 <- getLevel(df,0.9)
          L75 <- getLevel(df, 0.75)
          L50 <- getLevel(df, 0.5)
          L25<- getLevel(df, 0.25)
          rm(df)
          gc()
          
          ##  Create the 1:1 plot:
          plot_list[[length(plot_list)+1]] <- ggplot(indat,
                                                     aes(x = OBS.DTE,
                                                         y= PRED.DTE))+
            geom_point(alpha=0.3, size = 1) + 
            theme_bw() + 
            geom_hline(yintercept = 0)+
            geom_vline(xintercept = 0)+
            stat_smooth(method="lm", fill=NA, colour="red",
                        alpha=0.6, size = 0.5) + 
            geom_abline(slope=1, intercept=0, linetype="dotted", 
                        colour="black", size =0.5, show.legend = TRUE) +
            scale_x_continuous(limits=c(min(indat$OBS.DTE),
                                        max(indat$OBS.DTE))) + 
            scale_y_continuous(limits=c(min(indat$PRED.DTE),
                                        max(indat$PRED.DTE))) +
            labs(x="", y="")+
            geom_path(data = L90, aes(x=x,y=y,colour = "1"),
                      show.legend = FALSE)+
            geom_path(data = L75, aes(x=x,y=y,colour = "2"),
                      show.legend = FALSE)+
            geom_path(data = L50, aes(x=x,y=y,colour = "3"),
                      show.legend = FALSE)+
            geom_path(data = L25, aes(x=x,y=y,colour = "4"),
                      show.legend = FALSE)+
            geom_abline(slope=1, intercept=0, linetype="dotted", 
                        colour="black", size =0.5, show.legend = TRUE) +
            geom_abline(slope=1, intercept=500, linetype="twodash", 
                        colour="grey20", size =0.25, show.legend = TRUE) +
            geom_abline(slope=1, intercept=-500, linetype="twodash", 
                        colour="grey20", size =0.25, show.legend = TRUE) +
            scale_color_manual(values = rev(cols),
                               labels = c("0.90","0.75","0.50","0.25"))+
            theme(axis.title.x = element_blank(), 
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(size = 4),
                  axis.text.y=element_text(size=4),
                  plot.title = element_text(size = 6, face = "bold"))+
            ggtitle(label = paste0(country_list[[i]], " ", cn, " ",y))
          
        }
        outdir <- ensure_dir(paste0(root, outroot,i,"/"))
      }
      ##  Arrange the two plots in a grid and output to file:
      # pdf(file = paste0(outdir, i,"_1_1Plots_",comparison_name,"_",y,".pdf"), 
      #     paper ="a4r",onefile = TRUE)
      tiff(file = paste0(outdir, i,"_1_1Plots_",comparison_name,"_2005_2010.tiff"),
           res = 500, compression = "lzw",
           width = 140,height={40*length(citynames)},units = "mm")
      grid.arrange(grobs = plot_list, ncol=2, 
                   left = "BSGM DTE (m)", 
                   bottom = "MAUPP DTE (m)")
      dev.off()
    }
    
    ##  If this is an admin unit representative sample of the country, plot 
    ##  the mean of every admin unit in order to understand how different 
    ##  things going into the RF are:
    if(comparison_name == "ESA Admin"){
      ##  Create a list to hold all of the plots desired:
      plot_list <- list()
      ##  NOTE THIS SECTION ASSUMES WE HAVE AN EXTRACTION FOR 2005 and 2010.
      ##  Also, in order to have the most efficient use of space, given the 
      ##  limits of the grid package and ggplot2 I will have to add the plot 
      ##  legends to the overall figure using Inkscape or something.
      years <- c(2005,2010)
      
      for(y in years){
        ##  Declare which file we're working with:
        f <- flist[grepl(paste0(".*comp_",y,".*"),flist)]
        print(paste0("     Processing file: ",f))
        
        ##  Read in the data:
        indat <- readRDS(f)
        
        ##  Remove any NAs which might have been acquired during the point value 
        ##  extraction because of projection-reprojection misalignement which will
        ##  never be eliminated:
        indat <- indat[complete.cases(indat),]
        
        ##  Aggregate the table  ----
        aggdat <- indat %>% group_by(GID) %>% 
          mutate(AVG.OBS.DTE = mean(OBS.DTE, na.rm = TRUE),
                 AVG.PRED.DTE = mean(PRED.DTE, na.rm = TRUE)) %>%
          dplyr::summarize(AVG.OBS.DTE = mean(OBS.DTE, na.rm = TRUE),
                           AVG.PRED.DTE = mean(PRED.DTE, na.rm = TRUE))
        
        
        # ##  Merge the desired data from the shapefile to the data.table:
        # aggdat <- as.data.table(merge(aggdat,as.data.table(inshp), by="GID"))
        # 
        # ##  Quickly calculate prevalence of transitions and initial and end BS:
        # aggdat[,TRAN.PREV := FIRST_N_TR/FIRST_COUN]
        # aggdat[,INIT.PREV := FIRST_N_T0/FIRST_COUN]
        # aggdat[,END.PREV := FIRST_N_T1/FIRST_COUN]
        
        ##    Create the plot  ----
        #  Get the paths needed to show the .9,.75,.5, and .25 density lines:
        ##    Solution derived from this great stackoverflow post
        ##  https://stackoverflow.com/questions/19329318/how-to-correctly-interpret-ggplots-stat-density2d
        df <- aggdat[,c("AVG.OBS.DTE","AVG.PRED.DTE")]
        L90 <- getLevel(df,0.9)
        L75 <- getLevel(df, 0.75)
        L50 <- getLevel(df, 0.5)
        L25<- getLevel(df, 0.25)
        rm(df)
        gc()
        
        ##  Determine the min and max of the plots based upon the country data 
        ##  so we have fixed axes between plots. Each vector gives the ordered 
        ##  limits as xmin, xmax, ymin, ymax.
        limlist <- list("MDG" = c(0,45000,0,25000),
                        "CRI" = c(-5000,30000,-2000,15000),
                        "VNM" = c(-5000,60000,-1000,15000),
                        "KHM" = c(-5000,90000,-5000,32000),
                        "NGA" = c(-7500,50000,-5000,25000),
                        "NPL" = c(-5000,75000,-2000,50000),
                        "PAK" = c(-5000, 100000,-2000,40000),
                        "PER" = c(-5000,120000,-2000,82000))
        
        ##  Create the 1:1 plot:
        plot_list[[length(plot_list)+1]] <- ggplot(aggdat, 
                                                   aes(x = AVG.OBS.DTE,
                                                       y= AVG.PRED.DTE))+
          geom_point(alpha=0.3, size = 1) + 
          theme_bw() + 
          stat_smooth(method="lm", fill=NA, colour="red",
                      alpha=0.6, size= 0.5) + 
          geom_abline(slope=1, intercept=0, linetype="dotted", 
                      colour="black", size =0.5, show.legend = FALSE) +
          scale_x_continuous(limits=c(limlist[[i]][1],limlist[[i]][2])) + 
          scale_y_continuous(limits=c(limlist[[i]][3],limlist[[i]][4])) +
          geom_hline(yintercept = 0)+
          geom_vline(xintercept = 0)+
          geom_path(data = L90, aes(x=x,y=y,colour = "1"),
                    show.legend = FALSE)+
          geom_path(data = L75, aes(x=x,y=y,colour = "2"),
                    show.legend = FALSE)+
          geom_path(data = L50, aes(x=x,y=y,colour = "3"),
                    show.legend = FALSE)+
          geom_path(data = L25, aes(x=x,y=y,colour = "4"),
                    show.legend = FALSE)+
          scale_color_manual(values = rev(cols),
                             labels = c("0.90","0.75","0.50","0.25"))+
          theme(axis.title.x = element_blank(), 
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 4),
                axis.text.y=element_text(size=4),
                plot.title = element_text(size = 6, face = "bold"))+
          labs(x="", y="")+
          ggtitle(paste(country_list[[i]]," ", y))
      }
      
      outdir <- ensure_dir(paste0(root, outroot,i,"/"))
      
      
      tiff(file = paste0(outdir, i,"_1_1Plots_",comparison_name,"_2005_2010.tiff"),
           res = 500,compression='lzw',
           width = 140,height=50,units = "mm")
      
      grid.arrange(grobs = plot_list, ncol =2, left ="Unit-Avg. BSGM DTE (m)", 
                   bottom= "Unit-Avg. ESA DTE (m)")
      
      dev.off()
    }
  }
}




####  FORMATTING CHOROPLETH MAPS OF CLASSIFICATION METRICS  ----------------------
##
prj_root <- "Research/BSGMGeneral/ValidationOutput/"
for(i in c("MDG","NGA")){
  ##  Retrieve the rds file:
  dtab <- readRDS(paste0(root,prj_root,i,
                         "/AdminContingencyTables/",i,
                         "_extract_admin_comp_ESACCI_2000_2012.rds"))
  ##  Load the shapefile:
  shppath <- Sys.glob(paste0(root, "Research/BSGM/data/",i,"/*.shp"))[1]
  lyrname <- strsplit(basename(shppath),".shp")[[1]]
  shp <- readOGR(dsn = shppath, layer = lyrname, stringsAsFactors = FALSE)
  
  ##  For every declared year:
  for(y in c(2000,2005,2010,2012)){
    ##  Subset for year:
    subtab <- dtab %>% filter(YEAR == y) %>%
      group_by(GID) %>%
      mutate(RECALL = TP/(TP+FN),
             PRECISION=TP/(TP+FP),
             SPECIFICITY = TN/(TN+FP),
             FSCORE=2*(((TP/(TP+FP))*(TP/(TP+FN)))/((TP/(TP+FP))+(TP/(TP+FN)))))
    
    ##  Merge the subset with the shapefile:
    shp_merge <- merge(shp, subtab, by = "GID")
    
    ensure_dir(paste0(root,prj_root,i,"/ChoroplethShapes/"))
    outlyr <- paste0(i,"_ChoroplethMetrics_",y)
    ##  Output the file for mapping manually in publication:
    writeOGR(shp_merge,
             dsn = paste0(root, prj_root, i,"/ChoroplethShapes/",outlyr,".shp"),
             layer = outlyr, driver = "ESRI Shapefile")
  }
}



####  BINNING DTE AGREEMENT/DISAGREEMENT FOR TABLE CONSTRUCTION  -----------------
##
##  Declare the countries we are interested in:
isolist <- c("NGA", "MDG")#c("NGA","MDG","KHM","VNM","PER", "CRI","NPL","PAK")

##  Comparison name can be either 'ESA' or 'MAUPP':
comparison_name <- "MAUPP"
comp_type <- "rand"

if(comparison_name == "ESA"){
  ##  Create a dataframe to hold all country's binned data:
  all_bin_dt <- data.frame("YEAR" = numeric(length = 0),
                           "BIN.250"= numeric(length = 0),
                           "BIN.500" = numeric(length = 0),
                           "BIN.750" = numeric(length = 0),
                           "BIN.1K" = numeric(length = 0),
                           "BIN.1KMORE" = numeric(length = 0))
}
if(comparison_name == "MAUPP"){
  ##  Create a dataframe to hold all country's binned data:
  all_bin_dt <- data.frame("YEAR" = numeric(length = 0),
                           "CITY" = character(length = 0),
                           "BIN.250"= numeric(length = 0),
                           "BIN.500" = numeric(length = 0),
                           "BIN.750" = numeric(length = 0),
                           "BIN.1K" = numeric(length = 0),
                           "BIN.1KMORE" = numeric(length = 0))
}


##  For every country in the declared isolist:
for(i in isolist){
  ##  Retrieve the annual data.tables paths.
  if(comparison_name == "ESA"){
    ##  Create a main data.table to hold our processed info for the specific country:
    maindt <- data.table()
    
    ##  Get all files that we want to find:
    flist <- Sys.glob(paste0(root, "Research/BSGMGeneral/ValidationOutput/", i,
                             "/",i,"_DTE_extract_",comp_type,"*_ESACCI.rds"))
    
    ##  For every one of those files:
    for(f in flist){
      ##  We are going to process the file, putting the DTE differences into 
      ##  bins before putting the aggregate in a single data.table for the 
      ##  country for producing tables and figures.
      ##
      ##  Retrieve the table year from the file path:
      year <- sub(".*_([0-9]{4})_ESACCI.rds", "\\1", f, perl = TRUE)
      ##  Read in the table:
      intab <- readRDS(f)
      ##  Remove any NAs which might have been extracted:
      intab <- intab[complete.cases(intab),]
      
      ##  Calculate the difference between each observation (ESA) and 
      ##  prediction (BSGM) for this given year's data:
      intab[, DTE.DIFF := OBS.DTE - PRED.DTE]
      
      ##  Bin the values into a grouping variable:
      intab[, DTE.BIN := 0]
      
      ##  +- 250 meters:
      intab[DTE.DIFF <= 250 & DTE.DIFF >= -250, DTE.BIN := 1]
      
      ##  +- 500 meters:
      intab[DTE.DIFF <= 500 & DTE.DIFF > 250 | DTE.DIFF >= -500 & DTE.DIFF < -250,
            DTE.BIN := 2]
      
      ##  +- 750 meters:
      intab[DTE.DIFF <= 750 & DTE.DIFF > 500 | DTE.DIFF >= -750 & DTE.DIFF < -500,
            DTE.BIN := 3]
      
      ##  +- 1000 meters:
      intab[DTE.DIFF <= 1000 & DTE.DIFF > 750 | DTE.DIFF >= -1000 & DTE.DIFF < -750,
            DTE.BIN := 4]
      
      ##  > +- 1000 meters:
      intab[DTE.DIFF > 1000 | DTE.DIFF < -1000, DTE.BIN := 5]
      
      ##  Add a column for the year:
      intab[, YEAR := as.numeric(year)]
      
      ##  Bind the table to the main table:
      maindt <- rbind(maindt, intab)
    }
    
    
    ##  TABLE CONSTRUCTION  ----
    ##
    ##  Need to calculate the number of observations per group:
    maindt[, BIN.COUNT := .N, by = list(DTE.BIN, YEAR)]
    
    ##  Retrieve count per Bin/year and prepare a matrix of the results for 
    ##  table making.
    ##  For every bin level:
    for(y in 2001:2011){
      subdt <- maindt[YEAR== y]
      ##  Retrieve the information:
      foovec <- c(y,
                  subdt[DTE.BIN == 1]$BIN.COUNT[1],
                  mean(subdt[DTE.BIN == 2]$BIN.COUNT, na.rm = TRUE),
                  mean(subdt[DTE.BIN == 3]$BIN.COUNT, na.rm = TRUE),
                  mean(subdt[DTE.BIN == 4]$BIN.COUNT, na.rm = TRUE),
                  mean(subdt[DTE.BIN == 5]$BIN.COUNT, na.rm = TRUE))
      
      ##  Bind it to a main table:
      all_bin_dt <- rbind(all_bin_dt, foovec)
    }
    
    ##  Adjust names:
    names(all_bin_dt) <- c("YEAR","BIN.250","BIN.500",
                           "BIN.750","BIN.1K","BIN.1KMORE")
    
  }
  
  ##  If we are working with the MAUPP data:
  if(comparison_name == "MAUPP"){
    ##  Create a main data.table to hold our processed info for the specific country:
    maindt <- data.table()
    ##  Retrieve all the files:
    flist <- Sys.glob(paste0(root, "Research/BSGMGeneral/ValidationOutput/",i,
                             "/",i,"_DTE_extract_rand_comp*_MAUPP.rds"))
    
    ##  For every one of those files:
    for(f in flist){
      ##  We are going to process the file, putting the DTE differences into 
      ##  bins before putting the aggregate in a single data.table for the 
      ##  country for producing tables and figures.
      ##
      ##  Retrieve the table year from the file path:
      year <- sub(".*_([0-9]{4})_MAUPP.rds", "\\1", f, perl = TRUE)
      
      ##  Retrieve the city:
      city <- sub(".*_rand_comp_(.*)_[0-9]{4}_MAUPP.rds","\\1", f, perl=TRUE)
      
      ##  Read in the table:
      intab <- readRDS(f)
      ##  Remove any NAs which might have been extracted:
      intab <- intab[complete.cases(intab),]
      
      ##  Calculate the difference between each observation (ESA) and 
      ##  prediction (BSGM) for this given year's data:
      intab[, DTE.DIFF := OBS.DTE - PRED.DTE]
      
      ##  Bin the values into a grouping variable:
      intab[, DTE.BIN := 0]
      
      ##  +- 250 meters:
      intab[DTE.DIFF <= 250 & DTE.DIFF >= -250, DTE.BIN := 1]
      
      ##  +- 500 meters:
      intab[DTE.DIFF <= 500 & DTE.DIFF > 250 | DTE.DIFF >= -500 & DTE.DIFF < -250,
            DTE.BIN := 2]
      
      ##  +- 750 meters:
      intab[DTE.DIFF <= 750 & DTE.DIFF > 500 | DTE.DIFF >= -750 & DTE.DIFF < -500,
            DTE.BIN := 3]
      
      ##  +- 1000 meters:
      intab[DTE.DIFF <= 1000 & DTE.DIFF > 750 | DTE.DIFF >= -1000 & DTE.DIFF < -750,
            DTE.BIN := 4]
      
      ##  > +- 1000 meters:
      intab[DTE.DIFF > 1000 | DTE.DIFF < -1000, DTE.BIN := 5]
      
      ##  Add a column for the year:
      intab[, YEAR := as.numeric(year)]
      
      ##  Add a Column for the city:
      intab$CITY <- city
      
      ##  Bind the table to the main table:
      maindt <- rbind(maindt, intab)
    }
    
    
    ##  TABLE CONSTRUCTION  ----
    ##
    ##  Need to calculate the number of observations per group:
    maindt[, BIN.COUNT := .N, by = list(DTE.BIN, YEAR, CITY)]
   
    
    ##  Retrieve count per Bin/year/city and prepare a matrix of the results for 
    ##  table making.
    ##  For every year and city level:
    for(y in c(2005,2010)){
      for(k in unique(maindt$CITY)){
        ##  Subset by year and city:
        subdt <- maindt[YEAR == y & CITY == k]
        ##  Retrieve the information:
        foo <- data.frame("YEAR"= y,
                          "CITY"= k,
                          "BIN.250" = subdt[DTE.BIN == 1]$BIN.COUNT[1],
                          "BIN.500" = subdt[DTE.BIN == 2]$BIN.COUNT[1],
                          "BIN.750" = subdt[DTE.BIN == 3]$BIN.COUNT[1],
                          "BIN.1K" = subdt[DTE.BIN == 4]$BIN.COUNT[1],
                          "BIN.1KMORE" = subdt[DTE.BIN == 5]$BIN.COUNT[1])
        
        ##  Bind it to a main table:
        all_bin_dt <- rbind(all_bin_dt, foo)
        
      }
    }
    ##  Adjust names:
    names(all_bin_dt) <- c("YEAR","CITY","BIN.250","BIN.500",
                           "BIN.750","BIN.1K","BIN.1KMORE")
  }
}
##  Once we have produced for all countries, add the iso to the dataframe in the 
##  order processed:
if(comparison_name=="ESA"){
  all_bin_dt <- data.table(all_bin_dt)
  all_bin_dt$ISO <- rep(isolist, each = 11)
  ##  Get the row sums:
  all_bin_dt[,TOTAL := rowSums(.SD),
             .SDcols=c("BIN.250","BIN.500","BIN.750","BIN.1K","BIN.1KMORE"),
             by = c("ISO","YEAR")]
  ##  Calculate the Percentages of frequencies:
  all_bin_dt[, 
             c("PER.BIN.250","PER.BIN.500",
               "PER.BIN.750","PER.BIN.1K",
               "PER.BIN.1KMORE") := list(BIN.250/TOTAL*100,
                                         BIN.500/TOTAL*100+BIN.250/TOTAL*100,
                                         BIN.750/TOTAL*100+BIN.500/TOTAL*100+BIN.250/TOTAL*100,
                                         BIN.1K/TOTAL*100+BIN.750/TOTAL*100+BIN.500/TOTAL*100+BIN.250/TOTAL*100,
                                         BIN.1KMORE/TOTAL*100+BIN.1K/TOTAL*100+BIN.750/TOTAL*100+BIN.500/TOTAL*100+BIN.250/TOTAL*100)]
  ##  Write out as a CSV for Supplemental Material:
  write.csv(all_bin_dt, 
            file = paste0(root,
                          "Research/BSGMGeneral/ValidationOutput/FrequencyByBin_All_ISOs_2000_2011_ESAComparison.csv"),
            row.names=FALSE)

}
if(comparison_name == "MAUPP"){
  all_bin_dt <- data.table(all_bin_dt)
  ##  Get the row sums:
  all_bin_dt[,TOTAL := rowSums(.SD,na.rm = T),
             .SDcols=c("BIN.250","BIN.500","BIN.750","BIN.1K","BIN.1KMORE"),
             by = c("CITY","YEAR")]
  ##  Calculate the Percentages of frequencies:
  all_bin_dt[, 
             c("PER.BIN.250","PER.BIN.500",
               "PER.BIN.750","PER.BIN.1K",
               "PER.BIN.1KMORE") := list(BIN.250/TOTAL*100,
                                         BIN.500/TOTAL*100+BIN.250/TOTAL*100,
                                         BIN.750/TOTAL*100+BIN.500/TOTAL*100+BIN.250/TOTAL*100,
                                         BIN.1K/TOTAL*100+BIN.750/TOTAL*100+BIN.500/TOTAL*100+BIN.250/TOTAL*100,
                                         BIN.1KMORE/TOTAL*100+BIN.1K/TOTAL*100+BIN.750/TOTAL*100+BIN.500/TOTAL*100+BIN.250/TOTAL*100)]
  ##  Write out as a CSV for Supplemental Material:
  write.csv(all_bin_dt, 
            file = paste0(root,
                          "Research/BSGMGeneral/ValidationOutput/FrequencyByBin_2000_2011_MAUPPComparison.csv"),
            row.names=FALSE)
}


####  CALCULATING ABSOLUTE DISAGREEMENT  ---------------------------------------
isolist <- c()
##  Declare years we want to calculate for:
for(i in isolist){
  for(y in years){
    if(compname == "ESA"){
      outname <- paste0(i,"_DTE_AbsoluteDisagreement_",compname,"_",y)
      ##  Retrieve the  predicted (BSGM) and observed data paths:
      predpath <- paste0(root, "Research/BSGMGeneral/DTEOutput/",i,
                         "_",y,"_DTE_WGS84.tif")
      obspath <- paste0(root, "Research/BSGMGeneral/DTEOutput/",i,
                        ifelse(i=="MDG",
                               paste0("_",y,"ESA"),
                               paste0("_ESA_Binary_190_",y)),
                        "_DTE_WGS84.tif")
      ##  Load them:
      predras <- raster(predpath)
      obsras <- raster(obspath)
      
      ##  Calculate the magnitude difference of every pixel:
      aeras <- abs(predras - obsras)
      
      ##  Save the output:
      writeRaster(aeras,
                  filename = paste0(root,"Research/BSGMGeneral/ValidationOutput/AbsoluteDisagreementRasters/",
                                    outname,".tif"),
                  format = "GTiff",
                  overwrite= TRUE,
                  datatype = "FLT4S",
                  options = c("COMPRESSION = LZW"))
      rm(predras,obsras)
      gc()
    }
    
    if(compname == "MAUPP"){
      ##  Retrieve the  predicted (BSGM) and observed data paths:
      predpath <- paste0(root, "Research/BSGMGeneral/DTEOutput/",i,
                         "_",y,"_DTE_WGS84.tif")
      predras <- raster(predpath)
      obspath <- Sys.glob(paste0(root, "Research/BSGMGeneral/DTEOutput/MAUPP_",i,"*",y,"_DTE_WGS84.tif"))
      for(f in flist){
        ##  Retrieve city name:
        city <- sub()
        outname <- paste0(i, city,"_DTE_AbsoluteDisagreement_",compname,"_",y)
        ##  Load them:
        obsras <- raster(obspath)
        
        cropras <- crop(predras,obsras,snap="near")
        
        ##  Calculate the magnitude difference of every pixel:
        aeras <- abs(predras - obsras)
        
        ##  Save the output:
        writeRaster(aeras,
                    filename = paste0(root,"Research/BSGMGeneral/ValidationOutput/AbsoluteDisagreementRasters/",
                                      outname,".tif"),
                    format = "GTiff",
                    overwrite= TRUE,
                    datatype = "FLT4S",
                    options = c("COMPRESSION = LZW"))
        }
      rm(predras,obsras,cropras)
      gc()
    }
  }
}




#### GUF+ DTE EXTRACTION  ---------------------------------------------------------
##
##  Below Block extracts and calculates metrics for the 2000-2012 model derived 
##  and comparison dataset DTE analyses:
iso_list <- c("EGY")#,"NGA","VNM")
#c("CRI","NPL","PAK","PER","NGA","MDG","VNM","KHM")

##  Declare the output directory:
outdir_user <- paste0(root,"Research/BSGMiGeneral/ValidationOutput/")

## Size for random samples:
sample_size <- 100000

years <-c(2010) #2001:2011
##  NOTE: compare_name 'MAUPP' is only valid for years 2005 and 2010 whereas 
##        'ESACCI' can use values from 2000 to 2012

##  Declare the comparison dataset we want to compare DTE with:
##    NOTE:  the MAUPP option can only be used for iso_list <- c("MDG","NGA"). 
##           ESACCI can be used with all study areas countries. Also, 
##           
##  For every declared country:
for( i in iso_list){
  ##  Print some processing output to the console:
  print(paste0("Processing ", i, "..."))
  print(Sys.time())
  ##  Ensure the output directory exists and if not, create it:
  ensure_dir(paste0(outdir_user,i,"/"))
  
  ##  Get the country Lvl1 raster path and load it:
  regpath <- paste0(root,
                    "Research\\BSGMi\\data\\",i,"_GUF+\\",
                    tolower(i),"_grid_100m_ccidadminl1.tif")
  
  region_ras <- raster(regpath)
  if(file.exists(paste0(outdir_user, i,"/",i,"GUF+_DTE_admin_sample_points.shp"))){
    print("Global indices already exist. Loading...")
    global_points <- readOGR(paste0(outdir_user,i,"/",i,"GUF+_DTE_all_points.shp"),
                             paste0(i,"_DTE_all_points"))
  }
  ##  But if it doesn't exist:
  if(!file.exists(paste0(outdir_user,i,"/",i,"GUF+_DTE_admin_sample_points.shp"))){
    print("Getting Points for Every Pixel...")
    
    glob_ind <- which(!is.na(values(region_ras)))
    
    ##  Change those Cell Indicies to spatial points:
    coords <- xyFromCell(region_ras,
                         glob_ind,
                         spatial = TRUE)
    ##  Turn into Spatial Points Data Frame with the Cell Index (CID) stored for
    ##  reference:
    ##  NOTE: The CID is relative to the Lvl1 subnational unit raster:
    globalshp <- SpatialPointsDataFrame(coords,
                                        data.frame("CID"=glob_ind),
                                        proj4string = CRS(as.character("+proj=longlat +datum=wgs84 +no_defs +ellps=wgs84 +towgs84=0,0,0")))
    
    ##  Memory cleaning
    rm(coords)
    gc()
  }
  
  ##  Declare the path to the transition raster for the period:
  tranpath <- paste0(root, "Research\\BSGMi\\output\\prj_2000-2015_",i,
                     "_GUF+\\tmp\\prj_2000-2015_",i,"_transition.tif")
  ##  Read in the tranisition raster and get the indices of the cells that transitioned:
  tranind <- which(values(raster(tranpath))==1)
  ##  For every defined year:
  for(y in years){
    print(paste0("    Working on year ", y,"..."))
    
    ##  Retrieve the year-specific BSGM-derived DTE raster path:
    ##  NOTE: This requires preprocessing the BSGM annual output using the DTE 
    ##        python script (DTE_v_batch_regex.py) which is dependent on ArcGIS
    ##        but could equally be done in QGIS or any other GIS with distance 
    ##        to and raster math capabilities. ArcGIS was used because the 
    ##        script had been written for a previous project.
    ##  Find a subset of these
    ##  Declare the path to the "predicted" DTE raster:
    predpath <- paste0(root,"Research\\BSGMi\\output\\prj_2000-2015_",i,
                       "_GUF+\\derived\\BSGM_Extentsprj_2000-2015_",i,"_",y,"_DTE.tif")
    ##  Declare the path of the extents of the observed dataset for the year
    obsextpath <- paste0(root,"Research\\BSGMi\\data\\",i,
                         "_GUF+\\",tolower(i),"_grid_100m_GUF+_",y,".tif")
    ##  Read in the observed extents raster and get the indices of the cells 
    ##  that show BS:
    obsind <- which(values(raster(obsextpath))==1)
    
    ##  Subset the points for which we will extract to only those that both 
    ##  transitioned in the period and existed by the year of interest (i.e. the 
    ##  intersection of the observed indices and the transition indices):
    subind <- base::intersect(tranind,obsind)
    
    ##  Save the SPDF as a shapefile for replicability:
    writeOGR(globalshp,
             paste0(outdir_user,i,"/",i,"_DTE_compare_points_",y,".shp"),
             driver = "ESRI Shapefile",
             layer = paste0(i,"_DTE_compare_points"),
             overwrite_layer = TRUE)
    
    
    ##  Otherwise process things as for a country wide sample:
    print("     Processing the global sample...")
    ##  Pull the random sample DTE values from both datsets for comparison:
    count_samp<- dteCompareExtract(predpath,
                                   obspath,
                                   regpath,
                                   samp_points = globalshp)
    count_samp <- count_samp[complete.cases(count_samp),]
    ##  Mark the records which are for the transition cells only:
    count_samp$TRAN <- 0
    count_samp[count_samp$CELLID %in% subind,]$TRAN <- 1
    ##  Save the values to an RDS data object:
    saveRDS(count_samp, paste0(outdir_user,i,"/",i,"GUF+_DTE_extract_compare_",
                               y,".rds"))
    print(Sys.time())
    }
}

indat <- count_samp

df <- indat[,c("OBS.DTE","PRED.DTE")]
L90 <- getLevel(df,0.1)
L75 <- getLevel(df, 0.25)
L50 <- getLevel(df, 0.5)
L25<- getLevel(df, 0.75)

##  Remove the unnecessary dataframe:
rm(df)
gc()


##  Create the 1:1 plot with no overlay of other data:
ggplot(indat, aes(x = OBS.DTE, y= PRED.DTE))+
  geom_point(alpha=0.075) + 
  theme_bw() + 
  stat_smooth(method="lm", fill=NA, colour="red", alpha=0.6) + 
  geom_abline(slope=1, intercept=0, linetype="dotted", 
              colour="black", size =1, show.legend = TRUE) +
  labs(x=paste0("Observed DTE (meters)"), y="BSGM DTE (meters)")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlim(-500,250)+
  ylim(-250,500)+
  geom_path(data = L90, aes(x=x,y=y,colour = "1"),size = 1,
            show.legend = TRUE)+
  geom_path(data = L75, aes(x=x,y=y,colour = "2"),size = 1,
            show.legend = TRUE)+
  geom_path(data = L50, aes(x=x,y=y,colour = "3"),size = 1,
            show.legend = TRUE)+
  geom_path(data = L25, aes(x=x,y=y,colour = "4"),size = 1,
            show.legend = TRUE)+
  scale_color_manual(values = rev(cols),
                     labels = c("0.90","0.75","0.50","0.25"))+
  guides(colour=guide_legend(title="Point Density\nPercentile"))

ggplot(indat, aes(x = {{log(abs(OBS.DTE)+1)+log(abs(PRED.DTE)+1)}/2}, 
                  y= {log(abs(OBS.DTE)+1)-log(abs(PRED.DTE)+1)}))+
  geom_point(alpha=0.1) + 
  geom_hline(yintercept = 1.96*sqrt(var({log(abs(indat$OBS.DTE)+1)-log(abs(indat$PRED.DTE)+1)})))+
  geom_hline(yintercept = -1.96*sqrt(var({log(abs(indat$OBS.DTE)+1)-log(abs(indat$PRED.DTE)+1)})))+
  theme_bw() + 
  labs(x=paste0("Average of\nObserved & Predicted\nDTE (meters)"),
       y="Difference\n(Observed - Predicted")

ggplot(indat, aes(x = {{log(abs(OBS.DTE)+1,base = 2)+log(abs(PRED.DTE)+1,base = 2)}/2}, 
                  y= {log(abs(OBS.DTE)+1,base = 2)-log(abs(PRED.DTE)+1,base = 2)}))+
  geom_point(alpha=0.1) + 
  geom_hline(yintercept = 1.96*sqrt(var({log(abs(indat$OBS.DTE)+1,base = 2)-log(abs(indat$PRED.DTE)+1, base = 2)},na.rm=T)))+
  geom_hline(yintercept = -1.96*sqrt(var({log(abs(indat$OBS.DTE)+1,base = 2)-log(abs(indat$PRED.DTE)+1, base = 2)},na.rm=T)))+
  theme_bw() + 
  labs(x=paste0("Average of\nObserved & Predicted\nDTE (meters)"), 
       y="Difference\n(Observed - Predicted")
