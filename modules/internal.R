##  THIS FILE CONTAINS INTERNAL FUNCTIONS WHICH ARE USED IN THE BSGM SCRIPT
##  TITLE:  INTERNAL .R
##  AUTHOR:  JEREMIAH J. NIEVES & MAKSYM BONDARENKO
##  VERSION: 1.0
##  LAST UPDATE: 2017-05-27
##  NOTES:  SOME OF THE FUNCTIONS WITHIN HERE ARE BSGM SPECIFIC FUNCTIONS, 
##          OTHERS ARE GENERAL, AND OTHERS ARE RELATED TO THE INTERACTION WITH
##          THE WORLDPOP DATABASE. FUNCTIONS RELATED TO THE LATTER TWO WHICH 
##          WERE WRITTEN IN PART OR IN ENTIRITY BY MAKSYM BONDARENKO ARE NOTED.

##  -----------------------------------------------------------------------  ##
ensure_dir <- function(d){
  ##  Function for ensuring that a directory exists and creating it if it does 
  ##  not; returns the path of the input path
  if(!dir.exists(d)){
    dir.create(d)
  }
  return(d)
}

pkgLoad <- function(x){
  #  This function loads a package and installs it if it is not already present
  #  Written by Maksym Bondarenko
  if(!require(x, character.only = TRUE)){
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE))stop("Package not found")
  }
}

##  -----------------------------------------------------------------------  ##
logConscole <- function(x){
  ##  This is a logging function which not only logs output messages in a text 
  ##  file, but also prints the message to the console if kDologConscole 
  ##  is TRUE.
  ##  Written by Maksym Bondarenko
  if(kDologConscole){
    cat("\n")
    cat("\n")
    cat(sprintf("%s", "+", 1:50))
    cat("\n")
    print(x)
    cat(sprintf("%s", "+", 1:50))
    cat("\n")
  }
}

##  -----------------------------------------------------------------------  ##
logr <- function(log_inidicator, 
                 log_file_path, 
                 message_string,
                 separator = "\n",
                 ...){
  ## "Boy, the Brits sure do love their LOGR."
  ##  Logging function I made which operates similarly to the logging function
  ##  Maksym made. Allows greater flexibility of output, inputs, and 
  ##  formatting.
  if(logindicator){
    cat(message_string,
        file = log_file_path,
        sep = separator,
        append = TRUE)
  }else{
    print(message_string)
    flush.console()
  }
}

##  -----------------------------------------------------------------------  ##
checkNoDataValueInRaster <- function(fileNamePath = NULL,
                                     NoDataValue = 9999){
  ##  This function checks for the existence of NO DATA in the raster's values.
  ##  Originally written by Maksym Bondarenko.
  ##  -------------------------
  
  ##  If the file path was not defined:
  if(is.null(fileNamePath)){
    stop(paste0("ERROR: Please specify a file path for the raster of interest:"))
  }
  
  ##  If the defined file does not exist:
  if(!file.exists(fileNamePath)){
    stop(paste0("ERROR: File ", fileNamePath, " does not exist!"))
  }
  
  ##  Get the raster info:
  rasterGdalInfo <- gdalinfo(fileNamePath,
                             "mm",
                             verbose = FALSE,
                             raw_output = TRUE)
  ##  Retrieve the No Data value as defined in the information:
  nindex <- grep('\\bNoData\\b', rasterGdalInfo)
  ##  Retrieve and split the relevant information:
  OutputStrSplitS <- strsplit( rasterGdalInfo[ nindex[1] ], "=" )
  
  ##  Create message indicating the NoData value retrieved:
  message(paste0("Raster ", 
                 fileNamePath, 
                 " has a NoData value of ",
                 OutputStrSplitS[[1]][2]))
  
  ##  If we are getting an obscene number of hits back for cells which have 
  ##  NoData values, give a warning message:
  # if(as.numeric(OutputStrSplitS[[1]][2] > -10000000)){
  #   stop(paste0("WARNING: Raster ",
  #               fileNamePath,
  #               " likely has the incorrect NoData value specified.",
  #               "Currently specified as: ",
  #               OutputStrSplitS[[1]][2]))  
  # }
}

##  -----------------------------------------------------------------------  ##
specify_decimal <- function(x,k){
  ##  This function formats a numeric to a specific decimal precision.
  ##  Written by Maksym Bondarenko
  format(round(x,k), nsmall = k)}

##  -----------------------------------------------------------------------  ##
checkExtentRaster <- function(r1, r2){
  ##  This function checks the extents of any two given rasters and if their 
  ##  extents do not match, the function rectifies them to match.
  ##  Written by Maksym Bondarenko.
  xmin.r1 <- specify_decimal( bbox(r1)[1,1], 5 )
  xmax.r1 <- specify_decimal( bbox(r1)[1,2], 5 )
  ymin.r1 <- specify_decimal( bbox(r1)[2,1], 5 )
  ymax.r1 <- specify_decimal( bbox(r1)[2,2], 5 )
  
  xmin.r2 <- specify_decimal( bbox(r2)[1,1], 5 )
  xmax.r2 <- specify_decimal( bbox(r2)[1,2], 5 )
  ymin.r2 <- specify_decimal( bbox(r2)[2,1], 5 )
  ymax.r2 <- specify_decimal( bbox(r2)[2,2], 5 )
  
  if( (xmin.r1) != (xmin.r2) | (xmax.r1) != (xmax.r2) | (ymin.r1) != (ymin.r2) | (ymax.r1) != (ymax.r2)){
    return(FALSE)
  }else{return(TRUE)}
}

##  -----------------------------------------------------------------------  ##
changeExtentRaster <- function(rFrom, rToPath){
  ##  This function changes the extent of two rasters.
  ##  Written by Maksym Bondarenko.
  ##  Retrieve the initial extents:
  xmin <- bbox(rFrom)[1,1]
  xmax <- bbox(rFrom)[1,2]
  ymin <- bbox(rFrom)[2,1]
  ymax <- bbox(rFrom)[2,2]
  
  ##  Retrieve the output file path information:
  rFileName <- basename(rToPath)
  rPath <- dirname(rToPath)
  
  ##  Use gdal warp to change the extents:
  system(paste0("gdalwarp -te ",
                ' ',xmin,
                ' ',ymin,
                ' ',xmax,
                ' ',ymax,
                ' ',rToPath,' ',
                paste0(rPath,"/","tmp_",rFileName)),
         ignore.stdout = FALSE,
         ignore.stderr = FALSE)
  
  ##  If the file already exists:
  if(file.exists(rToPath)){unlink(rToPath, recursive = TRUE, force = FALSE)}
  file.rename(from = paste0(rPath,"/","tmp_",rFileName), to = rToPath)
}

##  -----------------------------------------------------------------------  ##
CreateSubFolderForPrj <- function(){
  ##  This function creates subfolders within "/Data/" for each country and 
  ##  within "/output/" for each country and the combined countries. Returned 
  ##  is a list of the output and data subfolders which were created.
  ##  Orignally written by Maksym Bondarenko; modified for BSGM.
  ##  Define the ouput tag:
  countries_tag_output <- paste0("prj_",input_rf_data$data$year0,"_",input_rf_data$data$year1)
  
  ##  For every covariate given:
  for( i in levels(rd.cvr.factors.iso3) ){
    ##  Data SubDirectory
    ##    Define the sub directory path:
    subDir.country <- paste0(root_path,"/data/",i)
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    
    ##  Temporary Output Subdirectory
    ##    Define the sub directory path:
    subDir.country.output.tmp <- paste0(root_path,"/output/",i,"/tmp/")
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    
    ##  Built Settlement (BS) Est. Output Subdirectory (.CSV):
    ##    Define the sub directory path:
    subDir.country.output.tmp <- paste0(root_path,"/output/",i,"/bsest/csv/")
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    
    ##  Built Settlement (BS) Est. Output Subdirectory (.RData):
    ##    Define the sub directory path:
    subDir.country.output.tmp <- paste0(root_path,"/output/",i,"/bsest/RData/")
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    ##  Set the next iterations tag output:
    countries_tag_output <- paste0(countries_tag_output, i)
  }
  output.path.countries <- paste0(root_path,"/output",countries_tag_output,"/")
  output.path.countries.tmp <- paste0(root_path, "/output/",countries_tag_output,"/tmp/")
  
  ##  If the temporary path does not exist create it:
  if(!file.exists(output.path.countries.tmp)){
    dir.create(output.path.countries.tmp, recursive = TRUE, showWarnings = FALSE)
  }
  
  ##  If only one country was defined in the input parameters, i.e. we are 
  ##  working with only one country, then the path to "/data/" will be the ISO
  ##  of the country, otherwise that path will be a new folder created upon the
  ##  ISOs of the multiple countries which were defined in the input parameters
  ##  e.g. "prj_2000_2012_PER_COL", and that folder will contian the merged 
  ##  rasters of the countries to be modeled.
  ##  If only one country is specified:
  if(length(rd.cvr.factors.iso3) == 1){
    data.path.countries <- paste0(root_path, 
                                  "/data/", 
                                  as.character(rd.cvr.factors.iso3[[1]]),
                                  "/")
    
    ##  If that folder does not already exist, create it:
    if(!file.exists(data.path.countries)){
      dir.create(data.path.countries,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
  }else{
      data.path.countries <- paste0(root_path,
                                    "/data/",
                                    countries_tag_output,
                                    "/")
  }
  if(!file.exists(data.path.countries)){
    dir.create(data.path.countries,
               recursive = TRUE,
               showWarnings = FALSE)
  }
  ##  Return a list of the output paths:
  return(list(output = output.path.countries, 
              data = data.path.countries))
}

##  -----------------------------------------------------------------------  ##
DownloadCovariates <- function(){
  ##  This function loads covariates from the WorldPop DataBase (DB) to the 
  ##  "/Data/" folder.
  ##  Written by Maksym Bondarenko
  ##  For every covariate given in the input parameters:
  for( i in 1:nrow(rd.cvr)){
    ##  Retrieve the current row information:
    current.row <- rd.cvr[i,]
    current.row.iso3 <- as.character( current.row$iso3 )
    current.row.folder <- as.character( current.row$folder )
    current.row.dst_name <- as.character( current.row$dst_name )
    current.row.description <- as.character( current.row$description )
    current.row.summary <- as.character( current.row$summary )
    current.row.class <- as.character( current.row$class )
    
    ##  Define the folder where the covariates should be:
    subDir.covariates <- paste0(root_path,
                                "/data/",
                                current.row.iso3,
                                "/",
                                current.row.folder)
    
    ##  If it doesn't exist, create it:
    if(file.exists(subDir.covariates)){
      dir.create(subDir.covariates,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
    
    ##  Define path of the covariate rasters within the folder:
    subFile.covariates <- paste0(subDir.covariates,"/",
                                 current.row.dst_name,".tif")
    
    ##  If those covariate files are not present locally, download them:
    if(!file.exists(subFile.covariates)){
      message(paste0("     Downloading ", 
                     current.row.dst_name,
                     " to ", 
                     current.row.iso3, 
                     " ", 
                     subDir.covariates))
      
      ##  Actully perform the download:
      getRasterWPDB(schema = paste0(wpdb.prefix.schema,
                                    tolower(current.row.iso3)),
                    table = current.row.dst_name,
                    fileName = subFile.covariates,
                    user = wpdb.user,
                    password = wpdb.password)
    }else{
      message(paste0("     Covariate ",
                     current.row.dst_name,
                     " already exists in ",
                     subDir.covariates))
    }
    ##  Record the current rows meta information:
    covariates[[current.row.iso3]][[current.row.dst_name]] <- list(
      dataset_folder = current.row.folder,
      dataset_name = paste0(current.row.dst_name, ".tif"),
      dataset_description = current.row.description,
      dataset_summary = current.row.summary,
      dataset_country = current.row.iso3,
      dataset_class = current.row.class
    )
    ##  Define the path:
    covariates[[current.row.iso3]][[current.row.dst_name]][["path"]] <- subFile.covariates
  }
  ##  Save the covariates object:
  save(covariates, file = paste0(output.path.countries.tmp,
                                 "covariates.RData"))
  ##  Convert it to JSON too while we're at it:
  covariates_json <- toJSON(covariates, pretty = TRUE)
  cat(covariates_json, file = paste0(output.path.countries.tmp,
                                       "covariates.json"))
    
  ##  Retrieve the admin ID raster
  ##    For every country given in the input parameters:
  for( i in levels(rd.cvr.factors.iso3)){
      ##  Define the 'census' directory:
      Dir.Census <- paste0(root_path,"/data/",i,"/CensusDerived/")
      
      ##  If the file does not exist:
      if(!file.exist(Dir.Census)){
        ##  Create it:
        dir.create(Dir.Census,
                   recursive = TRUE,
                   showWarnings = FALSE)
      }
      zonal_raster_path <- paste0(Dir.Census, "census_zones.tif")
      
      if(!file.exists(zonal_raster_path)){
        logConscole("Downloading zonal raster from database. census_zones.tif has ADMINID.")
        
        getRasterWPDB(schema = paste0(wpdb.prefix.schema,
                                      tolower(i)),
                      table = paste0(tolower(i),"adminid"),
                      fileName = zonal_raster_path,
                      user = wpdb.user,
                      password = wpdb.password)
      }
  }
  ##  Return the covariates object:
  return(covariates)
}

##  -----------------------------------------------------------------------  ##
get.covariates.var.names <- function(){
  ##  This function gets the covariate names.
  ##  Written by MAksym Bondarenko.
  ##  Create an empty vector to store things in:
  covariates.var.names <- c()
  
  ##  For every covariate item:
  for( icvritm in 1:length( covariates[[1]] ) ){
    ##  Retrieve the name of the item:
    covariate <- covariates[[1]][[icvritm]]
    var_name_class <- covariate$dataset_class
    covariates.var.names <- c(covariates.var.names, var_name_class)
  }
  
  ##  Sort the names:
  sort(covariates.var.names)
  
  ##  Return the sorted names:
  return(covariates.var.names)
}

##  -----------------------------------------------------------------------  ##
get.df.for.merging.rst.cvr <- function(country_factor, covariates_cntrys_obj){
  ##  This function assists in handling multiple countries by returning a
  ##  dataframe containing the covariates and their corresponding raster paths
  ##  for every covariate of every input country. Sepcifically, the dataframe
  ##  produced by this function assists in merging the rasters.
  ##  Written by Maksym Bondarenko.
  ##  Declare an empty dataframe and adminid path:
  emptyDfclassPath <- data.frame(class = character(), path = character())
  adminIdPath <- ""
  
  
  ##  Begin: Class - Path Retrieval  --------------------
  ##    This section of the function takes the covariates, of all countries 
  ##    being processed, one-by-one and retrieves the meta/path info which is 
  ##    then placed in one of two dataframes; 
  ##      1) holding the class of covariate and the path
  ##      2) holding the class of covariate, the folder it is in, and the 
  ##         summary information
  ##
  ##  Establish a counter:
  icount <- 0
  ##  For every country given:
  for( icountry in levels(country_factor)){
    ##  Increase the counter by 1:
    icount <- {icount+1}
    ##  Create an temporary dataframe:
    emptyDfclassPathTmp <- data.frame(class = character(), path = character())
    ##  Create an empty data frame which will contain the folders:
    emptyDfclassFolder <- data.frame(class = character(),
                                     folder = character(),
                                     summary = character())

    ##  For every covariate in that country's repetoire, as given in the 
    ##  covariate objects holding country specific covariate records:
    for( i in 1:length(covariates_cntrys_obj[[icountry]])){
      ##  Retrieve information about that covariate specifically:
      covariate <- covariates_cntrys_obj[[icountry]][[i]]
      dataset_name <- covariate$dataset_name
      dataset_path <- covariate$path
      dataset_folder <- covariate$dataset_folder
      dataset_class <- covariate$dataset_class
      dataset_summary <- covariate$dataset_summary
      
      ##  Add the info to relevant temporary dataframes:
      emptyDfclassPathTmp <- rbind(emptyDfclassPathTmp,
                                   data.frame(class=dataset_class,
                                              path=dataset_path))
      emptyDfclassFolder <- rbind(emptyDfclassFolder, data.frame(class=dataset_class,
                                                                 folder=dataset_folder,
                                                                 summary=dataset_summary))
    }
    ##  If this was the first iteration:
    if(icount==1){
      ##  Assign the df:
      emptyDfclassPath <- emptyDfclassPathTmp
    }else{
      ##  Otherwise merge with the previous info by class:
      emptyDfclassPath <- merge(as.data.frame(emptyDfclassPath),
                                as.data.frame(emptyDfclassPathTmp),
                                by="class",
                                sort = FALSE)
    }
    
    ##  Save the path to the admin zone raster within a single character string
    ##  with paths to different country's raster separated by a single space:
    adminIdPath <- paste(adminIdPath,
                         paste0(root_path,"/data/",icountry,"Census/Derived/census_zones.tif"),
                         spr = " ")
  }
  ##  Begin:  Merging Covariate Paths Into Single Path for Merging  -----
  ##    This section of the function takes the two previous created 
  ##    dataframes and merges the paths of the individual, country specific
  ##    covariates into a single character string of multiple paths, much like
  ##    was done for the admin raster paths above, representing a single class
  ##    of covariate.
  ##
  Dfclass <- data.frame(class = character(), folder = character(),
                        summary = character(), path = character())
  
  ##  For every row, i.e. country specific covariate, we put in the previous df:
  for(i in 1:nrow(emptyDfclassFolder)){
    ##  Start empty string for path:
    pat <- ""
    ##  Retrieve a df subset for a single class:
    ds <- emptyDfclassPath[emptyDfclassPath$class == as.character(emptyDfclassFolder$class[[i]]),]
    
    ##  For every one of the columns in that subset:
    for(ids in names(ds)){
      ##  If the name isn't "class":
      if(ids!="class"){
        ##  Create a single character holding the paths of each separated by a
        ##  space:
        pat <- paste(pat,
                     as.character(ds[[ids]]),
                     spr = " ")
      }
    }
    
    ##  Add to the df as a new row with pertinent info:
    Dfclass <- rbind(Dfclass,
                     data.frame(class=as.character(emptyDfclassFolder$class[[i]]),
                                folder=as.character(emptyDfclassFolder$folder[[i]]),
                                summary=as.character(emptyDfclassFolder$summary[[i]]),
                                path=pat))
  }
  ##  Add the admin raster paths as an entry to that df as well:
  Dfclass <- rbind(Dfclass,
                   data.frame(class = "AdminId", 
                              folder = "Census/Derived",
                              summary = "AdminId",
                              path = adminIdPath))
  
  ##  Return the final dataframe:
  return(Dfclass)
}

##  -----------------------------------------------------------------------  ##
merging.rst.cvr.multy.country <- function(country_factor, 
                                          covariates_cntrys_obj,
                                          mode="single"){
  ##  This function actually carries out the merging of the covariates for 
  ##  multiple countries. By setting mode to "cluster", the function will carry
  ##  out the merging in parallel to speed up processing.
  if( mode == "cluster"){
    pkgLoad('doSNOW')
    
    ##  Create the dataframe needed to assist with merging:
    df.for.merging <- get.df.for.merging.rst.cvr(country_factor,
                                                 covariates_cntrys_obj)
    
    ##  Initialize the cluster:
    cl <- makeSOCKcluster(cluster_workers)
    registerDoSNOW(cl)
    
    ##  Load the required packages, functions, and variables on the cores:
    #clusterExport( cl, library(randomForest) )
    clusterExport( cl, "data.path.countries" )
    clusterExport( cl, "gdal_merge_path" )
    #clusterExport( cl, df.for.merging)
    
    logConscole("Start merging rasters in a stack for RF...")
    
    ##  Start a progress bar:
    pb <- txtProgressBar(max = nrow(df.for.merging), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    ##  Carry out the parallel processing:
    r <- foreach(i = 1:nrow(df.for.merging), .options.snow = opts) %dopar%
      {##  Declare needed paths:
      fileOutput <- paste0( as.character( df.for.merging[i, "class"] ), ".tif" )
      filePathOutput <- paste0( data.path.countries, as.character(df.for.merging[i, "folder"]), "/")
    
      ##  If file does not exist create it:
      if(!file.exists(filePathOutput)){dir.create(filePathOutput, 
                                                  recursive = TRUE,
                                                  showWarnings = FALSE)}
      ##  Carryout the merging:
      print(
        system(
          paste0(gdal_merge_path,
                 ' -a_nodata -99999 -of GTiff ',
                 paste0('-co COMPRESS=LZW '),
                 paste0('-co BIGTIFF=YES '),
                 paste0('-o ', filePathOutput, fileOutput, ' '),
                 as.character( df.for.merging[i,"path"] ) ),
          ignore.stdout = FALSE,
          ignore.stderr = FALSE
          )
        )
      }
    ##  Close the progress bar and stop the cluster:
    close(pb)
    stopCluster(cl)
  }else{
    ##  If the mode is 'single':
    df.for.merging <- get.df.for.merging.rst.cvr(country_factor,
                                                 covariates_cntrys_obj)
    logConscole("Start merging rasters in a stack for RF...")
    
    for(i in 1:nrow(df.for.merging)){
      fileOutput <- paste0(as.character(df.for.merging[i,"class"]),".tif")
      filePathOutput <- paste0(data.path.countries, 
                               as.character(df.for.merging[i,"folder"]),"/")
      
      if(!file.exists(filePathOutput)){dir.create(filePathOutput,
                                                  recursive = TRUE,
                                                  showWarnings = FALSE)}
      
      print(paste0("Merging ", as.character(df.for.merging[i,"path"])))
      print(paste0("To ", filePathOutput, fileOutput))
      print("-------------------------------------------------------------")
      
      ##  Carryout the merging:
      system(
        paste0(gdal_merge_path,
               '-ot Float64 -a_nodata -99999 -of GTiff ',
               paste0('-co COMPRESS=LZW '),
               paste0('-co BIGTIFF=YES '),
               paste0('-o ', filePathOutput, fileOutput, ' '),
               as.character( df.for.merging[i,"path"] ) ),
        ignore.stdout = FALSE,
        ignore.stderr = FALSE
      )
    }
  }
}

##  -----------------------------------------------------------------------  ##
create.covariates.list.for.RF <- function(country_factor,
                                          covariates_cntrys_obj){
  ##  This function creates a list of the covariates used in the RF script
  ##  which matches the expected 'one name for one covariate' schema in the RF,
  ##  i.e. this removes any country specific prefix.
  ##  Written by Maksym Bondarenko.
  ##  Create an empty list to hold the covariates:
  covariates.new <- list()
  
  ##  Retrieve the merging dataframe:
  df.for.merging <- get.df.for.merging.rst.cvr(country_factor,
                                               covariates_cntrys_obj)
  
  logConscole("Creating a new covariate list for the RF...")
  
  for(i in 1:nrow(df.for.merging)){
    class.t <- as.character(df.for.merging[i,"class"])
    folder.t <- as.character(df.for.merging[i,"folder"])
    summary.t <- as.character(df.for.merging[i,"summary"])
    
    ##  Remove AdminID and Watermask from the covariates list:
    if((class.t != 'AdminId') | (class.t!='Watermask')){
      if(length(country_factor > 1)){
        filePathOutput <- paste0(data.path.countries, folder.t,"/")
        fileOutput <- paste0(as.character(df.for.merging[i,"class"]),".tif")
        fileNamePath <- paste0(filePathOutput,fileOutput)
      }else{
        fileNamePath <- as.character(df.for.merging[i,"path"])
      }
      
      covariates.new[[class.t]] <- list(
        dataset_folder = folder.t,
        dataset_name = class.t,
        dataset_description = "",
        dataset_summary = summary.t,
        path = fileNamePath
      )
    }
  }
  return(covariates.new)
}