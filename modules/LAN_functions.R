Download_LAN <- function(){
  ##  If we have logging turned on:
  if (bsgm.DologDEBUG) 
    d.quiet=FALSE
  else
    d.quiet=TRUE
  
  loginfo("Download LAN files from WP FTP if they dont exist already ")
  ##  For every country declared:
  for ( i in bsgm.input.countries ) {
    ##  Retrieve a list of the available covariates:
    df.ListCountryCovariates <- wpgpListCountryCovariates(ISO3=i, 
                                                          username = bsgm.ftp.username, 
                                                          password = bsgm.ftp.password, 
                                                          detailed = T
    )
    
    ##  Pull the last year off the year sequence because we are going to 
    ##  ultimately lag the LAN data and don't need the last year for 
    ##  interpolative purposes and we don't have LAN data for the latter years 
    ##  of extrapolation:
    years <- year_seq[-length(year_seq)]
    ##  Construct a list of the LAN files of interest based upon the year 
    ##  sequence years less than the first year we do not have LAN data for:
    bsgm.LAN.sel <- as.character(bsgm.LAN.cvr.names$NAME[bsgm.LAN.cvr.names$YEAR %in%(years[years<2017])])
    
    ##  For every covariate the user declared to be of interest in the input.R 
    ##  file:
    for ( j in bsgm.LAN.sel) {
      ##  Retrieve only the covariate which name matches the current covariate 
      ##  of iteration:
      df.filtered <- df.ListCountryCovariates[df.ListCountryCovariates$CvtName == j,]
      
      ##  Declare the folder where we will store it
      cvr.folder <-  paste0(root_path,"/","data/", i,"/LAN/")
      ##  Declare the name of the covariate raster we will save it as:
      cvr.rst.name <-  paste0(df.filtered$RstName,".tif")
      ##  Retrieve the covariate name:
      cvr.name <-  df.filtered$CvtName
      ##  Retrieve the covariate description:
      cvr.description <-  df.filtered$Description
      
      ##  I that file does not already exist locally:
      if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
        logdebug( paste0('Start downloading Covariate: ',j," for ",i))
        ##  Download it:
        df.download <- wpgpGetCountryCovariate(df.user=NULL,
                                               ISO3=i,
                                               covariate=j,
                                               destDir=cvr.folder,
                                               username=bsgm.ftp.username,
                                               password=bsgm.ftp.password,
                                               quiet=d.quiet)    
      }else{
        ##  Otherwise note it and pass:
        logdebug( paste0(' LAN data: ',j," for ",i,
                         " already exists in ",cvr.folder))
      }
      
      ##  Assemble a LANs list object which will store the associated 
      ##  metadata and path for each LAN data:
      LANs[[i]][[j]] <- list(
        dataset_folder      =  cvr.folder,
        dataset_filename    =  cvr.rst.name,
        dataset_description =  cvr.description,
        dataset_summary     =  "",
        dataset_country     = i,
        dataset_class       = j
      )
      LANs[[i]][[j]][["path"]] <- paste0(cvr.folder,"/",cvr.rst.name)
    }
  }  
  
  ##  Save the LANs object as a RData and a JSON file:
  save(LANs, file=paste(bsgm.output.path.countries.tmp,
                              bsgm.countries.LAN.Rdata, sep=""))
  LANs_json <- toJSON(LANs, pretty = TRUE)
  cat(LANs_json, file=paste(bsgm.output.path.countries.tmp,
                                  bsgm.countries.LAN.json , sep=""))   
  
  ##  Return the covariates object:
  return(LANs) 
}




create_LAN_stack <- function(){
  ##  Create a raster stack from all LAN covariates:
  ##  Create an empty list to hold the rasters:
  list_lan_ras <- list()
  
  ##  For every raster name in the list of names used in the RF:
  ##  WARNING:  THIS IS HARD CODED FOR SINGLE COUNTRY USE CURRENTLY.
  for (i in 1:length(names(lan.covariates[[1]]))){
    ##  Retrieve the name:
    var_name <- names(lan.covariates[[1]])[i]
    ##  Assign that raster path to the varname:
    assign(var_name, raster( lan.covariates[[1]][[var_name]]$path ), envir = .GlobalEnv)
    ##  Put the raster path in the list:
    list_lan_ras[[i]] <-  get(var_name, envir = .GlobalEnv)
  }
  
  ##  Stack all the rasters we just retrieved:
  lan_ras_stack <- stack(list_lan_ras)
  
  ##  Return the raster stack object:
  return(lan_ras_stack)
}


lag_LAN <- function(){
  ##  Takes the LAN raster stack and temporally lags it by the prior year's 
  ##  brightness:
  lagged_LAN <- subset(LAN_stack, 2:nlayers(LAN_stack))-
    subset(LAN_stack, 1:(nlayers(LAN_stack)-1))
  if(nlayers(lagged_LAN)==1){lagged_LAN <- stack(lagged_LAN)}
  return(lagged_LAN)
}