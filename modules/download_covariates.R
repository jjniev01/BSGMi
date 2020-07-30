##  Written by Maksym Bondarenko and Jeremiah J. Nieves
##  Adapted and commented by Jeremiah J. Nieves
Download_Covariates <- function(){
  ##  If we have logging turned on:
  if (bsgm.DologDEBUG) 
    d.quiet=FALSE
  else
    d.quiet=TRUE
  
  loginfo("Download Covariates files from WP FTP if they dont exist already ")
  ##  For every country declared:
  for ( i in bsgm.input.countries ) {
    ##  Retrieve a list of the available covariates:
    df.ListCountryCovariates <- wpgpListCountryCovariates(ISO3=i, 
                                                          username = bsgm.ftp.username, 
                                                          password = bsgm.ftp.password, 
                                                          detailed = T
                                                          )
    
    ##  For every covariate the user declared to be of interest in the input.R 
    ##  file:
    for ( j in bsgm.input.cvr ) {
      ##  Retrieve only the covariate which name matches the current covariate 
      ##  of iteration:
      df.filtered <- df.ListCountryCovariates[df.ListCountryCovariates$CvtName == j,]
      
      ##  Declare the folder where we will store it
      cvr.folder <-  paste0(root_path,"/","data/", i)
      ##  Declare the name of the covariate raster we will save it as:
      cvr.rst.name <-  paste0(df.filtered$RstName,".tif")
      ##  Retrieve the covariate name:
      cvr.name <-  df.filtered$CvtName
      ##  Retrieve the covariate description:
      cvr.description <-  df.filtered$Description
      
      ##  If that file does not already exist locally:
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
        logdebug( paste0(' Covariate: ',j," for ",i,
                         " already exists in ",cvr.folder))
      }
      
      ##  Assemble a covariates list object which will store the associated 
      ##  metadata and path for each covariate:
      covariates[[i]][[j]] <- list(
        dataset_folder      =  cvr.folder,
        dataset_filename    =  cvr.rst.name,
        dataset_description =  cvr.description,
        dataset_summary     =  "mean",
        dataset_country     = i,
        dataset_class       = j
      )
      covariates[[i]][[j]][["path"]] <- paste0(cvr.folder,"/",cvr.rst.name)
    }

    ##  Download water mask and zonal (L1) and the built-settlement extents at 
    ##  t0:
    for ( k in c(bsgm.water.mask, bsgm.ccidadminl1) ) {
      cvr.folder <-  paste0(root_path,"/","data/", as.character(i))
      cvr.rst.name <-  paste0(tolower(i),"_grid_100m_",as.character(k),".tif")
      cvr.name <-  as.character(k)
      cvr.description <-  as.character(k)
      
      if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
        
        logdebug( paste0('Start downloading Covariate: ',k," for ",i))
        
        wpgpGetCountryCovariate(df.user=NULL,
                                ISO3=i,
                                covariate=as.character(k),
                                destDir=cvr.folder,
                                username=bsgm.ftp.username,
                                password=bsgm.ftp.password,
                                quiet=d.quiet)
        
        covariates[[i]][[k]] <- list(
          dataset_folder      =  cvr.folder,
          dataset_filename    =  cvr.rst.name,
          dataset_description =  as.character(k),
          dataset_summary     =  as.character(k),
          dataset_country     =  as.character(i),
          dataset_class       =  as.character(k)
        )     
        
        covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,
                                                 "/",paste0(tolower(i),
                                                            "_grid_100m_",
                                                            as.character(k),
                                                            ".tif"))        
        
      } else{
        
        logdebug( paste0(' Covariate: ',k," for ",i,
                         " alsready exists in ",cvr.folder))
        
        covariates[[i]][[k]] <- list(
          dataset_folder      =  cvr.folder,
          dataset_filename    =  cvr.rst.name,
          dataset_description =  as.character(k),
          dataset_country     =  as.character(i),
          dataset_class       =  as.character(k)
        )            
        covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,"/",
                                                 paste0(tolower(i),
                                                        "_grid_100m_",
                                                        as.character(k),".tif"))        
      } 
    }  # End of downloading water mask, L1, and t0 extents
    
    ##  If we are interpolating, dowload the t1 BS extents raster:
    
    # k <- bsgm.t1.extents
    # cvr.folder <-  paste0(root_path,"/","data/", as.character(i))
    # cvr.rst.name <-  paste0(tolower(i),"_grid_100m_",as.character(k),".tif")
    # cvr.name <-  as.character(k)
    # cvr.description <-  as.character(k)
    # 
    # if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
    #   
    #   logdebug( paste0('Start downloading Covariate: ',k," for ",i))
    #   
    #   wpgpGetCountryCovariate(df.user=NULL,
    #                           ISO3=i,
    #                           covariate=as.character(k),
    #                           destDir=cvr.folder,
    #                           username=bsgm.ftp.username,
    #                           password=bsgm.ftp.password,
    #                           quiet=d.quiet)
    #   
    #   covariates[[i]][[k]] <- list(
    #     dataset_folder      =  cvr.folder,
    #     dataset_filename    =  cvr.rst.name,
    #     dataset_description =  as.character(k),
    #     dataset_summary     =  as.character(k),
    #     dataset_country     =  as.character(i),
    #     dataset_class       =  as.character(k)
    #   )     
    #   
    #   covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,
    #                                            "/",paste0(tolower(i),
    #                                                       "_grid_100m_",
    #                                                       as.character(k),
    #                                                       ".tif"))        
    #     
    # 
    #   
    #   logdebug( paste0(' Covariate: ',k," for ",i,
    #                    " alsready exists in ",cvr.folder))
    #   
    #   covariates[[i]][[k]] <- list(
    #     dataset_folder      =  cvr.folder,
    #     dataset_filename    =  cvr.rst.name,
    #     dataset_description =  as.character(k),
    #     dataset_country     =  as.character(i),
    #     dataset_class       =  as.character(k)
    #   )            
    #   covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,"/",
    #                                            paste0(tolower(i),
    #                                                   "_grid_100m_",
    #                                                   as.character(k),".tif"))        
    #   
    # }
  }  
  
  ##  Save the covariates object as a RData and a JSON file:
  save(covariates, file=paste(bsgm.output.path.countries.tmp,
                              bsgm.countries.fln.Rdata, sep=""))
  covariates_json <- toJSON(covariates, pretty = TRUE)
  cat(covariates_json, file=paste(bsgm.output.path.countries.tmp,
                                  bsgm.countries.fln.json , sep=""))   
  
  ##  Return the covariates object:
  return(covariates) 
}



# get.covariates.var.names <- function( ){
#   
#   covariates.var.names <- c()
#   
#   for ( icvritm in 1:length(covariates[[1]]) ) { 
#     covariate <- covariates[[1]][[icvritm]]
#     var_name_class <- covariate$dataset_class
#     covariates.var.names <- c(covariates.var.names, var_name_class)
#   }    
#   
#   sort(covariates.var.names)
#   return(covariates.var.names)
# }

##  WILL DEVELOP LATER DEPENDING ON HOW WE ARE STORING THE COUNTRY SPECIFIC 
##  POPULATION LAYERS
# Download_Population_Maps <- function(){
#   ##  If we have logging turned on:
#   if (bsgm.DologDEBUG) 
#     d.quiet=FALSE
#   else
#     d.quiet=TRUE
#   
#   loginfo("Download Population Map files from WP FTP if they dont exist already")
#   
# }



DownloadFileFromWPFTP <- function(file_path, dest_file, quiet, method="auto") {
  
  wpgpFTP <- "ftp.worldpop.org.uk"
  credentials <- paste(bsgm.ftp.username, bsgm.ftp.password, sep = ":")
  file_remote <- paste0('ftp://',credentials,'@',wpgpFTP,'/WP515640_Global/',file_path)
  
  tmStartDw  <- Sys.time()
  
  checkStatus <- tryCatch(
    {
      utils::download.file(file_remote, destfile=dest_file,mode="wb",quiet=quiet, method=method)
    },
    error=function(cond){
      message(paste("URL does not seem to exist:", file_remote))
      message("Here's the original error message:")
      message(cond)
    },
    warning=function(cond){
      message(paste("URL caused a warning:", file_remote))
      message("Here's the original warning message:")
      message(cond)
    },
    finally={
      if (!quiet){
        tmEndDw  <- Sys.time()
        #message(paste("Processed URL:", file_remote))
        message(paste("It took ", tmDiff(tmStartDw ,tmEndDw,frm="hms"), "to download" ))
      }
    }
  )
  
  if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
    return(NULL)
  } else{
    return(1)
  }
}

DownloadPopRasterInterp <- function(){
  ##  Downloading ppp population from FTP from 
  r_fn <- list()

  ##  For every input country:
  for(i in bsgm.input.countries){
    
    tif_1 <- paste0(root_path,"/data/",i,
                          "/Pop/ppp_prj_",t0,"_",i, ".tif")

    tif_2 <- paste0(root_path,"/data/",i,
                          "/Pop/ppp_prj_",t1,"_",i, ".tif")
    
    
    tif_1_ftp <- paste0("/ppp_datasets/",t0,"/",i,"/", tolower(i),"_ppp_wpgp_",t0, ".tif")
    tif_2_ftp <- paste0("/ppp_datasets/",t1,"/",i,"/", tolower(i),"_ppp_wpgp_",t1, ".tif")
    
    if(!file.exists(tif_1)){ 
      print(paste0("Copying ppp_prj_",t0,"_",i, ".tif from WP FTP"))
      DownloadFileFromWPFTP(tif_1_ftp, tif_1, TRUE, method="auto") 
    }
    
    if(!file.exists(tif_2)){ 
      print(paste0("Copying ppp_prj_",t1,"_",i, ".tif from WP FTP"))
      DownloadFileFromWPFTP(tif_2_ftp, tif_2, TRUE, method="auto") 
    }
    
  }

}



DownloadLANRaster <- function(){
  ##  Downloading LAN rasters from FTP from 

  year_seq <- seq(as.numeric(t0), as.numeric(t1), by = 1)
  
  ##  For every input country:
  for(i in bsgm.input.countries){
    
    for(t in 1:(length(year_seq))){
      
      if (year_seq[t] == 2011) next
      
      
      if(year_seq[t] < 2011){
        lan_pattern <- "_dmsp_"}
      else{lan_pattern <- "_viirs_"}
      
      y <- year_seq[t]
      
      tif_lan <- paste0(root_path,"/data/",i,
                        "/LAN/derived/"
                        ,i,lan_pattern,y,"_normlag_",y+1,"-",y,".tif")
      
      tif_ftp <- paste0("LAN/",i,
                        "/LAN/derived/"
                        ,i,lan_pattern,y,"_normlag_",y+1,"-",y,".tif")
      
      if(!file.exists(tif_lan)){ 
        print(paste0("Copying ",i,lan_pattern,y,"_normlag_",y+1,"-",y,".tif"))
        DownloadFileFromWPFTP(tif_ftp, tif_lan, TRUE, method="auto") 
      }else{
        print(paste0("We have already download the file before ",i,lan_pattern,y,"_normlag_",y+1,"-",y,".tif"))
      }
      
    }


  }
  
}





