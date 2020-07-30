create_dirs_for_prj <- function(){
  ##  Create a subfolder in /Data/ for each country and /output/ for country 
  ##  and countries.
  logdebug('Start creating folders for project')
  
  ##  Declare a character tag to append to the output files:
  countries_tag_output <- paste0('prj_',bsgm.timeframe)
  
  ##  For every country input into model:
  for ( i in bsgm.input.countries ) { 
    ##  Declare the country specific subdirectory:
    subDir.country <- paste0(root_path,"/","data/", i)
    
    ##  If the country specific data directory doesn't already exist, create it:
    if(!file.exists(subDir.country)){ 
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE) 
      logdebug(paste0('Created a folder: ',subDir.country))
     }
    
    ##  Declare the country specific temporary output folder:
    subDir.country.output.tmp <- paste0(root_path,"/","output/", i , "/tmp/")
    
    ##  If that temporary output folder does not already exist, create it:
    if(!file.exists(subDir.country.output.tmp)){ 
      dir.create(subDir.country.output.tmp, 
                 recursive = TRUE,
                 showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',subDir.country.output.tmp))
    }
    
    ##  Declare the country specific data directory for Population raster data:
    subDir.country.Pop <- paste0(subDir.country,"/Pop/")
    ##  If the folder doesn't already exist create it:
    if(!file.exists(subDir.country.Pop)){ 
      dir.create(subDir.country.Pop, recursive = TRUE, showWarnings = FALSE) 
      logdebug(paste0('Created a folder: ',subDir.country.Pop))
    }
    
    ##  If we are using the lights at night reweighting:
    if(bsgm.LAN.weighting==TRUE){
      ##  Declare the country specific data directory for LAN data:
      subDir.country.LAN <- paste0(subDir.country,"/LAN/")
      ##  If the folder doesn't already exist create it:
      if(!file.exists(subDir.country.LAN)){ 
        dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE) 
        logdebug(paste0('Created a folder: ',subDir.country.LAN))
      }
      
      ##  Declare a temporary sub folder within the LAN folder:
      subDir.country.LAN.tmp <- paste0(subDir.country.LAN,"tmp/")
      ##  If the folder doesn't already exist create it:
      if(!file.exists(subDir.country.LAN.tmp)){ 
        dir.create(subDir.country.LAN.tmp,
                   recursive = TRUE, showWarnings = FALSE) 
        logdebug(paste0('Created a folder: ',subDir.country.LAN.tmp))
      }
      
      ##  Declare a derived sub folder within the LAN folder:
      subDir.country.LAN.derived <- paste0(subDir.country.LAN,"derived/")
      ##  If the folder doesn't already exist create it:
      if(!file.exists(subDir.country.LAN.derived)){ 
        dir.create(subDir.country.LAN.derived,
                   recursive = TRUE, showWarnings = FALSE) 
        logdebug(paste0('Created a folder: ',subDir.country.LAN.derived))
      }
    }
    # ##  Declare the country specific zonal stats folder for CSV outputs:
    # subDir.country.output.zst <- paste0(root_path,"/",
    #                                     "output/", i , "/zonal_stats/csv/")
    # ##  If that folder does not already exist, create it:
    # if(!file.exists(subDir.country.output.zst)){ 
    #   dir.create(subDir.country.output.zst,
    #              recursive = TRUE,
    #              showWarnings = FALSE)
    #   logdebug(paste0('Created a folder: ',subDir.country.output.zst))
    # }
    
    # ##  Declare the country specific zonal stats folder for RData outputs:
    # subDir.country.output.zstRData <- paste0(root_path,"/","output/", 
    #                                          i , "/zonal_stats/RData/")
    # ##  If that folder does not already exist, create it:
    # if(!file.exists(subDir.country.output.zstRData)){ 
    #   dir.create(subDir.country.output.zstRData, 
    #              recursive = TRUE, 
    #              showWarnings = FALSE)
    #   logdebug(paste0('Created a folder: ',subDir.country.output.zstRData))
    # }    
    
    ##  Construct the tag output in a procedural manner so if we have more than
    ##  one country in our model run, we have a tag which includes them all 
    ##  separated by an underscore:
    countries_tag_output <- paste(countries_tag_output, i, sep = "_")    
  }
  
  ##  Declare the path for the output and temporary output folders which can 
  ##  account for multiple countries and is where the merged output files will 
  ##  reside:
  output.path.countries <- paste(root_path, "/output/", countries_tag_output, "/", sep="")
  output.path.countries.tmp <- paste(root_path, "/output/", countries_tag_output, "/tmp/", sep="")
  ##  If that temporary folder does not already exist, create it:
  if(!file.exists(output.path.countries.tmp)){ 
    dir.create(output.path.countries.tmp, recursive = TRUE, showWarnings = FALSE)
    logdebug(paste0('Created a folder: ',output.path.countries.tmp))
  }
  
  ##  If we have only one country then the path to /Data/ will be ISO of that
  ##  country otherwise it will be a new folder created based on the combined 
  ##  ISOs [example prj_2000_BTN_NPL]. This folder will contain the merged 
  ##  rasters for RF.
  if (length(bsgm.input.countries) == 1){
    data.path.countries <- paste(root_path, "/data/", as.character(bsgm.input.countries[[1]]), "/", sep="")
    ##  If that folder does not already exist, create it:
    if(!file.exists(data.path.countries)){ 
      dir.create(data.path.countries, recursive = TRUE, showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',data.path.countries))
    }  
  }else{
    data.path.countries <- paste(root_path, "/data/", countries_tag_output, "/", sep="")
    ##  If that folder does not already exist, create it:
    if(!file.exists(data.path.countries)){ 
      dir.create(data.path.countries, recursive = TRUE, showWarnings = FALSE)
    }  
  } 
  
  ##  Return a list of objects which represent the output path, the data pat, 
  ##  and the country(ies) tag:
  return(list(output=output.path.countries, 
              data=data.path.countries, 
              countries_tag=countries_tag_output))
}