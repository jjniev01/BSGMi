require(raster)
require(data.table)
require(FNN)
require(dplyr)
require(sf)
require(rgdal)
require(precrec)
require(ggplot2)
require(ggridges)
require(randomForest)
require(gridExtra)
require(RColorBrewer)
require(precrec)
require(scales)
source("D:/Research/BSGMiv1a/accessories/Geoprocessing_Functions.R")


ensure_dir <- function(d){
  ##  Function for ensuring that a directory exists and creating it if it does 
  ##  not; returns the path of the input path
  if(!dir.exists(d)){
    dir.create(d)
  }
  return(d)
}


####


gdal_polygonizeR <- function(x, 
                             outshape=NULL, 
                             gdalformat = 'ESRI Shapefile',
                             pypath=NULL, 
                             readpoly=TRUE, 
                             quiet=TRUE){
  
  ## gdal_polygonizeR
  ## Getting rasters into shape from R
  ## John Baumgartner's Research
  ## https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
  ##
  ##  This function turns a raster into a polygon via gdal
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}




####  RF Validation  -----------------------------------------------------------




rfValidate <- function(root = "D:/",
                       prj_root = "Research/BSGMi/Rework Data/",
                       iso,
                       base_year = 2000,
                       sampsize = 50000,
                       sample_order = NULL,
                       outdir,
                       prevalence = NULL,
                       model_tag = NULL){
  ##  This function takes a RF utilized and validates it against unsampled 
  ##  locations from the modelled country of interest by extracting raster 
  ##  values at every given index and then saves those values off as a vector on
  ##  disk at the given output directory. 
  ##  NOTE:  Requires the raster and randomForest packages be loaded in the 
  ##         environment.
  ##
  ##
  ##  Arguments
  ##  root     the base disk we are operating on
  ##  prj_root     the subdirectory of root where we are operating in
  ##  iso     character vector element of 3-letter ISO code of countries which 
  ##          are to be processed
  ##  base_year    numeric vector element showing which RF model we are 
  ##               interested in; also used for accessing the correct folder in 
  ##               the default output location following the deafult output 
  ##               naming conventions
  ##  outdir     directory where the output should be stored. Note that a 
  ##             subdirectory will be created for each ISO within this directory
  ##             and it is within this directory that the output tables will be 
  ##             stored
  ##             e.g. "F:/BSGMGeneral/ValidationOutput/"
  ##  prevalence if TRUE, fix the marginals by sampling equally from the known
  ##             transition and nontransition cells
  covariateExtract <- function(covariate_path, index_vec,
                               out_path, covariate_name){
    ##  Bring in the raster:
    covariate <- raster(covariate_path)
    
    ##  Extract the values by cell index:
    covariate_values <- extract(covariate,index_vec)
    
    ##  Save that vector to disk:
    save(covariate_values, paste0(outpath,covariate_name,"_vector",region,"_",timeframe,".RData"))
  }
  
  
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
      assign(var_name, covariates[[var_name]]$path, envir = .GlobalEnv)
      ##  Put the raster path in the list:
      list_ras[[i]] <-  get(var_name, envir = .GlobalEnv)
    }
    ##  Stack all the rasters we just retrieved:
    ras_stack <- stack(list_ras)
    
    ##  Return the raster stack object:
    return(ras_stack)
  }
  
  ##	Configure project and default 'local' data folders (typically these are 
  ##    static and should never change as they are constructed here mainly to 
  ##		take into account possibly having moved the RF base folder):
  ##  NOTE:  SHOULD NOT NEED TO BE CHANGED
  ##	Setup output and project paths, creating empty folders if necessary:
  #outdir <- ensure_dir(paste0(outdir,iso))
  #outtmp <- ensure_dir(paste0(outdir,"tmp/"))
  
  ##  List the directories in the prj output folder
  dir_list <- list.dirs(paste0(root,prj_root,"output/"), recursive = FALSE)
  
  ##  Get the folder we are interested in:
  prj_fold <- grep(paste0("prj_",model_tag,"_",iso), dir_list, 
                   perl = TRUE, value = TRUE)
  
  ##  Load in the final model:
  ##  Name is bsgmfit_final
  load(list.files(paste0(prj_fold,"/tmp/"),
                  pattern = paste0("bsgmfit_final_prj_.*[.]Rdata"),
                  full.names = TRUE))
  
  
  ##  Load in the transition raster:
  trans_ras <- raster(list.files(paste0(prj_fold,"/tmp/"),
                                 pattern = paste0("prj_",model_tag,"_",iso,"_transition_2000_2015.tif"),
                                 full.names = TRUE))
  
  ##  Load the admin raster which will serve as a region mask:
  region_mask <- raster(Sys.glob(paste0(root,prj_root,"data",
                                        "/",iso,"/*ccidadminl1.tif")))
  
  covariates <- vector(mode = "list", length = length(names(bsgmfit_final$forest$xlevels)))
  ##  Transform all the covariate paths names to the local setup of directories:
  for(i in 1:length(names(bsgmfit_final$forest$xlevels))){
    covariates[[i]]$path <- paste0(root,prj_root, "data","/",
                                   iso,"/",tolower(iso),"_grid_100m_",
                                   gsub("[.]","\\+",
                                        names(bsgmfit_final$forest$xlevels)[i]),
                                   ".tif")
  }
  names(covariates) <- names(bsgmfit_final$forest$xlevels)
  ##  Load in the covariate stack:
  covariate_stack <- creat_raster_stack()
  
  ##  Replace the names of the stack:
  names(covariate_stack) <- names(covariates)
  
  ##  Load in the sampled indices of the stack:
  if(length(Sys.glob(paste0(prj_fold,
                            "/tmp/*sampled_indices*.RData")))!=0){
    print(paste0("Loading ", Sys.glob(paste0(prj_fold,"/tmp/*sampled_indices*.RData"))[1]))
    ##  Name is sampled_indices
    load(Sys.glob(paste0(prj_fold,"/tmp/*sampled_indices*.RData"))[1])
  }
  if(length(Sys.glob(paste0(prj_fold,
                            "/tmp/*sampled_indices*.RData")))==0){
    ##  Must have been a parallel run by Maksym's function; Need to load in two 
    ##  RData objects and combine:
    sampled_indices <- c()
    print(prj_fold)
    for(b in c(Sys.glob(paste0(prj_fold,"tmp/bsgm.table.CellIndex_1*")),
               Sys.glob(paste0(prj_fold,"tmp/bsgm.table.CellIndex_0_NA*")))){
      load(b)
      sampled_indices <- c(sampled_indices,
                           get(strsplit(basename(b),".Rdata")[[1]])[,1])
    }
    print(paste0("Saving indices to ",
                 prj_fold,"/tmp/prj_",model_tag,"_",iso,"_sampled_indices.RData"))
    save("sampled_indices", 
         file = paste0(prj_fold,"/tmp/prj_",model_tag,"_",iso,"_sampled_indices.RData"))
  }
  
  ##
  ##
  ##  NOTE:  Nothing below here should need to be changed with regular use.
  ##
  ##
  
  ##  Set the random seed:
  set.seed(2011)
  
  
  ##  END:  GENERAL SETTINGS AND FUNCTIONS
  ######
  ######
  ##  BEGIN:  EXTRACTING VALIDATION DATASET
  ##
  ##  If the prevalence option is on, validate on equal number of transitions and 
  ##  non-transitions:
  if(!is.null(prevalence) & is.null(sample_order) ){
    ##  Determine if there are enough transitions cells left to meet the sample 
    ##  size:
    diff_cells <- setdiff(which(values(trans_ras)==1),
                          sampled_indices)
    sampsize <- min(sampsize,length(diff_cells))
    ##  This helps prevent us from trying to pull millions of samples if we were 
    ##  to calculate sampsize2 as sampsize/prevalence:
    if(prevalence < 0.1){
      sampsize2 <- 100000
      sampsize <- round(sampsize2*prevalence)
    }
    ##  Get cell indices of the transition cells which were not sampled in the 
    ##  model production:
    trans_val_indices <- sample(diff_cells, 
                                sampsize)
    
    ##  Get some new non-transition indices which had not been sampled prior. 
    ##    Randomly sample those which were not used in the production of the model
    ##    and lay within valid regions. This sample will be equal in size to the 
    ##    new transition validation indices:
    nontrans_val_indices <- sample(setdiff(which(!is.na(values(region_mask)) &
                                                   values(trans_ras)!=1 &
                                                   values(region_mask)!=0),
                                           sampled_indices),
                                   sampsize2)
    
    ##  Merge these indices into a single vector:
    val_indices <- c(trans_val_indices, nontrans_val_indices)
    ##  Create a new data frame to hold these indices and their true transition 
    ##  value (0 or 1) and start populating it:
    val_df <- data.table(Cell_index = val_indices,
                         OBS = c(rep(1,length(trans_val_indices)),
                                 rep(0,length(nontrans_val_indices))))
  }
  ##  If no prevalence specified calculate on a true random sample:
  if(is.null(prevalence) & is.null(sample_order)){
    val_indices <- sample(setdiff(which(!is.na(values(trans_ras))),
                                  sampled_indices), 
                          {sampsize*2})
    val_values <- extract(trans_ras,val_indices)
    ##  Create a new data frame to hold these indices and their true transition 
    ##  value (0 or 1) and start populating it:
    val_df <- data.table(Cell_index = val_indices,
                         OBS = val_values)
    rm(val_values)
    gc()
  }
  
  ##  If we passed a vector of indices to sample, use them:
  if(!is.null(sample_order)){
    ##  Remove the indices which were used in the training of the model:
    val_indices <- setdiff(sample_order,
                           sampled_indices)
    val_values <- extract(trans_ras, val_indices)
    val_df <- data.table(Cell_index = val_indices,
                         OBS = val_values)
    rm(val_values)
    gc()
  }
  
  print("Setting up for covariate extraction...")
  print(paste0("Extracting ",length(covariates)," covariates from ", length(val_indices), " cells..."))
  
  ##  For every covariate in the random forest:
  for(i in 1:length(bsgmfit_final$forest$xlevels)){
    print(paste0("Extracting from ",names(bsgmfit_final$forest$xlevels)[i]))
    start_time <- proc.time()[3]
    
    ##  Extract the values by cell index:
    covariate_values <- extract(covariate_stack[[names(bsgmfit_final$forest$xlevels)[i]]],
                                val_df$Cell_index)
    
    ##  Save that vector onto the dataframe:
    val_df[,(names(bsgmfit_final$forest$xlevels)[i]) := covariate_values]
    end_time <- proc.time()[3]
    print(paste0("Extraction time: ", end_time-start_time, " secs"))
  }
  
  ##  Remove any incomplete cases because the RF don't play that game:
  val_df <- na.omit(val_df)
  
  rm(trans_ras,region_mask,covariate_stack)
  gc()
  ##  END:  EXTRACTING VALIDATION DATASET
  ######
  
  
  ######
  ##  BEGIN:  VALIDATION PREDICTIONS
  ##
  ##  Predict the values based upon the val_df covariates:
  pred <- predict(bsgmfit_final, 
                  newdata = val_df[ , names(bsgmfit_final$forest$xlevels), 
                                    with = FALSE],
                  predict.all = TRUE,
                  type = "response")
  
  ##    Retrieve the predicted class response using a majority threshold:
  predicted<- as.numeric(levels(pred$aggregate))[pred$aggregate]
  
  ##  Attach the predicted responses to the data frame:
  val_df[, PRED := predicted]
  
  
  ##  Now get the probabilities attached to these
  ##  Predict the values based upon the val_df covariates:
  pred <- predict(bsgmfit_final, 
                  newdata = val_df[ , names(bsgmfit_final$forest$xlevels), 
                                    with = FALSE],
                  predict.all = TRUE,
                  type = "prob")
  
  ##    Retrieve the predicted class response:
  predicted<- pred$aggregate[,2]
  
  ##  Attach the predicted probability responses to the data frame:
  val_df[, PROB:=predicted] 
  
  ##  Add new columns for the cross tabulation and prepoulate with 0s:
  #val_df[, c("TP","FP","FN","TN") := 0]
  
  
  ##  Save the validation dataframe:
  print(paste0("Writing to ",outdir,
               "validation_datatable_",
               iso, "_",model_tag,
               {if(!is.null(prevalence)){paste0("_prevalence_",
                                                prevalence)}},
               {if(!is.null(sample_order)){"_admin_rep"}},
               ".rds"))
  saveRDS(val_df, 
          file=paste0(outdir,
                      "validation_datatable_",
                      iso, "_",model_tag,"_",
                      ".RDS"))
  print(paste0("Finished validation for ",iso, " ", model_tag))
}




####




####  ROC PRC PLOTTERS  --------------------------------------------------------
plotROCPRC <- function(data, score_col = "", label_col = "", 
                       sim_performance = TRUE, outdir, file_name, data_tag,
                       width, height, res,
                       ...){
  ##  This function leverages the precrec package and puts out multiple plots of 
  ##  the ROC and PRC along with returning the useful metrics associated with 
  ##  each.
  ####
  ##  If plotting simulated classifiers is desired:
  if(sim_performance == TRUE){
    ##  Random model and perfect model; m1 is "random" and "m2" is "perfect":
    modat <- list("PROB" = join_scores(runif(nrow(data)),
                                             c(rep(1,sum(data$OBS,na.rm = T)),
                                               rep(0,{nrow(data)-sum(data$OBS)})),
                                             data$PROB),
                        "OBS" = join_scores(c(rep(1,sum(data$OBS),na.rm = T),
                                              rep(0,{nrow(data)-sum(data$OBS,na.rm = T)})),
                                            c(rep(1,sum(data$OBS,na.rm=T)),
                                              rep(0,{nrow(data)-sum(data$OBS,na.rm =T)})),
                                            data$OBS),
                        "MODEL" = c("Random",
                                    "Perfect",
                                    data_tag),
                        "DSID"=c(1,2,3))
  }else{}
  ##  Evaluate the models for precision recall curves:
  evaldat <- evalmod(mmdata(scores = modat$PROB,
                            labels = modat$OBS,
                            modnames = modat$MODEL,
                            dsids = modat$DSID),
                     mode = "rocprc")
  
  ##  Modify the data for custom ggplot implementation:
  fortdat <- ggplot2::fortify(evaldat)
  
  ##  Create the plots:
  plots <- list()
  plots[[1]] <- ggplot(data = fortdat[fortdat$curvetype == "ROC",],
                       aes(x=x, y=y, color=modname))+
    geom_line(size = 0.5, show.legend = F)+
    scale_x_continuous(name = "1-Specificity",
                       breaks = c(0.00,0.25,0.50,0.75,1.00))+
    scale_y_continuous(name = "Sensitivity",
                       breaks = c(0.00,0.25,0.50,0.75,1.00))+
    theme_bw()+
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y = element_text(size =7))
  
  plots[[2]] <- ggplot(data = fortdat[fortdat$curvetype == "PRC",],
                       aes(x=x,y=y,color=modname))+
    geom_line(size = 0.5, show.legend = F)+
    scale_x_continuous(name = "Recall",
                       breaks = c(0.00,0.25,0.50,0.75,1.00))+
    scale_y_continuous(name = "Precision",
                       breaks = c(0.00,0.25,0.50,0.75,1.00))+
    theme_bw()+
    theme(strip.text.x = element_blank(),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 7)#,
          #strip.background = element_rect(colour = "white", fill = "white"),
          # legend.position = c(0.4,0.5),
          # legend.title = element_text(size = 6),
          # legend.text = element_text(size = 4),
          # legend.key.height = unit(2,"mm"),
          # legend.background = element_rect(fill = "white", 
          #                                  color = "grey50")
          )+
    labs(color = "Model")
  
  ##  Initialize the graphics output:
  tiff(filename = paste0(outdir, file_name),
       width = width, height = height, 
       units = "mm", res = res)
  ##  Plot the figures
  grid.arrange(grobs = plots, ncol = 2)
  ##  Turn off graphics device
  dev.off()
}




####








################################################################################
## POST-REWORK DATA MANAGEMENT
################################################################################
##  ROC PRC Plotting Function  ----
##  RF Validation ----
####  RF VALIDATION  -----------------------------------------------------------
##
##  Below block validates every RF object for the 2000-2012 models
##
##  Declare which countries we are carrying out the validation of the RF on:
iso3 <- c("VNM")
model_tag <- "2000-2005-2010-2015"
data_tag <- "esa_cls190"
root <-  "D:/Research/BSGMiv1a/"
outdir <- ensure_dir(paste0(root,"output/prj_",model_tag,"_",iso3,"/derived/"))
##  Declare the number of samples which should be taken; see the 'rfValidate' 
##  function to understand just how this parameter operates, particularly within 
##  the context of the biased sampling to create psuedo validation sets with a 
##  fixed prevalence:
sampvol <- 1000

##    Independent hold-out validation of pixel level classifier plotted across a
##      range of thresholds and 
##  For very country:
for(i in iso3){
  ##  Retrieve the RF object PATHS:
  rf_paths <- c(paste0(root,"output/prj_",model_tag,"_",i,"/tmp/bsgmfit_final_prj_",model_tag,"_",i,".RData"))
  ##  Create a preallocated list to store the validations in:
  val_list <- vector(mode= "list", length = length(rf_paths))
  ##  For every RF object:
  for(r in 1:length(rf_paths)){
    ##  Compute a validation set if one does not already exist:
    # if(!file.exists(paste0(root,"output_",data_type,"/prj_2000-2015_",i,"_",
    #                        data_type,"/derived/validation_datatable_",
    #                        i, "_",data_type,"_2000.RDS"))){
    ##  Get the validation set:
    rfValidate(root = "D:/",
               prj_root = "Research/BSGMiv1a/",
               iso = i, 
               outdir = outdir,
               model_tag = model_tag)
    #}
    ##  Load the validation set path into the list:
    val_list[[r]] <- paste0(root,"output/prj_",model_tag,"_",i,
                            "/derived/validation_datatable_",
                            i, "_",model_tag,"_.RDS")
  }
  
  ##  Combine the validation frames together, noting which came from which 
  ##  model:
  for(v in 1:length(val_list)){
    # data_type <- gsub(".*output_(.*)/prj.*","\\1",val_list[[v]])
    valdt <- readRDS(file = val_list[[v]])
    # valdt$DATA <- data_type
    
    ##  Create the plots of ROC and PRC curves for the country/city:
    plotROCPRC(valdt, score_col = "PROB", label_col = "OBS", sim_performance = T, 
               outdir = paste0(root,"Figures/",i,"/"), data_tag = data_tag, 
               res = 500, width = 85.5, height = 50,
               file_name = paste0(i,"_",data_tag,
                                  "_2000-2015_ROCPRC_Plots.tif"))
  }
}




####  Zonal Calculations of Binary Agreement  ----------------------------------
##  Description
##  Indices of cells that transitioned by the end of every period, as determined
##  by the transition raster, and are not NA as given by the region raster are 
##  identified.
##
##  Those indices are used to extract the observed and predicted values, 0 or 1,
##  from every identified cell and are translated into TP, FP, FN, and TN.
##
##    This is used to produce tabular pixel level contingency data for each 
##    year.
##
##  Pixel level Observed, Predicted, and TP, FP, FN, TN are then aggregated 
##  (sum) to the unit level and metrics are derived.
##  
##    This is used to produce tabular unit level contingency data for each year.
##  
##  The unit level contingency and null model data is joined to the 
##  corresponding shapefile by the GID.
##
##    This is used to produce year specific shapefiles that have the unit-level 
##    contingency and null model data.
##
##  The annual datasets of the unit-level contingency data are then collected 
##  and put into a single file.
####

root <-  "D:/Research/BSGMiv1a/"
iso3 <- c("CHE","PAN","UGA","VNM")
##  Comparison years:
years <- c(2001:2004,2006:2009,2011:2014)
year_tag <- "2000-2005-2010-2015"
data_tag <- "esa_cls190"
for(i in iso3){
  ##  Directory where predicted extents should be:
  preddir <- paste0(root,"output/prj_",year_tag,"_",i,"/")
  ##  Directory where observed extents should be:
  obsdir <- paste0(root, "data/",i,"/")
  # obsdir <- paste0(root, "data/",i,"_GUF+/")
  ##  Directory for output:
  outdir <- ensure_dir(paste0(preddir,"derived/"))
  
  ##  The GID raster path:
  regfile_path <- paste0(obsdir, tolower(i),"_grid_100m_ccidadminl1.tif")
  ##  GID raster:
  region_ras <- raster(regfile_path)
  
  ##  For every year:
  for(y in years){
    ##  Get the period specific transition raster path:
    if(y < 2005){
      transfile_path <- paste0(preddir,"tmp/prj_",year_tag,"_",i,"_transition_2000_2005.tif")
    }
    if(y >2005 & y <2010){
      transfile_path <- paste0(preddir,"tmp/prj_",year_tag,"_",i,"_transition_2005_2010.tif")
    }
    if(y <2015 & y >2010){
      transfile_path <- paste0(preddir,"tmp/prj_",year_tag,"_",i,"_transition_2010_2015.tif")
    }
    ##  Transition raster path for GUF+ data:
    #transfile_path <- paste0(preddir,"tmp/prj_",year_tag,"_",i,"_transition_2000_2015.tif")
    trans_ras <- raster(transfile_path)
    
    print(paste0("Working on ",i," ",y))
    
    ##  If the shapefile doesn't already exist:
    if(!file.exists(paste0(obsdir, tolower(i),
                           "_grid_100m_ccidadminl1.shp"))){
      ##  Create it:
      gdal_polygonizeR(paste0(obsdir,
                              tolower(i),"_grid_100m_ccidadminl1.tif"),
                       outshape = paste0(obsdir,
                                         tolower(i),"_grid_100m_ccidadminl1.shp"))
      ##  Kludge to remove supposed improper geometries:
      foo_shp <- st_buffer(st_read(dsn=paste0(obsdir,
                                           tolower(i),"_grid_100m_ccidadminl1.shp"),
                                layer=paste0(tolower(i),"_grid_100m_ccidadminl1")),
                        0.0)
      
      ##  Select all shapes that don't have the GID (DN) of zero which is water,
      ##  group the records by their GID, and then dissolve the boundaries by 
      ##  GID so there is one feature per GID:
      foo_shp<- foo_shp[foo_shp$DN!=0,] %>% 
        group_by(DN) %>%
        summarise(geometry = sf::st_union(geometry)) %>%
        ungroup()
      ##  Rename "DN" to GID:
      names(foo_shp)[1] <- "GID"
      
      ##  If all the geometries are valid, write it out to disk:
      if(all(st_is_valid(foo_shp))){
        st_write(foo_shp, 
                 dsn = paste0(obsdir,tolower(i),
                              "_grid_100m_ccidadminl1.shp"),
                 layer = paste0(tolower(i),
                                "_grid_100m_ccidadminl1"),
                 driver = "ESRI Shapefile",
                 delete_dsn = TRUE)
      }else{
        stop("All geometries are not valid")
      }
    }
    
    ##  Load it in:
    shp <- read_sf(paste0(obsdir,tolower(i),
                          "_grid_100m_ccidadminl1.shp"),
                   layer = paste0(tolower(i),
                                  "_grid_100m_ccidadminl1"),
                   stringsAsFactors = F)
    ##  Ensure that the identifying field name is GID
    names(shp)[1] <- "GID"
    ##  For some reason, when the shapefile is read in, it says there are 
    ##  various invalid geometries even with our check prior to writing to disk;
    ##  however, adding a buffer of zero then makes all of them valid.
    shp <- st_buffer(shp, 0.0)
    
    ##  Get the binary files of interest both predicted and observed:
    #  ##  Binary file for GUF+
    # predfile_path <- paste0(preddir, "BSGM_Extentsprj_2000-2015_",i,"_",y,".tif")
    predfile_path <- paste0(preddir, "BSGM_1.1a_Extents_prj_",year_tag,"_",i,"_",y,".tif")
    obsfile_path <- paste0(obsdir, tolower(i), "_grid_100m_",data_tag,"_",y,".tif")
    
    
    ##  Read in the raster data:
    pred_ras <- raster(predfile_path)
    obs_ras <- raster(obsfile_path)
    
    
    ##  Find the cells that transitioned by the end year of the period of 
    ##  interest, e.g. 2005, 2010, 2015, and are not NA in the region ras.
    ##    Get the cell indices:
    foo_ind <- intersect(which(!is.na(values(region_ras))), 
                         which(values(trans_ras)==1))
    
    ##    Extract the GID of the Region Raster, the Predicted Value, and the 
    ##    Observed Value and create a data.table:
    foo_table <- data.table("CID"=foo_ind,
                            "GID" = region_ras[foo_ind],
                            "PRED" = pred_ras[foo_ind],
                            "OBS" = obs_ras[foo_ind])
    
    ##  Calculate the number of TP, FP, TN, FN:
    foo_table[,c("TP","FP","TN","FN") := 0]
    foo_table[OBS == 1 & PRED == 1, TP := 1]
    foo_table[OBS == 1 & PRED == 0, FN := 1]
    foo_table[OBS == 0 & PRED == 1, FP := 1]
    foo_table[OBS == 0 & PRED == 0, TN := 1]
    ##  Add an identifying year column:
    foo_table[,YEAR := y]
    ##  Ensure we don't have any areas of water:
    foo_table <- copy(foo_table[GID!=0,])
    ##  Save the pixel level contingency data:
    saveRDS(foo_table, 
            file = paste0(outdir, i,"_",data_tag,
                          "_pixel_level_contingency_data_",y,".RDS"))
    
    ##  Copy the pixel level data table to then aggregate it:
    aggtab <- copy(foo_table)
    ##  Drop the CID and GID and YEAR columns from the aggregate table prior to
    ##  aggregation:
    aggtab[,c("CID","GID","YEAR") := NULL]
    
    ##  Aggregate to the subnational unit level and join to the shapefile we 
    ##  brought in earlier:
    aggtab <- aggregate.data.frame(aggtab,
                                   by = list(foo_table$GID),
                                   simplify = F,
                                   FUN = sum, 
                                   na.rm = T)
    names(aggtab)[1] <- "GID"
    ##  Convert the column types to numeric as they are coming out as class list 
    ##  from aggregate:
    aggtab$PRED <- as.numeric(aggtab$PRED)
    aggtab$OBS <- as.numeric(aggtab$OBS)
    aggtab$TP <- as.numeric(aggtab$TP)
    aggtab$FP <- as.numeric(aggtab$FP)
    aggtab$TN <- as.numeric(aggtab$TN)
    aggtab$FN <- as.numeric(aggtab$FN)
    ##  Add a year column:
    aggtab$YEAR <- y
    ##  Return to a data.table format:
    aggtab <- as.data.table(aggtab)
    ##  Calculate derived metrics from the TP,FP,FN,TN:
    aggtab[, RECALL := TP/{TP+FN}, 
           by = list(GID)]
    aggtab[, PRECISION := TP/{TP+FP}, 
           by = list(GID)]
    aggtab[, F1 := {2*(PRECISION*RECALL)/(PRECISION+RECALL)}, 
           by = list(GID)]
    aggtab[, SPECIFICITY := {TN/(FP+TN)}, 
           by = list(GID)]
    aggtab[, OVERACC := {{TN+TP}/(TP+FN+FP+TN)}, 
           by = list(GID)]
    aggtab[, QUANTDIS.0 := {abs({FN-FP}/(TP+FP+FN+TN))}, 
           by = list(GID)]
    aggtab[, QUANTDIS.1 := {abs({FP-FN}/(TP+FP+FN+TN))}, 
           by = list(GID)]
    aggtab[, QUANTDIS := {{QUANTDIS.0+QUANTDIS.1}/2}, 
           by = list(GID)]
    aggtab[, ALLOCDIS.0 := {2*min({FP/{TP+TN+FP+FN}}, {FN/{TP+TN+FP+FN}})},
           by = list(GID)]
    aggtab[, ALLOCDIS.1 := {2*min({FN/{TP+TN+FP+FN}}, {FP/{TP+TN+FP+FN}})},
           by = list(GID)]
    aggtab[, ALLOCDIS := {ALLOCDIS.0+ALLOCDIS.1}/2, 
           by = list(GID)]
    aggtab[, PROP.CORRECT := {{TN+TP}/{TP+TN+FP+FN}}, 
           by = list(GID)]
    
    ##  Save the aggregate table as an RDS object:
    saveRDS(data.table(aggtab), 
            file = paste0(outdir, i,"_",data_tag,
                          "_admin_level_contingency_data_",y,".RDS"))
    
    ##  Merge the aggegate table with the shapefile:
    aggshp <- merge(shp, as.data.frame(aggtab), by = "GID")
    
    ##  Pull in the null model for the country and all years:
    null_data <- readRDS(Sys.glob(paste0(outdir,
                                         "*_NullBootbyGIDandYear_500_",year_tag,".RDS")))
    ##  Null data for GUF+:
    # null_data <- readRDS(Sys.glob(paste0(outdir,
    #                                      "*_NullBootbyGIDandYear_300_",year_tag,".RDS")))
    
    ##  Join the null data to the contingency data:
    aggshp<- merge(aggshp, as.data.frame(null_data[YEAR==y,]),
                   by = "GID", all.x=T)
    
    ##  Write the shapefile with the data:
    if(all(st_is_valid(aggshp))){
      st_write(aggshp,
               dsn = paste0(outdir,i,"_",data_tag,"_contingency_data_",y,".shp"),
               layer = paste0(i,"_",data_tag,"_contingency_data_",y),
               driver = "ESRI Shapefile",
               delete_dsn = TRUE)
    }else{stop("Aggshp geometries are not all valid.")}
  }
  ##  Combine all annual datasets into a single dataset for each country:
  outdir <- ensure_dir(paste0(preddir,"derived/"))
  
  ##  Aggregate all the annual datasets into a single data.table
  dts <- Sys.glob(paste0(outdir, i,"_",data_tag,
                         "_admin_level_contingency_data_*.RDS"))
  for(d in 1:length(dts)){
    if(d ==1){
      dt <- readRDS(file = dts[d])
    }
    if(d!=1){
      dt <- rbind(dt, readRDS(file=dts[d]))
    }
  }
  saveRDS(dt,file=paste0(outdir, i,"_",data_tag,
                         "_admin_level_contingency_data_",year_tag,".RDS"))
}




####  CELL LEVEL CLASSIFICATION MEASUREMENT GRAPHICS AND TABLES  ---------------
##  Description:
##  Takes the study area wide cell level contingency data and calculates cell 
##  level metrics for that study area and places it in a .csv.
####

root <-  "D:/Research/BSGMiv1a/"
iso3 <- c("CHE","PAN","UGA","VNM")
years <- c(2001:2004,2006:2009,2011:2014)
year_tag <- "2000-2005-2010-2015"
data_tag <- "esa_cls190"

##      ----
##  For every country:
for(l in 1:length(iso3)){
  ##  Here we are going to output a csv with specified fields of:
  ##  ISO, MODEL, YEAR, TP, FP, FN, TN, NCELLS, RECALL, PRECISION, F1, 
  ##  SPECIFICITY, OVER.ACC, QUANTDIS, ALLOCDIS
  i <- iso3[l]
  ##  If this is the first run, add the column names to the csv file:
  if(l == 1){
    foodf <- data.frame("ISO"= character(length=0),
                        "MODEL" =character(length=0),
                        "YEAR"=numeric(length=0),
                        "TP"=numeric(length=0),
                        "FP"=numeric(length=0),
                        "FN"=numeric(length=0),
                        "TN"=numeric(length=0),
                        "NCELLS"=numeric(length=0), 
                        "RECALL"=numeric(length=0), 
                        "PRECISION"=numeric(length=0),
                        "F1"=numeric(length=0),
                        "SPECIFICITY"=numeric(length=0),
                        "OVER.ACC"=numeric(length=0), 
                        "QUANTDIS"=numeric(length=0),
                        "ALLOCDIS"=numeric(length=0),
                        stringsAsFactors = F)
  }
  ##  For every year:
  for(y in years){
    ##  Add the country and model tags to the data:
    foodf$ISO <- as.character(foodf$ISO)
    foodf$MODEL <- as.character(foodf$MODEL)
    ##  Directory where predicted extents should be:
    # preddir <- paste0(root,"output_GUF+/prj_2000-2015_",i,"_GUF+/")
    preddir <- paste0(root,"output/prj_",year_tag,"_",i,"/")
    outdir <- paste0(preddir,"derived/")
    
    ##  Load the RDS data.table object of the pixel level data:
    # dt <- readRDS(paste0(outdir, i,"_GUF+_pixel_level_contingency_data.RDS"))
    dt <- readRDS(paste0(outdir, i,"_",data_tag,
                         "_pixel_level_contingency_data_",y,".RDS"))
    
    ##  Total number of pixels:
    cell_n <- nrow(dt)
    
    ##  Interior cells (a,b,c,d) top left to bottom right:
    cell_a <- sum(dt$TP)
    cell_b <- sum(dt$FP)
    cell_c <- sum(dt$FN)
    cell_d <- sum(dt$TN)
    
    ##  Row marginals (e,f) from top to bottom:
    cell_e <- sum(dt$PRED)
    cell_f <- cell_n - sum(dt$PRED)
    
    ##  Column marginals (g,h) from left to right:
    cell_g <- sum(dt$OBS)
    cell_h <- cell_n - sum(dt$OBS)
    
    ##  Calculate Recall (Sensitivity/Producer's Accuracy):
    recall <- cell_a/cell_g
    ##  Calculate Precision:
    precision <- cell_a/cell_e
    ##  Calculate Specificity:
    specificity <- cell_d/cell_h
    ##  Calculate the F-statistic:
    fstat <- (2*precision*recall)/(precision+recall)
    ##  Calculate Overall Accuracy (Proportion timed correctly:
    over_acc <- (cell_a+cell_d)/cell_n
    ##  Quantity and Allocation disagreement kappa (Pontius et al 2011):
    QUANTDIS.0 <- {abs({cell_b-cell_c}/cell_n)}
    QUANTDIS.1 <- {abs({cell_c-cell_b}/cell_n)}
    QUANTDIS <- {QUANTDIS.0+QUANTDIS.1}/2
    ALLOCDIS.0 <- {2*min({cell_b/cell_n}, {cell_c/cell_n})}
    ALLOCDIS.1 <- {2*min({cell_c/cell_n}, {cell_b/cell_n})}
    ALLOCDIS <- {ALLOCDIS.0+ALLOCDIS.1}/2
    PROP.CORRECT <- {{cell_d+cell_a}/{cell_n}}
    
    ##  Construct the row of data:
    row <- list(i, year_tag, y, cell_a, cell_b, cell_c, cell_d, cell_n, recall, 
                 precision, specificity, fstat, over_acc, QUANTDIS,
                 ALLOCDIS)
    names(row) <- c("ISO", "MODEL", "YEAR", "TP", "FP", "FN", "TN", "NCELLS", 
                        "RECALL", "PRECISION", "F1", "SPECIFICITY", "OVER.ACC", 
                        "QUANTDIS", "ALLOCDIS")
    ##  Append it to the csv:
    foodf <- rbind(foodf, row)
  }
}
write.csv(foodf, 
          file = paste0(root, "CountryLevelPixelComparisons.csv"),
          row.names = F)




####  COMBINING THE NULL MODELS AND ALL MODEL  ---------------------------------
####  CONFIGURATION DATASETS IN ONE DATA TABLE FOR ADMIN LEVEL
iso3 <- c("CHE","PAN","UGA","VNM")
root <- "D:/Research/BSGMiv1a/"
output_directory <- paste0(root)
##  Declare the model names and the period list we have in each:
model_list <- list("2000-2005-2010-2015" = list("1"=c(2000,2005),
                                                "2"=c(2005,2010),
                                                "3"=c(2010,2015)))

##  Model list for GUF+:
# model_list <- list("2000-2015" = list("1"=c(2000,2015)))


##  Start a counter:
##  WARNING:  THIS NEEDS THE NULL MODELS CALCULATED PRIOR TO RUNNING  ##
cnt <- 1
##  For every country in the list:
for(i in iso3){
  ##  For every item in the model_list:
  for(m in 1:length(model_list)){
    print(paste0("Working on ",i," ",names(model_list)[m]))
    ##  Retrieve the model's year tag(s):
    year_tag <- names(model_list[m])
    if(year_tag == "2000-2005-2010-2015"){
      year_tag <- "2000-2015"
    }
    ##  Declare the output root:
    out_root <- paste0( root, "output/prj_",
                        ifelse(names(model_list[m])=="2000-2005-2010-2015",
                               "2000-2005-2010-2015",
                               year_tag),
                        "_",i,"/derived/")
    ## Retrieve the aggregated contingency RDS format data.table:
    foodt <- readRDS(Sys.glob(paste0(out_root,i,"_*_admin_level_contingency_data_",
                                     ifelse(names(model_list[m])=="2000-2005-2010-2015",
                                            "2000-2005-2010-2015",
                                            year_tag),".RDS")))
    ##  Add a model column that gives a model specific tag:
    foodt[,MODEL := ifelse(names(model_list[m])=="2000-2005-2010-2015",
                           "2000-2005-2010-2015",year_tag)]
    ##  Make sure the GID column is a numeric:
    foodt$GID <- as.numeric(foodt$GID)
    foodt$YEAR <- as.numeric(foodt$YEAR)
    
    ##  Add a period column that gives a model period specific tag:
    ##  For every period in the period list of the specific model we are 
    ##  working on:
    for(p in 1:length(model_list[[m]])){
      ##  Get the initial year and end year of the period:
      init_year <- model_list[[m]][[p]][1]
      end_year <- model_list[[m]][[p]][2]
      ##  Get the actual period tag:
      period_tag <- names(model_list[[m]])[p]
      ##  Tag records in the dt approriately:
      foodt[YEAR > init_year & YEAR < end_year,
            PERIOD := period_tag]
    }
    
    ##  Find and bring in the corresponding null model:
    if(!file.exists(Sys.glob(paste0(out_root,i,
                                   "_*_NullBootbyGIDandYear_*_",
                                   ifelse(names(model_list[m])=="2000-2005-2010-2015",
                                          "2000-2005-2010-2015",year_tag),
                                   ".RDS")))){
      stop("No null model has been calculated for this model. Please run the null model and ensure all others have been run prior to compiling.")
    }
    ##  Read in the admin level null model data:
    nulldt <- readRDS(Sys.glob(paste0(out_root,i,
                                      "_*_NullBootbyGIDandYear_*_",
                                      ifelse(names(model_list[m])=="2000-2005-2010-2015",
                                             "2000-2005-2010-2015",year_tag),
                                      ".RDS")))
    ##  Ensure the GID and YEAR are numeric:
    nulldt$GID <- as.numeric(nulldt$GID)
    nulldt$YEAR <- as.numeric(nulldt$YEAR)
    ##  Merge the null model results by year and GID with the data:
    setkey(foodt,GID,YEAR)
    setkey(nulldt,GID,YEAR)
    
    ##  Change the names to the desired format:
    names(nulldt)[c(4,7,10,13,16,19,22)] <- c("RECALL.NULL","PRECISION.NULL",
                                           "F1.NULL","SPECIFICITY.NULL",
                                           "OVER.ACC.NULL","QUANT.NULL",
                                           "ALLOC.NULL")
    ##  Merge the columns over:
    foodt[nulldt, 
          names(nulldt)[c(4,7,10,13,16,19,22)] := nulldt[,.SD,.SDcols = c(4,7,10,13,16,19,22)]]
    ##  Mark what country it is from:
    foodt[,ISO:=i]
    
    ##  If this is the first iteration we are carrying out:
    if(cnt==1){
      ##  Then foodt becomes the main storage data.table we will append all 
      ##  other things to:
      main_dt <- copy(foodt)
    }
    if(cnt!=1){
      ##  Otherwise append:
      main_dt <- rbind(main_dt,copy(foodt))
    }
    
    ##  Increase the counter to non 1 after the first run:
    cnt <- 2
  }
  ##  Output the final compiled file:
  saveRDS(main_dt, 
          file = paste0(output_directory,"AdminLvl_Contingencies_MultiModels_",
                        i,".RDS"))
}




####  RF VARIABLE IMPORTANCE PLOTTING  -----------------------------------------
##  Extracting and compiing variable importance data
root <-  "D:/Research/BSGMiv1a/"
iso3 <- c("CHE","PAN","UGA","VNM")


##  Create a holder for the importance data:
impdt <- data.table("ISO" = character(length = 0),
                    "DATA" = character(length = 0),
                    "MODEL.GROUP" = character(length = 0),
                    "VAR.NAME" = character(length = 0),
                    "ACC.TYPE" = character(length = 0),
                    "ACC.DEC" = numeric(length=0))

##  Pull all the GUF+ and ESA RF objects :
esamods <- list.files(paste0(root,"output"), recursive = T, full.names = T)
esamods <- grep(".*/(prj_2000-2005-2010-2015_VNM|prj_2000-2005-2010-2015_UGA|prj_2000-2005-2010-2015_PAN|prj_2000-2005-2010-2015_CHE)/tmp/bsgmfit_final_.*.Rdata",esamods,value= T)


##  For every ESA model object:
for(e in esamods){
  ##  Note what BS data was being used:
  dat <- "ESA"
  if(sub(".*(prj_2000-2015_VNM).*","\\1",e)=="prj_2000-2015_VNM"){
    dat <- "GUF+"
  }
  ##  Pull the iso:
  i <- gsub(".*/bsgmfit_final_.*_([A-Z]{3})[.]Rdata","\\1",e)
  ##  Load the model object (comes in with var name bsgmfit.final):
  load(e)
  ##  Pull the importances and variable names:
  varnames <- dimnames(bsgmfit_final$importance)[[1]]
  perincmse <- bsgmfit_final$importance
  
  ##  Set a temporary dataframe that we will rbind to the data.table:
  foodat <- data.frame("ISO" = rep(i, 3*length(varnames)),
                       "DATA" = rep(dat, 3*length(varnames)),
                       "MODEL.GROUP" = rep(paste0(i,"_",dat),3*length(varnames)),
                       "VAR.NAME" = varnames,
                       "ACC.TYPE" = c(rep("0",length(varnames)),
                                      rep("1",length(varnames)),
                                      rep("Avg.", length(varnames))),
                       "ACC.DEC" = c(perincmse[,1],perincmse[,2],
                                     perincmse[,4]))
  impdt <- rbind(impdt,foodat)
}
impdt[,PLOTGROUP := paste0(ISO," ",DATA)]
##  Save the file:
saveRDS(impdt, file = paste0(root, "aggregate_variable_importance_CHE_PAN_UGA_VNM.RDS"))



##    Facted VarImpPlot-type Diagram (by Country and Datasource)
shapes <- c(0,1,2,6)
importance_dt <- readRDS(paste0(root, "aggregate_variable_importance_CHE_PAN_UGA_VNM.RDS"))
names(shapes) <- unique(importance_dt$PLOTGROUP)
tiff(file = paste0(root,"VariableImportancesAll.tiff"),
     width = 190, height = 90, units = "mm", res = 300)
print(ggplot(importance_dt[ACC.TYPE == "Avg.",],
             aes(y=log(ACC.DEC,base=10),
                 x=reorder(as.factor(VAR.NAME), 
                           log(ACC.DEC,base=10), max,order = T, 
                           na.rm =T),
                 group = as.factor(PLOTGROUP),
                 shape = as.factor(PLOTGROUP)))+
        geom_point(size = 1.5,position = position_dodge(width = 0.75))+
        scale_shape_manual(values=shapes)+
        theme_bw()+
        xlab("Variable")+
        ylab("Avg. Log Decrease in Gini")+
        labs(color = "Model",
             shape = "Model")+
        annotation_logticks(sides="l")+
        scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x),
                      labels = trans_format("log10",math_format(10^.x)))+
        theme(axis.text.x = element_text(angle = -45, hjust = 0, 
                                         vjust = 1, size = 7),
              axis.text.y = element_text(size = 7),
              legend.title = element_text(size = 8),
              axis.title.x = element_text(size = 8),
              axis.title.y = element_text(size = 8),
              legend.text = element_text(size = 7)))

dev.off()
