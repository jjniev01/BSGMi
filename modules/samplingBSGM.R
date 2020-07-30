##  THIS FILE CONTAINS FUNCTIONS WHICH ARE USED TO CARRY OUT SAMPLING OF 
##  INDIVIDUAL PIXELS OF A GIVEN STUDY AREA FOR THE BSGM.
##  TITLE:  SAMPLING BSGM .R
##  AUTHOR:  JEREMIAH J. NIEVES
##  VERSION: 1.0
##  LAST UPDATE: 2017-05-30
##  NOTES:  
##
##
##

##  -----------------------------------------------------------------------  ##
calcTransitions <- function(year0_ras, year1_ras, year0, year1){
  ##  This function takes two BS rasters and finds which cells transitioned 
  ##  within the time period. Those cell indices are then indicated as 
  ##  transitional and output as a new raster.
  ##  
  ##  PARAMETERS
  ##  year0_ras - BS extent binary raster corresponding to year t0
  ##  
  ##  year1_ras - BS extent binary raster corresponding to year t1
  ## 
  ##  VALUE
  ##  Output binary raster indicating which cells transitioned between years t0
  ##  and t1.
  ##  ---------------------------------------------------------------------  ##
  ##  Make a copy of the initial year:
  trans_ras <- year0_ras
  
  ##  Make all of its non_NA, 1 values zero:
  trans_ras[which(values(trans_ras) ==1)] <- 0
  
  ##  Determine which cells were BS in each year, i.e. returns cell indices 
  ##  where it is BS:
  y1_bs <- which(values(year1_ras) == 1)
  y0_bs <- which(values(year0_ras) == 1)
  
  ##  From those, find out which indices were in y1, but not in y0:
  t_logic <- (y1_bs %in% y0_bs)
  t_indices <- y1_bs[!t_logic]
  
  ##  Turn those indices in our 'blank' trans_ras to a value of 1:
  trans_ras[t_indices] <- 1
  
  ##  Write that raster to file:
  writeRaster(trans_ras,
              paste0(bsgm.output.path.countries.tmp, 
                     bsgm.countries.tag,"_transition_",year0,"_",year1,".tif"),
              format = "GTiff",
              datatype = "INT2U",
              overwrite = overwrite,
              options = c("COMPRESS = LZW"))
  
  ##  Return the raster object:
  return(trans_ras)
}



  
overunderSample <- function(transition_raster_obj,
                            cov_stack,
                            cov_names,
                            sample_factor = 1,
                            trans_factor = 0.8,
                            distance_sample = FALSE,
                            sample_distance = 2000,
                            sample_cut_mark = 50000,
                            year0,
                            year1){
  ##  
  ##
  ##  PARAMETERS:
  ##    sample_factor - e.g. a sample factor of 2 will result in twice as many 
  ##      nontransition samples as transition samples
  ##
  ##    trans_factor - specifies what proportion of all transition cells should
  ##      be sampled
  ##
  ##    distance_sample - Should a distance threshold be used in the sampling?
  ##      WARNING: This is very resource expensive.
  ##
  ##    sample_distance - the radius to be used in the sampling distance, if 
  ##      active, in meters
  ##
  ##    sample_cut_mark - limit of the number of transition cells sampled
  ##      If this variable is less than the sample given by the sampling factor
  ##      This will override the above set sampling factor. This was insituted
  ##      due to memory limits and extremely large sample sizes being picked 
  ##      with minimal, at most, model performance gain at the cost of 
  ##      resources and time.
  ##
  ##  VALUE
  ##  A big matrix object which contains the covariate values and the outcome of
  ##  interest for the cells which were sampled is written to file and the PATH
  ##  to that object is returned by this function.
  ##
  ##  ---------------------------------------------------------------------  ##
  ##  BEGIN:  RASTER SOURCING
  ##  Rename the transition raster for ease of previous code reference:
  transition_ras <- transition_raster_obj
  
  ##  Bring in the Raster stack of covariates:
  covariate_stack <- cov_stack
  
  ##	Create a raster object based on our census data zones file to store our 
  ##  predictions (see note above about why this is using census_mask instead
  ##	of zonal_raster):
  prediction_raster <- region_mask
  ##
  ##  END:  RASTER SOURCING
  ######
  
  
  ######
  ##  BEGIN:  SAMPLING OF CELL INDICES
  ##  Extracting transitional cell indices  -------------------------------------
  ##  Retrieve the transition cell indices:
  start_time <- proc.time()[3]
  print("Retrieving transitional indices...")
  transition_ind <- which(values(transition_ras) == 1 &
                            values(region_mask) == 1)
  
  print("Sampling transitional indices...")
  ##  If the sample cut mark is higher than the number of samples dictated by the
  ##  sample factor is TRUE:
  if(sample_cut_mark > round(length(transition_ind) * trans_factor)){
    ##  Go with the sample factor based number:
    sample_size <- round(length(transition_ind) * trans_factor)
    ##  Randomly sample approximately 80% of those cells:
    transition_ind_sub <- sample(transition_ind,
                                 sample_size,
                                 replace = FALSE)
  }
  ##  If the sample cut mark is equal to or less than the sample factor 
  ##  determined use the sample cut number:
  if(sample_cut_mark <= round(length(transition_ind)*trans_factor)){
    sample_size <- sample_cut_mark
    ##  Randomly sample approximately 80% of those cells:
    transition_ind_sub <- sample(transition_ind,
                                 sample_size,
                                 replace = FALSE)
  }
  # logr(logconsole,
  #      #log_file_path = logfile,
  #      message_string = paste0("Transition random sample: ",
  #                              proc.time()[3] - start_time," secs"))
  
  print("Sampling non-transitional indices...")
  start_time <- proc.time()[3]
  ##  Retrieve a random sample of all other cell indices which are in the 
  ##  sample and did not experience transition:
  other_ind <- sample(setdiff(which(values(region_mask) == 1), transition_ind),
                      sample_size,
                      replace = FALSE)
  # logr(logconsole,
  #      #log_file_path = logfile,
  #      message_string = paste0("Non-transition random sample: ",
  #                              proc.time()[3]-start_time," secs"))
  
  print("Compiling all sampled indices...")
  ##  Compile the two vectors of sample indices:
  sampled_indices <- c(transition_ind_sub, other_ind)
  
  ##  Memory management:
  rm("transition_ind","other_ind")
  gc()
  
  ##  Create a vector to indicate whether the row represents a cell of 
  ##  transition:
  transition_vec <- c(rep(1,length(transition_ind_sub)),
                      rep(0,(length(sampled_indices)-length(transition_ind_sub))))
  
  
  ##  Consolidating sample indices  ---------------------------------------------
  ##  We now need to consolidate the sample indices into a big matrix which can 
  ##  then be used to extract values from all covariate datasets.
  ##  NOTE:  You'll possibly want to empty the temporary folder prior to each run.
  ##  Store the number of rows that will be needed in the big matrix:
  n_sampled <- length(sampled_indices)
  
  ##  Write the final vector to file as a backup should something go wrong and 
  ##  for replicability:
  save(sampled_indices, file = paste0(bsgm.output.path.countries.tmp,
                                      bsgm.countries.tag,
                                      "_sampled_indices_",year0,"_",year1,".RData"))
  
  # logr(logconsole,
  #      #log_file_path = logfile,
  #      message_string = paste0(n_sampled, " samples compiled."))
  
  
  
  print("Creating big matrix object...")
  ##  Starting length will be the length of 
  rowlen <- n_sampled
  collen <- length(names(covariates))+3 ##  3 accounts for the validation fold indicator, cell index, and the output
  
    # logr(logconsole,
  #      #log_file_path = logfile,
  #      message_string = paste0("Building a big matrix of ", 
  #                              rowlen, " x ", collen, " dimensions..."))
  
  ##  Get column names, i.e. the covariate names:
  # cov_names <-c()
  # for(i in 1:length(covariates)){
  #   cov_names <- c(cov_names,covariates[[i]]$dataset_folder)
  # }
  ##  Create a data table object we use for storing extracted covariates and 
  ##  other sampled data:
  bsgm.table <- data.table("CellIndex" = sampled_indices,
                           "Transition" = transition_vec,
                           "VFold" = sample(1:5, n_sampled, replace = TRUE))
  
  ##  Memory management:
  rm("transition_vec")
  gc()
  
  ##  END:  SAMPLING OF CELL INDICES
  ######
  
  ######
  ##  BEGIN:  COVARIATE EXTRACTION
  ##  Setup  --------------------------------------------------------------------
  ##  Here we'll divide up the covariate files into a list where they wll be 
  ##  distributed to cores. Each core will extract the values from the covariate
  ##  raster based upon the master_indices. The extracted values will then be 
  ##  saved off as a file to disk where they will then be read in one at a time 
  ##  and appended, using cbind, to the big matrix on disk. 
  print("Setting up for covariate extraction...")
  print(paste0("Extracting ",length(cov_names),
               " covariates from ", length(sampled_indices), " cells..."))
  
  ##  For every covariate:
  for( i in 1:length(cov_names)){
    ##  Extract the cells from the covariate raster stack and place them in the 
    ##  data.table to a column corresponding to the covariate name:
    bsgm.table[[cov_names[i]]] <- extract(covariate_stack@layers[[i]], 
                                    bsgm.table[["CellIndex"]])
    
  }
  
  
  
  print("Writing data.table to file...")
  start_time <- proc.time()[3]
  save(bsgm.table, 
       file = paste0(bsgm.output.path.countries.tmp,
                     bsgm.countries.tag,"_",
                     bsgm.version,"_",year0,"_",year1,".RData"))
  # logr(logconsole,
  #      #log_file_path=logfile,
  #      message_string= paste0("Writing Time for Big Matrix: ",
  #                             proc.time()[3]-start_time," secs"))
  
  
  ##  Clean up memory footprint:
  rm("transition_ras")
  gc()
  return(bsgm.table)
  ##  END:  COVARIATE EXTRACTION
  ######
}