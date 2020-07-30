tmDiffc <- function(start, end, frm="hms") {
  ##  This function determines the difference between a beginning and end time 
  ##  argument and outputs it in a given format.
  
  ##  Get the elapsed time in seconds and hours:
  dsec <- as.numeric(difftime(end, start, units = c("secs")))
  hours <- floor(dsec / 3600)
  
  ##  If the format is hours minutes seconds (hms):
  if (frm == "hms" ){
    ##  Derive the elapsed minutes and remnant seconds:
    minutes <- floor((dsec - 3600 * hours) / 60)
    seconds <- dsec - 3600*hours - 60*minutes
    
    ##  Format the string output:
    out=paste0(
      sapply(c(hours, minutes, seconds), function(x) {
        formatC(x, width = 2, format = "d", flag = "0")
      }), collapse = ":")
    ##  Return that formatted output:
    return(out)
  }else{
    ## Return the hours output:
    return(hours)
  }
}

################################

create_bsgm_table_1 <- function(transition_raster, 
                                region_mask, 
                                minblocks=NULL, 
                                silent=FALSE) {
  ##  This function identifies what cells are known to transition (value of 1)...
  ##  Note the start time:
  tStart <- Sys.time()
  
  ##  Inititalize a dataframe to hold the cell indices:
  bsgm.table <- data.frame(CellIndex=integer(),stringsAsFactors=FALSE)
  
  ##  Stack the transition raster and the region mask:
  cvr_stack <-stack(transition_raster,region_mask)
  
  ## If no minimum number of blocks is specified:
  if (is.null(minblocks)) {
    ##  Determine the number of blocks to create using a function from the 
    ##  wpUtilities package:
    minblocks <- wpGetBlocksNeed(cvr_stack, cores=bsgm.cluster_workers, n=2)  
  }
  
  ##  Remove the covariate stack from memory:
  rm(cvr_stack)
  
  ##  Break the transition raster into the specified number of blocks:
  blocks <- blockSize(transition_raster,minblocks=minblocks)
  
  ##  Initiate a cluster with the specified number of cores to begin determining
  ##  the cell indices of transition in parallel:
  cl <- makeCluster(bsgm.cluster_workers)
  
  ##  Register the snow backend with the foreach package:
  registerDoSNOW(cl)
  
  ##  Export the transition raster, the region mask raster, and the cell index 
  ##  table (created earlier), and the blocks object to every cluster core:
  clusterExport(cl, c("transition_raster", "region_mask","bsgm.table"), envir=environment())
  clusterExport(cl, "blocks", envir=environment())
  
  ##  Create a text based progress bar to keep us updated on things:
  pb <- txtProgressBar(min = 1, max = blocks$n, style = 3, width = 80)
  ##  Create a progress function:
  progress <- function(n) {
    ##  Determine how many and what tasks we are working with and return as a 
    ##  vector:
    ch.pb <- unlist(lapply(1:bsgm.cluster_workers,
                           function(i) {return(i*round(blocks$n/bsgm.cluster_workers))}),
                    use.names=FALSE)
    ##  If the current task is within the identified tasks, set the progress bar 
    ##  accordingly to indicate where we are:
    if (n %in% ch.pb & !silent) { 
      setTxtProgressBar(pb, n)
      ##  If we are at the end of our tasks to process, set the bar to its 
      ##  maximum:
    }else if(n==blocks$n & !silent){
      setTxtProgressBar(pb, n)
    }
    
  }
  ##  Store progress in a list:
  opts <- list(progress = progress)
  
  ##  For every value i from 1 to the number of blocks we have, do the specified 
  ##  actions in the '%dopar%' block and combine the outputs in order using the 
  ##  rbind function:
  oper <- foreach(i=1: blocks$n ,
                  .combine=rbind,
                  .inorder=TRUE,
                  .packages='raster',
                  .multicombine=TRUE,
                  .options.snow = opts) %dopar% 
                  {
                    ##  Perform the below actions for every given i, in parallel.
                    ##
                    ##  Retrieve the values of the raster corresponding to the 
                    ##  block we are currently processing:
                    row_data_transition_raster <- getValues(transition_raster, 
                                                            row=blocks$row[i], 
                                                            nrows=blocks$nrows[i])
                    ##  Retrieve the corresponding values of the region mask 
                    ##  raster for the block we are currently working on:
                    row_data_region_mask <- getValues(region_mask, 
                                                      row=blocks$row[i], 
                                                      nrows=blocks$nrows[i])
                    ##  Retrieve the number of columns in the transition raster:
                    nncol <- ncol(transition_raster)
                    
                    ##  If this is the first block:
                    if (i==1){
                      ##  Indicate what block is the beginning of the data.frame
                      ##  and what number of cells will be the end of all 
                      ##  processed blocks
                      start.df <- 1
                      end.df <- blocks$nrows[i]*nncol
                    }else{
                      ##  Indicate what row of the output data.frame this block 
                      ##  should start and end at:
                      start.df <- nncol*blocks$row[i] - nncol + 1
                      end.df <- (nncol*blocks$row[i] + blocks$nrows[i]*nncol) - nncol
                      end.df-start.df
                    }
                    ##  How many rows/cells is this:
                    end.df-start.df
                    ##  Fill in the cell indices we are working with:
                    df <- data.frame(CellIndex = as.numeric(start.df:end.df) )
                    ##  Add the corresponding transition and region mask raster
                    ##  values to the data.frame:
                    df$transition_raster <- as.numeric(row_data_transition_raster)
                    df$row_data_region_mask <- row_data_region_mask
                    
                    ##  Subset this data.frame to only include records where the
                    ##  transition raster and region mask raster values are not 
                    ##  NA:
                    df2 <- df[!is.na(df$transition_raster) & !is.na(df$row_data_region_mask),]
                    ##  Further subset the data.frame to only include records 
                    ##  where a transition occurred:
                    bsgm.table<- df2[df2$transition_raster == 1, ]
                    
                    ##  Return the data.frame of identified transition cell 
                    ##  indices for the given block:
                    return(bsgm.table)
                  }
  
  ##  End the cluster:
  stopCluster(cl)
  ##  Close the progress bar:
  close(pb)
  
  ## Note the completion time and print it to console:
  tEnd <-  Sys.time()
  print(paste("Elapsed Processing Time:", tmDiffc(tStart,tEnd)))
  
  ##  Return the data.frame of transition indices of ALL blocks:
  return(oper)
}

####################################################################################################
####################################################################################################
####################################################################################################


create_bsgm_table_2 <- function(transition_raster, 
                                region_mask, 
                                total_seach, 
                                minblocks=NULL, 
                                silent=FALSE) {
  ##  TODO:  CONFIRM WHAT THIS FUNCTION DOES.
  ##  This function processes the transition raster and regon mask to identify 
  ##  the cells which are known not to transition, i.e. have a value of 0. This 
  ##  function operates similar to the function create_bsgm_table_1.
  
  ##  Note the starting time:
  tStart <- Sys.time()
  
  ##  Inititate a data.frame to store block IDs, total blocks, and the count of 
  ##  blocks:
  bsgm.table.count <- data.frame(idBlock=integer(),
                                 BlockTotalCount=numeric(),
                                 BlockCount=numeric(),
                                 stringsAsFactors=FALSE)
  
  ##  Stack the transition raster and the region mask:
  cvr_stack <- stack(transition_raster,region_mask)
  
  ##  If no minimum number of blocks have been specified:
  if (is.null(minblocks)) {
    ##  Determine the minimum number using a function from the wpUtilities 
    ##  package:
    minblocks <- wpGetBlocksNeed(cvr_stack, cores=bsgm.cluster_workers, n=100)  
  }
  
  ##  Remove the covariate stack from memory:
  rm(cvr_stack)
  
  ##  Determine the number of blocks to break the process into:
  blocks <- blockSize(transition_raster, minblocks=minblocks)
  
  ##  Inititate a cluster based upon the given number of cores:
  cl <- makeCluster(bsgm.cluster_workers)
  ##  Register the snow backend with the foreach package:
  registerDoSNOW(cl)
  
  ##  Export the transition raster, regon mask raster, the data.frame to store 
  ##  cell indices, and the block object to every core of the cluster:
  clusterExport(cl, c("transition_raster", "region_mask","bsgm.table.count"),
                envir=environment())
  clusterExport(cl, "blocks", envir=environment())
  
  ##  Create a text-based progress bar:
  pb <- txtProgressBar(min = 1, max = blocks$n, style = 3, width = 80)
  ##  Define a function for updating the progress bar based upon the blocks 
  ##  being processed;  see create_bsgm_table_1 function for comments regarding 
  ##  this subfunction:
  progress <- function(n) {
    ch.pb <- unlist(lapply(1:bsgm.cluster_workers, function(i) {return(i*round(blocks$n/bsgm.cluster_workers))}), use.names=FALSE)
    if (n %in% ch.pb & !silent) { 
      setTxtProgressBar(pb, n)
    }else if(n==blocks$n & !silent){
      setTxtProgressBar(pb, n)
    }
  }
  opts <- list(progress = progress)
  
  ##  For every block, perform the task detailed in the '%dopar%' block:
  oper <- foreach(i=1: blocks$n ,
                  .combine=rbind,
                  .inorder=TRUE,
                  .packages='raster',
                  .multicombine=TRUE,
                  .options.snow = opts) %dopar% 
                  {
                    ##   Retrieve the raster values of the transition and region 
                    ##   raster corresponding to the block we are currently 
                    ##   working with:
                    row_data_transition_raster <- getValues(transition_raster,
                                                            row=blocks$row[i],
                                                            nrows=blocks$nrows[i])
                    row_data_region_mask <- getValues(region_mask,
                                                      row=blocks$row[i],
                                                      nrows=blocks$nrows[i])
                    ##  Determine the number of columns in the transition 
                    ##  raster:
                    nncol <- ncol(transition_raster)
                    
                    ##  If this is the first block:
                    if (i==1){
                      ##  The first row of this block's data.frame corresponds 
                      ##  to cell index 1:
                      start.df <- 1
                      ##  The last row of this block's data.frame corresponds to 
                      ##  the following cell index:
                      end.df <- blocks$nrows[i]*nncol
                    }else{
                      ##  The first row of this block's data.frame corresponds to 
                      ##  the following cell index:
                      start.df <- nncol*blocks$row[i] - nncol + 1
                      ##  The last row of this block's data.frame corresponds to 
                      ##  the following cell index:
                      end.df <- (nncol*blocks$row[i] + blocks$nrows[i]*nncol) - nncol
                    }
                    ##  Populate the cell index column based upon the relative
                    ##  position of the block:
                    df <- data.frame(CellIndex = as.numeric(start.df:end.df) )
                    ##  Retrieve the corresponding transition and region mask 
                    ##  raster values:
                    df$transition_raster <- as.numeric(row_data_transition_raster)
                    df$row_data_region_mask <- row_data_region_mask
                    
                    ##  Subset this data.frame to only records that do not have 
                    ##  NA values:
                    df2 <- df[!is.na(df$transition_raster) & !is.na(df$row_data_region_mask),]
                    
                    ##  Further subset them to records which were not considered 
                    ##  to have undergone transition, i.e. value of 2 in the 
                    ##  transition raster:
                    bsgm.table <- df2[df2$transition_raster == 2, ]
                    
                    ##  Retrieve the number of rows that remain in this subset 
                    ##  data.frame:
                    cnt2 <- nrow(bsgm.table)
                    
                    ##  If the count is greater than zero:
                    if ( cnt2 > 0 ){
                      ##  Declare the number of records which have values of 2 
                      ##  or 3:
                      cnt <- nrow(df2[df2$transition_raster == 2 | df2$transition_raster == 3, ])
                    }else{
                      ##  Otherwise declare the count as zero:
                      cnt <- 0
                    }
                    ##  Fill in columns relating to information about the block 
                    ##  itself:
                    df.rt <- data.frame(idBlock=i, BlockTotalCount=cnt,
                                        BlockCount=cnt2)
                    ##  Modify the column names:
                    colnames(df.rt) <- c("idBlock", "BlockTotalCount",
                                         "BlockCount")
                    ##  Return the blocks processed data.frame:
                    return(df.rt)
                  }
  ##  Close the cluster:
  stopCluster(cl)
  ##  Close the progress bar:
  close(pb)
  
  ##  Note the end time and print the elapsed time to console:
  tEnd <-  Sys.time()
  print(paste("Elapsed Processing Time first part of create_bsgm_table_2: ",
              tmDiffc(tStart,tEnd)))
  
  ####
  ##  Begin to identify the cells of non-transition.
  ##
  ## Note the start time:
  tStart <- Sys.time()
  
  ##  Determine the number of blocks which do not have any cells of 
  ##  non-transition:
  table_count_srch <- oper[oper$BlockCount > 0, ]
  ##  Determine the total number of cells which need to be identified, 
  ##  excluding NAs:
  total.count_srch <- sum(as.numeric(table_count_srch$BlockTotalCount),na.rm=TRUE)
  ##  Determine the percent of cells that need to be identified:
  table_count_srch$per <- (table_count_srch$BlockTotalCount*100)/total.count_srch
  table_count_srch$srch <- round((table_count_srch$per*total_seach)/100)  
  
  
  print(paste0("Inside 2 ", nrow(table_count_srch)))
  
  ##  Inititate the cluster with the specified number of cores:
  cl <- makeCluster(bsgm.cluster_workers)
  ##  Register the snow backend with foreach:
  registerDoSNOW(cl)
  ##  Export the transition raster, the region mask, the derived values for 
  ##  identification and the blocks object:
  clusterExport(cl, c("transition_raster", 
                      "region_mask",
                      "table_count_srch",
                      "total_seach"), envir=environment())
  clusterExport(cl, "blocks", envir=environment())
  
  ##  Start a progress bar:
  pb <- txtProgressBar(min = 1, max = blocks$n, style = 3, width = 80)
  
  
  oper.nt <- foreach(i=1: blocks$n ,
                     .combine=rbind,
                     .inorder=TRUE,
                     .packages=c('raster','dplyr'),
                     .multicombine=TRUE,
                     .options.snow = opts) %dopar% {
                       ##  If the identified number to find in this block was 
                       ##  greater than zero:
                       if ( nrow(table_count_srch[table_count_srch$idBlock == i,]) > 0 ) {
                         ##  Retrieve the values of the transition and region 
                         ##  mask raster:
                         row_data_transition_raster <- getValues(transition_raster,
                                                                 row=blocks$row[i],
                                                                 nrows=blocks$nrows[i])
                         row_data_region_mask <- getValues(region_mask,
                                                           row=blocks$row[i],
                                                           nrows=blocks$nrows[i])
                         ##  Retrieve the number of columns in the transition 
                         ##  raster:
                         nncol <- ncol(transition_raster)
                         ##  If this is the first iteration:
                         if (i==1){
                           start.df <- 1
                           end.df <- blocks$nrows[i]*nncol
                         }else{
                           start.df <- nncol*blocks$row[i] - nncol + 1
                           end.df <- (nncol*blocks$row[i] + blocks$nrows[i]*nncol) - nncol
                         }
                         ##  Populate the CellIndices for the block and assign 
                         ##  the corresponding transition and region raster 
                         ##  values:
                         df.nt <- data.frame(CellIndex = as.numeric(start.df:end.df))
                         df.nt$transition_raster <- as.numeric(row_data_transition_raster)
                         df.nt$row_data_region_mask <- row_data_region_mask
                         
                         ##  Exclude records which are NA or are not a value of 
                         ##  2 or 3:
                         df2.nt <- df.nt[!is.na(df.nt$transition_raster) & !is.na(df.nt$row_data_region_mask),]
                         df2_land.nt <- df2.nt[df2.nt$transition_raster == 3 | df2.nt$transition_raster == 2, ]
                         ##  Update the percent search:
                         per.search <- filter(table_count_srch, idBlock == i)$srch
                         ##  Sample a number of the records based upon the 
                         ##  per.search value:
                         df2_land_filter.nt <- sample_n(df2_land.nt, 
                                                        per.search,
                                                        replace=FALSE)
                         
                         ##  If the transition raster values are greater than 
                         ##  zero, replace them with zeros to align with the 
                         ##  original BSGM framework of 1 and 0 to indicate 
                         ##  transitions and non-transitions:
                         df2_land_filter.nt$transition_raster[df2_land_filter.nt$transition_raster > 0 ]  <- 0
                         ##  Rename the columns:
                         colnames(df2_land_filter.nt) <- c("CellIndex", 
                                                           "transition_raster", 
                                                           "row_data_region_mask")
                         ##  Retrun the data.frame:
                         return(df2_land_filter.nt)
                       }else{
                         return(NULL)
                       }
                     }
  
  ##  Close the cluster and progress bar:
  stopCluster(cl)
  close(pb)
  
  ##  Note the end time and output the elapsed time to console:
  tEnd <-  Sys.time()
  print(paste("Elapsed Processing Time:", tmDiffc(tStart,tEnd)))
  
  return(oper.nt)  
}

#####################################################################
#####################################################################

calcPredictTransition_prl <- function(covariate_sample_stack,
                                      region_mask) {
  ##  This function compiles the identified transition and non-transition cells,
  ##  determines how many should be sampled, samples that number, extracts and
  ##  formats the covariate values of those sampled cells, fits the initial 
  ##  random forest model, and then fits the final random forest model.
  ####
  ##
  ##  If the table of identified transitioned cells exists:
  if(file.exists(paste(bsgm.output.path.countries.tmp, 
                       "bsgm.table.CellIndex_1.Rdata", 
                       sep="")) ){
    ##  Load it:
    load(paste(bsgm.output.path.countries.tmp, 
               "bsgm.table.CellIndex_1.Rdata", 
               sep=""))				  
    
  }else{
    ##  Retrieve the cell indices of cells which transitioned:
    bsgm.table.CellIndex_1 <- create_bsgm_table_1(raster(paste0(bsgm.output.path.countries.tmp,
                                                                bsgm.countries.tag,
                                                                "_transition_123.tif")
    ), 
    region_mask)
    
    ##  Save that data.frame as a RData object:
    save(bsgm.table.CellIndex_1, 
         file=paste(bsgm.output.path.countries.tmp, 
                    "bsgm.table.CellIndex_1.Rdata", 
                    sep="")) 											  
  } 
  
  cat("\n")
  ##  Retrieve the total number of cells that underwent transition and print it 
  ##  to console:
  total.number.px.trnstn.cls <- nrow(bsgm.table.CellIndex_1) 
  print(paste0("Total number of transitioned cells ",total.number.px.trnstn.cls))
  
  ## Declare the total number of cells that did not undergo transition that we 
  ##  are going to select:
  total.number.px.req.cls <- total.number.px.trnstn.cls
  
  ##  Determine the total number of pixels in the study area:
  total.number.px.rst <- wpGetTotalNumberPx(raster(t0extentsPathFileName))
  
  ##  If transition pixels are more then 10% of the total number of pixels:
  if ( (nrow(bsgm.table.CellIndex_1) * 100)/total.number.px.rst > 10   ) {
    
    cat("\n")
    print(paste0("Transitions make up more then 10% of all study area pixels."))
    print(paste0("Sampling all transitions..."))
    cat("\n")  
    ##  Determine how many cells we need to sample:
    ten.per.px.frm.total <- round((total.number.px.rst * 10)/100)
    ##  Sample that number of records from the identified transition cells:
    bsgm.table.CellIndex_1 <- as.data.table(sapply(bsgm.table.CellIndex_1[],
                                                   sample,
                                                   ten.per.px.frm.total))
    ##  Declare the number of pixels sampled that way we know how many to sample 
    ##  of the non-transitions:
    total.number.px.req.cls <- ten.per.px.frm.total
  } 
  
  ##  If the table of cells identified as having not transitioned exists:
  if(file.exists(paste(bsgm.output.path.countries.tmp, 
                       "bsgm.table.CellIndex_0_NA.Rdata", 
                       sep="")) ){
    ##  Load it:
    load(paste(bsgm.output.path.countries.tmp, 
               "bsgm.table.CellIndex_0_NA.Rdata", 
               sep=""))				  
  }else{
    ##  Otherwise identify those cells:
    bsgm.table.CellIndex_0_NA <- create_bsgm_table_2(raster(paste0(bsgm.output.path.countries.tmp,
                                                                   bsgm.countries.tag,
                                                                   "_transition_123.tif")
    ), 
    region_mask,
    total.number.px.req.cls)
    
    ##  And save the resulting data.frame as an RData object:
    save(bsgm.table.CellIndex_0_NA,
         file=paste(bsgm.output.path.countries.tmp,
                    "bsgm.table.CellIndex_0_NA.Rdata",
                    sep="")) 		
  }  
  
  
  # Remove any NA values from the tables, should they exist:
  bsgm.table.CellIndex_0 <- bsgm.table.CellIndex_0_NA[complete.cases(bsgm.table.CellIndex_0_NA), ]
  bsgm.table.CellIndex_0 <- bsgm.table.CellIndex_0[rowSums(is.na(bsgm.table.CellIndex_0))==0,]
  ##  Merge the identified cells of transition and non-transition:
  bsgm.table.CellIndex <- (merge(bsgm.table.CellIndex_1,
                                 bsgm.table.CellIndex_0,
                                 all=TRUE))
  
  ##  Retrieve the name of covariates in the covariate stack that exists:
  cov_names.tmp <- names(covariate_sample_stack)
  
  ##  Remove the now unnecessary dataframes from memory:
  rm(bsgm.table.CellIndex_0,
     bsgm.table.CellIndex_1,
     bsgm.table.CellIndex_0_NA)
  
  ##  If the sampled covariate value table exists:
  if(file.exists(paste(bsgm.output.path.countries.tmp, 
                       "bsgm_table_x.Rdata", 
                       sep="")) ){
    ##  Load it:
    load(paste(bsgm.output.path.countries.tmp, 
               "bsgm_table_x.Rdata", 
               sep=""))				  
  }else{
    ##  Otherwise, begin the extraction process of covariate values at the 
    ##  locations of our identified samples.
    ##
    ##  If we cannot perform this extraction using RAM resources, as determined 
    ##  by the canProcessInMemory function:
    if (!canProcessInMemory(covariate_sample_stack,
                            max(nlayers(covariate_sample_stack)) * 2)) {
      ##  Carry out the extraction in parallel:
      cat("\n")
      print(paste0("Start extracting balue based on CellIndex in parallel mode"))
      cat("\n")
      
      ##  Inititate the cluster with 4 cores (this is done because of typical 
      ##  memory limits and the size of the covariate stack which must be copied 
      ##  to each and every core):
      cl <- makeCluster(4)
      ##  Register the DoParallel backend with the foreach package:
      registerDoParallel(cl)
      ##  Export the names of covariates, the covariate stack, and the cell 
      ##  indices data.frame to every core:
      clusterExport(cl, "cov_names.tmp", envir=environment())
      clusterExport(cl, "covariate_sample_stack", envir=environment())
      clusterExport(cl, "bsgm.table.CellIndex", envir=environment())
      
      ##  For every covariate:
      ret.extract <- foreach(i = 1:length(cov_names.tmp),
                             .packages=c('data.table','raster')) %dopar% 
                             {
                               ##  Do the following:
                               ##  Extract the covariate values as a data.frame,
                               ##  keeping track of the CellIndex from which it 
                               ##  came:
                               x <-  as.data.table(extract(covariate_sample_stack@layers[[i]],
                                                           bsgm.table.CellIndex$CellIndex))
                               ##  Set the names of the data.frame:
                               setnames(x, "V1",cov_names.tmp[i])
                             }
      ##  Bind each covariate extraction to a main table as a new column:
      bsgm.table.x <- do.call('cbind', ret.extract)
      
      ##  Close the cluster:
      stopCluster(cl)
    }else{
      cat("\n")
      print(paste0("Start extracting balue based on CellIndex in a loop"))
      cat("\n")
      ##  For every covariate:
      for( i in 1:length(cov_names.tmp)){
        
        ##  If this is the first covariate to be processed:
        if (i==1) {
          ##  Extract the covariate values, keeping track ofthe CellIndices, and 
          ##  place them in a new data.table:
          bsgm.table.x.fst.cl.nm <- 
            bsgm.table.x <- setNames(data.table(extract(covariate_sample_stack@layers[[i]], 
                                                        bsgm.table.CellIndex$CellIndex) ),
                                     cov_names.tmp[[i]])
        }else{
          ##  Extract the covariate values, keeping track ofthe CellIndices, and
          ##  add them to the data.table as a new column:
          bsgm.table.x[[cov_names.tmp[i]]] <- extract(covariate_sample_stack@layers[[i]], 
                                                      bsgm.table.CellIndex$CellIndex)
        }
      } 
    }
  }  
  
  ##  If any NA values were extracted in the process fro mthe covariate rasters:
  if ( any(is.na(bsgm.table.x)) == TRUE ) {
    
    print(paste0("There is/are some NA value(s) in the bsgm.table.x."))
    print(paste0("Removing them..."))
    ##  Remove the corresponding rows:
    cells.na <- bsgm.table.x[!complete.cases(bsgm.table.x), which = TRUE ] 
    bsgm.table.x <- bsgm.table.x[,][!cells.na]
    ##  Make sure it is still a data.table:
    setDT(bsgm.table.CellIndex)
    ##  Remove the corresponding cellindices from the "y_data":
    bsgm.table.CellIndex <- bsgm.table.CellIndex[,][!cells.na]
    bsgm.table.CellIndex<-(as.data.frame(bsgm.table.CellIndex))
  } 
  
  ##  Create the final covariate stack that will be used in predicting:
  covariate_stack <- create_final_raster_stack()
  
  ##  Rename the layers to the shortened covariate names:
  names(covariate_stack) <- c(names(covariates), "region_mask")
  ##  Pull the transition indicator values as a vector:
  y_data <<- bsgm.table.CellIndex[[2]]
  ##  Transform it into a factor:
  y_data <<- factor(y_data, levels = c(0,1), labels = c("0","1"))
  
  ##  Declare the names of our predictor covariates to be used, e.g. exlucdes 
  ##  region mask as being used for prediction:
  fixed_predictors <- names(bsgm.table.x)[1:length(covariates)] 
  ##  Pull the values of our predictive covariates:
  x_data <<- bsgm.table.x[,fixed_predictors, with = FALSE]
  ##  Estimate the intial BSGM Random Forest (RF):
  bsgmfit <<- get_bsgmfit()
  ##  Determine the final BSGM RF:
  bsgmfit_final <<- get_bsgmfit_final()
  ##  REmove the memory heavy proximity information:
  bsgmfit_final$proximity <<- NULL
}  