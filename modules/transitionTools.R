##  THIS FILE CONTAINS FUNCTIONS USED FOR THE TRANSITIONING OF CELLS FOR OUTPUT 
##  OF ANNUAL BS EXTENTS
##  TITLE:  transitionTools .R
##  AUTHOR:  JEREMIAH J. NIEVES
##  VERSION: 1.0
##  LAST UPDATE: 2017-06-02
##  NOTES:  
##
##
##
##  -----------------------------------------------------------------------  ##
bsInterpolate <- function(task_item){
  ##  This function is designed to work on a single task unit within a
  ##  cluster where a task unit is a single list of vectors from the
  ##  list of lists created by the preClusterProcess function.
  ##  Carries out the parallellized interpolation process that
  ##  determines which indices will be transitioned for the year and
  ##  returns a vector of indices to be handled by another function which
  ##  carries out the actual 'transition' pf values within a raster before
  ##  writing the transitioned raster (see function bsTransition() ).
  ##
  ##  Parameters:
  ##  -  task_item which is a list of three vectors:
  ##     i)  admin unit indices
  ##     ii)  transition probabilities corresponding to those indices
  ##     iii)  the number of transitions which are to take place in that unit
  ##
  ##  Values:
  ##  -  Vector of cell indices to transition for the admin unit associated
  ##     with the task item
  ##
  ##  ---------------------------------------------------------------------  ##
  ##  Retrieve the number of cells to convert for that year transition:
  n_convert <- task_item[[3]]
  
  print(paste0("    ", n_convert, " cells to transition."))
  ##  Retrieve all the cell indices from the task item:
  print(paste0("    Pulling transition cell indices from task item..."))
  admin_index <- task_item[[1]]
  
  
  
  ##  Pull the corresponding probabilities of those indices:
  print("        Pulling corresponding probabilities...")
  prob <- task_item[[2]]
  
  ##  Create a psuedo index for the transition probabilties vector:
  vi <- 1:length(prob)
  
  ##  Randomize the indices so we dont get any left-right, up-down bias:
  rvi <- sample(vi, length(vi), replace = FALSE)
  
  ##  Reorder the probabilities and the cell indices by the randomized
  ##  vector indices:
  randomized_trans_prob <- prob[rvi]
  randomized_ind_trans <- admin_index[rvi]
  
  ##  Properly sort probabilities in descending order and return the index
  ##  based upon the indices from the randomized probabilities vector:
  sort_indices <- sort(randomized_trans_prob,
                       decreasing = TRUE,
                       index.return = TRUE)[[2]]
  
  ##  Extract the probabilities:
  sorted_trans_probabilities <- randomized_trans_prob[sort_indices]
  sorted_trans_indices <- randomized_ind_trans[sort_indices]
  
  ##  Pull the correct number of indices to transition for the year:
  indices_convert <-data.frame("CONV"=numeric(length = n_convert))
  indices_convert$CONV <- sorted_trans_indices[1:n_convert]
  
  ##  Return the indices:
  return(indices_convert)
}

##  -----------------------------------------------------------------------  ##




bsTransitionMap <- function(init_BS_raster_path, trans_vector, output_name){
  ##  This function takes a vector of transition indices and changes the values
  ##  of the cells with those indices, in the init_BS_raster, to '1', 
  ##  indicating that they transitioned from non-built-settlement to settlement
  ##
  ##  Parameters:
  ##  -  init_BS_raster_path is the path to the built settlement extent raster 
  ##     for the time period immediately preceeding the year being modeled
  ##  -  trans_vector is a numeric vector of cell indices which should be 
  ##     transitioned; these indices are compiled from the bsInterpolate 
  ##     function which was run in parallel over all admin units which had 
  ##     growth in the year being modeled.
  ##  -  output_name is the name to give to the output raster, including file 
  ##     type suffix, but not the full path
  ##
  ##  Values:
  ##  -  Writes an output raster to file at the predetermined output path
  ##
  ##  ---------------------------------------------------------------------  ##
  ##  Bring in the raster:
  init_bs_ras <- raster(init_BS_raster_path)
  
  ##  Transition the indices:
  init_bs_ras[trans_vector] <- 1
  
  ##  Write the raster:
  print(paste0("Writing predicted urban extents to ", bsgm.output.path.countries))
  writeRaster(init_bs_ras,
              filename = paste0(bsgm.output.path.countries, output_name),
              format = "GTiff",
              datatype = "INT1U",
              overwrite = TRUE,
              options = "COMPRESS = LZW")
  print("Writing Complete!")
  
  rm(init_bs_ras)
  gc()
}

##  -----------------------------------------------------------------------  ##




clusterTasker <- function(task_list,...){
  ##  Description:
  ##  Create a function which creates a cluster process which will be used for 
  ##  the interpolation and extrapolation phases in which we iterate and 
  ##  retrieve the indices of cells which are predicted to transition. This is 
  ##  based upon the cluster framework created by Forest R. Stevens for the 
  ##  WorldPop population model. https://github.com/ForrestStevens/WorldPop-RF
  ##
  ##  Parameters
  ##
  ##  Values
  ##
  ##
  ##  ---------------------------------------------------------------------  ##
  ##	Start the timer:
  tStart <- Sys.time()
  
  ##	Pull the cluster:
  cl <- getCluster()
  on.exit( returnCluster() )
  
  ##	Determine the number of cores we're working with:
  nodes <- length(cl)
  
  ##	Pass off required libraries and data to the cluster workers:
  clusterEvalQ(cl, {
    require(raster)
  })
  ##  Pass off the required data and functions to the nodes in the cluster
  ##   - this includes the list of lists used for informing predictions, and the
  ##     task function which creates the predictions for each admin unit:
  
  clusterExport(cl, c("gid_list","admin_sub_df","grow_info"))
  
  taskMaker <- function(i){
    ##  This function is designed to work on a single task unit within a 
    ##  cluster where a task unit is a single list of vectors from the 
    ##  list of lists created by the preClusterProcess function.
    ##  Carries out the parallellized interpolation process that 
    ##  determines which indices will be transitioned for the year and 
    ##  returns a vector of indices to be handled by another function which
    ##  carries out the actual 'transition' pf values within a raster before
    ##  writing the transitioned raster (see function bsTransition() ).
    ##
    ##  Parameters:
    ##  -  task_item which is a list of three vectors:
    ##     i)  admin unit indices
    ##     ii)  transition probabilities corresponding to those indices
    ##     iii)  the number of transitions which are to take place in that unit
    ##
    ##  Values:
    ##  -  Vector of cell indices to transition for the admin unit associated 
    ##     with the task item
    ##
    ##  ---------------------------------------------------------------------  ##
    g <- gid_list[i]
    #print(paste0("     Unit ", g))
    admin_ind <- admin_sub_df$IND[admin_sub_df$GID == g]
    ##  Store the admin_ind and the corresponding probabilities in a list
    ##  within a listthe list under the character representation of the admin
    ##  id so we can retrieve them in our chunking of tasks:
    task <- list(admin_ind,
                 admin_sub_df$PROB[admin_sub_df$GID == g],
                 grow_info$EST.CHANGE[grow_info$GID == g])
    
    return(task)
  }
  
  
  
  ##	Start all nodes on a prediction:
  for (i in 1:nodes) {
    ##  Send the taskMaker function call to the ith node with ith task from
    ##  the prediction_list as an argument and tag it with the value i
    sendCall(cl[[i]], taskMaker, i, tag=i)
  }
  
  
  ##	Create our primary cluster processing loop, recalling that we already
  ##		have clusters running:
  cat("Total tasks to process: ", length(task_list), "\n")
  for (i in 1:length(task_list)) {
    ##	Receive results from a node:
    predictions <- recvOneData(cl)
    
    ##	Check if there was an error:
    if (!predictions$value$success) {
      stop("ERROR: Cluster barfed...\n\n", predictions)
    }
    
    ##	Which block are we processing:
    block <- predictions$value$tag
    #cat("Received block: ", block, "\n")
    #flush.console()
    
    ##	Now store our task item (list of three objects) in our task list:
    task_list[[i]] <- predictions$value$value
    
    
    ##	Check to see if we are at the end of our tasklist:
    ni <- nodes + i
    if (ni <= length(task_list)) {
      ##	And if not, send it to the cluster node that just gave us
      ##		our last result...
      sendCall(cl[[predictions$node]], taskMaker, ni, tag=ni)
    }
    tEnd <- Sys.time()
    wpProgressMessage(i,
                      max = length(task_list),
                      label = paste0("Received chunk ", ni,
                                     " Processing Time: ",
                                     wpTimeDiff(tStart,tEnd))
                      )
  }
  

  # print(paste("Total Elapsed Prediction Time:",
  #             proc.time()[3] - start_time, "seconds"))
  # flush.console()
  # if(logconsole){
  #   cat(paste0("Transition Process Total Elapsed time for ",t0," to ",t1,": ", proc.time()[3]-start_time, " seconds"),
  #       file = logfile,
  #       sep = "\n",
  #       append = TRUE)}
  
  ##  Return the cluster transition vector so we can change our raster map:
  return(task_list)
}