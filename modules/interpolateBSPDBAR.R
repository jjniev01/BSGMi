interpolateBSPDBAR <- function(i){
  ##  Retrieve GID:
  g <- gid_list[i]
  
  ##  Make sure our input object is a data.table:
  out_df <- data.table(pop_df[pop_df$GID == g,])
  
  ##  Determine the "t" values of each year, i.e. we are setting our origin arbitrarily to t0:
  out_df[,t := YEAR - min(observed_years), by = GID]
  
  pred_df <- data.frame("t"= {observed_years - min(observed_years)}, 
                        "BS.PDBAR" = out_df[YEAR %in% observed_years,]$BS.PDBAR)
  
  ##  Determine if we are needing to use a linear or the natural spline function:
  if(observed_years == 2){
    ##  Figure out for what values of t we need to predict for:
    predict_t <- out_df$t[!(out_df$t %in% pred_df$t)]
    
    ##  Get the initial BSPDBAR value at t = 0:
    init_BSPD <- pred_df[pred_df$t == 0,"BS.PDBAR"]
    end_BSPD <- pred_df[pred_df$t == max(pred_df$t), "BS.PDBAR"]
    tot_time <- max(pred_df$t)
    ##  Fit a simple linear and predict:
    out_df[t %in% predict_t, BS.PDBAR := {{{end_BSPD - init_BSPD}/tot_time} * t + init_BSPD}]
  }
  
  if(observed_years != 2){
    ##  Determine vector of knots:
    k_vec <- out_df[YEAR %in% observed_years[observed_years != min(observed_years) & observed_years != max(observed_years)], t]
    foo_spline <- lm(BS.PDBAR ~ splines::ns(t, knots = k_vec), data = pred_df)
    ##  Figure out for what values of t we need to predict for:
    predict_t <- out_df$t[!(out_df$t %in% pred_df$t)]
    out_df[t %in% predict_t,
       BS.PDBAR := predict.lm(foo_spline, 
                              newdata = data.frame("t"=predict_t))]
    }
  
  return(as.data.frame(out_df[YEAR %in% setdiff(year_seq,observed_years),
                              c("GID","YEAR","BS.PDBAR")]))
}




cluster_BSPDBAR <- function(pred_df,...){
  ##  Description:
  ##  "Let's do the time warp aga-" seriously though,
  ##    Cluster task farm where each task is the model selection, fitting, 
  ##    of a series of model classes and the prediction of the mean, with 95%
  ##    intervals if applicable, and returning that to the main hadler as a 
  ##    dataframe where it is stored in a single dataframe before being output 
  ##    as a single dataframe for all years and given GIDs.
  ##
  ##  Parameters:
  ##
  ##  Values:
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
    require(data.table)
  })
  ##  Pass off the required data and functions to the nodes in the cluster
  ##   - this includes the lists used for informing predictions, and the
  ##     task functions that create the predictions/info for each subnational 
  ##     unit:
  clusterExport(cl, c("gid_list",
                      "pop_df",
                      "observed_years",
                      "year_seq",
                      "t0",
                      "t1",
                      "predict_length",
                      "interpolateBSPDBAR"))
  
  ##	Start all nodes on a prediction:
  for (i in 1:nodes) {
    ##  Send the taskMaker function call to the ith node with ith task from
    ##  the prediction_list as an argument and tag it with the value i
    sendCall(cl[[i]], interpolateBSPDBAR, i, tag=i)
  }
  
  
  ##	Create our primary cluster processing loop, recalling that we already
  ##		have clusters running:
  cat("Total tasks to process: ", length(gid_list), "\n")
  for (i in 1:length(gid_list)) {
    ##	Receive results from a node:
    predictions <- recvOneData(cl)
    
    ##	Check if there was an error:
    if (!predictions$value$success) {
      stop("ERROR: Cluster barfed...\n\n", predictions)
    }
    
    ##	Which block are we processing:
    block <- predictions$value$tag
    # cat("Received gid: ", block, "\n")
    # flush.console()
    
    ##	Now store our predictions in their proper rows of the data.frame:
    pred_df[{i+(i-1)*(predict_length-1)}:
    {i+i*{predict_length-1}},] <- predictions$value$value
    
    
    ##	Check to see if we are at the end of our tasklist:
    ni <- nodes + i
    if (ni <= length(gid_list)) {
      ##	And if not, send it to the cluster node that just gave us
      ##		our last result...
      sendCall(cl[[predictions$node]], interpolateBSPDBAR, ni, tag=ni)
    }
    tEnd <- Sys.time()
    wpProgressMessage(i,
                      max = length(gid_list),
                      label = paste0("Received chunk ", ni,
                                     " Processing Time: ",
                                     wpTimeDiff(tStart,tEnd)))
  }
  
  
  ##  Return the full df of results:
  return(pred_df)
}
