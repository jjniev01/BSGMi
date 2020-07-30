interpolateBSPOP <- function(i){
  ##  Retrieve GID:
  g <- gid_list[i]
  
  ##  Make sure our input object is a data.table:
  out_df <- copy(data.table(pop_df[pop_df$GID == g,]))
  ##  For every period, fit a logistic growth curve:
  for(p in unique(out_df[YEAR %in% year_seq,]$PERIOD)){
    ##  Determine the "t" values of each year, i.e. we are setting our origin arbitrarily to t0:
    out_df[PERIOD==p | YEAR == period_list[[p]][2],
           t := YEAR - min(YEAR)]
    
    ##  Pre-transform the BS.POP counts to fit a logistic growth curve to:
    ##  WARNING:  Here we are essentially taking the log of the urban rural ratio so
    ##            we will need to keep all of our safeguards in to avoid insanely 
    ##            large URR values when there are near saturation or saturation 
    ##            values.
    ##  NOTE:  Will induce -Inf values for years which were not observed, 
    ##         initially.
    out_df[PERIOD==p | YEAR == period_list[[p]][2],
           BS.POP.TRANS := log({BS.POP/(POP-BS.POP)},
                               base = exp(1))]
    
    ##  If we have a "zero" at one or more of the observed points, BUT not 
    ##  all, then supplant it with a small offset:
    out_df[((PERIOD==p & YEAR %in% observed_years) |
             YEAR== period_list[[p]][2]) &
             !is.finite(BS.POP.TRANS) &
             POP != 0, 
           BS.POP.TRANS := log({1/{POP-1}}, base = exp(1))]
    ##  Fit a linear model based upon those observed values with the corresponding t 
    ##  value:
    foo_lin <- lm(BS.POP.TRANS ~ t, 
                  data=out_df[(PERIOD==p & YEAR %in% observed_years)|
                                YEAR==period_list[[p]][2]])
    ##  Retrive the c and r values
    c_value <- foo_lin$coefficients[[1]]
    r_value <- foo_lin$coefficients[[2]]
    ##  Extract the proportional growth rate r and the constant c_value
    out_df[PERIOD==p, PGR := r_value]
    out_df[PERIOD==p, C.VALUE := c_value]
    
    
    ##  Predict the BS.POP.TRANS values for unobserved years between 0 and T:
    out_df[PERIOD==p & YEAR %in% setdiff(year_seq,observed_years),
           BS.POP.TRANS :=  predict(foo_lin,
                                    newdata = data.frame("t"=out_df[PERIOD==p & YEAR %in% setdiff(year_seq,observed_years),]$t))]
  }
  ##  Back transform those values into the actual BS.POP counts:
  out_df[YEAR %in% setdiff(year_seq,observed_years),
         BS.POP := {POP*exp(BS.POP.TRANS)}/{1+exp(BS.POP.TRANS)}]
    
  return(as.data.frame(out_df[YEAR %in% setdiff(year_seq,observed_years),
                              c("GID","YEAR","t","BS.POP","PGR","C.VALUE")]))
}


cluster_BSPOP <- function(pred_df,...){
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
                      "interpolateBSPOP",
                      "period_list"))
  
  ##	Start all nodes on a prediction:
  for (i in 1:nodes) {
    ##  Send the taskMaker function call to the ith node with ith task from
    ##  the prediction_list as an argument and tag it with the value i
    sendCall(cl[[i]], interpolateBSPOP, i, tag=i)
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
      sendCall(cl[[predictions$node]], interpolateBSPOP, ni, tag=ni)
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
