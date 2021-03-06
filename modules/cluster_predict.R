#####
##	BEGIN:	Predict for gridded covariates

##	Let's parallelize the process using the snow package:
##		NOTE:  The 00.1, alpha, script used the new parallel package
##			but it seems like this is a simpler way given that we have
##			to write out our results one block at a time...

##		NOTE also:  If you've changed the predictor set then you 
##			need to change the column renaming in the cluster_predict()
##			function and the subset of the RasterLayer objects in the
##			raster brick that gets created before the process is started...

cluster_predict <- function(prediction_raster, 
                            quant_output=FALSE,
                            nmb=1,	 
                            minblocks=NULL) {
  
  tStart <- Sys.time()

  # if (is.null(minblocks)) {
  #   minblocks <- wpGetBlocksNeed(prediction_raster, 
  #                                cores=bsgm.cluster_workers,
  #                                n=nmb)  
  # }
  
  
  cl <- getCluster()
  on.exit( returnCluster() )  
  
  nodes <- length(cl)
  blocks <- blockSize(prediction_raster,chunksize=100000, minblocks=nodes*4)
  loginfo(paste0("Covariate_stack will be divided to ",blocks$n,' blocks'))
  
  clusterEvalQ(cl, {
    require(raster)
    require(randomForest)
  })  
  
  if (quant_output) {
    clusterExport(cl, c("bsgmfit_final", "bsgmfit_quant", "covariate_stack"))
  } else {
    clusterExport(cl, c("bsgmfit_final", "covariate_stack"))
  }
  
  clusterExport(cl, "blocks", envir=environment())
  clusterExport(cl, "quant_output", envir=environment()) 
  
  #################################################################################
  #################################################################################
  ##	Define the function that will be run on each cluster to do the predictions:
  #
  call_predictions <- function (i) {
    
    row_data <- data.frame( getValues(covariate_stack, 
                                      row=blocks$row[i], 
                                      nrows=blocks$nrows[i]) )
    
    ## Convert field names to something more manageable and 
    ## that matches our bsgmfit variable list:
    ## Full covariate stack:
    #
    names(row_data) <- c(names(bsgmfit_final$forest$xlevels),"region_mask")
    
    ##	Detect if we have any NA or Inf values, and that the values are 
    ##		covered by our census administrative units:
    #
    na_present <- apply(is.na(row_data), 1, any)
    inf_present <- apply(row_data == -Inf | row_data == Inf, 1, any)
    region_mask_vec <- (is.na(row_data$region_mask))
    
    ## Use the first if you want to mask out water pixels, this can greatly
    ## speed up predictions over areas with a lot of water, however, you
    ## run the risk of having no predictions in the resulting dataset
    ## if you have a census block small enough that it might only have
    ## water cover (GeoCover/GlobCover is what determines the water mask):
    #
    roi_subset <- (!na_present & !inf_present & !region_mask_vec)
    
    
    ##	Create a set of predictions based on our covariates:
    #
    predictions <- numeric(length=length(row_data[,1]))
    predictions[] <- NA
    
    if (quant_output) {
      predictions <- data.frame("rf_pred"=predictions, 
                                "rf_sd"=predictions, 
                                "rf_05"=predictions, 
                                "rf_50"=predictions, 
                                "rf_95"=predictions)              
    }else{
      predictions <- data.frame("rf_pred"=predictions, 
                                "rf_sd"=predictions)              
    }
    
    
    ## I f we have data where NAs or Inf values are not present then we 
    ## predict for those cells (where we subset our data according to the 
    ## roi_subset and remove the census zone and water mask columns (length(row_data) - 2):
    #
    if (sum(roi_subset) > 0) {
      
      ##  Use the below to get probabilities:
      prediction_set <- predict(bsgmfit_final,
                                newdata=row_data[roi_subset,1:(length(row_data)-1)],
                                predict.all=TRUE, 
                                type = "prob")
      
      ##    Retrieve the probability of transition predictions from the prediction set:
      ##    Enable below for the proportion of votes, i.e. the probability of transition:
      predictions$rf_pred[roi_subset] <- prediction_set$aggregate[,2]
      ##  Use the below to get the binary classification response:
      # prediction_set <- predict(bsgmfit_final, 
      #                           newdata=row_data[roi_subset,1:(length(row_data)-2)],
      #                           predict.all=TRUE,
      #                           type = "response")
      #
      ##  Convert the individual tree predictions to numeric:
      class(prediction_set$aggregate) <- "numeric"
      predictions$rf_sd[roi_subset] <- apply(prediction_set$individual, MARGIN=1, sd)
      
      if (quant_output) {
        prediction_set <- predict(bsgmfit_quant, 
                                  newdata=row_data[roi_subset,1:(length(row_data)-1)],
                                  quantiles=c(0.05, 0.50, 0.95))
        predictions$rf_05[roi_subset] <- prediction_set[,1]
        predictions$rf_50[roi_subset] <- prediction_set[,2]
        predictions$rf_95[roi_subset] <- prediction_set[,3]
      }
    }
    
    return(predictions)
  } 
  #
  ##
  #################################################################################
  #################################################################################
  
  
  
  ##	Start all nodes on a prediction:
  for (i in 1:nodes) {
    sendCall(cl[[i]], call_predictions, i, tag=i)
  }  
  
  ## Start the raster writer object so we can store our results as they
  ## come back from our cluster:  
  #
  prediction_raster <- writeStart(prediction_raster, 
                                  filename=paste0(bsgm.output.path.countries, 
                                                  bsgm.predict.density.rf.pred), 
                                  format="GTiff", 
                                  datatype="FLT4S", 
                                  overwrite=TRUE, 
                                  options=c("COMPRESS=LZW"))
  
  
  sd_raster <- prediction_raster
  sd_raster <- writeStart(sd_raster, 
                          filename=paste0(bsgm.output.path.countries, bsgm.predict.density.rf.sd), 
                          format="GTiff", 
                          datatype="FLT4S", 
                          overwrite=TRUE, 
                          options=c("COMPRESS=LZW"))  
  
  
  if (quant_output) {
    prediction_raster_05 <- prediction_raster
    prediction_raster_05 <- writeStart(prediction_raster_05, 
                                       filename=paste0(bsgm.output.path.countries, bsgm.predict.density.rf.pred_05), 
                                       format="GTiff", 
                                       datatype="FLT4S", 
                                       overwrite=TRUE, 
                                       options=c("COMPRESS=LZW"))
    
    prediction_raster_50 <- prediction_raster
    prediction_raster_50 <- writeStart(prediction_raster_50, 
                                       filename=paste0(bsgm.output.path.countries, bsgm.predict.density.rf.pred_50), 
                                       format="GTiff", 
                                       datatype="FLT4S", 
                                       overwrite=TRUE, 
                                       options=c("COMPRESS=LZW"))
    
    prediction_raster_95 <- prediction_raster
    prediction_raster_95 <- writeStart(prediction_raster_95, 
                                       filename=paste0(bsgm.output.path.countries, bsgm.predict.density.rf.pred_95), 
                                       format="GTiff", 
                                       datatype="FLT4S", 
                                       overwrite=TRUE, 
                                       options=c("COMPRESS=LZW"))
  }
  
  
  
  ########################################################################
  ##
  ## Create our primary cluster processing loop, recalling that we already
  ## have clusters running:
  #
  
  
  for (i in 1:blocks$n) {
    
    ##	Receive results from a node:
    predictions <- recvOneData(cl)
    
    ##	Check if there was an error:
    if (!predictions$value$success) {
      stop("ERROR: Cluster barfed...\n\n", predictions)
    }
    
    ##	Which block are we processing:
    block <- predictions$value$tag
    
    
    prediction_raster <- writeValues(prediction_raster, 
                                     predictions$value$value$rf_pred, 
                                     blocks$row[block])
    sd_raster <- writeValues(sd_raster, 
                             predictions$value$value$rf_sd, 
                             blocks$row[block])
    
    
    if (quant_output) {
      
      prediction_raster_05 <- writeValues(prediction_raster_05, 
                                          predictions$value$value$rf_05, 
                                          blocks$row[block])
      
      prediction_raster_50 <- writeValues(prediction_raster_50, 
                                          predictions$value$value$rf_50, 
                                          blocks$row[block])
      
      prediction_raster_95 <- writeValues(prediction_raster_95, 
                                          predictions$value$value$rf_95, 
                                          blocks$row[block])
    }
    
    
    ##	Check to see if we are at the end of our block list:
    ni <- nodes + i
    if (ni <= blocks$n) {
      sendCall(cl[[predictions$node]], call_predictions, ni, tag=ni)
    }
    tEnd <-  Sys.time()
    
    wpProgressMessage(i, 
                      max=blocks$n, 
                      label= paste0("received block ",ni,
                                    " Processing Time: ",
                                    wpTimeDiff(tStart,tEnd)))
  }
  
  prediction_raster <- writeStop(prediction_raster)
  sd_raster <- writeStop(sd_raster)
  
  if (quant_output) {
    prediction_raster_05 <- writeStop(prediction_raster_05)
    prediction_raster_50 <- writeStop(prediction_raster_50)
    prediction_raster_95 <- writeStop(prediction_raster_95)
  }
  
  
  return(prediction_raster)
}  