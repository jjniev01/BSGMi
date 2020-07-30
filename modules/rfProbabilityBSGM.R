##  THIS FILE CONTAINS FUNCTIONS WHICH ARE USED IN THE BSGM SCRIPT TO PREDICT 
##  THE PROBABILITY SURFACE
##  TITLE:  RFPROBABILITYBSGM .R
##  AUTHOR:  JEREMIAH J. NIEVES
##  VERSION: 1.0
##  LAST UPDATE: 2017-05-27
##  NOTES:  
##  -----------------------------------------------------------------------  ##
rfProbabilityEstimate <- function(estimate_RF = TRUE, 
                                  fixed_set = NULL, 
                                  cov_names){
  ##
  ##
  ##  PARAMETERS:
  ##
  ##
  ##  VALUES:
  ##
  ##  
  ##  Estimate Random Forest -------------------------------
  setwd(bsgm.output.path.countries)
  
  
  ##	Check to see whether we run the RF code at all, and if not we just
  ##		load existing bsgmfit_final.RData and bsgmfit_quant.RData that we
  ##		must have copied or created before into the output/XXX/tmp folder:
  if (estimate_RF) {
    ##	Set up response and covariate dataframes for the random forest modeling  -
    ##  Retrieve the response variable (i.e. transition):
    y_data <- bsgm.table[[2]]
    ##  Convert to a factor:
    y_data <- factor(y_data, levels = c(0,1), labels = c("0","1"))
    
    fixed_predictors <- get_fixed_predictors() 
    
    ##  Full covariate set, removing unnecessary covariates from x_data:
    x_data <- bsgm.table[,fixed_predictors, with = FALSE]
    
    ##	Subset x_data to remove NAs:
    #indexX <- complete.cases(x_data)

    ##	Subset y_data to remove any of interest:
    #indexY <- !is.na(y_data)

    ##	Subset data according to indices:
    #y_data <- y_data[indexX & indexY]
    #x_data <- x_data[indexX & indexY,]

    ##	Check our memory footprint just to make sure:
    #memory.size()
    
    
    if (file.exists(paste0(bsgm.output.path.countries.tmp, "init_bsgmfit.RData"))) {
      # logr(logconsole,
      #      log_file_path = logfile,
      #      message_string= "Tuning of our randomForest BSGM classification was done before. Loading init_bsgmfit.RData")
      load(file=paste0(bsgm.output.path.countries.tmp, "init_bsgmfit.RData")) 
      
    }else{
      ##	Now we are going tune our randomForest : 
      start_time = proc.time()[3]
      # init_bsgmfit = tuneRF(x=x_data,
      #                      y=y_data,
      #                      plot=TRUE, 
      #                      mtryStart=ncol(x_data)/3, 
      #                      trace=TRUE, 
      #                      doBest=TRUE, 
      #                      na.action=na.omit, 
      #                      importance=TRUE, 
      #                      proximity=FALSE, 
      #                      replace=TRUE)
      init_bsgmfit = tuneRF(x=x_data,
                           y=y_data,
                           plot=TRUE,
                           mtryStart=ncol(x_data)/3,
                           ntreeTry=500,
                           improve=0.0001,
                           stepFactor=1.20,
                           trace=TRUE, 
                           doBest=TRUE,
                           #nodesize=length(y_data)/750,
                           na.action=na.omit,
                           importance=TRUE,
                           proximity=FALSE,
                           sampsize=min(c(length(y_data), 1000)),
                           replace=TRUE)
      print(init_bsgmfit)
      #capture.output(init_bsgmfit, file = logfile, append = TRUE)
      # logr(logconsole,
      #      log_file_path=logfile,
      #      message_string = paste("Elapsed Initial RF Estimation Time:", 
      #                             proc.time()[3] - start_time, "seconds"))
      
      
      ##	Save off our init_bsgmfit object for this set of data:
      save(init_bsgmfit, file=paste0(bsgm.output.path.countries.tmp,
                                     "init_bsgmfit.RData"))
    }
    
    
    
    if (file.exists(paste0(bsgm.output.path.countries.tmp, "bsgmfit.RData"))) {
      load(file=paste0(bsgm.output.path.countries.tmp, "bsgmfit.RData"))	
      
    }else{
      
      if (is.null(fixed_set)) {
        print("Beginning iterative covariate selection process... ") 	
        
        ##	Now we will optimize the model by iteratively removing any 
        ##		covariates with negative increases in node purity:
        
        ##	Get list of covariates that have an importance score greater than 0:
        importance_scores <- importance(init_bsgmfit)[order(importance(init_bsgmfit)[,3], decreasing=TRUE),]
        pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
        
        if (length(pos_importance) == length(importance_scores[,1])) {
          
          x_data <- x_data[,pos_importance,with = FALSE]
          # bsgmfit = tuneRF(x=x_data,
          #                 y=y_data,
          #                 plot=TRUE, 
          #                 mtryStart=ncol(x_data)/3, 
          #                 trace=TRUE, 
          #                 doBest=TRUE, 
          #                 na.action=na.omit, 
          #                 importance=TRUE, 
          #                 proximity=FALSE, 
          #                 replace=TRUE)
          bsgmfit = tuneRF(x=x_data,
                          y=y_data,
                          plot=TRUE,
                          mtryStart=ncol(x_data)/3,
                          ntreeTry=500,
                          improve=0.0001, 
                          stepFactor=1.20,
                          trace=TRUE,
                          doBest=TRUE,
                          #nodesize=length(y_data)/750,
                          na.action=na.omit,
                          importance=TRUE,
                          proximity=FALSE,
                          sampsize=min(c(length(y_data), 1000)),
                          replace=TRUE)
          
        }else{
          
          while (length(pos_importance) < length(importance_scores[,1])) {
            ##	Subset our x_data to just those columns having positive scores:
            x_data <- x_data[,pos_importance, with = FALSE]
            
            #bsgmfit = tuneRF(x=x_data, y=y_data, plot=TRUE, mtryStart=length(x_data)/3, ntreeTry=length(y_data)/20, improve=0.0001, stepFactor=1.20, trace=TRUE, doBest=TRUE, nodesize=length(y_data)/1000, na.action=na.omit, importance=TRUE, proximity=FALSE, sampsize=length(y_data), replace=TRUE) 
            # bsgmfit = tuneRF(x=x_data,
            #                 y=y_data,
            #                 plot=TRUE, 
            #                 mtryStart=ncol(x_data)/3, 
            #                 trace=TRUE, 
            #                 doBest=TRUE, 
            #                 na.action=na.omit, 
            #                 importance=TRUE, 
            #                 proximity=FALSE, 
            #                 replace=TRUE)
            bsgmfit = tuneRF(x=x_data,
                            y=y_data,
                            plot=TRUE,
                            mtryStart=ncol(x_data)/3,
                            ntreeTry=500,
                            improve=0.0001,
                            stepFactor=1.20,
                            trace=TRUE,
                            doBest=TRUE,
                            #nodesize=length(y_data)/750,
                            na.action=na.omit,
                            importance=TRUE,
                            proximity=FALSE,
                            sampsize=min(c(length(y_data), 1000)),
                            replace=TRUE)
            
            ##	Re-check importance scores:
            importance_scores <- importance(bsgmfit)[order(importance(bsgmfit)[,3], decreasing=TRUE),]
            pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
            print(bsgmfit)
            
          } ## End of while loop
        }
        
        # logr(logconsole,
        #      log_file_path = logfile,
        #      message_string= paste0("Elapsed Iterative Fitting Time:", 
        #                             proc.time()[3] - start_time, "seconds"))
        
      }else{
        
        bsgmfit = randomForest(x=x_data, 
                              y=y_data, 
                              mtry=bsgmfit_final_old$mtry, 
                              ntree=bsgmfit_final_old$ntree, 
                              #nodesize=length(y_data)/750, 
                              importance=TRUE, 
                              proximity=TRUE)
        print(bsgmfit)
        
      }
      
      print("Saving  bsgmfit.RData... ")
      ##	Save off our bsgmfit object for this set of data:
      save(bsgmfit, file=paste0(bsgm.output.path.countries.tmp, "bsgmfit.RData"))
    }
    
    ###	Check our diagnostics:
    ###	Recall that running predict on the randomForest object will return OOB predictions:
    #
    #summary(bsgmfit)
    #plot(bsgmfit)
    #
    #predict(bsgmfit)
    #importance(bsgmfit)
    #
    ###	Get the variable names of the top 20 predictors:
    #names(importance(bsgmfit)[,"%IncMSE"][order(importance(bsgmfit)[,"%IncMSE"], decreasing=TRUE)])[1:20]
    #
    ###	Sort all covariates by they $IncMSE:
    #importance(bsgmfit)[order(importance(bsgmfit)[,1], decreasing=TRUE),]
    
    
    #varImpPlot(bsgmfit)
    #varUsed(bsgmfit)
    #summary(treesize(bsgmfit))
    #plot(bsgmfit)
    
    # ##	Partial Plots:
    # for (var_name in names(x_data)) {
    # 	eval( parse( text=
    # 		paste(
    # 			"partialPlot(x=bsgmfit, pred.data=x_data, x.var=\"",
    # 			as.character(var_name),
    # 			"\")"
    # 		, sep="")
    # 	))
    # }
    
    
    ##	For continuous regression, plot observed vs. predicted:
    #plot(x=y_data, y=predict(bsgmfit), ylim=c(min(y_data),max(y_data)), xlim=c(min(y_data),max(y_data)))
    #abline(a=0, b=1, lty=2)
    
    ###	For continuous regression, plot residuals vs. observed:
    #plot(x=y_data, y=(y_data - predict(bsgmfit)), xlim=c(0,max(y_data)))
    #abline(a=0, b=0, lty=2)
    #
    ###	For continuous regression, plot residuals vs. fitted:
    #plot(x=predict(bsgmfit), y=(y_data - predict(bsgmfit)), xlim=c(0,max(y_data)))
    #abline(a=0, b=0, lty=2)
    
    varImpPlot(bsgmfit)
    
    ##	Another alternative is to use Quantile Regression Forests to generate
    ##		prediction intervals.  We'll fit a quantile regression using
    ##		the tuning parameters pulled from the bsgmfit object above:
    set.seed(2011)
    bsgmfit_final <- randomForest(x=x_data, 
                                 y=y_data, 
                                 mtry=bsgmfit$mtry, 
                                 ntree=bsgmfit$ntree, 
                                 #nodesize=length(y_data)/500, 
                                 importance = TRUE,
                                 keep.inbag=TRUE,
                                 keep.forest = TRUE,
                                 proximity=FALSE,#proximity = TRUE,
                                 na.action = na.omit)
    set.seed(2011)
    bsgmfit_quant <- quantregForest(x=x_data, 
                                   y=y_data, 
                                   mtry=bsgmfit$mtry, 
                                   ntree=bsgmfit$ntree) 
    
    ##	Save off our bsgmfit object for this set of data:
    save(bsgmfit_final, file=paste0(bsgm.output.path.countries.tmp, "bsgmfit_final.RData"))
    #load(file=paste(bsgm.output.path.countries.tmp, "bsgmfit_final.RData", sep=""))
    save(bsgmfit_quant, file=paste0(bsgm.output.path.countries.tmp, "bsgmfit_quant.RData"))
    #load(file=paste(bsgm.output.path.countries.tmp, "bsgmfit_quant.RData", sep=""))
    
    
    ##	Last, to save on memory we don't have any need for the proximity 
    ##		matrix for prediction purposes, and for census data with many, many
    ##		units this proximity matrix can be extremely large.  We remove it
    ##		here from the bsgmfit_final object since this object will be 
    ##		duplicated across nodes of the cluster.  If you need it it is saved
    ##		with the object and can be load() from disk:
    bsgmfit_final$proximity <- NULL
    
    ##	END:	Estimate RandomForest model
    #####
  }
}




predictRF <- function(bsgmfit_obj_path){
  ##  Load the estimated model objects:
  load(file=bsgmfit_obj_path)
  #load(file=bsgmquant_obj_path)
  
  ##	Let's parallelize the process using the snow package:
  ##		NOTE also:  If you've changed the predictor set then you 
  ##			need to change the column renaming in the cluster_predict()
  ##			function and the subset of the RasterLayer objects in the
  ##			raster brick that gets created before the process is started...
  
  ##	Create the cluster process within a function:
  cluster_predict <- function(prediction_raster, quant_output=FALSE, ...) {
    ##	Start the timer:
    tStart <- Sys.time()
    
    ##	Pull the cluster:
    cl <- getCluster()
    on.exit( returnCluster() )
    
    ##	Determine the number of cores we're working with:
    nodes <- length(cl)
    
    ##	Generate a set of blocks on which to process the raster
    #blocks <- blockSize(prediction_raster, chunksize=200000, minblocks=nodes*4)
    blocks <- blockSize(prediction_raster, chunksize=100000, minblocks=nodes*4)
    
    #pb <- tkProgressBar(title = "Predicting Probability of Urban Transition:", min = 0, max = blocks$n, width = 300)
    
    ##	Pass off required libraries and data to the cluster workers:
    clusterEvalQ(cl, {
      require(raster)
      require(randomForest)
    })
    
    #clusterExport(cl, c("bsgmfit", "covariate_stack", "transY"))
    if (quant_output) {
      clusterExport(cl, c("bsgmfit_final", "bsgmfit_quant", "covariate_stack"))
    } else {
      clusterExport(cl, c("bsgmfit_final", "covariate_stack"))
    }
    
    ##	Since "blocks" only exists inside this function's environment, we need
    ##		to call environment() to get our current environment rather than the 
    ##		global environment assumed by clusterExport():
    clusterExport(cl, "blocks", envir=environment())
    clusterExport(cl, "quant_output", envir=environment())
    
    ##	Define the function that will be run on each cluster to do the predictions:
    clFun <- function (i) {
      
      row_data <- data.frame( getValues(covariate_stack,
                                        row=blocks$row[i],
                                        nrows=blocks$nrows[i]) )
      
      ##	Convert field names to something more manageable and that matches our bsgmfit variable list:
      
      ###	Full covariate stack:
      names(row_data) <- names(bsgmfit_final$forest$xlevels)
      
      ##	Detect if we have any NA or Inf values, and that the values are 
      ##		covered by our census administrative units:
      na_present <- apply(is.na(row_data), 1, any)
      inf_present <- apply(row_data == -Inf | row_data == Inf, 1, any)
      
      ##	Use the first if you want to mask out water pixels, this can greatly
      ##		speed up predictions over areas with a lot of water, however, you
      ##		run the risk of having no predictions in the resulting dataset
      ##		if you have a census block small enough that it might only have
      ##		water cover (GeoCover/GlobCover is what determines the water mask):
      roi_subset <- (!na_present & !inf_present & !region_mask)
      #roi_subset <- (!na_present & !inf_present & !census_mask)
      
      ##	Create a set of predictions based on our covariates:
      predictions <- numeric(length=length(row_data[,1]))
      predictions[] <- NA
      
      #predictions <- data.frame("rf_pred"=predictions, "rf_pred_alt"=predictions, "rf_sd"=predictions, "rf_cv"=predictions)
      #predictions <- data.frame("rf_pred"=predictions, "rf_sd"=predictions)
      predictions <- data.frame("rf_pred"=predictions, 
                                "rf_sd"=predictions, 
                                "rf_05"=predictions, 
                                "rf_50"=predictions, 
                                "rf_95"=predictions)
      
      ##	If we have data where NAs or Inf values are not present then we predict 
      ##  for those cells (where we subset our data according to the roi_subset 
      ##  and remove the census zone and water mask columns (length(row_data) - 
      ##  2):
      if (sum(roi_subset) > 0) {
        #predictions[roi_subset] <- predict(bsgmfit, newdata=row_data[roi_subset,1:(length(row_data)-2)])
        ##  Use the below to get probabilities:
        prediction_set <- predict(bsgmfit_final,
                                  newdata=row_data[roi_subset,1:length(row_data)],
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
        ##    Retrieve the predicted class response:
        #predictions$rf_pred[roi_subset] <- prediction$aggregate[,]
        
        
        #predictions$rf_pred_alt[roi_subset] <- apply(transY(prediction_set$individual, inverse=TRUE), MARGIN=1, mean)
        #predictions$rf_sd[roi_subset] <- apply(transY(prediction_set$individual, inverse=TRUE), MARGIN=1, sd)
        #predictions$rf_cv[roi_subset] <- (predictions$rf_sd[roi_subset] / predictions$rf_pred[roi_subset]) * 100
        
        ##  Convert the individual tree predictions to numeric:
        class(prediction_set$aggregate) <- "numeric"
        predictions$rf_sd[roi_subset] <- apply(prediction_set$individual, MARGIN=1, sd)
        
        if (quant_output) {
          prediction_set <- predict(bsgmfit_quant, 
                                    newdata=row_data[roi_subset,1:(length(row_data)-2)],
                                    quantiles=c(0.05, 0.50, 0.95))
          predictions$rf_05[roi_subset] <- prediction_set[,1]
          predictions$rf_50[roi_subset] <- prediction_set[,2]
          predictions$rf_95[roi_subset] <- prediction_set[,3]
        }
      }
      
      return(predictions)
    }
    
    ##	Start all nodes on a prediction:
    for (i in 1:nodes) {
      sendCall(cl[[i]], clFun, i, tag=i)
    }
    
    ##	Start the raster writer object so we can store our results as they
    ##		come back from our cluster:
    setwd(bsgm.output.path.countries)
    
    prediction_raster <- writeStart(prediction_raster, 
                                    filename=paste0("predict_probability", 
                                                    piece_text, ".tif"), 
                                    format="GTiff", 
                                    datatype="FLT4S", 
                                    overwrite=TRUE, 
                                    options=c("COMPRESS=LZW"))
    
    #prediction_raster_alt <- prediction_raster
    #prediction_raster_alt <- writeStart(prediction_raster_alt, filename=paste0("predict_density_alt", piece_text, ".tif"), format="GTiff", datatype="FLT4S", overwrite=TRUE, options=c("COMPRESS=LZW"))
    sd_raster <- prediction_raster
    sd_raster <- writeStart(sd_raster, 
                            filename=paste0("predict_probability_sd", 
                                            piece_text, ".tif"), 
                            format="GTiff", 
                            datatype="FLT4S", 
                            overwrite=TRUE, 
                            options=c("COMPRESS=LZW"))
    #cv_raster <- prediction_raster
    #cv_raster <- writeStart(cv_raster, filename=paste0("predict_density_cv", piece_text, ".tif"), format="GTiff", datatype="FLT4S", overwrite=TRUE, options=c("COMPRESS=LZW"))
    
    if (quant_output) {
      prediction_raster_05 <- prediction_raster
      prediction_raster_05 <- writeStart(prediction_raster_05, filename=paste0("predict_probability_05", piece_text, ".tif"), format="GTiff", datatype="FLT4S", overwrite=TRUE, options=c("COMPRESS=LZW"))
      prediction_raster_50 <- prediction_raster
      prediction_raster_50 <- writeStart(prediction_raster_50, filename=paste0("predict_probability_50", piece_text, ".tif"), format="GTiff", datatype="FLT4S", overwrite=TRUE, options=c("COMPRESS=LZW"))
      prediction_raster_95 <- prediction_raster
      prediction_raster_95 <- writeStart(prediction_raster_95, filename=paste0("predict_probability_95", piece_text, ".tif"), format="GTiff", datatype="FLT4S", overwrite=TRUE, options=c("COMPRESS=LZW"))
    }
    
    ##	Create our primary cluster processing loop, recalling that we already
    ##		have clusters running:
    cat("Total blocks to process: ", blocks$n, "\n")
    for (i in 1:blocks$n) {
      ##	Receive results from a node:
      predictions <- recvOneData(cl)
      
      ##	Check if there was an error:
      if (!predictions$value$success) {
        stop("ERROR: Cluster barfed...\n\n", predictions)
      }
      
      ##	Which block are we processing:
      block <- predictions$value$tag
      cat("Received block: ", block, "\n")
      flush.console()
      
      ##	Now store our predictions in our prediction
      #prediction_raster <- writeValues(prediction_raster, transY(predictions$value$value, inverse=TRUE), blocks$row[block])
      
      prediction_raster <- writeValues(prediction_raster, 
                                       predictions$value$value$rf_pred, 
                                       blocks$row[block])
      #prediction_raster_alt <- writeValues(prediction_raster_alt, predictions$value$value$rf_pred_alt, blocks$row[block])
      sd_raster <- writeValues(sd_raster, predictions$value$value$rf_sd, blocks$row[block])
      #cv_raster <- writeValues(cv_raster, predictions$value$value$rf_cv, blocks$row[block])
      
      if (quant_output) {
        prediction_raster_05 <- writeValues(prediction_raster_05, predictions$value$value$rf_05, blocks$row[block])
        prediction_raster_50 <- writeValues(prediction_raster_50, predictions$value$value$rf_50, blocks$row[block])
        prediction_raster_95 <- writeValues(prediction_raster_95, predictions$value$value$rf_95, blocks$row[block])
      }
      
      
      ##	Check to see if we are at the end of our block list:
      ni <- nodes + i
      if (ni <= blocks$n) {
        ##	And if not, send it to the cluster node that just gave us
        ##		our last result...
        sendCall(cl[[predictions$node]], clFun, ni, tag=ni)
      }
      
      setTkProgressBar(pb, i, label=paste( round(i/blocks$n*100, 0),"% done"))
    }
    
    prediction_raster <- writeStop(prediction_raster)
    
    #prediction_raster_alt <- writeStop(prediction_raster_alt)
    sd_raster <- writeStop(sd_raster)
    #cv_raster <- writeStop(cv_raster)
    
    if (quant_output) {
      prediction_raster_05 <- writeStop(prediction_raster_05)
      prediction_raster_50 <- writeStop(prediction_raster_50)
      prediction_raster_95 <- writeStop(prediction_raster_95)
    }
    
    close(pb)
    print(paste("Elapsed Prediction Time:", proc.time()[3] - start_time, "seconds"))
    flush.console()
    
    return(prediction_raster)
  }
  
  
  ##	Load the covariate raster stack of our cropped covariates:
  #covariate_stack_full <- stack(paste0(bsgm.output.path.countries.tmp,"covariatebrick_",region,"_",timeframe,".tif"))
  ##  Restore the layer names in proper order they were stacked from 01.1:
  #names(covariate_stack_full) <- c("Transition","DTEUrban","Pr0","Pr1","Pr5","Travel","Protected","Elevation","Slope","region_mask","water_mask")
  #names(covariate_stack_full) <- c("Transition","DTEUrban","Pr0","Pr1","Pr5","Travel","Protected","Elevation","Slope","region_mask","water_mask")
  ##  Gety subset of stack layers names:
  #cov_sub <- c(names(bsgmfit_final$forest$xlevels),"region_mask","water_mask")
  ##  Limit our modeling to those identified as important or are used as limiting parameters of the modeling:
  #covariate_stack <- covariate_stack_full[[cov_sub]]
  
  ##  Save memory by removing the full stack from mem:
  #rm("covariate_stack_full")
  #gc()
  ##	This block can be used to process subsets, especially if you're running
  ##		up against memory limits (leave piece_text defined as empty for 
  ##		file naming purposes):
  piece_text <- ""
  
  ##	Create a raster object based on our region data zones file to store our predictions:
  prediction_raster <- region_mask
  
  ###	Alternatively, could try a brick() instead of stack() for speed of 
  ###		extraction, however building the brick may take a very long time
  ###		as all raster covariates would need to be combined in memory...
  #raster_list <- eval(parse( text=
  #	paste(
  #		"list(",
  #			paste( c(names(bsgmfit$forest$xlevels), "census_mask", "water_raster"), collapse=","),
  #		")",
  #	sep="")
  #))
  #
  #covariate_stack <- brick(raster_list)
  
  
  beginCluster(n=bsgm.cluster_workers)
  prediction_raster <- cluster_predict(prediction_raster, quant_output=FALSE)
  endCluster()
  ##	END:	Predict for gridded covariates
  #####
  
}