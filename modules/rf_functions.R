######################################################################################
get_bsgmfit_final_old<- function() {
  ##  Function which retrieves previously constructed bsgmfit.RData objects.
  err_mess <- ""
  err_bool <- FALSE
  
  
  ##  Check if a bsgmfit object exists for combined countries (like NPL_BTN):
  ##    Declare the country tag from the fixed set parameter:
  countries.tag   <- as.character(bsgm.fixed.set[1]) 
  
  ##  For every country in the fixed set:
  # for ( i in 1:length(bsgm.fixed.set) ) {
  #   ##  If only one coutnry is specified in the fixed set, pass:
  #   if (i==1) next()
  #   ##  Construct a multi-country tag to search for the bsgmfit object:
  #   countries.tag <- paste(countries.tag, bsgm.fixed.set[i], sep = "_") 
  # }  
  ##  Declare the directory and path where we should find it:
  # bsgmfit.final.old.folder   <- paste(root_path, '/output/prj_', bsgm.input.year,
  #                                    '_',countries.tag, '/tmp/',sep="")
  bsgmfit.final.old.file.path   <- bsgm.fixed.set 
  
  ##  If it exists where we expect it to:
  if(file.exists(bsgmfit.final.old.file.path) ){
    
    logdebug(paste0('Loading old bsgmfit final from: ',bsgmfit.final.old.file.path))
    
    ##  Load it:
    local_env.bsgmfit_final = local({load(file=bsgmfit.final.old.file.path);environment()})
    bsgmfit.final.old <- local_env.bsgmfit_final$bsgmfit_final
    
    ##  Return it:
    return(bsgmfit.final.old)
    
  }else{
    err_mess <- paste0('Old bsgmfit final does not exist for: ',countries.tag,'\nPlease check that file exist in : ', bsgmfit.final.old.file.path)
  }
  
  ##  Load bsgmfit for each individual country if combined does not exist:
  ##  Select fixed countries excluding the current bsgm.input.countries:
  bsgm.fixed.set.without.input.countries <- bsgm.fixed.set[!bsgm.fixed.set %in%
                                                           bsgm.input.countries]
 
  ##  For every one of the input countries:
  for ( i in 1:length(bsgm.fixed.set.without.input.countries) ) {
    old_counry <-  as.character(bsgm.fixed.set.without.input.countries[i])
    
    ##  Declare where we should find it:
    bsgmfit.final.old.folder   <- paste(root_path, '/output/prj_', bsgm.input.year,'_',old_counry, '/tmp/',sep="")
    bsgmfit.final.old.file.path   <- paste0(bsgmfit.final.old.folder,'bsgmfit_final_prj_',bsgm.input.year,'_',old_counry,'.Rdata') 
    
    ##  If it exists:
    if(file.exists(bsgmfit.final.old.file.path)){
      ##  Load it:
      local_env.bsgmfit_final = local({load(file=bsgmfit.final.old.file.path); environment()})
      if ( i == 1 ){
        logdebug(paste0('Loading old bsgmfit final for: ',old_counry,' from ',bsgmfit.final.old.file.path))
        bsgmfit.final.old <- local_env.bsgmfit_final$bsgmfit_final

        #bsgmfit.final.old$proximity <- NULL
        #bsgmfit.final.old$predicted <- 0
        }else{
          #bsgmfit.final.old$proximity <- NULL
          #bsgmfit.final.old$predicted <- 0
          ##  Combine it with the other bsgmfit finals:
          logdebug(paste0('Combine old bsgmfit final for: ',old_counry,' from ',bsgmfit.final.old.file.path))
          bsgmfit.final.old <- combine(bsgmfit.final.old, local_env.bsgmfit_final$bsgmfit_final)
          }
    }else{
      err_mess <- paste0('Old bsgmfit final does not exist for: ',i,'\nPlease check that file exist in : ', bsgmfit.final.old.file.path)
      err_bool <- TRUE
    }    
  }  
  
  if (err_bool == FALSE) {
    
    return(bsgmfit.final.old)
    
  }else{

    stop(err_mess)  
    
  }
}

######################################################################################
## Set the fixed_set to existing countries if you are using an existing
##    set of randomForest objects to predict from:
#
set_fixed_set_to_existing_countries <- function() {
  
if (!is.null(bsgm.fixed.set)) {

  err_mess <- ""
  err_bool <- FALSE
  
  # first check bsgmfit for the combined countries like NPL_BTN
  #  
  if ( length(bsgm.fixed.set) > 1 ){

      countries.tag   <- as.character(bsgm.fixed.set[1]) 
  
      for ( i in 1:length(bsgm.fixed.set) ) {
        if (i==1) next()
        countries.tag <- paste(countries.tag, bsgm.fixed.set[i], sep = "_") 
      }  
  
        bsgmfit.final.old.folder   <- paste(root_path, '/output/prj_', bsgm.input.year,'_',countries.tag, '/tmp/',sep="")
        bsgmfit.final.old.file.path   <- paste0(bsgmfit.final.old.folder,'bsgmfit_final_prj_',bsgm.input.year,'_',countries.tag,'.Rdata') 
        bsgmfit.quant.old.file.path   <- paste0(bsgmfit.final.old.folder,'bsgmfit_quant_prj_',bsgm.input.year,'_',countries.tag,'.Rdata') 
                                         
                                         
      if( (file.exists(bsgmfit.final.old.file.path))  &  (file.exists(bsgmfit.quant.old.file.path)  )){
    
        logdebug(paste0('Laoding old bsgmfit final from: ',bsgmfit.final.old.file.path))
        logdebug(paste0('Laoding old quant final from: ',bsgmfit.quant.old.file.path))
        
        #loading bsgmfit_final in local enviremnt
        local_env.bsgmfit_final = local({load(file=bsgmfit.final.old.file.path); environment()})
        local_env.bsgmfit_quant = local({load(file=bsgmfit.quant.old.file.path); environment()})
    
        local_env.bsgmfit_final$bsgmfit_final$proximity <- NULL
        local_env.bsgmfit_final$bsgmfit_final$predicted <- 0
        local_env.bsgmfit_quant$bsgmfit_quant$predicted <- 0
    
        bsgmfit_final <<- local_env.bsgmfit_final$bsgmfit_final
        bsgmfit_quant <<- local_env.bsgmfit_quant$bsgmfit_quant
        
        #assign(bsgmfit_final, local_env.bsgmfit_final$bsgmfit_final, envir = .GlobalEnv) 
        #assign(bsgmfit_quant, local_env.bsgmfit_quant$bsgmfit_quant, envir = .GlobalEnv) 
    
      return(TRUE)
    
      }else{
        err_mess <- paste0('Old bsgmfit final does not exist for: ',countries.tag,'\nPlease check that file exist in : ', bsgmfit.final.old.file.path)
        err_bool <- TRUE
      }
  }
  # 
  ###########################################################   
  
  
  # load bsgmfit for the each countries if combined does not exist
  #
  tmpBool <- FALSE
  # first check if our main country in the list
  if ( length(bsgm.input.countries[bsgm.input.countries %in% bsgm.fixed.set]) > 0){
    
    #tmpFilePath_final <-  paste0(bsgm.output.path.countries.tmp,bsgm.bsgmfit.final.RData)
    #tmpFilePath_quant <-  paste0(bsgm.output.path.countries.tmp,bsgm.bsgmfit.quant.RData)
    
    #if( (file.exists(tmpFilePath_final))  &  (file.exists(tmpFilePath_quant))  ){
        bsgmfit_final_combined <- bsgmfit_final
        bsgmfit_quant_combined <- bsgmfit_quant
        tmpBool <- TRUE
    #}
  }  

  # get list of the fixed.set without main countries
  bsgm.fixed.set.without.input.countries <- bsgm.fixed.set[!bsgm.fixed.set %in% bsgm.input.countries]
  
  for ( i in 1:length(bsgm.fixed.set.without.input.countries) ) {
    
    old_counry <-  as.character(bsgm.fixed.set.without.input.countries[i])

    bsgmfit.final.old.folder   <- paste(root_path, '/output/prj_', bsgm.input.year,'_',old_counry, '/tmp/',sep="")
    bsgmfit.final.old.file.path   <- paste0(bsgmfit.final.old.folder,'bsgmfit_final_prj_',bsgm.input.year,'_',old_counry,'.Rdata') 
    bsgmfit.quant.old.file.path   <- paste0(bsgmfit.final.old.folder,'bsgmfit_quant_prj_',bsgm.input.year,'_',old_counry,'.Rdata') 
    
      if(file.exists(bsgmfit.final.old.file.path)  &  file.exists(bsgmfit.quant.old.file.path )){
      
          logdebug(paste0('Loading old bsgmfit final  for: ',old_counry,' from ',bsgmfit.final.old.file.path))
          logdebug(paste0('Loading old bsgmfit quant  for: ',old_counry,' from ',bsgmfit.quant.old.file.path))
      
          # loading bsgmfit_final in local enviremnt
          local_env.bsgmfit_final = local({load(file=bsgmfit.final.old.file.path); environment()})
          local_env.bsgmfit_quant = local({load(file=bsgmfit.quant.old.file.path); environment()})
      
          if ( i==1 & tmpBool==FALSE ){
            
            bsgmfit_final_combined <- local_env.bsgmfit_final$bsgmfit_final
            bsgmfit_quant_combined <- local_env.bsgmfit_quant$bsgmfit_quant

            bsgmfit_final_combined$proximity <- NULL
            bsgmfit_final_combined$predicted <- 0
            bsgmfit_quant_combined$predicted <- 0

          }else{
            
            local_env.bsgmfit_final$bsgmfit_final$proximity <- NULL
            local_env.bsgmfit_final$bsgmfit_final$predicted <- 0
            local_env.bsgmfit_quant$bsgmfit_quant$predicted <- 0
            
            bsgmfit_final_combined <- combine(bsgmfit_final_combined, local_env.bsgmfit_final$bsgmfit_final)
            bsgmfit_quant_combined <- combine(bsgmfit_quant_combined, local_env.bsgmfit_quant$bsgmfit_quant)
            
             
          }
          
        err_bool <- FALSE 
        
      }else{
        err_mess <- paste0('Old bsgmfit final does not exist for: ',old_counry,'\nPlease check that file exist in : ', bsgmfit.final.old.file.path)
        err_bool <- TRUE
      }    
 
   }  

  
   if (err_bool == FALSE) {
     
     #assign(bsgmfit_final, bsgmfit_final_combined, envir = .GlobalEnv) 
     #assign(bsgmfit_quant, bsgmfit_quant_combined, envir = .GlobalEnv) 
    
     bsgmfit_final <<- bsgmfit_final_combined
     bsgmfit_quant <<- bsgmfit_quant_combined
     
     ##  Save off our bsgmfit object for this set of data:
     save(bsgmfit_final, file=paste(bsgm.output.path.countries.tmp, "bsgmfit_final_combined.RData", sep=""))
     save(bsgmfit_quant, file=paste(bsgm.output.path.countries.tmp, "bsgmfit_quant_combined.RData", sep=""))

     return(TRUE)
     
   }else{
     
     stop(err_mess)
     return(FALSE)
     
   }

  
 } ## end   if (!is.null(bsgm.fixed.set)) {
  
}




######################################################################################
######################################################################################
######################################################################################
## getting a list of all covariates
#
get_fixed_predictors <- function() {
  
  if (!is.null(bsgm.fixed.set)) {
 
  bsgmfit.final.old <- get_bsgmfit_final_old()
  fixed.predictors <- row.names(importance(bsgmfit.final.old))
  
  }else{
    
    fixed.predictors <- names(bsgm.table)[3+1:length(covariates)] 
  }
  
  return(fixed.predictors)
}


######################################################################################
## Tuning of our randomForest bsgmulation density regression
#
#
get_init_bsgmfit <- function() {
  
  if (file.exists(paste0(bsgm.output.path.countries.tmp, 
                        bsgm.init.bsgmfit.RData))) {
    
    loginfo("Tuning of our randomForest bsgm probability was done before. Loading init_bsgmfit.RData")
    load(file=paste0(bsgm.output.path.countries.tmp,
                    bsgm.init.bsgmfit.RData)) 

    
  }else{
    loginfo("Start tuning of our randomForest bsgm transition classification.")
    
    start_time <- Sys.time()

    #init_bsgmfit = tuneRF(x=x_data,
    #                      y=y_data,
    #                      plot=TRUE,
    #                      mtryStart=ncol(x_data)/3,
    #                      ntreeTry=length(y_data)/20,
    #                      improve=0.0001,
    #                      stepFactor=1.20,
    #                      trace=TRUE, 
    #                      doBest=TRUE,
    #                      #nodesize=length(y_data)/750,
    #                      na.action=na.omit,
    #                      importance=TRUE,
    #                      proximity=FALSE,
    #                      sampsize=min(c(length(y_data), 1000)),
    #                      replace=TRUE) 
    

    # as we do a classification mtryStart should be floor(sqrt(ncol(x_data))) and 
    # ntreeTry his should not be set to too small a number, to ensure
    # that every input row gets predicted at least a few times. (from randomForest manual)
    
    init_bsgmfit = tuneRF(x=x_data,
                          y=y_data,
                          plot=TRUE,
                          mtryStart= floor(sqrt(ncol(x_data))),
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
    
    
    
    end_time <- Sys.time()
    loginfo(paste("End tuning RF. Elapsed Fitting Time:",
                  tmDiff(start_time,end_time)))
    ##	Save off our init_bsgmfit object for this set of data:
    save(init_bsgmfit, 
         file=paste0(bsgm.output.path.countries.tmp,
                    bsgm.init.bsgmfit.RData))
    
  }
 
  return(init_bsgmfit)
}


######################################################################################
## optimize the model
#
#
get_bsgmfit <- function() {
  
  if (file.exists(paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.RData))) {
    
    loginfo(paste0("Loading bsgmfit object from ",bsgm.bsgmfit.RData))
    load(file=paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.RData))
    
    varImpPlot(bsgmfit) 
    
    return(bsgmfit)
  }
  
  set.seed(2011)
  
  if (is.null(bsgm.fixed.set)) {
     
    logdebug("Fixed_set is NULL. Will optimize the model... ") 	
    start_time <- Sys.time()
    init_bsgmfit <- get_init_bsgmfit()
    
    ##	Now we will optimize the model by iteratively removing any 
    ##		covariates with negative increases in node purity:
    
    ##	Get list of covariates that have an importance score greater than 0:
    importance_scores <- importance(init_bsgmfit)[order(importance(init_bsgmfit)[,1],
                                                        decreasing=TRUE),]
    pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
    
    if (length(pos_importance) == length(importance_scores[,1])) {
      
      x_data <- x_data[,pos_importance,with = FALSE]
      
      bsgmfit = tuneRF(x=x_data, 
                      y=y_data, 
                      plot=TRUE, 
                      mtryStart= floor(sqrt(ncol(x_data))),
                      ntreeTry=500,                      
      #                mtryStart=length(x_data)/3, 
      #                ntreeTry=length(y_data)/20, 
                      improve=0.0001, 
                      stepFactor=1.20, 
                      trace=TRUE, 
                      doBest=TRUE, 
                      nodesize=length(y_data)/1000, 
                      na.action=na.omit, 
                      importance=TRUE, 
                      proximity=FALSE, 
                      sampsize=min(c(length(y_data), 1000)), 
                      replace=TRUE) 
      
      
      
    }else{
      
      while (length(pos_importance) < length(importance_scores[,1])) {
        
        logdebug(" Jumping into the [while (length(pos_importance) < length(importance_scores[,1])) ] ... ")
        ##	Subset our x_data to just those columns having positive scores:
        x_data <- x_data[,pos_importance, with = FALSE]
        
        bsgmfit = tuneRF(x=x_data, 
                        y=y_data, 
                        plot=TRUE, 
                        mtryStart= floor(sqrt(ncol(x_data))),
                        ntreeTry=500,
                        improve=0.0001, 
                        stepFactor=1.20, 
                        trace=TRUE, 
                        doBest=TRUE, 
                        nodesize=length(y_data)/1000, 
                        na.action=na.omit, 
                        importance=TRUE, 
                        proximity=FALSE, 
                        sampsize=min(c(length(y_data), 1000)), 
                        replace=TRUE) 
        
        ##	Re-check importance scores:
        importance_scores <- importance(bsgmfit)[order(importance(bsgmfit)[,1], 
                                                       decreasing=TRUE),]
        pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
        print(bsgmfit)
        
      } ## End of while loop
    }
    
    end_time <- Sys.time()
    loginfo(paste("Elapsed Fitting Time:", tmDiff(start_time,end_time)))
    
  } else {
    
    bsgmfit_final_old <- get_bsgmfit_final_old()
    
    bsgmfit = randomForest(x=x_data, 
                          y=y_data, 
                          mtry=bsgmfit_final_old$mtry, 
                          ntree=bsgmfit_final_old$ntree, 
                          nodesize=length(y_data)/1000, 
                          importance=TRUE, 
                          proximity=FALSE)
    print(bsgmfit)
    
  }  
  
  loginfo(paste0("Saving ",bsgm.bsgmfit.RData))
  ##	Save off our bsgmfit object for this set of data:
  save(bsgmfit, file=paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.RData)) 
  
  
  
  ##	For continuous regression, plot observed vs. predicted:
  # plot(x=y_data, y=predict(bsgmfit), ylim=c(min(y_data),max(y_data)), xlim=c(min(y_data),max(y_data)))
  # abline(a=0, b=1, lty=2)
  
  ###	For continuous regression, plot residuals vs. observed:
  #plot(x=y_data, y=(y_data - predict(bsgmfit)), xlim=c(0,max(y_data)))
  #abline(a=0, b=0, lty=2)
  #
  ###	For continuous regression, plot residuals vs. fitted:
  #plot(x=predict(bsgmfit), y=(y_data - predict(bsgmfit)), xlim=c(0,max(y_data)))
  #abline(a=0, b=0, lty=2)
  
  varImpPlot(bsgmfit)  
  
  return(bsgmfit)
  
}


######################################################################################
##	Another alternative is to use Quantile Regression Forests to generate
##		prediction intervals.  We'll fit a quantile regression using
##		the tuning parameters pulled from the bsgmfit object above:

get_bsgmfit_final <- function() {
  
  if (file.exists(paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.final.RData))) {
    
    loginfo(paste0("Loading bsgmfit object from ",bsgm.bsgmfit.final.RData))
    load(file=paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.final.RData))
    
  }else{


    set.seed(2011)
    bsgmfit_final <- randomForest(x=x_data, 
                              y=y_data, 
                              mtry=bsgmfit$mtry, 
                              ntree=bsgmfit$ntree, 
                              nodesize=length(y_data)/1000, 
                              importance=TRUE, 
                              proximity=FALSE,
                              do.trace=FALSE)
  
    loginfo(paste0("Saving bsgmfit_final object  ",bsgm.bsgmfit.final.RData))
    save(bsgmfit_final, 
         file=paste0(bsgm.output.path.countries.tmp, 
                    bsgm.bsgmfit.final.RData))
  
  } 
  
  return(bsgmfit_final)
  
}

######################################################################################
#
#
#
get_bsgmfit_quant <- function() {
  
  if (file.exists(paste0(bsgm.output.path.countries.tmp, 
                         bsgm.bsgmfit.quant.RData))) {
    
    loginfo(paste0("Loading bsgmfit object from ",bsgm.bsgmfit.quant.RData))
    load(file=paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.quant.RData))
    
  }else{  
  
    set.seed(2011)
  
    bsgmfit_quant <- quantregForest(x=x_data, 
                                    y=y_data, 
                                    mtry=bsgmfit$mtry, 
                                    ntree=bsgmfit$ntree, 
                                    nodesize=length(y_data)/1000)
  
    loginfo(paste0("Saving bsgmfit_quant object  ",bsgm.bsgmfit.quant.RData))
    save(bsgmfit_quant, file=paste0(bsgm.output.path.countries.tmp,
                                   bsgm.bsgmfit.quant.RData))
  
  } 
  
  return(bsgmfit_quant)
  
}




######################################################################################
#
#
save_rf_pred <- function(rf_pred,mask) {
  
  logdebug(paste0('Saving : ',bsgm.predict.density.rf.pred))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density.rf.pred), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_pred), start =1 )
  sd <- writeStop(sd)
  
}

######################################################################################
#
#
save_rf_sd <- function(rf_sd, mask) {
  
  logdebug(paste0('Saving : ',bsgm.predict.density.rf.sd))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density.rf.sd), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
}

######################################################################################
#
#
save_all_pred <- function(rf_sd,mask) {
  
  save_rf_pred (rf_sd$result_rf_pred,mask)
  save_rf_sd(rf_sd$result_rf_sd,mask)
  
  colnames(oper) <-c('result_rf_pred',
                     'result_rf_sd',
                     'result_rf_05',
                     'result_rf_50',
                     'result_rf_95')
  
  bsgm.predict.density_rf_05 <- paste0("predict_density_rf_05_",
                                       bsgm.countries.tag, ".tif")
  bsgm.predict.density_rf_50 <- paste0("predict_density_rf_50_",
                                       bsgm.countries.tag, ".tif")
  bsgm.predict.density_rf_95 <- paste0("predict_density_rf_95_",
                                       bsgm.countries.tag, ".tif")
  
  logdebug(paste0('Saving : ',bsgm.predict.density_rf_05))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density_rf_05), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
  logdebug(paste0('Saving : ',bsgm.predict.density_rf_50))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density_rf_50), 
                   format="GTiff", datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
  logdebug(paste0('Saving : ',bsgm.predict.density_rf_95))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density_rf_95), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)   
}




retrieveBSGMRF <- function(){
  if(file.exists(bsgm.fixed.set) ){
    
    logdebug(paste0('Loading old bsgmfit final from: ',
                    bsgm.fixed.set))
    
    ##  Load it:
    local_env.bsgmfit_final = local({load(file=bsgm.fixed.set);environment()})
    bsgmfit.final.old <- local_env.bsgmfit_final$bsgmfit_final
    
    ##  Return it:
    return(bsgmfit.final.old)
    
  }else{
    stop(paste0('Old bsgmfit final does not exist at: ', 
                bsgm.fixed.set,
                '\nPlease check that the file exists.'))
  }
  
}



retrieveBSGMRF_quant <- function(){
  if(file.exists(bsgm.rf.quant.obj.extrap.path) ){
    
    logdebug(paste0('Loading old bsgmfit quant from ',
                    bsgm.rf.quant.obj.extrap.path))
    
    ##  Load it:
    local_env.bsgmfit_quant = local({load(file=bsgm.rf.quant.obj.extrap.path); environment()})
    bsgmfit.quant.old <- local_env.bsgmfit_quant$bsgmfit_quant
    
    ##  Return it:
    return(bsgmfit.quant.old)
    
  }else{
    err_mess <- paste0('Old bsgmfit quant does not exist at: ', 
                       bsgm.rf.quant.obj.extrap.path,
                       '\nPlease check that the file exists.')
  }
  
}

rasterize_data  <- function(df,val_col_name,flname) {
  ##  Declare the column names:
  colnames(df) <- c("ADMINID", val_col_name)
  
  ##  Use the L1 admin areas as the template raster:
  rst <- raster(censusmaskPathFileName)
  
  ##  Rasterize the values:
  tmpRaster <- wpRasterize(rst,
                           df,
                           cores=bsgm.cluster_workers,
                           filename=paste0(bsgm.output.path.countries.tmp, flname),
                           overwrite=TRUE,
                           silent=TRUE, 
                           minblk = wpGetBlocksNeed(rst, 
                                                    cores=bsgm.cluster_workers,
                                                    n=nmb))
  
  
  return(tmpRaster)
  
}