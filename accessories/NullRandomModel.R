require(data.table)
require(raster)
require(snow)
require(wpUtilities)

##  Set the seed:
set.seed(1964)


root <- "D:/Research/BSGMiv1a/"
iso <- "CHE"
#data_type <- "GUF+"
##  User inputs:
##    Number of repititions to do:
repititions <- 500
##    Set the number of cores to use:
cluster_workers <- 6
##    Declare what years have observations:
observed_years <- c(2000,2005,2010,2015)
##    Sequence of non-observed years:
year_seq <- c(2001:2004,2006:2009,2011:2014)
##    Observed year tag:
year_tag <- "2000-2005-2010-2015"
##    Set the output directory:
outdir <- paste0(root, "output","/prj_",year_tag,"_",iso,"/derived/")


##  Define the function that will actually be performing the single 
##  simulation (repitition):
boot_sim <- function(i,threshold){
  ##  Randomly sample a year for every row (possible transition) we have:
  foodt[,YEAR:=sample({min_val:max_val}, size = draw_size, replace = T)]
  ##  If the year is 10 or less, this corresponds to prior to 2010 as 
  ##  transitioning and therefore being built in 2010:
  foodt[,PRED :=0]
  foodt[YEAR <= threshold, PRED := 1]
  ##  Calculate the TP, FP, FN, TN:
  foodt[OBS == 1 & PRED == 1, TP := 1]
  foodt[OBS == 0 & PRED == 1, FP := 1]
  foodt[OBS == 1 & PRED == 0, FN := 1]
  foodt[OBS == 0 & PRED == 0, TN := 1]
  ##  Calculate the TP, FP, FN, TN sums (i.e. the a,b,c,&d cell of a 
  ##  contingency table):
  foo_list <- list(sum(foodt$TP, na.rm = T),
                   sum(foodt$FP, na.rm = T),
                   sum(foodt$FN, na.rm = T),
                   sum(foodt$TN, na.rm = T))
  ##  Return the sums:
  return(foo_list)
}

##  Create a list of the model periods:
period_list <- vector(mode = "list", 
                      length = {length(observed_years)-1})
if(length(period_list)>1){
  for(o in 2:length(observed_years)){
    ##  Populate with the observed period endpoints:
    period_list[[{o-1}]] <- c(observed_years[{o-1}], observed_years[o])
  }
}
if(length(period_list)==1){
  period_list[[1]] <- c(observed_years[1], observed_years[2])
}

##  Set up our bootstrap iterator:
bootstraps <- {1:repititions}

##  For every period in the period list:
for(p in 1:length(period_list)){
  period_tag <- paste0(period_list[[p]][1],"_",period_list[[p]][2])
  ##  Subset of years for the period:
  year_sub <- year_seq[year_seq>period_list[[p]][1] & year_seq <period_list[[p]][2]]
  if(p==1){
    min_val <- 1 
  }
  if(p!=1){min_val <- period_list[[p]][1]+1-period_list[[1]][1]}
  max_val <- {period_list[[p]][2]}-{period_list[[1]][1]}
  
  ##  Path to the transition raster:
  transpath <- paste0(root,"output/prj_",year_tag,"_",iso,
                      "/tmp/prj_",year_tag,"_",iso,"_transition_",
                      period_tag,".tif")
  print(paste0("Transition Raster: ",transpath))
  
  ##  For every year of the period:
  for(y in year_sub){
    print(paste0("Working on ", iso, " ", y))
    print(paste0("Performing ", repititions, " repititions"))
    print(paste0("Minimum value: ", min_val))
    print(paste0("Maximum value: ", max_val))
    
    ##  Set the year threshold to determine what things transitioned:
    threshold <- {y-period_list[[1]][1]}
    ##  Path to the observed extents raster we wish to compare against:
    # obspath <- paste0(root,"data_",data_type,"/",iso,"/",tolower(iso),"_grid_100m_",
    # ifelse(data_type == "ESA", "GUF+",data_type),"_2010.tif")
    obspath <- paste0(root,"data","/",iso,"/",tolower(iso),"_grid_100m_",
                      "esa_cls190_",y,".tif")
    print(paste0("Observed Extents: ", obspath))
    
    ##  Path to the regional admin id raster:
    regpath <- paste0(root,"data","/",iso,"/",tolower(iso),
                      "_grid_100m_ccidadminl1.tif")
    
    ##  Pull the transition indices and place in a datatable and the observed 
    ##  values:
    print("Pulling transition indices, GID, and observed values...")
    foodt <- data.table(CID = which(values(raster(transpath))==1))
    foodt[,GID := raster(regpath)[foodt$CID]]
    foodt[,OBS := raster(obspath)[foodt$CID]]
    
    print("Ensuring no missing values...")
    x <- nrow(foodt)
    ##  Make sure only complete cases remain:
    foodt <- foodt[complete.cases(foodt),]
    
    print(paste0("Removed ", {x - nrow(foodt)}," rows with missing values."))
    
    ##  Establish a column for holding TP, FP, FN, & TN indicators:
    foodt[, c("TP","FP","FN","TN") := 0]
    ##  Note the number of transitions/draws needed for each simulation:
    draw_size <- nrow(foodt)
    
    print(paste0("Each simulation will have ", draw_size, " random simulations of transition location and timing."))
    
    ##  Set up a preallocated data.table to hold the bootstrap outputs:
    simdt <- data.table(B =rep(bootstraps,each=length(foodt$GID)))
    simdt[,GID := rep(foodt$GID, times = length(bootstraps))]
    simdt[,c("TP","FP","FN","TN") := 0] 
    
    
    ##  Define the parallel task farm which will be doing our bootstrap simulations:
    cluster_nullSim <- function(simdt,...){
      ##  Description:
      ##  "Let's do the time warp aga-" seriously though,
      ##    Cluster task farm where each task is the model selection, fitting, 
      ##    of a series of model classes and the prediction of the mean, with 95%
      ##    intervals if applicable, and returning that to the main hadler as a 
      ##    dataframe where it is stored in a single dataframe before being output 
      ##    as a single dataframe for all years and given GIDs.
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
        require(data.table)
      })
      
      ##  Pass off the required data and functions to the nodes in the cluster
      ##   - this includes the lists used for informing predictions, and the
      ##     task functions that create the predictions/info for each subnational 
      ##     unit:
      clusterExport(cl, c("bootstraps",
                          "boot_sim",
                          "draw_size",
                          "foodt", 
                          "min_val",
                          "max_val",
                          "threshold"
      ))
      
      ##	Start all nodes on a prediction:
      for (i in 1:nodes) {
        ##  Send the taskMaker function call to the ith node with ith task from
        ##  the prediction_list as an argument and tag it with the value i
        sendCall(cl[[i]], boot_sim, args = c(i, threshold), tag=i)
      }
      
      
      ##	Create our primary cluster processing loop, recalling that we already
      ##		have clusters running:
      cat("Total tasks to process: ", length(bootstraps), "\n")
      for (i in 1:length(bootstraps)) {
        ##	Receive results from a node:
        predictions <- recvOneData(cl)
        
        ##	Check if there was an error:
        if (!predictions$value$success) {
          stop("ERROR: Cluster barfed...\n\n", predictions, bootstraps[i])
        }
        
        ##	Which block are we processing:
        block <- predictions$value$tag
        # cat("Received gid: ", block, "\n")
        # flush.console()
        
        ##	Now store our predictions in their proper rows of the data.frame:
        simdt[B == bootstraps[i], c("TP","FP","FN","TN"):= predictions$value$value] 
        
        ##	Check to see if we are at the end of our tasklist:
        ni <- nodes + i
        if (ni <= length(bootstraps)) {
          ##	And if not, send it to the cluster node that just gave us
          ##		our last result...
          sendCall(cl[[predictions$node]], boot_sim,args =c(ni, threshold), tag=ni)
        }
        tEnd <- Sys.time()
        wpProgressMessage(i,
                          max = length(bootstraps),
                          label = paste0("Received bootstrap", ni,
                                         " Processing Time: ",
                                         wpTimeDiff(tStart,tEnd)))
      }
      
      
      ##  Return the full df of results:
      return(simdt)
    }
    
    ##  Start the cluster and run the simulation:
    beginCluster(cluster_workers)
    simresults <- cluster_nullSim(simdt)
    endCluster()
    rm(simdt,foodt)
    gc()
    
    ##  Remove any GID 0 as it is water:
    simresults <- simresults[GID!=0,]
    ##  Aggregate results by B and GID:
    simagg <- simresults[,list(TP = sum(TP, na.rm = T),
                               FP = sum(FP, na.rm = T),
                               TN = sum(TN, na.rm = T),
                               FN = sum(FN, na.rm = T)), 
                         by = list(B,GID)]
    ##  Calculate Derived Metrics:
    simagg[, RECALL := TP/{TP+FN}, 
           by = list(B,GID)]
    simagg[, PRECISION := TP/{TP+FP}, 
           by = list(B,GID)]
    simagg[, FSCORE := {2*(PRECISION*RECALL)/(PRECISION+RECALL)}, 
           by = list(B,GID)]
    simagg[, SPECIFICITY := {TN/(FP+TN)}, 
           by = list(B,GID)]
    simagg[, OVERACC := {{TN+TP}/(TP+FN+FP+TN)}, 
           by = list(B,GID)]
    simagg[, QUANTDIS.0 := {abs({FN-FP}/(TP+FP+FN+TN))}, 
           by = list(B,GID)]
    simagg[, QUANTDIS.1 := {abs({FP-FN}/(TP+FP+FN+TN))}, 
           by = list(B,GID)]
    simagg[, QUANTDIS := {{QUANTDIS.0+QUANTDIS.1}/2}, 
           by = list(B,GID)]
    simagg[, ALLOCDIS.0 := {2*min({FP/{TP+TN+FP+FN}}, {FN/{TP+TN+FP+FN}})},
           by = list(B,GID)]
    simagg[, ALLOCDIS.1 := {2*min({FN/{TP+TN+FP+FN}}, {FP/{TP+TN+FP+FN}})},
           by = list(B,GID)]
    simagg[, ALLOCDIS := {ALLOCDIS.0+ALLOCDIS.1}/2, 
           by = list(B,GID)]
    simagg[, PROP.CORRECT := {{TN+TP}/{TP+TN+FP+FN}}, 
           by = list(B,GID)]
    ##  Add a year tag:
    simagg[,YEAR := y]
    
    ##  Save the year specific simulation results:
    saveRDS(simagg, 
            file = paste0(outdir,iso,"_esa_cls190_NullBootAgg_",
                          repititions,"_",y,".RDS"))
    rm(simagg)
    gc()
  }
}

##  Retrieve all the individual year specific files for aggregation:
sim_files <- Sys.glob(paste0(outdir,iso,"_esa_cls190_NullBootAgg_",
                             repititions,"_*.RDS"))
for(s in 1:length(sim_files)){
  ##  Load the file:
  foo <- readRDS(sim_files[s])
  ##print(summary(foo))
  if(s == 1){
    xx <- copy(foo)
  }
  if(s!=1){
    ##  Append the records:
    xx <- rbind(xx,foo)
  }
}
rm(foo)
gc()
sim <- copy(xx)
##  Create a summary by year, i.e. each row is a year summarizing the 
##  entire country performance, where we get the approximated 95% confidence 
##  interval by year:
xx <- xx[,list(RECALL.025=quantile(RECALL,0.025,na.rm=T),
               RECALL.500=quantile(RECALL,0.5,na.rm=T),
               RECALL.975=quantile(RECALL,0.975,na.rm=T),
               PRECISION.025=quantile(PRECISION,0.025,na.rm=T),
               PRECISION.500=quantile(PRECISION,0.5,na.rm=T),
               PRECISION.975=quantile(PRECISION,0.975,na.rm=T),
               FSCORE.025=quantile(FSCORE,0.025,na.rm=T),
               FSCORE.500=quantile(FSCORE,0.5,na.rm=T),
               FSCORE.975=quantile(FSCORE,0.975,na.rm=T),
               SPECIFICITY.025=quantile(SPECIFICITY,0.025,na.rm=T),
               SPECIFICITY.500=quantile(SPECIFICITY,0.5,na.rm=T),
               SPECIFICITY.975=quantile(SPECIFICITY,0.975,na.rm=T),
               OVERACC.025=quantile(OVERACC,0.025,na.rm=T),
               OVERACC.500=quantile(OVERACC,0.5,na.rm=T),
               OVERACC.975=quantile(OVERACC,0.975,na.rm=T),
               QUANT.025=quantile(QUANTDIS,0.025,na.rm=T),
               QUANT.500=quantile(QUANTDIS,0.5,na.rm=T),
               QUANT.975=quantile(QUANTDIS,0.975,na.rm=T),
               ALLOC.025=quantile(ALLOCDIS,0.025,na.rm=T),
               ALLOC.500=quantile(ALLOCDIS,0.5,na.rm=T),
               ALLOC.975=quantile(ALLOCDIS,0.975,na.rm=T)),
       by = YEAR]
saveRDS(xx,
        file = paste0(outdir,iso,"_esa_cls190_NullBootCountrywidebyYear_",
                      repititions,"_",year_tag,".RDS"))
rm(xx)
gc()


##  Now get the summary values by GID and year (median and nonparametric 
##   estimate of 95 percentile of each metric), i.e. each subnational unit 
##   should have multiple rows with each row representing a year:
sim <- sim[,list(RECALL.025=quantile(RECALL,0.025,na.rm=T),
                 RECALL.500=quantile(RECALL,0.5,na.rm=T),
                 RECALL.975=quantile(RECALL,0.975,na.rm=T),
                 PRECISION.025=quantile(PRECISION,0.025,na.rm=T),
                 PRECISION.500=quantile(PRECISION,0.5,na.rm=T),
                 PRECISION.975=quantile(PRECISION,0.975,na.rm=T),
                 FSCORE.025=quantile(FSCORE,0.025,na.rm=T),
                 FSCORE.500=quantile(FSCORE,0.5,na.rm=T),
                 FSCORE.975=quantile(FSCORE,0.975,na.rm=T),
                 SPECIFICITY.025=quantile(SPECIFICITY,0.025,na.rm=T),
                 SPECIFICITY.500=quantile(SPECIFICITY,0.5,na.rm=T),
                 SPECIFICITY.975=quantile(SPECIFICITY,0.975,na.rm=T),
                 OVERACC.025=quantile(OVERACC,0.025,na.rm=T),
                 OVERACC.500=quantile(OVERACC,0.5,na.rm=T),
                 OVERACC.975=quantile(OVERACC,0.975,na.rm=T),
                 QUANT.025=quantile(QUANTDIS,0.025,na.rm=T),
                 QUANT.500=quantile(QUANTDIS,0.5,na.rm=T),
                 QUANT.975=quantile(QUANTDIS,0.975,na.rm=T),
                 ALLOC.025=quantile(ALLOCDIS,0.025,na.rm=T),
                 ALLOC.500=quantile(ALLOCDIS,0.5,na.rm=T),
                 ALLOC.975=quantile(ALLOCDIS,0.975,na.rm=T)),
           by = list(GID,YEAR)]

##  Get rid of columns to then get rid of duplicates and have one observation 
##  per GID YEAR combonation:
saveRDS(sim, 
        file = paste0(outdir,iso,"_esa_cls190_NullBootbyGIDandYear_",
                      repititions,"_",year_tag,".RDS"))

