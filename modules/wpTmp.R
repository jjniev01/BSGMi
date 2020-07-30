##  AUTHORS: MAKSYM BONDARENKO & JEREMIAH J. NIEVES
wpDFcalcBS_POP <- function(df, t0, t1, cores=NULL, cblk=1, silent=TRUE){
  ##  This function calculates the population estimated to be within BS for 
  ##  every non-end point year and GID.
  ####
  ##
  ##  Define a subfunction which determines cores to use based upon input 
  ##  parameters, checks one of the input parameters, and breaks up the given 
  ##  data into chunks:
  chunk_data <- function(x,n){
    ##  Split the data into chunks:
    split(x, cut(seq_along(x), n, labels = FALSE))
    ##  If cblk parameter is not an integer, raise a fuss:
    if (!cblk%%1==0) stop(paste0("cblk should be integer"))
    
    ## Get the number of real physical cores (not virtual) in machine:
    max.cores <- parallel:::detectCores(logical = TRUE)
    
    ##  If number of cores is not specified:
    if (is.null(cores)) {
      ##  Use the number of cores available minus one:
      cores <- max.cores - 1
    }
  }  
  ##  Initiate the cluster with the specified number of cores:
  beginCluster(n=cores)
  
  ##  Note the process start time:
  tStart <- Sys.time()
  ##  Retrieve the initiated cluster:
  cl <- getCluster()
  ##  Retrieve the number of cores in the cluster:
  nodes <- length(cl)
  
  ##  Retrieve the unique GIDs we are working with:
  g.unq <- unique(df$GID)
  
  ##  Chunk the GIDs up based upon the input parameters
  g.unq.chunk <- chunk_data(g.unq, cores*cblk)
  
  ##  Use the number of chunks to declare our "block" size:
  blocks <- length(g.unq.chunk)  
  
  ##  If we haven't invoked the silent option, give some console output:
  if (!silent) { 
    cat(paste0('\nTotal blocks ', blocks))
    cat('\n')
  }      
  ##  Export the blocks object, the data.frame, the chunks, and the initial and 
  ##  end time of the modelign period we are working with:
  clusterExport(cl, c("blocks", "df","g.unq.chunk", "t0", "t1"),
                envir=environment())
  
  ##  Define a subfunction which operates on a block by block basis:
  wpDFsub <- function(i){
    ##  This function determines the estimated BS population for each year based 
    ##  upon the estimated proportion of population in BS and the total 
    ##  population, for every GID and non-end point year.
    ####
    ##
    tryCatch({
      ##  Multiply the total population values by the proportion of population 
      ##  estimated to be within builtsettlements for every GID and non-end 
      ##  point year:
      df.tmp <- df$POP[df$GID %in% g.unq.chunk[[i]] & df$YEAR != t0 & df$YEAR != t1]*
        df$PBS[df$GID %in% g.unq.chunk[[i]] & df$YEAR != t0 & df$YEAR != t1]
      }, 
      error = function(e) stop(paste0("The block ", i, " caused the error: ", e, "'")))
    
    ##  Return the modified data.frame:
    return(df.tmp)
  }     
  
  ##  For every core we have at our disposal:
  for (i in 1:nodes) {
    ##  Begin the processing on some initial blocks:
    parallel:::sendCall(cl[[i]], wpDFsub, i, tag=i)
  }      
  
  ##  Use the input data.frame as the template for our output data.frame:
  out <- df
  
  ##  For every block:
  for (i in 1:blocks){
    ##  Receive the processed output from a node:
    d <- parallel:::recvOneData(cl)
    ##  If it failed, indicate so and stop all processing:
    if (! d$value$success ) {
      stop(paste0('Cluster error in block ',i))
    }
    
    ##  Note the end processing time:
    tEnd <-  Sys.time()
    
    ##  Retrieve the block tag of the retrieved value:
    b <- d$value$tag
    
    ##  Update the the progress bar based upon the retrieved block:
    ch.pb <- unlist(lapply(1:cores,function(i){return(i*round(blocks/cores))}),
                    use.names=FALSE)
    ##  Output the progressbar should we not be running silent:
    if (i %in% ch.pb & !silent) { 
      wpProgressMessage(i, max=blocks, 
                        label= paste0("received block ",i, " Processing Time: ",
                                      wpTimeDiff(tStart,tEnd)))
    }else if(i==blocks & !silent){
      wpProgressMessage(i, max=blocks, label= paste0("received block ",i,
                                                     " Processing Time: ",
                                                     wpTimeDiff(tStart,tEnd)))
    }   
    ##  Store the output value in the output data.frame:
    out$BS.POP[out$GID %in% g.unq.chunk[[b]] &
                 out$YEAR != t0 & out$YEAR != t1] <- d$value$value
    
    ##  Give the next block to that node which just turned in data if there is 
    ##  still more data to process:
    ni <- nodes + i
    if (ni <= blocks) {
      parallel:::sendCall(cl[[d$node]], wpDFsub, ni, tag=ni)
    }
  }
  
  ##  Close the cluster:
  endCluster()
  
  ##  Return the modified data.frame which has the annual GID-specific BS.POP 
  ##  estimates:
  return(out)
}
  

##  ------------------------------------------------------------------------  ##




wpDFcalcBS_CNT <- function(df, t0, t1, cores=NULL, cblk=1, silent=TRUE){
  ##  This function determines the number of BS cells in every GID in parallel
  ##  based upon the estimated BS Population and the estimated BS Population 
  ##  Density.
  ####
  ##
  ##  Define a subfunction which determines cores to use based upon input 
  ##  parameters, checks one of the input parameters, and breaks up the given 
  ##  data into chunks:
  chunk_data <- function(x,n){
    ##  Split the data into chunks:
    split(x, cut(seq_along(x), n, labels = FALSE))
    ##  If cblk parameter is not an integer, raise a fuss:
    if (!cblk%%1==0) stop(paste0("cblk should be integer"))
    
    ## Get the number of real physical cores (not virtual) in machine:
    max.cores <- parallel:::detectCores(logical = TRUE)
    
    ##  If number of cores is not specified:
    if (is.null(cores)) {
      ##  Use the number of cores available minus one:
      cores <- max.cores - 1
    }
  }  
  ##  Initiate the cluster with the specified number of cores:
  beginCluster(n=cores)
  
  ##  Note the start time of processing:
  tStart <- Sys.time()
  
  ##  Retrieve the initiated cluster noting the number of cores initialized:
  cl <- getCluster()
  nodes <- length(cl)
  
  ##  Retrieve the GIDs we are working with:
  g.unq <- unique(df$GID)
  ##   Chunk the vector of GIDs:
  g.unq.chunk <- chunk_data(g.unq, cores*cblk)
  ##  The number of chunks becomes our number of "blocks":
  blocks <- length(g.unq.chunk) 
  
  ##  If we are not running silent put some output to the console:
  if (!silent) { 
    cat(paste0('\nTotal blocks ', blocks))
    cat('\n')
  }        
  ##  EXport required objects to the cores:
  clusterExport(cl, c("blocks", "df","g.unq.chunk", "t0", "t1"),
                envir=environment())
  
  ##  Define a subfunction for blockwise processing:
  wpDFsub <- function(i){
    tryCatch({
      ##  Determine the amount of BS area given the BS.POP and BS Pop density:
      df.tmp <- df$BS.POP[df$GID  %in% g.unq.chunk[[i]] &
                            df$YEAR != t0 & df$YEAR != t1] / 
        df$BS.PDBAR[df$GID  %in% g.unq.chunk[[i]] &
                      df$YEAR != t0 & df$YEAR != t1]},
      error = function(e) stop(paste0("The block ", i,
                                      " caused the error: ", e, "'")))
    ##  Return the modified data.frame:
    return(df.tmp)
  }     
  
  ##  Start by giving some inital blocks to the nodes:
  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], wpDFsub, i, tag=i)
  }      
  
  ## Use the input data.frame as a template for the output data.frame:
  out <- df
  
  ##  For every block:
  for (i in 1:blocks) {
    ##  Retrieve the processed block:
    d <- parallel:::recvOneData(cl)
    ## If there was a failure, stop all processing and indicate so:
    if (! d$value$success ) {
      stop(paste0('Cluster error in block ', i))
    }
    ##  Note the end processing time:
    tEnd <-  Sys.time()
    ##  Retrieve the block ID of the block we just retrieved:
    b <- d$value$tag
    
    ##  Update the progress bar based upon this block ID:
    ch.pb <- unlist(lapply(1:cores, 
                           function(i){return(i*round(blocks/cores))}),
                    use.names=FALSE)
    ##  If we aren't running silent, indicate progress on the console output:
    if (i %in% ch.pb & !silent) { 
      wpProgressMessage(i, max=blocks, label= paste0("received block ",i,
                                                     " Processing Time: ",
                                                     wpTimeDiff(tStart,tEnd)))
    }else if(i==blocks & !silent){
      wpProgressMessage(i, max=blocks, label= paste0("received block ",i,
                                                     " Processing Time: ",
                                                     wpTimeDiff(tStart,tEnd)))
    }    
    
    ##  Store the processed output from the block to the output data.frame:
    out$BS.CNT[out$GID %in% g.unq.chunk[[b]] &
                 out$YEAR != t0 & out$YEAR != t1] <- d$value$value
    
    ##  If there is more data to process, give the node that just turned in a 
    ##  block, another block to process:
    ni <- nodes + i
    if (ni <= blocks) {
      parallel:::sendCall(cl[[d$node]], wpDFsub, ni, tag=ni)
    }
  }
  ##  Close the cluster:
  endCluster()
  
  ##  Return the modified data.frame containing processed outputs:
  return(out)
}