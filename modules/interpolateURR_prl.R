#
#
#
interpolateURR_prl <- function(df, init_year, end_year, cores=NULL, cblk=1, silent=TRUE) {
  
  chunk_data <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
  

  df <- df %>% mutate(URR = BS.POP/(POP-BS.POP))
  

  # get real physical cores in a computer
  max.cores <- parallel:::detectCores(logical = TRUE)
  
  if (is.null(cores)) {
    cores <- max.cores - 1
  }
  
  beginCluster(n=cores)
  
  tStart <- Sys.time()
  
  cl <- getCluster()
  nodes <- length(cl)
  
  if (!cblk%%1==0) stop(paste0("cblk should be integer"))
  
  g.unq <- unique(df$GID)
  
  g.unq.chunk <- chunk_data(g.unq, cores*cblk)
  
  blocks <- length(g.unq.chunk) 
  
  
  if (!silent) { 
    cat(paste0('\nTotal blocks ', blocks))
    cat('\n')
  }        
  
  clusterExport(cl, c("blocks", "df","g.unq.chunk", "init_year", "end_year"), envir=environment())
  
  
  wpDFsub <- function(i) {
    
    tryCatch({
      
      df.tmp <- df[which(df$GID %in% g.unq.chunk[[i]]),] 
      
      ##  Retrieve the initial URR value:
      urr0 <- df.tmp$URR[(df.tmp$YEAR == init_year)]
      
      ##  Retrive the end period URR value:
      urr1 <- df.tmp$URR[(df.tmp$YEAR == end_year)]
      
      
      ##  Calculate the average rate of change, i.e. RUR:
      rur <- (1/(end_year-init_year)) * log(urr1/urr0, base = exp(1))
      
      
      
      ##  Interpolate the URR for all years between the initial and end years:
      df.out <-  urr0 * exp(rur * (df.tmp$YEAR[df.tmp$YEAR != init_year
                                               & df.tmp$YEAR != end_year] 
                                   - init_year))      

      
    }, error = function(e) stop(paste0("The block ", i, " caused the error: ", e, "'")))
    
    return(df.out)
  }     
  
  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], wpDFsub, i, tag=i)
  }      
  
  out <- df
 
  for (i in 1:blocks) {
    
    d <- parallel:::recvOneData(cl)
    
    if (! d$value$success ) {
      stop('cluster error')
    }
    
    tEnd <-  Sys.time()
    
    b <- d$value$tag
    
    
    ch.pb <- unlist(lapply(1:cores, function(i) {return(i*round(blocks/cores))}), use.names=FALSE)
    if (i %in% ch.pb & !silent) { 
      wpProgressMessage(i, max=blocks, label= paste0("received block ",i, " Processing Time: ", wpTimeDiff(tStart,tEnd)))
    }else if(i==blocks & !silent){
      wpProgressMessage(i, max=blocks, label= paste0("received block ",i, " Processing Time: ", wpTimeDiff(tStart,tEnd)))
    }    
    
    output.res <- d$value$value
  
    out$URR[out$GID %in% g.unq.chunk[[b]] & 
              out$YEAR != init_year & 
              out$YEAR != end_year] <- output.res  
    
    
    # need to send more data
    #
    ni <- nodes + i
    if (ni <= blocks) {
      #print(paste0("sending ", ni, "of ", blocks))
      parallel:::sendCall(cl[[d$node]], wpDFsub, ni, tag=ni)
    }
  }
  
  endCluster()
  
  return(out)
}
