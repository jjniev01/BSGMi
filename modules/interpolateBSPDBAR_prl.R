#
#
#
interpolateBSPDBAR_prl <- function(df, init_year, end_year, cores=NULL, cblk=1, silent=TRUE) {
  
  chunk_data <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
  
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
        
      ##  Calculate initial and end BS Population Density:
      bsd0 <- df.tmp[df.tmp$YEAR == init_year, "BS.PDBAR"]
      bsd1 <- df.tmp[df.tmp$YEAR == end_year, "BS.PDBAR"]    

      
      ##  Calculate the average rate of change in BS population density:
      l_bar <- log(bsd1/bsd0, base = exp(1)) / (end_year-init_year)

      
      df.out <- bsd0 * exp(l_bar * (df.tmp$YEAR[df.tmp$YEAR != init_year 
                                                & df.tmp$YEAR != end_year] 
                                    - init_year))
      
     
    }, error = function(e) stop(paste0("The block ", i, " caused the error: ", e, "'")))
    
    return(list(a = l_bar, b = df.out))
  }     
  
  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], wpDFsub, i, tag=i)
  }      
  
  out <- df
  out["L_BAR"] <- NaN 
  
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

    
#    out$L_BAR[out$GID %in% g.unq.chunk[[b]]] <- data.frame(matrix(unlist(output.res), 
#                                                           nrow=length(output.res[1]), byrow=T),
#                                                           stringsAsFactors=FALSE)[,1]
    
    out$L_BAR[out$GID %in% g.unq.chunk[[b]]] <- output.res[[1]]  
    
    out$BS.PDBAR[out$GID %in% 
                   g.unq.chunk[[b]] & 
                   out$YEAR != init_year & 
                   out$YEAR != end_year] <- output.res[[2]]   
    
    
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
