#
#
#
weightChanges_prl <- function(df, year_seq, cores=NULL, cblk=1, silent=TRUE) {
  df <- pop_df
  
  ######### First part  part 
  
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
  
  clusterExport(cl, c("blocks", "df","g.unq.chunk", "year_seq", "t0", "t1"), envir=environment())
  
  
  wpDFsub <- function(i) {
    
    tryCatch({

      #i<-4
      df.out <- data.frame("GID" = numeric(),
                              "YEAR.0" = numeric(),
                              "YEAR.T"=numeric(),
                              "DIFF" = numeric())
      
      df.tmp <- df[which(df$GID %in% g.unq.chunk[[i]]),] 

      
      for( d in 1:(length(year_seq)-1) ){

        ##  Carry out the annual difference calculation in terms of number of 
        ##  built settlement cells:
        delta_cells <- df.tmp$BS.CNT[df.tmp$YEAR==year_seq[d+1]] - df.tmp$BS.CNT[df.tmp$YEAR==year_seq[d]]
        
        ##  Store the new dataframe row as a vector to later rbind to bsdiff_df:

        df.out <- rbind(df.out, data.frame("GID" = g.unq.chunk[[i]], "YEAR.0" = year_seq[d], "YEAR.T"=year_seq[d+1], "DIFF" = delta_cells))
        

      } 
      
      
      ##  Carry out the handling of negative cases:
      

      
    #  df.out$DIFF[is.na(df.out$DIFF)] <- 0

      for(gid in g.unq.chunk[[i]]){

        observedchange <- df.tmp$BS.CNT[df.tmp$GID == gid & df.tmp$YEAR == t1] -
          df.tmp$BS.CNT[df.tmp$GID == gid & df.tmp$YEAR == t0]

        gid_years <- df.out$YEAR.0[df.out$GID == gid]
        
        boo_vec <- c()

        neg_flag <- df.out$DIFF[df.out$YEAR.0 %in% gid_years &
                                  df.out$GID == gid &
                                         !is.na(df.out$DIFF)] < 0

        boo_vec <- c(boo_vec, neg_flag)
        

        
        case_ib_flag <- observedchange != 0 & 
          sum(boo_vec[!is.na(boo_vec)]) < length(boo_vec[!is.na(boo_vec)]) &
          sum(boo_vec[!is.na(boo_vec)]) != 0 
        
        case_ii_flag <- observedchange == 0 & 
          sum(boo_vec[!is.na(boo_vec)]) < length(boo_vec[!is.na(boo_vec)]) &
          sum(boo_vec[!is.na(boo_vec)]) != 0
        
        if(case_ib_flag) df.out$DIFF[df.out$GID == gid & df.out$DIFF < 0] <- 0
        if(case_ii_flag) df.out$DIFF[df.out$GID == gid] <- 0
        
      }
      ##  End Carry out the handling of negative cases:

      
    }, error = function(e) stop(paste0("The block ", i, " caused the error: ", e, "'")))
    
    return(df.out)
  }     
  
  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], wpDFsub, i, tag=i)
  }      
  
  
  bsdiff_df <- data.frame("GID" = numeric(),
                          "YEAR.0" = numeric(),
                          "YEAR.T"=numeric(),
                          "DIFF" = numeric())
  
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

    
    bsdiff_df <- rbind(bsdiff_df, d$value$value)

    # need to send more data
    #
    ni <- nodes + i
    if (ni <= blocks) {
      #print(paste0("sending ", ni, "of ", blocks))
      parallel:::sendCall(cl[[d$node]], wpDFsub, ni, tag=ni)
    }
  }
  
  endCluster()
  

  ######### Second part 
  ########################################################################
  ########################################################################
  
  print("Calculate the weighted demand for BS cells:: weightChanges_prl Part 2")
  
  ########################################################################
  ########################################################################
  ########################################################################
  
  
  
  
  beginCluster(n=cores)
  
  tStart <- Sys.time()
  
  cl <- getCluster()
  nodes <- length(cl)

  
  g.unq <- unique(bsdiff_df$GID)
  
  g.unq.chunk <- chunk_data(g.unq, cores*cblk)
  
  blocks <- length(g.unq.chunk) 
  
  
  if (!silent) { 
    cat(paste0('\nTotal blocks ', blocks))
    cat('\n')
  }        
  
  clusterExport(cl, c("blocks", "df", "bsdiff_df","g.unq.chunk", "year_seq","t0","t1"), envir=environment())
  
  
  wpDFsub2 <- function(i) {

    tryCatch({
      

      df.tmp <- bsdiff_df[which(bsdiff_df$GID %in% g.unq.chunk[[i]]),] 
      
#      df.tmp$DIFF[is.na(df.tmp$DIFF)] <- 0

      
#      df.tmp <- df.tmp %>% mutate(DIFF = replace(DIFF, which(DIFF < 0), 0))
      
      
      aggbsdiff <- aggregate(df.tmp$DIFF ~ df.tmp$GID,
                             FUN = sum, na.action=NULL)
      names(aggbsdiff)<-c("GID","EST.TOT.DIFF")
      
      df.tmp$W.EST.DIFF <- NA
      
      df.tmp$W.EST.DIFF <- df.tmp$DIFF/ aggbsdiff$EST.TOT.DIFF
      
      df.tmp$W.EST.DIFF[df.tmp$W.EST.DIFF == -Inf | df.tmp$W.EST.DIFF == Inf] <- 0

      df.tmp$W.EST.DIFF[is.na(df.tmp$W.EST.DIFF)] <- 0
      
      
    }, error = function(e) stop(paste0("The block ", i, " caused the error: ", e, "'")))
    
    return(df.tmp)
  }    

  
  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], wpDFsub2, i, tag=i)
  }      
  

  bsdiff_df$W.EST.DIFF <- NA
  
  
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

    bsdiff_df[which(bsdiff_df$GID %in% g.unq.chunk[[b]]),] <- output.res
    

    # need to send more data
    #
    ni <- nodes + i
    if (ni <= blocks) {
      #print(paste0("sending ", ni, "of ", blocks))
      parallel:::sendCall(cl[[d$node]], wpDFsub2, ni, tag=ni)
    }
  }
  
  endCluster()
  
  
  
  
  ######### Third part 
  ########################################################################
  ########################################################################
  
  print("Calculate the weighted demand for BS cells:: weightChanges_prl Part 3")
  
  ########################################################################
  ########################################################################
  ########################################################################

  
  beginCluster(n=cores)
  
  tStart <- Sys.time()
  
  cl <- getCluster()
  nodes <- length(cl)
  
  
  g.unq <- unique(bsdiff_df$GID)
  
  g.unq.chunk <- chunk_data(g.unq, cores*cblk)
  
  blocks <- length(g.unq.chunk) 
  
  df$EST.CHANGE <- 0
  
  if (!silent) { 
    cat(paste0('\nTotal blocks ', blocks))
    cat('\n')
  }        
  
  clusterExport(cl, c("blocks", "df", "bsdiff_df","g.unq.chunk", "year_seq","t0","t1"), envir=environment())
  
  
  wpDFsub3 <- function(i) {
    
    tryCatch({

      df.tmp <- bsdiff_df[which(bsdiff_df$GID %in% g.unq.chunk[[i]]),] 
      df.in_data_frame <- df[which(df$GID %in% g.unq.chunk[[i]]),] 

      for(y in df.tmp$YEAR.0){

        obs_diff <- df.in_data_frame$BS.CNT[df.in_data_frame$YEAR == t1] - df.in_data_frame$BS.CNT[df.in_data_frame$YEAR == t0]
        

        foo_change <- round(df.tmp$W.EST.DIFF[df.tmp$YEAR.0 == y] * obs_diff)
        
        ##  Put the new est. changes in the proper row of the main dataframe:
        df.in_data_frame$EST.CHANGE[df.in_data_frame$YEAR == y] <- foo_change        
        
      }
      
########## anchorCheck start
      for(gid in g.unq.chunk[[i]]){
        
        observedchange <- df.in_data_frame$BS.CNT[df.in_data_frame$GID == gid & df.in_data_frame$YEAR==t1] -
          df.in_data_frame$BS.CNT[df.in_data_frame$GID == gid & df.in_data_frame$YEAR==t0]   
        
        gid_years <- df.in_data_frame$YEAR[df.in_data_frame$GID == gid]
        
        estimatedchange <- sum(df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid], na.rm = TRUE)
        gate <- (observedchange - estimatedchange) 
      
        if(is.na(gate)) gate <- 0
        
        while(gate != 0){
          if(gate > 0){
            i <- sample(year_seq[1:length(year_seq)-1], 1, replace = FALSE)

            df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid & 
                                          df.in_data_frame$YEAR == i] <- df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid & 
                                                                                                       df.in_data_frame$YEAR == i] + 1

            estimatedchange <- sum(df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid], 
                                   na.rm = TRUE)

            gate <- (observedchange- estimatedchange)
          }

          if(gate < 0){

            i <- sample(year_seq[1:length(year_seq)-1], 1, replace = FALSE)

            while(df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid & df.in_data_frame$YEAR == i] <= 0){

              i <- sample(year_seq[1:length(year_seq)-1], 1, replace = FALSE)
            }

            df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid &
                                          df.in_data_frame$YEAR == i] <- df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid &
                                                                                                       df.in_data_frame$YEAR == i] - 1

            estimatedchange <- sum(df.in_data_frame$EST.CHANGE[df.in_data_frame$GID == gid], na.rm = TRUE)

            gate <- (observedchange - estimatedchange) 
          }
          gate <- (observedchange- estimatedchange) 
        }        
        
        
        
      }  
########## anchorCheck end
      
 
      
      
    }, error = function(e) stop(paste0("The block ", i, " caused the error: ", e, "'")))
    
    return(df.in_data_frame)
  }    
  

  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], wpDFsub3, i, tag=i)
  }      
  

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
    
    df[which(df$GID %in% g.unq.chunk[[b]]),] <- output.res

    # need to send more data
    #
    ni <- nodes + i
    if (ni <= blocks) {
      #print(paste0("sending ", ni, "of ", blocks))
      parallel:::sendCall(cl[[d$node]], wpDFsub3, ni, tag=ni)
    }
  }
  
  endCluster()
  
  
  return(df)
}
