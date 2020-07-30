##  Written by Maksym Bondarenko
##  Adapted and Commented by Jeremiah J. Nieves

load.pop.table <- function(){

  if (!is.null(bsgm.input.poptables)){
    
    if(!file.exists(as.character(bsgm.input.poptables[bsgm.input.countries[[icountry]]])) ){
      logwarn(paste0("Trying to open POP TABLE for ",bsgm.input.countries[[icountry]]," from file ",
                     bsgm.input.poptables[bsgm.input.countries[[icountry]]]))
      stop(paste("Can not load a POP TABLE. Please check your inputs in input.R",
                 sep=""))
      return(FALSE)
    }  
    loginfo(paste0("Will load POP TABLE for ",bsgm.input.countries[[icountry]]," from a local file ",
                   bsgm.input.poptables[bsgm.input.countries[[icountry]]]))
    df <- as.data.frame(read.dbf(bsgm.input.poptables[bsgm.input.countries[[icountry]]]))    
    return(df)
    
  }else{
    
    # if we alreay have the POP table downloaded then we just load it
    file_local <- paste0(bsgm.output.path.countries.tmp,'/', 
                         tolower(bsgm.input.countries[icountry]),
                         '_population_2000_2020.csv')
    
    if(file.exists(file_local) ){
      df <- utils::read.csv(file_local, stringsAsFactors=FALSE,header = TRUE)
      #df <- df[ c('GID', paste0('P_',bsgm.input.year)) ]
      #colnames(df) <-  c("ADMINID", "ADMINPOP") 
      return(df)    
    }  

    # if we do not have the POP table downloaded then we will download from WP FTP
    loginfo(paste0("Will load POP TABLE for ",
                   bsgm.input.countries[icountry],
                   " from WP FTP "))
    
    df <- wpgpGetPOPTable(ISO3 =bsgm.input.countries[icountry], 
                          year = year_seq,
                          destDir = bsgm.output.path.countries.tmp,
                          username = bsgm.ftp.username, 
                          password = bsgm.ftp.password
    )
    ##  Rename back to what they came from CIESIN as because our formatting 
    ##  function expects these:
    #colnames(df) <- c("GID","UNP2000","UNP2001","UNP2002","UNP2003","UNP2004", 
    #                  "UNP2005","UNP2006","UNP2007","UNP2008","UNP2009",
    #                  "UNP2010","UNP2011","UNP2012","UNP2013","UNP2014",
    #                  "UNP2015","UNP2016","UNP2017","UNP2018","UNP2019",
    #                  "UNP2020")
    return(df)
  }
}

##########################################################################

load.pop.farea <- function(icountry){
  
  # if we alreay have the POP table downloaded then we just load it
  file_local <- paste0(bsgm.output.path.countries.tmp,'/', tolower(icountry),
                       '_px_area_ZS_sum.csv')
  
  if(file.exists(file_local) ){
    
    df <- utils::read.csv(file_local, stringsAsFactors=FALSE,header = TRUE)
  
  }else{
    
    df <-  wpgpGetZonalStats(ISO3 =icountry, 
                                 covariate = "px_area", 
                                 stat = "sum", 
                                 destDir = output.country.tmp, 
                                 username = bsgm.ftp.username,
                                 password = bsgm.ftp.password
    )
 
  }
  
  #remove all 0 adminID 
  #colnames(df) <-  c("ADMINID", "F_AREA") 
  
  # I have add this check as I found some Nan in zonal stats
  # Need to be checked why we are getting this Nan
  #
  if ( check.df.has.na(df) ){
    cat("\n")
    logwarn("----------")
    logwarn("----------")
    logwarn(paste0("F_AREA has NAN valu pleas check the file ", icountry))
    logwarn(paste0("Please check  F_AREA csv file ", file_local))
    logwarn(paste0("The Nan values will be replaced by 1."))
    logwarn("----------")
    logwarn("----------")
    df[is.na(df)] <- 10000
  }
  
  
  return(df[df$GID != 0, ])   
}

##########################################################################

load.pop.table.and.farea <- function(icountry){

  df <-  merge( as.data.frame(load.pop.table(icountry)), 
                as.data.frame(load.pop.farea(icountry)), 
                by="GID", 
                sort=FALSE)
        
  if (!is.null(bsgm.input.adminids)){
    #df <- df[df$ADMINID %in% bsgm.input.adminids[[icountry]],]
    df <- df[df$ADMINID %in%  as.character(unlist(bsgm.input.adminids[icountry])),]
  }
  
  return(df)

}