##  THIS FILE CONTAINS FUNCTIONS USED FOR THE ASPATIAL PREDICTIONS OF NEW BS 
##  DEMAND AND THE TABLE BASED INFORMATION WHICH GOES INTO IT.
##  TITLE:  bsTableTools .R
##  AUTHOR:  JEREMIAH J. NIEVES & MAKSYM BONDARENKO
##  VERSION: 1.0
##  LAST UPDATE: 2018-03-29
##  NOTES:  
##
##
##
##  -----------------------------------------------------------------------  ##

tidyCIESIN <- function(orig_df, times = seq(2000,2020, by=1)){
  ##  Function to transform data from CIESIN into a more palatable format 
  ##  (i.e. tidy data).
  ##  Description:  Takes the non-tidy data from CIESIN and makes is tidy per 
  ##                Wickham 2013 (doi: 10.18637/jss.v059.i10) and then subsets 
  ##                it by years given as a numeric vector argument. This will 
  ##                let us take advantage of plyr later on in this script.
  ##
  ##  Parameters: 
  ##  indata -  Input data from CIESIN containing the unique geoID of admin 
  ##            units and associated time specific populations; give as a 
  ##            character path
  ##  times  -  vector of years of interest for which we will subset the 
  ##            CIESIN origin data in order to keep our output dataframes tidy 
  ##            and readable (As much as possible)
  ##
  ##  Values:
  ##  Returns a dataframe object in long format (i.e. tidy data)
  ##
  ##  ---------------------------------------------------------------------  ##
  #  Bring in the population dataframe which will form the base of our data 
  ##  work and storage:
  print("Bringing in original population data...")
  start_time <- proc.time()[3]
  
  ##  Let's get tidy.
  print("Rearranging data to tidy format...")
  ##  For every time point we specified earlier:
  for(y in 1:length(times)){
    if(y == 1){
      ##  If it is the first iteration, populate a new dataframe with the first
      ##  year's records:
      pop_df <- data.frame("GID" = orig_df$GID, 
                           "YEAR" = rep(times[y],length(orig_df$GID)), 
                           "POP" = orig_df[, paste0("UNP",times[y])])
    }else{
      ##  Otherwise, just rbind the new rows on the end:
      pop_df <- rbind(pop_df,
                      data.frame("GID"  = orig_df$GID,
                                 "YEAR" = times[y],
                                 "POP"  = orig_df[, paste0("UNP",times[y])])
      )
    }
  }
  print("Data tidying complete!")
  print(paste0("Elapsed time: ", proc.time()[3]-start_time, " seconds"))
  
  ##  Return the new dataframe object:
  return(pop_df)
}


##  -----------------------------------------------------------------------  ##




getBSPopulation <- function(population_path_list,
                            built_settlement_path_list,
                            admin_zonal_raster, 
                            pop_df_obj, 
                            process_years,
                            prl = TRUE,
                            silent = TRUE,
                            overwrite = TRUE){
  ##  Description:
  ##  This function takes a population density surface raster, a spatially
  ##  coincident built settlement raster (binary), and a admin unit based 
  ##  zonal raster to extract the population total of the built area in each
  ##  admin unit.
  ##
  ##  Parameters:
  ##  -  Path pointing to the population density raster at t0
  ##  -  Path pointing to the population density raster at t0 + dt
  ##  -  Path pointing to the built settlement binary raster at t0
  ##  -  Path pointing to the built settlement binary raster at t0 + dt
  ##  -  Admin zonal ID raster object
  ##  -  Population dataframe object
  ##  -  Output path for output things
  ##  -  Numeric vector of years for which we will be procesing, will need to 
  ##     correspond to years which are included in what would be 
  ##     "observed_years", e.g. c(2012,2014)
  ##  -  Should we overwrite existing items?
  ##
  ##  Value:
  ##  -  A dataframe containing a column with the admin unit unique ID as well
  ##     as a column representing the total population included in the admin 
  ##     unit (i.e. built settlement population, bsp), the number of built
  ##     cells at that time point in the admin units, and the built settlement
  ##     population density (i.e. pd) at t = 0 and t = 0 + dt 
  ##
  ##  ---------------------------------------------------------------------  ##
  ##  Bring in the population dataframe which will form the base of our data 
  ##  work and storage:
  start_time <- proc.time()[3]
  start_time2 <- proc.time()[3]
  pop_df <- pop_df_obj
  
  ##  If the overwrite parameter is True:
  if(overwrite){
    ##  NOTE:  If in future I need to run this for more than two years, I can 
    ##         feed the BS paths as a vector and the pop ras paths as a vector,
    ##         where their order lines up and then process through accordingly
    ##         would also work as a dataframe with a character column for paths.
    ##  For every declared year of processing:
    for(i in 1:length(process_years)){
      y <- process_years[i]
      ##  Extract the BS information:
      
      ##  Extract the intial built settlement extent information:
      bs_raster_path <- built_settlement_path_list[[as.character(y)]]
      pop_raster_path <- population_path_list[[bsgm.input.countries]][[as.character(y)]] 
      ##  Extract the info and put in a temporary dataframe:
      foo_df <- extractBSToDF(bs_raster_path, 
                              pop_raster_path, 
                              admin_zonal_raster,
                              prl,
                              silent)
      ##  Add the year to the dataframe so we join it properly:
      foo_df$YEAR <- y
      if(i == 1){
        ext_df <- foo_df  
      }
      if(i != 1){
        ##   Add the rows to the overall extractions:
        ext_df <- rbind(ext_df,foo_df)
      }
    }
    ##  Merge with the over all pop_df, retaining all rows of pop_df based 
    ##  upon the GID and year:
    pop_df <- left_join(pop_df, as.data.frame(ext_df), 
                        by = c("GID","YEAR"))
    
    pop_df <- as.data.frame(pop_df)
    ##  Rename the columns:
    names(pop_df) <- c("GID","YEAR","POP","BS.POP","BS.CNT")
    
    print("Calculating built settlement population densities...")
    ##  Create new columns which calculate the time specific pop density of built
    ##  settlement areas at time t0 and t0+dt:
    pop_df <- pop_df %>% dplyr::mutate(BS.PDBAR = BS.POP/BS.CNT)
    #pop_df <- pop_df %>% dplyr::mutate(PBS = BS.POP/POP)
    
    ##  If the BS.CNT comes back as 0, then repopulate the BS.POP and the 
    ##  BS.PDBAR fields as 0 instead of NA as the NA's cause issues down the 
    ##  line; it is ok to have NAs in the BS.CNT field as those will be 
    ##  filled as we estimate for those years:
    pop_df$BS.POP[is.na(pop_df$BS.POP) | is.nan(pop_df$BS.POP)] <- 0
    pop_df$BS.PDBAR[is.na(pop_df$BS.PDBAR) | is.nan(pop_df$BS.PDBAR)] <- 0
    #pop_df$PBS[is.na(pop_df$PBS) | is.nan(pop_df$PBS)] <- 0
    pop_df$BS.CNT[is.na(pop_df$BS.CNT) | is.nan(pop_df$BS.CNT)] <- 0
    
    
    print("Built Settlement Population Extraction Complete!")
    print(paste0("Built Settlement Population Extraction Total Elapsed time: ", 
                 proc.time()[3]-start_time, " seconds"))
  }
  else{
    # ##  Load the file:
    # logr(logconsole,
    #      log_file_path = logfile,
    #      message_string = "BS Population File Already Exists.")
    # logr(logconsole,
    #      log_file_path = logfile, 
    #      message_string = paste0("     Loading ", internal_out, 
    #                              region,"_",ugm_version,
    #                              "_BSPopulation_Extractions.RData"))
    # 
    # load(paste0(internal_out, region,"_",ugm_version,
    #             "_BSPopulation_Extractions.RData"))
    
  }
  ## Return the dataframe:
  return(pop_df)
}


##  -----------------------------------------------------------------------  ##




extractBSToDF <- function(BS_raster_path, 
                          population_raster_path, 
                          admin_ras,
                          prl,
                          silent){
  ##  PARAMETERS:
  ##  BS_raster_path - path pointing to the built settlement raster of choice
  ##  
  ##  population_raster_path - path pointing to the corresponding population 
  ##    raster 
  ##
  ##  admin_ras - raster containing the admin zones  
  ## 
  ##  VALUES:
  ##    Dataframe containing columns for GID, BS.CNT, and BS.POP; intended to 
  ##    be merged with the initial CIESIN provided data
  ##
  ##  Read the input BS raster:
  bs_ras <- raster(BS_raster_path)
  
  ##  Read in the population raster:
  pop_ras <- raster(population_raster_path)
  
  ##  Extract the cell indices of the BS Raster and add them to a new data 
  ##  frame as a column.
  ##  If we are not operating in parallel:
  if(!prl){ 
    ##  Do it in memory:
    bs_indices <- which(values(bs_ras) == 1)
  }else{
    ##  Utilize the parallel based function from  ...:
    ##  TODO:  Track down the location of this function; might be in an updated 
    ##         version of wpUtilities.
    bs_indices <- wpGetindexesWhichValues(x = bs_ras, 
                                          v=1, 
                                          cores=bsgm.cluster_workers, 
                                          silent=silent)  
  }    
  ##  Create a data.frame to hold the identified indices and the corresponding 
  ##  GIDs:
  foo_df_long <- data.frame("BSINDEX" = bs_indices, 
                            "GID" = numeric(length=length(bs_indices)))
  
  ##  Extract the GID from the admin raster using the cell indices extracted in
  ##  the previous step and add it to the dataframe as a corresponding column.
  ##  If we are not operating in parallel:
  if(!prl){ 
    ##  Retrieve them in memory:
    bs_gid <- values(admin_ras)[bs_indices]
  }else{
    ##  TODO:  Track down the loaction of this function for reference; might be
    ##  in an updated wpUtilities package.
    bs_gid <- wpGetValuesbyInds(x = admin_ras, 
                                v=bs_indices,
                                cores=bsgm.cluster_workers,
                                silent=silent)  
  }    
  ##  Add the values to the data.frame column:
  foo_df_long$GID <- bs_gid
  
  ##  Retain only complete cases in the dataframe, i.e. remove any records
  ##  which have extracted a GID value of NA:
  foo_df_long <- foo_df_long[complete.cases(foo_df_long),]
  foo_df_long <- foo_df_long[foo_df_long$GID != 0,]
  
  ##  Extract the population count values from the population raster using the
  ##  extracted BS cell indices stored in the dataframe.
  ##  If we are not operating in parallel:
  if(!prl){ 
    ##  Extract the BS POP values in memory:
    bs_pop <- values(pop_ras)[foo_df_long$BSINDEX]
  }else{
    ##  TODO:  Track down the loaction of this function for reference; might be
    ##  in an updated wpUtilities package.
    bs_pop <- wpGetValuesbyInds(x = pop_ras, 
                                v=foo_df_long$BSINDEX, 
                                cores=bsgm.cluster_workers, 
                                silent=silent)  
  }     
  
  ##  Add the retrieved BS Population values as a column into the table:
  foo_df_long$BSPOP_SINGLE <- bs_pop
  
  ##  Remove the BS and population rasters from memory:
  rm(bs_ras, pop_ras)
  gc()
  
  ##  Aggregation of extracted data  -----
  ##  Start a new dataframe which calculates the number of BS cells by GID, the
  ##  BSPOP by GID; Make sure to add a column which indicates the year as a 
  ##  numeric:
  foo_df_agg <- foo_df_long %>%
    group_by(GID) %>%
    dplyr::mutate(BS.POP = sum(BSPOP_SINGLE, na.rm = TRUE),
           #BS.CNT = n())%>%dplyr::select("GID","BS.POP","BS.CNT")
           BS.CNT = n())%>%
    dplyr::select("GID","BS.POP","BS.CNT")
  
  ##  Remove duplicates:
  foo_df_agg <- foo_df_agg[!duplicated(foo_df_agg$GID),]
  ##  Return that dataframe for merging with the base pop dataframe:
  return(foo_df_agg)
}


##  -----------------------------------------------------------------------  ##




interpolateURR <- function(in_data_frame,
                           init_year,
                           end_year){
  ##  This function calculates the Urban Rural Ratio (URR) at the end points of 
  ##  the modeling period and then interpolates the URR for all GIDs using GID 
  ##  specific exponential growth/decay functions.
  ####
  ##
  ##  Calculate the URR for the observed points that we have:
  ##    NOTE: will produce NAs or Infs initially.
  in_data_frame <- in_data_frame %>% dplyr::mutate(URR = BS.POP/(POP-BS.POP))
  
  ##  Retrieve the unique GID values:
  g <- unique(in_data_frame$GID)
  
  ##  Retrieve the initial URR value of all GIDs:
  urr0 <- in_data_frame$URR[(in_data_frame$GID == g) &
                              (in_data_frame$YEAR == init_year)]
  
  ##  Retrive the end period URR values of all GIDs:
  urr1 <- in_data_frame$URR[(in_data_frame$GID == g) &
                              (in_data_frame$YEAR == end_year)]
  
  ##  Calculate the average rate of change, i.e. RUR, for all GIDs:
  rur <- (1/(end_year-init_year)) * log(urr1/urr0, base = exp(1))
  
  ##  Interpolate the URR for all years and unique GIDs between the initial and
  ##  end years:
  in_data_frame$URR[in_data_frame$GID == g &
                      in_data_frame$YEAR != init_year &
                      in_data_frame$YEAR != end_year] <-
    urr0 * exp(rur * (in_data_frame$YEAR[in_data_frame$GID == g & 
                                           in_data_frame$YEAR != init_year &
                                           in_data_frame$YEAR != end_year] 
                      - init_year)
    )
  ##  Return the now filled data.frame:
  return(in_data_frame)
}


##  -----------------------------------------------------------------------  ##




interpolateBSPDBAR <- function(in_data_frame,
                               init_year,
                               end_year){
  ##  This function interpolates the average BS population density of each 
  ##  subnational unit using GID specific exponential growth/decay functions.
  ####
  ##
  ##  Retrieve every unique GID:
  g <- unique(in_data_frame$GID)
  
  ##  Calculate initial and end BS Population Density:
  bsd0 <- in_data_frame[in_data_frame$GID == g & 
                          in_data_frame$YEAR == init_year, "BS.PDBAR"]
  bsd1 <- in_data_frame[in_data_frame$GID == g & 
                          in_data_frame$YEAR == end_year, "BS.PDBAR"]
  
  ##  Calculate the average rate of change in BS population density:
  l_bar <- log(bsd1/bsd0, base = exp(1)) / (end_year-init_year)
  
  ##  Add the l_bar value into a new column:
  in_data_frame[in_data_frame$GID==g,"L_BAR"] <- l_bar
  
  ##  Estimate the year specific built settlement population density for all 
  ##  GIDs for the years we are estimating:
  in_data_frame$BS.PDBAR[in_data_frame$GID == g & 
                           in_data_frame$YEAR != init_year &
                           in_data_frame$YEAR != end_year] <-
    bsd0 * exp(l_bar * (in_data_frame$YEAR[in_data_frame$GID == g &
                                             in_data_frame$YEAR != init_year &
                                             in_data_frame$YEAR != end_year] 
                        - init_year))
  
  ##  Return the now filled data.frame:
  return(in_data_frame)
}


##  -----------------------------------------------------------------------  ##




handleNegativeCases <- function(indf, difference_df, gid){
  ##  Description:
  ##  This function operates on an input dataframe and looks at a SINGLE admin.
  ##  unit at a time. It checks for negative estimated differences in that 
  ##  admin unit across its entire time period and certain conditions under 
  ##  which those negative cases occurred prior to the reweighting of the 
  ##  estimated changes.
  ##  Should conditions be met, the function then corrects the estimated 
  ##  differences to ensure that there are not issues in the reweighting 
  ##  procedure, the reweighted estimated changes, and the final check that the
  ##  sum of weighted estimated changes equals the observed changes which 
  ##  anchor the entire model.
  ##
  ##  The specific cases of negative differences, their origins, and the 
  ##  assumptions and logic in how they are handled are as follows:
  ##  
  ##  CASE IA:
  ##      All predicted differences are negative, but the observed changes are 
  ##      greater than zero.
  ##    ORIGIN: Built population decreases, but built settlement increases 
  ##      either becasue of differences in imagery and sensitivity of original 
  ##      datasets (i.e. artifact) or because the relationship between 
  ##      population and built settlement area are inverse of what would be 
  ##      expected.
  ##    IMPLICATION:  Lacking any other information, we will assume that the
  ##      greatest built settlement changes occurred circa the biggest 
  ##      population magnitude changes
  ##    HANDLING: The reweighting scheme makes all the weights positive by 
  ##      virtue of all indiviudal differences being negative; no further 
  ##      action necessary.
  ##
  ##  CASE IB:
  ##    Some differences are negative, but the observed changes are greater 
  ##    than zero
  ##    ORIGIN: Comes about by the population decreasing for a year while 
  ##      outpacing the predicted built settlement population density decrease
  ##    IMPLICATION: Built settlement growth during this period is unlikely 
  ##      compared to other years in the total transition period.
  ##    HANDLING: Set the difference for that year, and therefore its weighted
  ##      difference to zero.
  ##      
  ##  CASE II:
  ##    No observed change in the built settlement count, but the population 
  ##    predicts a net increase or decrease in the settlement count, resulting
  ##    in negatives between some years.
  ##    ORIGIN: Relationships between population and built settlement counts 
  ##      are not straightforward and not stationary either through time and 
  ##      space. Compoiunding this, there are inaccuracies in the original 
  ##      built settlement data and even the population estimates. Any of 
  ##      these errors, in conjunction with my model assumptions, could
  ##      combine to result in this.
  ##    IMPLICATIONS: Best to continue with the base assumption that the input
  ##      built settlement data is the best we have in knowing if any transition 
  ##      occurred.
  ##    HANDLING:  Set all differences to zero in order to match the observed 
  ##      changes
  ##
  ##  NOTE:  This makes individual records in the final saved dataframe appear 
  ##         to not be "traceable" through the equations laid out for predicting
  ##         transitions. They are, but the entire context of a given admin unit 
  ##         through time will need to be considered in order to determine if 
  ##         one of the case flags is raised. Further versions may explicitly 
  ##         include a case flag in the output dataset.
  ##
  ##  WARNING: This is a modifying function which will alter values and records 
  ##           of the original input dataframe.
  ##
  ##  Parameters:
  ##  -  Input dataframe
  ##  -  Unique record identifier value
  ##
  ##  Values:
  ##  -  Returns the modified dataframe
  ## 
  ##  ---------------------------------------------------------------------  ##
  ##  Note the start time of processing:
  start_time <- proc.time()[3]
  
  
  ##  Handle negative cases prior to passing these off to the weighted 
  ##  differences and anchor checks.
  ##  Take care of any NAs:
  difference_df$DIFF[is.na(difference_df$DIFF)] <- 0
  
  ##  Get the total observed change, i.e. as determined by the input BS extents:
  observedchange <- indf$BS.CNT[indf$GID == gid & indf$YEAR == t1] -
    indf$BS.CNT[indf$GID == gid & indf$YEAR == t0]
  
  
  ##  Determine Special Case Status  ---- 
  ##  Determine if we have special negative conditions which make any balancing
  ##  handling either special or unnecessary.
  ##
  ##  Retrieve all the years we are working with here, e.g. if t0 == 2000 and 
  ##  t1 == 2012 then the gid years retrieved here will be 2000, 2001, ..., 
  ##  2011:
  gid_years <- difference_df$YEAR.0[difference_df$GID == gid]
  
  ##  Create a holder (boolean vector) to hold the condition of whether 
  ##  difference is negative or not:
  boo_vec <- c()
  
  ##  For every difference pair of years in the sequence for the given GID.
  ##  First, determine if any of the estimated differences are, in fact, 
  ##  negative and are not NA:
  ##  NOTE:  This returns a logical vector of TRUEs and FALSEs where TRUE 
  ##         indicates that the predicted transitions for the year of the given 
  ##         GID are negative.
  neg_flag <- difference_df$DIFF[difference_df$YEAR.0 %in% gid_years &
                                   difference_df$GID == gid &
                                   !is.na(difference_df$DIFF)] < 0
  
  ##  Append that to the boo_vec:
  boo_vec <- c(boo_vec, neg_flag)
  
  
  ##  NOTE:  The below cases are mutually exclusive of each other, but not 
  ##         exhaustive of possible conditions. The remaining conditions are
  ##         implied to be handled by the regular reweighting and anchor 
  ##         checks with no prior adjustment.
  ##
  ##  NOTE:  If the sum of boo_vec is equal to its length, all differences 
  ##         are negative and if the sum if equal to 0, then all differences
  ##         are equal to or greater than zero.
  
  ##  Are all the differences negative?
  case_ia_flag <-sum(boo_vec[!is.na(boo_vec)])==length(boo_vec[!is.na(boo_vec)])
  
  ##  Are some of the differences negative, but not all and the observed 
  ##  changes do not equal zero?
  case_ib_flag <- observedchange != 0 & 
    sum(boo_vec[!is.na(boo_vec)]) < length(boo_vec[!is.na(boo_vec)]) &
    sum(boo_vec[!is.na(boo_vec)]) != 0 
  
  ##  Are there some negatives and zero observed differences for the
  ##  given gid?
  case_ii_flag <- observedchange == 0 & 
    sum(boo_vec[!is.na(boo_vec)]) < length(boo_vec[!is.na(boo_vec)]) &
    sum(boo_vec[!is.na(boo_vec)]) != 0
  
  ##  If a Case Ia flag was raised:
  if(case_ia_flag){
    ##  Because the reweighting will have a negative denominator (i.e. sum of
    ##  all negative diff.s), no action is necessary, but is good to know that
    ##  it occurred. Feel free to put a print statement in here; I did once. My 
    ##  console was full of output.
  }
  if(case_ib_flag){
    ##  Set the differences of negative years to 0:
    difference_df$DIFF[difference_df$GID == gid & difference_df$DIFF < 0] <- 0
  }
  if(case_ii_flag){
    ##  Set all the differences to 0:
    ##  NOTE:  Later I could modify this to keep the predicted differences and
    ##         implement the correction, currently below, during the 
    ##         reweighting or anchor check procedure. This would allow me to 
    ##         examine all predicted changes compared to the actual to see how
    ##         the model does prior to heuristic adjustment.
    difference_df$DIFF[difference_df$GID == gid] <- 0
  }
  
  ##  Return the difference_df rows we brought in:
  return(difference_df[difference_df$GID == gid,])
}


##  -----------------------------------------------------------------------  ##




anchorCheck <- function(indt_obj){
  ##  Description:
  ##  This function operates on an input dataframe and looks at a single admin
  ##  unit at a time. It checks that the estimated changes are equal to the 
  ##  total observed changes for that admin unit across its entire time period.
  ##  Should the values not match, the function then adds or subtracts randomly
  ##  from the available timepoints until the estimated transitions equal the 
  ##  observed transition totals.
  ##
  ##  WARNING: This is a modifying function which will alter values and records 
  ##           of the original input dataframe.
  ##
  ##  Parameters:
  ##  -  Input dataframe
  ##  -  Unique record identifier value
  ##
  ##  Values:
  ##  -  Returns the modified dataframe
  ## 
  ##  ---------------------------------------------------------------------  ##
  ##  Note the start time of processing:
  indt <- copy(indt_obj)
  start_time <- proc.time()[3]
  for(g in unique(indt$GID)){
    for(p in 1:length(period_list)){
      print(paste0("Anchor check for: ", g, " period ",p))
      ##  Get the total change observed:
      observedchange <- indt[GID == g & PERIOD == p,]$OBS.CHANGE[1]
      
      ##  Determine Special Case Status  ---- 
      ##  Determine if we have special negative conditions which make any balancing
      ##  handling either special or unnecessary.
      ##
      ##  Retrieve all the years we are working with here for the given GID:
      gid_years <- indt[GID == g & PERIOD == p,]$YEAR
      
      ##  Carry Out Anchor Checks  ----
      ##  Get the total change estimated:
      ##  EXAMPLE:  If modeling with t0 <- 2000 and t1 <- 2012 then we have values 
      ##            calculated for 2000 - 2011 where those values represent 
      ##            transitions predicted to occur in 2000-2001, 2001-2002, ..., 
      ##            2011-2012. Later, we do not explicitly map the transitions for 
      ##            the 2011 (2011-2012) transitions because they are represented by
      ##            the 2012 input extents.
      estimatedchange <- sum(indt[GID == g & PERIOD == p,]$EST.CHANGE, na.rm = TRUE)
      gate <- (observedchange - estimatedchange) 
      
      ##  In case we have an NA in the data for some inexplicable reason:
      if(is.na(gate)){
        print("Estimated change is NA; skipping...")
        ##  Reset the gate:
        gate <- 0}
      ##  If the observed and predicted changes are equal:
      if(gate == 0){print("Changes already balanced.")}
      ##  If the difference does NOT equal zero:
      while(gate != 0){
        ##  If the difference is positive, i.e. underestimation:
        if(gate > 0){
          print("Correcting - addition...")
          flush.console()
          ##  Randomly sample one of the transition years:
          i <- sample(gid_years[1:length(gid_years)-1], 1, replace = FALSE)
          ##  Add one to the estimated total for that record:
          indt[GID == g & YEAR == i, EST.CHANGE := EST.CHANGE+1] 
          #<- indf$EST.CHANGE[indf$GID == gid & indf$YEAR == i] + 1
          ##  Recalculate the estimated total:
          estimatedchange <- sum(indt[GID == g & PERIOD == p,]$EST.CHANGE, 
                                 na.rm = TRUE)
          ##  Update the gate value:
          gate <- (observedchange - estimatedchange)
        }
        ##  If the difference is negative, i.e. overestimation:
        if(gate < 0){
          print("Correcting - subtraction...")
          flush.console()
          ##  Randomly sample one of the transition years:
          i <- sample(gid_years[1:length(gid_years)-1], 1, replace = FALSE)
          ##  While the sampled year has predicted changes less than 0:
          while(indt[GID == g & YEAR == i,]$EST.CHANGE <= 0){
            ##  If the selected value is negative or equal to zero, we 
            ##  don't want to subtract, so resample:
            i <- sample(gid_years[1:length(gid_years)-1], 1, replace = FALSE)
            }
          
          ##  Subtract one from the estimated total for that record:
          indt[GID == g & YEAR == i, EST.CHANGE := EST.CHANGE-1]
          
          ##  Recalculate the estimated total:
          estimatedchange <- sum(indt[GID == g & PERIOD == p,]$EST.CHANGE, 
                                 na.rm = TRUE)
          ##  Update the gate value:
          gate <- (observedchange - estimatedchange) 
        }
        ##  Update the gate value:
        gate <- (observedchange - estimatedchange) 
      }
      print("Anchor check complete!")
    }
  }
  ##  Return the modified data.frame:
  return(indt)
}


##  -----------------------------------------------------------------------  ##




weightChanges <- function(in_data_frame){
  start_time <- proc.time()[3]
  
  ##  Set seed for replicability of random sampling:
  set.seed(2011)
  
  ##  BS Difference Computations  ----
  ##  Calculate the difference in built settlement area, i.e. the demand, 
  ##  between each successive year and add demand as individual columns
  ##  for each year specific demand.
  ##
  
  in_data_table <- as.data.table(in_data_frame)
  in_data_table[, DIFF := {BS.CNT - data.table::shift(BS.CNT, n = 1L,
                                                      fill = NA, type = "lag")},
                by = GID]
  ##  Determine what period every record is in and label it:
  for(p in 1:length(period_list)){
    if(p==length(period_list)){
      in_data_table[YEAR >= period_list[[p]][1] & YEAR <= period_list[[p]][2], PERIOD :=p]
    }
    if(p != length(period_list)){
      in_data_table[YEAR >= period_list[[p]][1] & YEAR < period_list[[p]][2], PERIOD :=p]
    }
  }
  
  ##  Get the initial and end observed BS.CNT value for each period:
  for(p in 1:length(period_list)){
    in_data_table[, t.0.BS.CNT := {BS.CNT[YEAR==period_list[[p]][1]]}, 
                  by = list(GID,PERIOD)]
  }
  
  ##  Pull only the years of observation:
  dtmp <- copy(in_data_table[YEAR %in% observed_years,])
  ##  Get the lagged difference for the observed changes by GID:
  dtmp[, 
       OBS.CHANGE := {data.table::shift(BS.CNT, n=1L, fill=NA, type = "lead")-BS.CNT},
       by = GID]
  ##  Remove the NAs to get rid of the duplicate periods for proper joining of 
  ##  the data to the main table:
  dtmp <- dtmp[!is.na(OBS.CHANGE),]
  dtmp <- dtmp[,c("GID","PERIOD","OBS.CHANGE")]
  
  ##  Set the keys to the GID and PERIOD:
  setkey(dtmp,GID,PERIOD)
  setkey(in_data_table,GID,PERIOD)
  ##  Merge those observed change values in:
  in_data_table <- merge(in_data_table,dtmp)
  ##  Get rid of temporary data:
  rm(dtmp)
  
  ##  Replace NA values in DIFF with zeros:
  in_data_table[is.na(DIFF), DIFF := 0]
  
  print("Finding negative cases...")
  ##  Flag all Difference Values that are negative:
  in_data_table[,NEG.FLAG := 0]
  in_data_table[DIFF < 0, NEG.FLAG := 1]
  
  ##  Handle the negative predicted values of DIFF from above:
  ##  Should conditions be met, the function then corrects the estimated 
  ##  differences to ensure that there are not issues in the reweighting 
  ##  procedure, the reweighted estimated changes, and the final check that the
  ##  sum of weighted estimated changes equals the observed changes which 
  ##  anchor the entire model.
  ##
  ##  The specific cases of negative differences, their origins, and the 
  ##  assumptions and logic in how they are handled are as follows:
  ##  
  ##  CASE IA:
  ##      All predicted differences are negative, but the observed changes are 
  ##      greater than zero.
  ##    ORIGIN: Built population decreases, but built settlement increases 
  ##      either becasue of differences in imagery and sensitivity of original 
  ##      datasets (i.e. artifact) or because the relationship between 
  ##      population and built settlement area are inverse of what would be 
  ##      expected.
  ##    IMPLICATION:  Lacking any other information, we will assume that the
  ##      greatest built settlement changes occurred circa the biggest 
  ##      population magnitude changes
  ##    HANDLING: The reweighting scheme makes all the weights positive by 
  ##      virtue of all indiviudal differences being negative; no further 
  ##      action necessary.
  ##
  ##  CASE IB:
  ##    Some differences are negative, but the observed changes are greater 
  ##    than zero
  ##    ORIGIN: Comes about by the population decreasing for a year while 
  ##      outpacing the predicted built settlement population density decrease
  ##    IMPLICATION: Built settlement growth during this period is unlikely 
  ##      compared to other years in the total transition period.
  ##    HANDLING: Set the difference for that year, and therefore its weighted
  ##      difference to zero.
  ##      
  ##  CASE II:
  ##    No observed change in the built settlement count, but the population 
  ##    predicts a net increase or decrease in the settlement count, resulting
  ##    in negatives between some years.
  ##    ORIGIN: Relationships between population and built settlement counts 
  ##      are not straightforward and not stationary either through time and 
  ##      space. Compoiunding this, there are inaccuracies in the original 
  ##      built settlement data and even the population estimates. Any of 
  ##      these errors, in conjunction with my model assumptions, could
  ##      combine to result in this.
  ##    IMPLICATIONS: Best to continue with the base assumption that the input
  ##      built settlement data is the best we have in knowing if any transition 
  ##      occurred.
  ##    HANDLING:  Set all differences to zero in order to match the observed 
  ##      changes
  ##    Determine the number of records for each GID/PERIOD combination:
  in_data_table[, ':=' (N.RECORDS = .N), by=list(GID,PERIOD)]
  ##    Get the sum of the negative flags by GID/PERIOD
  in_data_table[, NEG.FLAG.SUM := sum(NEG.FLAG),by=list(GID,PERIOD)]
  
  print("Classifying negative case types...")
  ##  Create the CASE.TYPE column:
  in_data_table[,NEG.CASE.TYPE := ""]
  ##  Find the case type Ia:
  in_data_table[NEG.FLAG.SUM == N.RECORDS, NEG.CASE.TYPE := "Ia"]
  ##  Flag the case type Ib:
  in_data_table[OBS.CHANGE != 0 & NEG.FLAG.SUM < N.RECORDS & NEG.FLAG.SUM != 0, 
                NEG.CASE.TYPE := "Ib"]
  ##  Flag the case type II:
  in_data_table[OBS.CHANGE == 0 & NEG.FLAG.SUM < N.RECORDS & NEG.FLAG.SUM != 0, 
                NEG.CASE.TYPE := "II"]
  
  print("Handling negative cases...")
  ##  Handle the negative cases that need attention. Cases Ia are handled during
  ##  the weighting portion as we inverse the weighting scheme, i.e. values 
  ##  closer to zero have greater weight for transition probability than those 
  ##  further (negative) from zero.
  ##  Handling Case Ib
  in_data_table[NEG.CASE.TYPE=="Ib" & DIFF<0, DIFF := 0]
  ##  Handling case II:
  in_data_table[NEG.CASE.TYPE=="II", DIFF := 0]
  
  
  print("Creating transition weights...")
  ##  Aggregate the negative handled DIFFs by period:
  in_data_table[,EST.TOT.DIFF := sum(DIFF, na.rm=T), by=list(GID,PERIOD)]
  
  ##  Create a weighted difference column by GID and period:
  in_data_table[,W.EST.DIFF := 0]
  in_data_table[NEG.CASE.TYPE == "Ia", 
                W.EST.DIFF := {{1/{DIFF/EST.TOT.DIFF}}/sum({1/{DIFF/EST.TOT.DIFF}},
                                                           na.rm=T)},
                by = list(GID,PERIOD)]
  in_data_table[NEG.CASE.TYPE != "II" | NEG.CASE.TYPE != "Ia",
                W.EST.DIFF := DIFF/EST.TOT.DIFF, by = list(GID,PERIOD)]
  
  
  
  
  ##  Change all the +-Inf values to zeros as these mostly come about
  ##  with dividing by zeros from the aggregated differences:
  ##  Change all the NAs to 0 as they will have come about from units which have
  ##  zero observed change, meaning we have tried to divide by zero:
  in_data_table[!is.finite(W.EST.DIFF) | is.na(W.EST.DIFF), W.EST.DIFF := 0]
  
  
  ##  Calculating the transitions distributed across time ----
  ##  Calculate the weighted changes and put them in the dataframe by year.
  ##  NOTE:  The changes will be placed with the year specific row for which 
  ##         they are expected to have occurred. For example, if t0 is 2012 
  ##         and t1 is 2014 then there are two weighted differences 
  ##         corresponding to between 2012-2013 and 2013-2014. In this 
  ##         case, say that the respective W.EST.DIFF are 0.53 and 0.47. We 
  ##         multiply those values by the observed diff from the entire time
  ##         period to get the weighted change and then round those changes 
  ##         to the nearest whole number of cells that are expected to change.
  ##         If the total real change was 2 cells, then the 2012-2013 period 
  ##         would have a rounded value of 1 and the 2013-2014 period would have
  ##         a rounded value of 0. The change of 1 is attributed to the YEAR ==
  ##         2012 record and the change of 0 is attributed to the 2013 record,
  ##         although the 2012 record changes will be shown in the '2013' map 
  ##         output and, given that the '2014' map output is the last year of 
  ##         the period we do not have to explicitly map those cells 
  ##         transitioning as the original spatial data is represented at 2014.
  ##
  ##         This above is still an issue as the real observed change is 2 and 
  ##         we need to anchor our counts to that. So there is a function called 
  ##         'anchorCheck' which will take the difference and randomly assign 
  ##         another cell, or subtract another cell, until the total is equal 
  ##         to the observed changes.
  ##
  ##  I am starting all estimated changes at zero in order to avoid infinite 
  ##  loops where the anchor function tries to keep adding to NA which just 
  ##  returns NA. Where year == t1 the EST.CHANGE will be 0 because in the 
  ##  bsdiff_df we do not calculate the change at t1.
  in_data_table[,EST.CHANGE := 0]
  
  print("Calculating estimated transitions for each year by GID and PERIOD...")
  in_data_table[, EST.CHANGE := round(W.EST.DIFF * OBS.CHANGE)]

  ##  Carry out the 'anchor,' i.e. total observed change check:
  in_data_table <- anchorCheck(in_data_table)

  ##  Save the dataframes to disk as the final dataframe should all 
  ##  demand be meetable, and the initial dataframe if the overflow procedure 
  ##  will be initiated:
  ##    NOTE:  For interpolation overflow should never need to be initiated since
  ##           all demand is weighted back to observed change.
  print("Saving data frames for replicability...")
  saveRDS(in_data_table, 
          file = paste0(bsgm.output.path.countries.tmp,
                        "BSGM_InterpolationDT_",bsgm.countries.tag,".RDS"))
  
  print("Interpolation estimation complete!")
  print(paste0("Elapsed time: ", proc.time()[3]-start_time, " seconds"))
  
  ##  Return the dataframe for interpolation in this script:
  return(in_data_table)
}