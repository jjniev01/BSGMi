##  INPUT FILE FOR LOCAL RUN  --------------------------------------------------
#####
##  STATIC SETUP
######
##  NOTICE:  IN PRACTICE NOTHING IN THIS SECTION SHOULD NEED TO BE REGULARLY 
##           EDITED UNLESS VERY SPECIFIC DETAILS NEED TO BE CHANGED ABOUT THE 
##           MODELING PROCESS.
######
##  Configuration options for the BSGM modeling and predictions.
##  Indicate the BSGM version used to produce the mapping products:
bsgm.version <- "1ia_local"

##  Named list matching t1 years with observed extents variable name for 
##  interpolation purposes:
#bsgm.obs.ext.cvr.names <- list("2012" = "ghsl_GUF_2012", 
#                               "2014" = "GUF_ghsl_2014")

##  Create a look up list for the type of LAN data (i.e. DMSP or VIIRS) based 
##  upon the year:
bsgm.LAN.cvr.names <- data.frame("NAME" = c("dmsp_2000",
                                            "dmsp_2001",
                                            "dmsp_2002",
                                            "dmsp_2003",
                                            "dmsp_2004",
                                            "dmsp_2005",
                                            "dmsp_2006",
                                            "dmsp_2007",
                                            "dmsp_2008",
                                            "dmsp_2009",
                                            "dmsp_2010",
                                            "dmsp_2011",
                                            "viirs_2012",
                                            "viirs_2013",
                                            "viirs_2014",
                                            "viirs_2015",
                                            "viirs_2016",
                                            NA,
                                            NA,
                                            NA,
                                            NA),
                                 "YEAR" = seq(2000, 2020, by = 1))
##
##  END:  STATIC SETUP
######


##  Declare the 3-letter ISO code(s) of the country(ies) you are interested in 
##  modeling.
##  NOTE:  You must declare the ISO codes of the countries you are modelign even
##         if you plan on only modeling portions of them, i.e. declaring 
##         specific admin IDs below or using a shapefile to subset them.
##  EXAMPLE:
##    bsgm.input.countries <- c("BTN","NPL")
bsgm.input.countries <- c("VNM")

##  Use LAN weighting:
bsgm.LAN.weighting = TRUE
##  Declare starting year of the modelling period for which we have observed 
##  built-settlement extents:
t0 <- 2000

##  Declare end year of the modelling period for which we have observed 
##  built-settlement extents:
t1 <- 2010

##  Overwrite the outputs?:
overwrite <- TRUE

##  If you are using specific Population tables, i.e. non-standard, stored 
##  locally, declare their paths here. Otherwise, the script will source the 
##  ones from the database when bsgm.input.poptables <- NULL.
##  EXAMPLE:
##    bsgm.input.poptables <- list(
##                               "NPL"="D:/WorldPop_Data/RandomeForest/BTN_POP_TABLE_FINAL.dbf", 
##                               "BTN"="D:/WorldPop_Data/RandomeForest/BTN_POP_TABLE_FINAL.dbf"
##                               )
bsgm.input.poptables <- NULL

##  Declare specific admin IDS by which to subset the above declared countries:
##  WARNING:  You can NOT use this option in conjunction with the shapefile 
##            subsetting option. At least one of the two subsetting options MUST
##            be set to NULL.
##  EXAMPLE:
##    bsgm.input.adminids <-  list(
##                              "BTN"=c(641378946,641378947,641378948), 
##                              "NPL"=c(524664944,524664945,524664946))
bsgm.input.adminids <- NULL
# bsgm.input.adminids <- list("VNM"=c(704191336,704191340,704191465,704191466,704191335,704191401,704191459,704191334,704191464,704191338,704191216,704191458,
#                                    704191339,704191398,704191219,704191230,704191232,704191402,704191210,704191229,704191342,704191349,704191358,704191399,704191233,
#                                    704191403,704191277,704191355,704191217,704191218,704191220,704191234,704191213,704191003,704191211,704191333,704191022,704191231,
#                                    704191214,704191292,704191344,704191332,704191212,704191392,704191395,704191352,704191357,704191354,704191393,704191353,704191394,
#                                    704191385,704191294,704191024,704191228,704190994,704191295,704191347,704191386,704191296,704191215,704191023,704191297,704191390,
#                                    704191223,704191387,704191388,704191389,704191391,704191293,704191226,704191356,704190996,704190995,704190972,704191011,704191221,
#                                    704191005,704191222,704191036,704190997,704191038,704191225,704191037,704191227,704191039,704191040,704191010,704191009,704190942,
#                                    704191012,704190952,704190953,704190950,704190954,704191017))

##  Declare the paths to the shapefiles subsetting the countries of interest 
##  which were declared above.
##  WARNING:  You can NOT use this option in conjunction with the adminID 
##            subsetting option. At least one of the two subsetting options MUST
##            be set to NULL.
##  EXAMPLE:
##    bsgm.input.shp <-  list( "BTN"="F:\\WorldPop\\RandomeForest\\shp\\BTN\\out.shp", 
##                            "NPL"="F:\\WorldPop\\RandomeForest\\shp\\NPL\\out.shp")
bsgm.input.shp <- NULL


##  Declare a list of the character representations of the covaraites with which
##  we intend to do modeling with:
##  NOTE:  You can use the function wpgpListCountryCovariates() from the 
##         wpgpCovariates library to see what all covariates are available, but 
##         most will remain the same between covariate runs excluding the year 
##         specific part of their name.
##         EXAMPLE:  
##           wpgpListCountryCovariates(ISO3="NPL",
##                                     username = "wpftp",
##                                     password = "qw12wq1sZQ")
##  
##  NOTE:  If you are interpolating from, e.g., 2000 to 2012 you should use the 
##         category 1 protected areas corresponding to the last year of
##         modeling, e.g. 2012.
##
##  WARNING:  Ensure that the covariates declared match the year declared for 
##            the bsgm.input.year variable.
##
##
##  WARNING:  UNLIKE REGULAR SCRIPT YOU WILL NEED TO MANUALLY DEFINE THE COVARIATE
##            NAMES AND WHETHER OR NOT THEY ARE PART OF THE "REGULAR" DATABASE 
##            OF EXPECTED COVARIATES. MAKE SURE COVARIATE FILE NAMES MATCH THE 
##            EXPECTED FILE NAME PATTERN SET FORTH BY THE WPGLOBAL COVARIATES.
bsgm.nonstand.cvr <- c("esa_cls190_dst_2000",
                       "esa_cls190_prp_1_2000",
                       "esa_cls190_prp_5_2000",
                       "esa_cls190_prp_10_2000",
                       "esa_cls190_prp_15_2000"
                       )
##  All observed extent years, preferrably listed in chronological order.
##  NOTE:  the earliest and latest year must correspond to the t0 and t1 BSGM extents year.
observed_years <- c(2000,2005,2010)
##  Key word name of the initial/earliest BS extent being given to the model:
bsgm.t0.extents <- "esa_cls190_2000"
##  Key word name of other BS extent datasets which should match the years, 
##  that are not t1 and t0, in the observed extents vector:
bsgm.other.extents <- c("esa_cls190_2005")
##  Key word name of the last/latest BS extent being given to the model:
bsgm.t1.extents <- "esa_cls190_2010"
##  Input predictive covariate key word names:
bsgm.input.cvr <- list("slope",
                       "topo",
                       "tt50k_2000",
                       "osmroa_dst",
                       "osmriv_dst",
                       "cciwat_dst",
                       "wclim_prec",
                       "wclim_temp",
                       "ccilc_dst011_2000",
                       "ccilc_dst040_2000",
                       "ccilc_dst130_2000",
                       "ccilc_dst150_2000",
                       "ccilc_dst160_2000",
                       "ccilc_dst200_2000",
                       # "urbanaccessibility_2015",
                       "wdpa_cat1_dst_2010"
)




##  What mode should the BSGM be run in? "interpolation" or "extrapolation"?
##  WARNING:  IF THE BSGM MODE IS "INTERPOLATION" THEN THE COVARIATE FOR THE 
##            BUILT EXTENTS AT TIME t1 NEED TO BE INCLUDED IN THE COVARIATE LIST
##            (bsgm.input.cvr) ABOVE.
#bsgm.mode <- "interpolation"


##  Declare if we are using a fixed set in this modeling, i.e. are we 
##  parameterizing, in part or in full, this RF model run upon another 
##  country's(ies') RF model object. 
##  EXAMPLE:
##    bsgm.fixed.set <- c("IND", "CHN")
##
bsgm.fixed.set <- NULL


nmb = 25
##  ------  NOTHING SHOULD BE MODIFIED WITHIN THESE BRACKETS BELOW THIS LINE
##  ------  IN REGULAR PRACTICE
##  Assign the proper covariate name for retrieval in primary script similar 
##  to how we download the watermask and the level 1 rasters:
#bsgm.t1.extents <- bsgm.obs.ext.cvr.names[[as.character(t1)]]
##
##  END:  BSGM CONFIGURATION
######