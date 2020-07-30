####
##  AUTHOR:  JEREMIAH J. NIEVES
##  INITIAL DATE:  2018-06-19
##  NOTES:
##
##
####




####  Moving Proportion Calculations -------------------------------------------
calcBinaryProportion <- function(in_raster_path, radius = 1, outfile_path){
  ##  Description:
  ##  Calculates proportions of binary data present within a 1, 5, 10, or 15 
  ##  cell radius.
  ##  NOTE:  Requires raster package
  ##  Declare the weight matrix of the focal window based upon the given radius:
  if(radius == 1){
    foc_wind <- matrix(nrow = 3, ncol = 3,
                       data = c(0,1,0,
                                1,1,1,
                                0,1,0))
  }
  if(radius == 5){
    foc_wind <- matrix(nrow = 11, ncol = 11,
                       data = c(0,0,0,0,0,1,0,0,0,0,0,
                                0,0,0,1,1,1,1,1,0,0,0,
                                0,0,1,1,1,1,1,1,1,0,0,
                                0,1,1,1,1,1,1,1,1,1,0,
                                0,1,1,1,1,1,1,1,1,1,0,
                                1,1,1,1,1,1,1,1,1,1,1,
                                0,1,1,1,1,1,1,1,1,1,0,
                                0,1,1,1,1,1,1,1,1,1,0,
                                0,0,1,1,1,1,1,1,1,0,0,
                                0,0,0,1,1,1,1,1,0,0,0,
                                0,0,0,0,0,1,0,0,0,0,0))
  }
  if(radius == 10){
    foc_wind <- matrix(nrow = 21, ncol = 21,
                       data = c(0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
                                0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                                0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
                                0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                                0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0))
  }
  if(radius == 15){
    foc_wind <- matrix(nrow = 31, ncol = 31,
                       data = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
                                0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                                0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                                0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
                                0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                                0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
                                0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
                                0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                                0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                                0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0))
    
  }
  
  ##  Read in the raster:
  inras <- raster(in_raster_path)
  
  print(paste0("Begining proportion calculations for radius of ", radius, 
               " cell and writing to ", outfile_path))
  
  ##  Calculate the density:
  focal(inras,
        foc_wind,
        filename = outfile_path,
        fun = mean, 
        na.rm = T, 
        pad = T, 
        padvalue = NA,
        format = "GTiff",
        dataType = "FLT8S",
        overwrite = T,
        options = c("COMPRESS = LZW"))
  
  print("Proportion calculation complete.")
}




####  DTE CALCULATION FUNCTION  ------------------------------------------------
calcDTE <- function(in_ras_path, outfile_path){
  ##  WARNING: THIS IS USED FOR SMALL AREAS ~ 500km x 500km for the GUF+ data
  ##  NOTE:  Requires the raster and FNN packages
  ####  Constants 
  ##  Earth radius in meters:
  e_radius <- 6378100
  ##  haversine function definition with location dependent radius:
  calcHav <- function(lat1,lat2,lon1,lon2){
    ##  Transfer coordinates to radians:
    lat1 <- lat1* pi/180
    lat2 <- lat2* pi/180
    lon1 <- lon1* pi/180
    lon2 <- lon2* pi/180
    
    havdist <- 2 * e_radius * asin(sqrt(sin({lat2-lat1}/2)^2 + 
                                          cos(lat1)*cos(lat2)* 
                                          sin({lon2-lon1}/2)^2))
    
    return(havdist)
  }
  ####
  ##  Bring in the binary raster:
  inras <- raster(in_ras_path)
  
  print("Retrieving non-NA cell indices...")
  ##  Get the cell indices of the non-NA cells:
  cellind <- which(!is.na(values(inras)))
  
  ##  Get the coordinates of all non-NA cells:
  studyarea_coords <- xyFromCell(inras,
                                 cellind,
                                 spatial = FALSE)
  
  ##  Copy as a template raster:
  template_ras <-inras
  
  ##  Turn all zeros into NA:
  oneras <- inras
  oneras[which(values(inras)==0)] <- NA
  
  ##  Determine the boundary pixels of that raster:
  boundras <- boundaries(oneras, 
                         type = "inner", 
                         classes = FALSE, 
                         directions = 8,
                         asNA = FALSE)
  
  ##  Get the coordinates of the boundary raster cells that have a value of 1:
  boundarycoords <- xyFromCell(boundras, 
                               which(values(boundras)==1),
                               spatial = FALSE)
  
  rm(boundras,oneras)
  gc()
  
  ##  Determine the nearest boundary pixel for all pixels:
  ##  Note this follows the order of the cell indices in studyarea_coords.
  nn_indices <- get.knnx(boundarycoords,
                         studyarea_coords,
                         algorithm = "kd_tree",
                         k = 1)$nn.index
  
  ##  Coallate the records:
  foo_frame <- data.frame("CID" = cellind,
                          "LAT" = studyarea_coords[, 2],
                          "LON" = studyarea_coords[, 1],
                          "NN_ID" = nn_indices,
                          "NLAT" = boundarycoords[nn_indices, 2],
                          "NLON" = boundarycoords[nn_indices, 1])
  
  print("Calculating haversine distance between cells and neighbors...")
  ##  Calculate the haversine distance:
  foo_frame <- foo_frame %>% mutate(HDIST = calcHav(LAT,NLAT,LON,NLON))
  
  print("Writing DTE values to raster...")
  ##  Place the values in the raster:
  template_ras[foo_frame$CID] <- foo_frame$HDIST
  
  print("Making inner values negative...")
  ##  Make the cells which were inside the binary area negative:
  foocid <- which(values(inras)==1)
  template_ras[foocid] <- -1 * template_ras[foocid]
  
  print("Writing DTE raster to file...")
  writeRaster(template_ras,
              filename = outfile_path,
              format = "GTiff",
              datatype = "FLT8S",
              overwrite = T,
              options = c("COMPRESS = LZW"))
}




polygonize_Binary <- function(inras_path, outdir, outfile_path=NULL, retain_0 = FALSE){
  ##  Turns a binary raster into a polygon with the option to retain zero values
  ##  or only return the values of 1. 
  ##
  ##  Requires raster, rgdal, and rgeos packages.
  ##  -----
  ##  Determine the outfile name:
  if(is.null(outfile_path)){
    outfile_name <- strsplit(basename(inras_path),".tif")[[1]]
    outfile_path <- paste0(outdir, outfile_name, ".shp")
  }
  print(paste0("     Reading in raster ", basename(inras_path),"..."))
  ##  Load the input raster:
  inras <- raster(inras_path)
  ##  Polygonize:
  if(retain_0 == F){
    print("     Polygonizing values of 1...")
    print(paste0("     ",Sys.time()))
    poly_obj <- rasterToPolygons(inras, 
                                 fun = function(x) x==1, 
                                 n = 16, 
                                 na.rm = T, 
                                 dissolve = T)
  }else{
    print("     Polygonizing raster...")
    print(paste0("     ",Sys.time()))
    poly_obj <- rasterToPolygons(inras, 
                                 n = 16, 
                                 na.rm = T, 
                                 dissolve = T)
  }
  
  print(paste0("     Polygonizing complete: ", Sys.time()))
  print(paste0("     Writing to file to ", dirname(outfile_path),"..."))
  writeOGR(poly_obj, 
           dsn = outfile_path,
           layer = ifelse(is.null(outfile_path),
                          outfile_name,
                          strsplit(basename(inras_path),".tif")[[1]]),
           driver = "ESRI Shapefile",
           overwrite_layer = T)
  
  print(paste0("     Complete!"))
}
