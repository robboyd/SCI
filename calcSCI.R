#' \code{calcSCI}
#'
#' This function calculates Species Climate Indices for all species in the data provided. 
#' @param dat string. A data.frame containing columns for species name, x coordinates, y coordinates, spatialUncertainty, year and an identifier (used to group the data - SCI will be calculated for each group). 
#' @param species Character string. column name in dat giving species names.
#' @param x string. Column name in dat giving x coordinates. Any coordinate and spatial reference systems are permitted.
#' @param y string. Column name in dat giving y coordinates. Any coordinate and spatial reference systems are permitted.
#' @param year string. Column name in dat giving years.
#' @param spatialUncertainty String. Column name in dat giving uncertainty associated with x and y. Any units are permitted. 
#' @param identifier String. Column name in dat giving record "identifiers". Identifiers are used to group the data; SCIs will be calculated separately for each group.
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param envDat String. A dataframe with n columns and N rows where n are environmental variables and N are the values of those variables at the locations of x and y in the occurrence data. N must be the same length as the coordinates in dat.
#' @param maxSpatUncertainty Numeric. Maximum permitted spatial uncertainty. All records more uncertain than this value will be dropped. Units must match the units in your data.
#' @return a list with two entries: 1) the PCA results with columns for period, identifier, scores.PC1,.. scores.PCn, xVar (proportion of variance explained by xPC) and yVar (as xVar but for yPC); and 2) the PCA plots.
#' @export

calcSCI <- function(dat,
                    species,
                    x,
                    y,
                    spatialUncertainty, 
                    year, 
                    identifier,
                    periods,
                    envDat,
                    maxSpatUncertainty = NULL) {
  
  if (any(!(c(species, x, y, year, spatialUncertainty, identifier) %in% colnames(dat)))) stop("You have specified columns that don't exist in dat.")
  
  dat <- createData(data = dat,
                    species,
                    x,
                    y,
                    year,
                    spatialUncertainty,
                    identifier)
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  if (!is.null(maxSpatUncertainty)) dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= maxSpatUncertainty, ]
  
  if (nrow(dat) == 0) stop("No records with with spatialUncertainty < maxSpatUncertainty")
  
  if (any(is.na(dat$year))) {
    
    warning("Removing data without a specified year")
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  dat <- dat[order(dat$year), ]
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
  }
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], i, dat$Period)
    
  }
    
  out <- lapply(1:length(periods),
                  function(x) {
                    
                    pDat <- dat[dat$Period == x, ]
                    
                    SCIs <- lapply(unique(pDat$species),
                                   function(y) {
                                     
                                     data.frame(species = y,
                                                period = x,
                                                SCI = mean(raster::extract(envDat,
                                                                           pDat[pDat$species == y, c("x", "y")]),
                                                           na.rm = T))
                                     
                                   })
                    
                    do.call("rbind", SCIs)
                    
                  })

  return(do.call("rbind", out))
  
}
