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
  
  if (nrow(envDat) != nrow(dat)) stop("nrow of environmental data does not equal nrow of species occurrence data.")
  
  if (!is.null(maxSpatUncertainty)) dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= maxSpatUncertainty, ]
  
  if (nrow(dat) == 0) stop("No records with with spatialUncertainty < maxSpatUncertainty")
  
  dat <- cbind(dat, envDat)
  
  envCols <- ((ncol(dat) - ncol(envDat)) + 1):ncol(dat)
  
  if (any(is.na(dat$year))) {
    
    warning("Removing data without a specified year")
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  if (any(is.na(dat[, envCols[1]]))) dat <- dat[-which(is.na(dat[, envCols[1]])), ]
  
  dat <- dat[order(dat$year), ]
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
  }
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)
    
  }
  
  if (is.null(backgroundEnvDat)) {
    
    plotDat <- lapply(unique(dat$identifier),
                      function(x) { 
                        pca <- stats::prcomp(dat[dat$identifier == x, envCols])
                        scores <- pca$x
                        data.frame(Period = dat$Period[dat$identifier == x],
                                   identifier = x,
                                   scores = scores,
                                   xVar = summary(pca)$importance[2,xPC] * 100,
                                   yVar = summary(pca)$importance[2,yPC] * 100)})
    
    plotDat <- do.call("rbind", plotDat)
    
  } else {
    
    plotDat <- lapply(unique(dat$identifier),
                      function(x) { 
                        pca <- stats::prcomp(backgroundEnvDat)
                        pca2 <- stats::predict(pca, dat[dat$identifier == x, envCols])
                        scores <- rbind(pca2, pca$x)
                        data.frame(Period = c(dat$Period[dat$identifier == x], rep("background", nrow(backgroundEnvDat))),
                                   identifier = x,
                                   scores = scores,
                                   xVar = summary(pca)$importance[2,xPC] * 100,
                                   yVar = summary(pca)$importance[2,yPC] * 100)})
    
    plotDat <- do.call("rbind", plotDat)
    
  }
  
  if (!is.null(backgroundEnvDat)) {
    
    p <- ggplot2::ggplot(data = plotDat, ggplot2::aes(x = plotDat[, (2 + xPC)], y = plotDat[, (2+yPC)], 
                                                      colour = Period, group = Period,
                                                      fill = factor(ifelse(Period == "background", "background", "samples")))) + 
      ggplot2::stat_ellipse(type = "norm", geom = "polygon", alpha = 0.25) +
      ggplot2::scale_fill_manual(aesthetics = "fill", values = c("red", "white"))
    
  } else {
    
    p <- ggplot2::ggplot(data = plotDat, ggplot2::aes(x = plotDat[, (2 + xPC)], y = plotDat[, (2+yPC)], 
                                                      colour = Period, group = Period)) + 
      ggplot2::stat_ellipse(type = "norm") 
    
  } 
  
  p <- p + ggplot2::facet_wrap(~identifier) +
    ggplot2::labs(x = paste0("PC", xPC, " (", round(plotDat$xVar[1], 2), "%)"),
                  y = paste0("PC", yPC, " (", round(plotDat$yVar[1], 2), "%)"),
                  fill = "") +
    ggplot2::theme_linedraw()
  
  
  return(list(data = plotDat,
              plot = p))
  
}