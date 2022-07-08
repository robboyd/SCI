library(SCI)
library(testthat)

context("test outputs")

data("dat")

data("envDat")

periods <- as.list(2001:2010)

test_that("check outputs are as expected", {
  
  expect_equal( length(calcSCI(dat = dat, 
                               periods = periods,
                               envDat = envDat, 
                               species = "species", 
                               spatialUncertainty = "spatialUncertainty",
                               x = "x",
                               y = "y",
                               year = "year",
                               identifier = "identifier")), 3 )
  

  expect_true( is.data.frame(calcSCI(dat = dat, 
                              periods = as.list(2001:2010),
                              envDat = envDat, 
                              species = "species", 
                              spatialUncertainty = "spatialUncertainty",
                              x = "x",
                              y = "y",
                              year = "year",
                              identifier = "identifier")))
  
  
})