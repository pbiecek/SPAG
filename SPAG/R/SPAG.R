# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


SPAG <- function(companiesDF, shp){

  require("rgeos")
  library("maptools")

  # Calculating the distance part of SPAG index

  flags <- unique(companiesDF[,4])
  vectorOfEmployment <- companiesDF[,3]
  totalEmployment <- sum(vectorOfEmployment)

  ICov <- sapply(flags,
                      function(x){
                        return (sum(companiesDF[companiesDF[,4]==x,3])/totalEmployment)
                      })

  # calculating base radius for other indexes

  area <- gArea(shp)

  rBase <- sapply(flags,
                      function(x){
                        return (sqrt(area/(sum(companiesDF[companiesDF[,4]==x,3])*pi)))
                      })


  rBaseDF <- data.frame(rBase, names=flags)

  baseRadiusVector <- sapply(companiesDF[,4],
           function(x){
             rBaseDF[rBaseDF$names==x,]$rBase
           })

  vectorOfRadius <- sqrt(companiesDF[,3])*baseRadiusVector
  xySP <- SpatialPoints(companiesDF[,1:2])
  circles <-gBuffer(xySP, quadsegs=150, byid=TRUE, width=vectorOfRadius)


  projekcja<-"+proj=longlat +datum=WGS84"
  region<-spTransform(shp, CRS(projekcja))

  # Coverage Index for all the companies
  IDist<- sapply(flags,
           function(x){
             theoreticalCompanies <- spsample(region, nrow(companiesDF[companiesDF[,4]==x,]), type="regular")
             theoreticalDF <- as.data.frame(theoreticalCompanies)
             theoreticalDist<-dist(as.matrix(theoreticalCompanies@coords))
             mean(dist(as.matrix(companiesDF[1:2])))/mean(theoreticalDist)
           })

  #Overlap Index

  IOver <- sapply(flags,
                  function(x){
                    unionArea <- unionSpatialPolygons(circles[companiesDF[,4]==x],rep(1,sum(companiesDF[,4]==x)))
                    gArea(unionArea)/gArea(circles[companiesDF[,4]==x])


  x <- list(flags = flags, IDistance = IDist, IOverlap = IOver, ICoverage = ICov, SPAG=IDist*IOver*ICov)
  class(x) <- "SPAG"

  return(x);
}
