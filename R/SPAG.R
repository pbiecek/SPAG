#' Function calculating the coverage, distance and overlap components of the SPAG Index.
#'
#' @param companiesDF - data frame with information regarding the companies. The data frame needs four columns:
#' the longitude and latitude, the number of employees in each company and a category to which the company is assigned.
#' The columns should be provided in the aforementioned order.
#' @param shp - SpatialPolygonsDataFrame object obtained via loading a shapefile
#'
#' @importFrom graphics plot
#' @importFrom stats dist
#' @import maptools
#' @import rgdal
#' @import rgeos
#' @import ggmap
#' @import ggplot2
#' @import sp
#'
#' @examples
#' data(CompaniesPoland)
#' data(ShapefilePoland)
#' spagIndex <- SPAG(CompaniesPoland,shp = ShapefilePoland)
#' print(spagIndex)
#' plot(spagIndex)
#' plot(spagIndex, addCompanies = FALSE)
#' plot(spagIndex, category = "A")
#' plot(spagIndex, category = "A") + coord_map("orthographic")
#' plot(spagIndex, category = "B")
#' plot(spagIndex, category = "C")
#' @export

SPAG <- function(companiesDF, shp){
  
  currentWarning <- getOption("warn")
  options(warn = -1)
  
  newCoordinateSystem<-"+init=epsg:3347" # In this coordinates a circle looks like a circle
  newCoordinateSystem<-"+proj=longlat +datum=WGS84" # CRS in degrees
  
  region<-spTransform(shp, CRS(newCoordinateSystem))
  
  
  # Calculating the coverage part of SPAG index:
  categories <- unique(companiesDF[,4])
  
  
  circles <-calcCircles(region,companiesDF,categories)
  
  # Most intensive part - calculating the union for categories and total
  CirclesUnionCategory <- lapply(categories,
                                 function(x){
                                   unionArea <- gUnaryUnion(circles[companiesDF[,4]==x])
                                   unionArea
                                 })
  CirclesUnionTotal <- gUnaryUnion(circles)
  
  CirclesUnionCategoryArea <- lapply(CirclesUnionCategory, function(x){
    gArea(x)
  })
  CirclesUnionTotalArea <- gArea(CirclesUnionTotal)
  names(CirclesUnionCategory) <- categories
  CirclesUnionCategory$total <- CirclesUnionTotal
  
  # Calculating the indexes
  IOver <- calcOverlapIndex(circles, companiesDF, categories, CirclesUnionCategoryArea, CirclesUnionTotalArea)
  ICov <- calcCoverageIndex(companiesDF[,c(3, 4)], categories)
  IDist <- calcDistanceIndex(companiesDF[,c(1, 2, 4)], region, categories)
  ISPAG = IDist*IOver*ICov
  
  categories <- c(categories, "Total")
  names(companiesDF) <- c("long","lat","emp", "categories")
  
  IndexDF <- data.frame(categories,IDist, IOver,ICov,ISPAG)
  companyList <- list(companies = companiesDF)
  
  x <- list( map = region , unionAreaList = CirclesUnionCategory, companies = companiesDF, SPAGIndex = IndexDF)
  class(x) <- "SPAG"
  
  options(warn = currentWarning)
  return(x);
}

calcCoverageIndex <- function(employmentCategoryDF, categories){
  # This data frame consists of two columns - the first with information about employment and the second one with categories
  totalEmployment <- sum(employmentCategoryDF[,1])
  ICov <- sapply(categories,
                 function(x){
                   return (sum(employmentCategoryDF[employmentCategoryDF[,2]==x,1])/totalEmployment)
                 })
  return(c(ICov,1))
}

calcDistanceIndex <- function(coordsCategoryDF, region, categories){
  
  IDist<- sapply(categories,
                 function(x){
                   theoreticalCompanies <- spsample(region, nrow(coordsCategoryDF[coordsCategoryDF[,3]==x,]), type="regular")
                   theoreticalDF <- as.data.frame(theoreticalCompanies)
                   theoreticalDist<-dist(as.matrix(theoreticalCompanies@coords))
                   meanDist <- mean(dist(as.matrix(coordsCategoryDF[coordsCategoryDF[,3]==x,c(1,2)])))/mean(theoreticalDist)
                   if (is.finite(meanDist)){
                     return(meanDist)
                   } else return(0)
                 })
  
  theoreticalCompanies <- spsample(region, nrow(coordsCategoryDF), type="regular")
  theoreticalDF <- as.data.frame(theoreticalCompanies)
  theoreticalDist <-dist(as.matrix(theoreticalCompanies@coords))
  IDistTotal <-  mean(dist(as.matrix(coordsCategoryDF[c(1,2)])))/mean(theoreticalDist)
  
  return(c(IDist, IDistTotal))
}

calcCircles <- function(region,companiesDF,categories){
  # Calculating base radius for other indexes
  # In order to do so I change the CRS to a system in which the area is not showed in degrees
  # as circles in those coordinates are oblate ellipses.
  area <- rgeos::gArea(region)
  rBase <- sapply(categories,
                  function(x){
                    return (sqrt(area/(sum(companiesDF[companiesDF[,4]==x,3])*pi)))
                  })
  
  rBaseTotal <- sqrt(area/(sum(companiesDF[,3])*pi))
  rBaseDF <- data.frame(rBase, names=categories)
  
  baseRadiusVector <- sapply(companiesDF[,4],
                             function(x){
                               rBaseDF[rBaseDF$names==x,]$rBase
                             })
  
  radiusVectorTotal <-sqrt(companiesDF[,3])*rBaseTotal
  
  vectorOfRadius <- sqrt(companiesDF[,3])*baseRadiusVector
  
  # Currently I assume the points in the data frame are traditional coordinates:
  xySP <- SpatialPoints(companiesDF[,c(1,2)], proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Transforming the coordinates to be in the same system as the shapefile
  newCoordinateSystem<-"+proj=longlat +datum=WGS84"
  xySP2 <- spTransform(xySP, CRS(newCoordinateSystem))
  
  # New circles will appear as circluar in plot
  return(gBuffer(xySP2, quadsegs=50, byid=TRUE, width=vectorOfRadius))
}

calcOverlapIndex <- function(circles, companiesDF, categories, CirclesUnionCategoryArea, CirclesUnionTotalArea){
  IOver <- mapply(function(x,y){
    y /gArea(circles[companiesDF[,4]==x])
  },categories,CirclesUnionCategoryArea)
  
  IOver <- c(IOver, CirclesUnionTotalArea/gArea(circles))
  
  return(IOver)
}

#' @export
plot.SPAG = function(x, category="total", addCompanies=TRUE){
  
  currentWarning <- getOption("warn")
  options(warn = -1)
  
  mapDF <- fortify(x$map)
  unionArea <- fortify(x$unionAreaList[[category]])

  if(category=="total"){
    companies <- x$companies
  } else {
    companies <- x$companies[x$companies[,4]==category,]
  }

  mapPlot <- ggplot() +
             geom_polygon(data=unionArea, aes(long, lat, group=group), colour='red', fill=NA) +
             geom_polygon(data=mapDF, aes(long, lat, group=group), colour='#808080', fill=NA) +
             theme_nothing() +
             labs(long="longitude", lat="latitude")

if(addCompanies){
  mapPlot <- mapPlot +
    geom_point(data=companies[,c(1,2)], aes(long,lat),size=0.4)
}
  options(warn = currentWarning)
  mapPlot
}

#' @export
print.SPAG = function(x, ...){
  print(x$SPAGIndex)
}
