#' Function calculating the coverage, distance and overlap components of the SPAG Index.
#'
#' @param companiesDF - data frame with information regarding the companies. The data frame needs four columns:
#' the longitude and latitude, the number of employees in each company and a category to which the company is assigned.
#' The columns should be provided in the aforementioned order.
#' @param shp - SpatialPolygonsDataFrame object obtained via loading a shapefile.
#' @param theoreticalSample - number of Companies used of the estimation of the average distance between companies assuming 
#' uniform distribution.
#' @param empiricalSample - number of companies used for the estimation of the average distance between companeis for which
#' the distance index is being calculated.
#'
#'
#'
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

SPAG <- function(companiesDF, shp, theoreticalSample=1000, empiricalSample=1000, numberOfSamples=1, eachArea=FALSE,
                 companiesProjection, CRSProjection){
  
  currentWarning <- getOption("warn")
  options(warn = -1)
  
  
  if(!missing(companiesProjection)){
    companySpatialPoints <- SpatialPoints(companiesDF[,c(1,2)], proj4string=companiesProjection)
  } else{
    companySpatialPoints <- SpatialPoints(companiesDF[,c(1,2)], proj4string=shp@proj4string)
  }
  
  if(!missing(CRSProjection)){
    if(!identical(shp@proj4string, CRS(CRSProjection))){
      #newCoordinateSystem<-"+init=epsg:3347" # In this coordinates a circle looks like a circle
      shp <- spTransform(shp, CRSProjection)
      companySpatialPoints <- spTransform(companySpatialPoints, CRSProjection)
      companiesDF[,c(1,2)] <- as.data.frame(companySpatialPoints)
     # CRSProjection <- CRS(CRSProjection)
    }
    CRSProjection <-CRS(CRSProjection)
  } else {
    CRSProjection <- shp@proj4string
  }
  
  if(!eachArea){
    result <- SPAGSingle(companiesDF, shp, theoreticalSample, empiricalSample, numberOfSamples, CRSProjection)
    return(result)
  } else {
    przypisanie <- SpatialPoints(companiesDF[,c(1,2)], proj4string=CRSProjection) %over% shp
    companiesDF <- companiesDF[!is.na(przypisanie$jpt_nazwa_),]
    przypisanie <- przypisanie[!is.na(przypisanie$jpt_nazwa_),]
    nazwyRegionow <- shp@data$jpt_nazwa_
    listaFinalna <- list()
    for (i in 1:length(nazwyRegionow)){
      daneTestowe <- companiesDF[przypisanie$jpt_nazwa_==nazwyRegionow[i],]
      shpTest <- SpatialPolygonsDataFrame(SpatialPolygons(list(shp@polygons[[i]]),proj4string = CRSProjection),shp@data[i,])
      listaFinalna[[as.character(nazwyRegionow[i])]] <- SPAGSingle(daneTestowe,shpTest,theoreticalSample, empiricalSample, numberOfSamples,CRSProjection)
    }
    return(listaFinalna)
  }
  
}


SPAGSingle <- function(companiesDF, shp, theoreticalSample=1000, empiricalSample=1000, numberOfSamples=1, CRSProjection){
  
  # Calculating the coverage part of SPAG index:
  categories <- unique(companiesDF[,4])
  
  circles <-calcCircles(shp,companiesDF,categories, CRSProjection)
  circlesList <- lapply(categories,
                        function(x){
                          circles$Categories[companiesDF[,4]==x]
                        })

  # One of the most intensive part - calculating the union for categories and total
  CirclesUnionCategory <- lapply(categories,
                                 function(x){
                                   unionArea <- gUnaryUnion(circles$Categories[companiesDF[,4]==x])
                                   unionArea
                                 })
  for (i in 1:length(CirclesUnionCategory)){
    CirclesUnionCategory[[i]]@polygons[[1]]@ID <- as.character(i)
  }
  circlesList <- setNames(circlesList, categories)
  CirclesUnionTotal <- gUnaryUnion(circles$Total)
  circlesListTotal <- SpatialPolygons(lapply(CirclesUnionCategory, function(x){x@polygons[[1]]}))
  CirclesUnionCategoryArea <- lapply(CirclesUnionCategory, function(x){
    gArea(x)
  })
  
  CirclesUnionTotalArea <- gArea(CirclesUnionTotal)
  names(CirclesUnionCategory) <- categories
  CirclesUnionCategory$Total <- CirclesUnionTotal
  circlesList$Total <- circles$Total
  # Calculating the indexes
  IOver <- calcOverlapIndex(circles, companiesDF, categories, CirclesUnionCategoryArea, CirclesUnionTotalArea)
  ICov <- calcCoverageIndex(companiesDF[,c(3, 4)], categories)
  IDist <- calcDistanceIndex(companiesDF[,c(1, 2, 4)], shp, categories,theoreticalSample, empiricalSample, numberOfSamples)
  ISPAG = IDist*IOver*ICov
  
  categories <- c(as.character(categories), "Total")
  names(companiesDF) <- c("long","lat","emp", "categories")
  
  IndexDF <- data.frame(categories,IDist, IOver,ICov,ISPAG)
  companyList <- list(companies = companiesDF)
  
 #x <- list( SPAGIndex = IndexDF)
 #print(class(companiesDF))
  #x@map = region
  #class(x) <- "SPAG"
  #setClass("SPAG", slots=c(map="SpatialPolygonsDataFrame", unionAreaList="list", companies="data.frame"))
  #x <- new("SPAG", map=region, unionAreaList = CirclesUnionCategory, companies = companiesDF)
  #options(warn = currentWarning)
  #slot(x, "map") <- region
  
 # x <- list( map = region , unionAreaList = CirclesUnionCategory, companies = companiesDF, SPAGIndex = IndexDF)
  #class(x) <- "SPAG"
  
#  x <- list(SPAGIndex = IndexDF)
#  class(x) <- "SPAG"
#  
#  attr(x, "map") <- region
#  attr(x, "unionAreaList") <- CirclesUnionCategory
#  attr(x, "companies") <- companiesDF
 # setClass("SPAG", representation(map="SpatialPolygonsDataFrame", unionAreaList="list", companies="data.frame", SPAGIndex ="data.frame", circles="list"))
# x <- new("SPAG",  map=shp, unionAreaList = CirclesUnionCategory, companies = companiesDF, SPAGIndex = IndexDF, circles=circlesList)
  x <- IndexDF
  attr(x, "IndexDF") <- IndexDF
  attr(x, "map") <- shp
  attr(x, "unionAreaList") <- CirclesUnionCategory
  attr(x, "companies") <- companiesDF
  attr(x, "circles") <-circlesList
 # setOldClass("data.frame")
  class(x) <- "SPAG"
  
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

calcDistanceIndex <- function(coordsCategoryDF, region, categories,theoreticalSample, empiricalSample, numberOfSamples){
  IDistFULL <- vector(mode="numeric", length=0)
  
  for (i in 1:numberOfSamples){
  IDist<- sapply(categories,
                 function(x){
                   n <- nrow(coordsCategoryDF[coordsCategoryDF[,3] == x,])
                   if(n>1){
                   nCompanies <- min(theoreticalSample,n)
                   theoreticalCompanies <- spsample(region, nCompanies, type="regular", offset = c(0,0))
                   theoreticalDF <- as.data.frame(theoreticalCompanies)
                   theoreticalDist<-dist(as.matrix(theoreticalCompanies@coords))
                   
                   nCompanies <- min(empiricalSample,n)
                   index<-sample(1:n, nCompanies, replace = FALSE)
                   companiesChoose = coordsCategoryDF[coordsCategoryDF[,3]==x,c(1,2)]
                   meanDist <- mean(dist(as.matrix(companiesChoose[index,])))/mean(theoreticalDist)
                   if (is.finite(meanDist)){
                     return(meanDist)
                   } else return(0)
                   } else return(0)
                 })
  n <- nrow(coordsCategoryDF)
  nCompanies <- min(theoreticalSample,n)
  theoreticalCompanies <- spsample(region, nCompanies, type="regular")
  theoreticalDF <- as.data.frame(theoreticalCompanies)
  theoreticalDist <-dist(as.matrix(theoreticalCompanies@coords))
  
  nCompanies <- min(empiricalSample,n)
  index<-sample(1:n, nCompanies, replace = FALSE)
  IDistTotal <-  mean(dist(as.matrix(coordsCategoryDF[index,c(1,2)])))/mean(theoreticalDist)
  IDistFULL <- rbind(IDistFULL, c(IDist,IDistTotal))
  }
  return(apply(IDistFULL,2,mean))
}

calcCircles <- function(region,companiesDF,categories, CRSProjection){
  # Calculating base radius for other indexes
  # In order to do so I change the CRS to a system in which the area is not showed in degrees
  # as circles in those coordinates are oblate ellipses.
 # if(is(region,"SpatialPolygonsDataFrame")){
    area <- rgeos::gArea(region)
#} else {
#    area <- region@area
#  }
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

  xySP <- SpatialPoints(companiesDF[,c(1,2)], proj4string=CRSProjection)

  # Transforming the coordinates to be in the same system as the shapefile
  # newCoordinateSystem<-"+proj=longlat +datum=WGS84"
  # xySP2 <- spTransform(xySP, CRS(newCoordinateSystem))
  
  # New circles will appear as circluar in plot
  return(list("Categories" = gBuffer(xySP, quadsegs=50, byid=TRUE, width=vectorOfRadius),
              "Total" = gBuffer(xySP, quadsegs=50, byid=TRUE, width=radiusVectorTotal)))
}

calcOverlapIndex <- function(circles, companiesDF, categories, CirclesUnionCategoryArea, CirclesUnionTotalArea){
  IOver <- mapply(function(x,y){
    y /gArea(circles$Categories[companiesDF[,4]==x])
  },categories,CirclesUnionCategoryArea)
  
  IOver <- c(IOver, CirclesUnionTotalArea/gArea(circles$Total))
  
  return(IOver)
}

#' @export
#plot.SPAG = function(x, category="Total", addCompanies=TRUE){
#  
#  currentWarning <- getOption("warn")
#  options(warn = -1)
#  
#  mapDF <- fortify(x$map)
#  unionArea <- fortify(x$unionAreaList[[category]])
#  tekstX = min(min(mapDF[,1]), min(unionArea[,1]))+1
#  tekstY = max(max(mapDF[,2]), max(unionArea[,2]))
#  if(category=="Total"){
#    companies <- x$companies
#  } else {
#    companies <- x$companies[x$companies[,4]==category,]
#  }
#  
#  mapPlot <- ggplot() +
#             geom_polygon(data=unionArea, aes(long, lat, group=group), colour='#D3D3D3', fill='#D3D3D3') +
#             geom_polygon(data=mapDF, aes(long, lat, group=group), colour='#808080', fill=NA) +
#             theme_nothing() +
#             labs(long="longitude", lat="latitude") #+
#            # annotate("text", x=tekstX, y=tekstY, label= paste("Category: ",category ))
#
#if(addCompanies){
#  mapPlot <- mapPlot +
#    geom_point(data=companies[,c(1,2)], aes(long,lat),size=0.4)
#}
#  
#  mapPlot
#}

ggplot.SPAG = function(x, category="Total", addCompanies=TRUE, circleUnion=FALSE){
  
  currentWarning <- getOption("warn")
  options(warn = -1)
  
  if(circleUnion){
    
    polygonArea <- fortify(attr(x,"unionAreaList")[[category]])
  } else {
    polygonArea <- fortify(attr(x,"circles")[[category]])
  }
  
  mapDF <- fortify(attr(x,"map"))
  tekstX = min(min(mapDF[,1]), min(polygonArea[,1]))+1
  tekstY = max(max(mapDF[,2]), max(polygonArea[,2]))
  
  if(category=="Total"){
    companies <- attr(x,"companies")
  } else {
    companies <- attr(x,"companies")[attr(x,"companies")[,4]==category,]
  }
  
  if(circleUnion){
  mapPlot <- ggplot() +
    geom_polygon(data=polygonArea, aes(long, lat, group=group), colour='#D3D3D3', fill='#D3D3D3') +
    geom_polygon(data=mapDF, aes(long, lat, group=group), colour='#808080', fill=NA) +
    theme_nothing() +
    labs(long="longitude", lat="latitude")
  } else{
    mapPlot <- ggplot() +
      geom_polygon(data=polygonArea, aes(long, lat, group=group), colour='#D3D3D3', fill=NA) +
      geom_polygon(data=mapDF, aes(long, lat, group=group), colour='#808080', fill=NA) +
      theme_nothing() +
      labs(long="longitude", lat="latitude")
    }#+
  # annotate("text", x=tekstX, y=tekstY, label= paste("Category: ",category ))
  
  if(addCompanies){
    mapPlot <- mapPlot +
      geom_point(data=companies[,c(1,2)], aes(long,lat),size=0.4)
  }
  
  mapPlot
}

plot.SPAG = function(x, category="Total", addCompanies=TRUE, circleUnion=FALSE){
  
  currentMargain <- par()$mar
  
  if(category=="Total"){
    companies <- attr(x,"companies")
  } else {
    companies <- attr(x,"companies")[attr(x,"companies")[,4]==category,]
  }
  
  if(circleUnion){
    polygonArea <- attr(x,"unionAreaList")[[category]]
  } else {
    polygonArea <- attr(x,"circles")[[category]]
  }
  
  par(mar = rep(0, 4))
  plot(attr(x,"map"), border='#808080')
  plot(polygonArea, add=TRUE)
 #plot(x@unionAreaList[["Total"]])
 #plot(x@map, border='#808080', add=TRUE)
 ##points(companies[,c(1,2)], add=TRUE)
 if(addCompanies){points(companies[,c(1,2)],pch=16,cex=0.2)}
  
 par(mar=currentMargain)
}

#' @export
print.SPAG = function(x, ...){
  print(attr(x, "IndexDF") )
}

