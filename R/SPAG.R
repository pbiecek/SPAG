#' @useDynLib SPAG
#' @importFrom Rcpp sourceCpp
NULL

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
                 columnAreaName, companiesProjection, CRSProjection, totalOnly=FALSE){
  
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
    }
    CRSProjection <-CRS(CRSProjection)
  } else {
    CRSProjection <- shp@proj4string
  }
  
  if(!eachArea){
    result <- SPAGSingle(companiesDF, shp, theoreticalSample, empiricalSample, numberOfSamples, CRSProjection, totalOnly)
    return(result)
  } else {
    
    # Error handling for calculating SPAG in each area
    if(missing(columnAreaName)){
      stop("eachArea was selected, but columnAreaName was not provided")
    }
    if(! columnAreaName %in% names(shp)){
      stop("Provided columnAreaName was not found in the map file.")
    }
    
    # Preparing regions to be analysed
    companiesPoints <- SpatialPoints(companiesDF[,c(1,2)], proj4string=CRSProjection) %over% shp
    companiesDF <- companiesDF[!is.na(companiesPoints[[columnAreaName]]),] #jpt_nazwa_
    companiesPoints <- companiesPoints[!is.na(companiesPoints[[columnAreaName]]),]
    regionNames <- shp@data[[columnAreaName]]
    finalList <- list()
    
    # calculating SPAG for each area
    for (i in 1:length(regionNames)){
      daneTestowe <- companiesDF[companiesPoints[[columnAreaName]]==regionNames[i],]
      if(nrow(daneTestowe)){
      shpTest <- SpatialPolygonsDataFrame(SpatialPolygons(list(shp@polygons[[i]]),proj4string = CRSProjection),shp@data[i,])
      finalList[[as.character(regionNames[i])]] <- SPAGSingle(daneTestowe,shpTest,theoreticalSample, empiricalSample,
                                                              numberOfSamples, CRSProjection, totalOnly)
      }
    }
    
    return(finalList)
  }
  
}


SPAGSingle <- function(companiesDF, shp, theoreticalSample=1000, empiricalSample=1000, numberOfSamples=1, CRSProjection,
                       totalOnly){
  
  # Calculating the coverage part of SPAG index:
  
  circles <-calcCircles(shp,companiesDF, CRSProjection, totalOnly)
  CirclesUnionCategory <- list()
  circlesList <- list()
  
  categories <- unique(companiesDF[,4])
  
  if(!totalOnly){
  circlesList <- lapply(categories,
                        function(x){
                          circles$Categories[companiesDF[,4]==x]
                        })

  CirclesUnionCategory <- lapply(categories,
                                 function(x){
                                   unionArea <- gUnaryUnion(circles$Categories[companiesDF[,4]==x])
                                   unionArea
                                 })
  
  for (i in 1:length(CirclesUnionCategory)){
    CirclesUnionCategory[[i]]@polygons[[1]]@ID <- as.character(i)
  }
  
  circlesList <- setNames(circlesList, categories)
  
  circlesListTotal <- SpatialPolygons(lapply(CirclesUnionCategory, function(x){x@polygons[[1]]}))
  CirclesUnionCategoryArea <- lapply(CirclesUnionCategory, function(x){
    gArea(x)
  })
  
  names(CirclesUnionCategory) <- categories
  
  }
  # One of the most intensive part - calculating the union for categories and total

  CirclesUnionTotal <- gUnaryUnion(circles$Total)
  CirclesUnionTotalArea <- gArea(CirclesUnionTotal)
  
  CirclesUnionCategory$Total <- CirclesUnionTotal
  circlesList$Total <- circles$Total
  
  # Calculating the indexes
  IOver <- calcOverlapIndex(circles, companiesDF, categories, CirclesUnionCategoryArea, CirclesUnionTotalArea, totalOnly)
  ICov <- calcCoverageIndex(companiesDF[,c(3, 4)], categories, totalOnly)
  IDist <- calcDistanceIndex(companiesDF[,c(1, 2, 4)], shp, categories,theoreticalSample, empiricalSample, numberOfSamples, totalOnly)
  ISPAG = IDist*IOver*ICov
  
  if(!totalOnly){
  categories <- c(as.character(categories), "Total") } else { categories <- "Total" }
  names(companiesDF) <- c("long","lat","emp", "categories")
  
  IndexDF <- data.frame(categories,IDist, IOver,ICov,ISPAG)
  companyList <- list(companies = companiesDF)

  x <- IndexDF
  attr(x, "IndexDF") <- IndexDF
  attr(x, "map") <- shp
  attr(x, "unionAreaList") <- CirclesUnionCategory
  attr(x, "companies") <- companiesDF
  attr(x, "circles") <-circlesList
  class(x) <- "SPAG"
  
  return(x);
}

calcCoverageIndex <- function(employmentCategoryDF, categories, totalOnly){
  # This data frame consists of two columns - the first with information about employment and the second one with categories
  if(!totalOnly){
    totalEmployment <- sum(employmentCategoryDF[,1])
    ICov <- sapply(categories, function(x){
                   return (sum(employmentCategoryDF[employmentCategoryDF[,2]==x,1])/totalEmployment)
                 })
  return(c(ICov,1))} else {return(1)}
}

calcDistanceIndex <- function(coordsCategoryDF, region, categories,theoreticalSample, empiricalSample, numberOfSamples, 
                              totalOnly){
  
  IDistFULL <- vector(mode="numeric", length=0)
  
    if(!totalOnly){
      IDist<- sapply(categories,function(x){
                       n <- nrow(coordsCategoryDF[coordsCategoryDF[,3] == x,])
                       if(n>1){
                         nCompanies <- min(theoreticalSample,n)
                         theoreticalCompanies <- spsample(region, nCompanies, type="regular", offset = c(0,0))
                         theoreticalDF <- as.matrix(as.data.frame(theoreticalCompanies))
                         theoreticalDist <- mean(dist(theoreticalDF))
                         theoreticalDist <- avgDist(theoreticalDF)

                         
                         nCompanies <- min(empiricalSample,n)
                         meanDist <- numeric(0)
                         for (i in 1:numberOfSamples){
                           index<-sample(1:n, nCompanies, replace = FALSE)
                           companiesChoose = coordsCategoryDF[coordsCategoryDF[,3]==x,c(1,2)]
                           meanDist <- c(meanDist,mean(dist(as.matrix(companiesChoose[index,])))/theoreticalDist)
                         }
                         meanDist <- mean(meanDist)
                         
                         if (is.finite(meanDist)){
                           return(meanDist)
                         } else return(0)
                       } 
                       else return(0)
                     })
    }
    
  n <- nrow(coordsCategoryDF)
  nCompanies <- min(theoreticalSample,n)
  theoreticalCompanies <- spsample(region, nCompanies, type="regular")
  theoreticalDF <- as.data.frame(theoreticalCompanies)
  theoreticalDist <-mean(dist(as.matrix(theoreticalCompanies@coords)))
  
  nCompanies <- min(empiricalSample,n)
  IDistTotal <- numeric(0)
  
  for (i in 1:numberOfSamples){
    index<-sample(1:n, nCompanies, replace = FALSE)
    IDistTotal <-  c(IDistTotal,mean(dist(as.matrix(coordsCategoryDF[index,c(1,2)])))/theoreticalDist)
  }
  IDistTotal <- mean(IDistTotal)
  if(!totalOnly){IDistFULL <- rbind(IDistFULL, c(IDist,IDistTotal))}else {IDistFULL <- rbind(IDistFULL,IDistTotal) }
 # }
  return(apply(IDistFULL,2,mean))
}

calcCircles <- function(region,companiesDF, CRSProjection, totalOnly){
  # Calculating base radius for other indexes
  # In order to do so I change the CRS to a system in which the area is not showed in degrees
  # as circles in those coordinates are oblate ellipses.
  # if(is(region,"SpatialPolygonsDataFrame")){
    area <- rgeos::gArea(region)
    
    if(!totalOnly){
      categories <- unique(companiesDF[,4])
  
      rBase <- sapply(categories,
                    function(x){
                      return (sqrt(area/(sum(companiesDF[companiesDF[,4]==x,3])*pi)))
                    })
      rBaseDF <- data.frame(rBase, names=categories)
      
      baseRadiusVector <- sapply(companiesDF[,4],
                                 function(x){
                                   rBaseDF[rBaseDF$names==x,]$rBase
                                 })
      vectorOfRadius <- sqrt(companiesDF[,3])*baseRadiusVector
    }
    
  rBaseTotal <- sqrt(area/(sum(companiesDF[,3])*pi))
  radiusVectorTotal <-sqrt(companiesDF[,3])*rBaseTotal

  
  # Currently I assume the points in the data frame are traditional coordinates:

  xySP <- SpatialPoints(companiesDF[,c(1,2)]) #SpatialPoints(companiesDF[,c(1,2)], proj4string=CRSProjection) - dodanie projekcji wykrzywia kó³ka

  # Transforming the coordinates to be in the same system as the shapefile
  # newCoordinateSystem<-"+proj=longlat +datum=WGS84"
  # xySP2 <- spTransform(xySP, CRS(newCoordinateSystem))
  
  # New circles will appear as circluar in plot
  if (!totalOnly){
  return(list("Categories" = gBuffer(xySP, quadsegs=50, byid=TRUE, width=vectorOfRadius),
              "Total" = gBuffer(xySP, quadsegs=50, byid=TRUE, width=radiusVectorTotal)))
  } else {
    return(list("Total" = gBuffer(xySP, quadsegs=50, byid=TRUE, width=radiusVectorTotal)))
  }
}

calcOverlapIndex <- function(circles, companiesDF, categories, CirclesUnionCategoryArea, CirclesUnionTotalArea, totalOnly){
  
  if(!totalOnly){IOver <- mapply(function(x,y){
    y /gArea(circles$Categories[companiesDF[,4]==x])
  },categories,CirclesUnionCategoryArea)
  
  IOver <- c(IOver, CirclesUnionTotalArea/gArea(circles$Total))
  } else {IOver <- CirclesUnionTotalArea/gArea(circles$Total) }
  
  return(IOver)
}

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

  xmin <- min(attr(x,"map")@bbox[1,1], polygonArea@bbox[1,1])
  xmax <- max(attr(x,"map")@bbox[1,2], polygonArea@bbox[1,2])
  ymin <- min(attr(x,"map")@bbox[2,1], polygonArea@bbox[2,1])
  ymax <- max(attr(x,"map")@bbox[2,2], polygonArea@bbox[2,2])
  
  plot(polygonArea, col='#808080', xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  plot(attr(x,"map"), border='#808080', add=TRUE)

 if(addCompanies){points(companies[,c(1,2)],pch=16,cex=0.2)}
}

#' @export
print.SPAG = function(x, ...){
  print(attr(x, "IndexDF") )
}

