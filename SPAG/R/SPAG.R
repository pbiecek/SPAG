#' Function calculating the coverage, distance and overlap components of the SPAG Index.
#'
#' @param companiesDF - data frame with information regarding the companies. At least four columns are required:
#' x and y coordinates of the company, the category of company and a numeric
#' @param shp - SpatialPolygonsDataFrame object obtained via loading a shapefile
#' @param xInd - number of the column in companiesDF with information regarding the latitude
#' @param yInd - number of the column in companiesDF with information regarding the longitude
#' @param empInd - number of the column in companiesDF with numeric data regarding the employment
#' @param categInd - number of the column in companiesDF information about the category of the company
#'
#' @examples 
#' data(CompaniesPoland)
#' data(ShapefilePoland)
#' spagIndex <- SPAG(CompaniesPoland,ShapefilePoland)
#' print(spagIndex)
#' plot(spagIndex)
#' plot(spagIndex, addCompanies=FALSE)
#' plot(spagIndex, "A")
#' plot(spagIndex, "B")
#' plot(spagIndex, "C")
#' 
#' @export

SPAG <- function(companiesDF, shp, xInd = 1, yInd=2, empInd = 3, categInd = 4){

  currentWarning <- getOption("warn")
  options(warn = -1)
  
  # Possibly fix this handle
  # TODO: Which Transform to use?
  if(is.projected(shp) %in% c(NA, FALSE)){
    stop("The shapefile has no projection")
  } else{
    shp <-spTransform(shp, CRS("+proj=tmerc +lat_0=0 +lon_0=18.99999999999998 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +towgs84=0,0,0 +units=m +no_defs"))
  }

  # TODO: In general - add some conditions on the shapefile - Coordinate System etc.
  # TODO: handle not empty shapefile 
  categories <- unique(companiesDF[,categInd])
  vectorOfEmployment <- companiesDF[,empInd]
  totalEmployment <- sum(vectorOfEmployment)

  # Calculating the coverage part of SPAG index:
  
  ICov <- sapply(categories,
                      function(x){
                        return (sum(companiesDF[companiesDF[,categInd]==x,empInd])/totalEmployment)
                      })

  # Calculating base radius for other indexes
  # In order to do so I change the CRS to a system in which the area is not showed in degrees
  # as circles in those coordinates are oblate ellipses.
  
  newCoordinateSystem<-"+init=epsg:3347"
  # CRS in degrees
  newCoordinateSystem<-"+proj=longlat +datum=WGS84"
  
  region<-spTransform(shp, CRS(newCoordinateSystem))

  area <- gArea(region)

  rBase <- sapply(categories,
                      function(x){
                        return (sqrt(area/(sum(companiesDF[companiesDF[,categInd]==x,empInd])*pi)))
                      })

  rBaseTotal <- sqrt(area/(sum(companiesDF[,empInd])*pi))

  rBaseDF <- data.frame(rBase, names=categories)

  baseRadiusVector <- sapply(companiesDF[,categInd],
           function(x){
             rBaseDF[rBaseDF$names==x,]$rBase
           })

  radiusVectorTotal <-sqrt(companiesDF[,empInd])*rBaseTotal

  vectorOfRadius <- sqrt(companiesDF[,empInd])*baseRadiusVector
  
  # Currently I assume the points in the data frame are traditional coordinates:
  xySP <- SpatialPoints(companiesDF[,c(xInd,yInd)], proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Transforming the coordinates to be in the same system as the shapefile
  xySP2 <- spTransform(xySP, CRS(newCoordinateSystem))
  
  # New circles will appear as circluar in plot
  circles <-gBuffer(xySP2, quadsegs=150, byid=TRUE, width=vectorOfRadius)

  # Coverage Index for all the companies
  IDist<- sapply(categories,
           function(x){
             theoreticalCompanies <- spsample(region, nrow(companiesDF[companiesDF[,categInd]==x,]), type="regular")
             theoreticalDF <- as.data.frame(theoreticalCompanies)
             theoreticalDist<-dist(as.matrix(theoreticalCompanies@coords))
             meanDist <- mean(dist(as.matrix(companiesDF[companiesDF[,categInd]==x,c(xInd,yInd)])))/mean(theoreticalDist)
             if (is.finite(meanDist)){
               return(meanDist)
             } else return(0)
           })

  theoreticalCompanies <- spsample(region, nrow(companiesDF), type="regular")
  theoreticalDF <- as.data.frame(theoreticalCompanies)
  theoreticalDist <-dist(as.matrix(theoreticalCompanies@coords))
  IDistTotal <-  mean(dist(as.matrix(companiesDF[c(xInd,yInd)])))/mean(theoreticalDist)

  #Overlap Index

  IOver <- sapply(categories,
                  function(x){
                    unionArea <- gUnaryUnion(circles[companiesDF[,categInd]==x])
                    gArea(unionArea)/gArea(circles[companiesDF[,categInd]==x])
                  })

  CategoryArea <- sapply(categories,
                        function(x){
                          unionArea <- gUnaryUnion(circles[companiesDF[,categInd]==x])
                          unionArea
                        })
  
  CategoryArea$proj4string = CRS("+proj=longlat +datum=WGS84")

  # wont work when name total is in a category
  # redundant code - clean it up
  
  names(CategoryArea) <- categories
  CategoryArea$total <- gUnaryUnion(circles)

  unionArea <- unionSpatialPolygons(circles,rep(1,nrow(companiesDF)))
  IOverTotal <- gArea(unionArea)/gArea(circles)


  ISPAG = IDist*IOver*ICov
  IndexDF <- data.frame(categories,IDist, IOver,ICov,ISPAG)

  IndexTotal <- data.frame("Total",IDistTotal,IOverTotal,1,IDistTotal*IOverTotal)

  names(IndexTotal) <- c("categories","IDist","IOver","ICov","ISPAG")
  IndexDF <- rbind(IndexDF,IndexTotal)
  
  companyList <- list(companies = CompaniesPoland, xInd = xInd, yInd=yInd, empInd = empInd, categInd = categInd)

  x <- list( map = region , unionAreaList = CategoryArea, companiesList = companyList, SPAGIndex = IndexDF)
  class(x) <- "SPAG"

  options(warn = currentWarning)
  return(x);
}


plot.SPAG = function(x, category="total", addCompanies=TRUE){

  mapDF <- fortify(x$map)
  unionArea <- fortify(x$unionAreaList[[category]])
  
  if(category=="total"){
    companies <- x$companiesList$companies
  } else {
    companies <- x$companiesList$companies[x$companiesList$companies[,x$companiesList$categInd]==category,]
  }
  
  mapPlot <- ggplot() +
             geom_polygon(data=unionArea, aes(long, lat, group=group), colour='red', fill=NA) +
             geom_polygon(data=mapDF, aes(long, lat, group=group), colour='#808080', fill=NA) +
             theme_bw() +
             labs(x="longitude", y="latitude")
  
if(addCompanies){mapPlot <- mapPlot + geom_point(data=companies[,c(x$companiesList$xInd,x$companiesList$yInd)], aes(x,y),size=0.4)}
  mapPlot
  
}

print.SPAG = function(x){
  print(x$SPAGIndex)
}
