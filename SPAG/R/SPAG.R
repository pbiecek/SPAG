#' Function calculating the coverage, distance and overlap components of the SPAG Index.
#'
#' @param companiesDF - data frame with information regarding the companies. At least four columns are required:
#' x and y coordinates of the company, the category of company and a numeric
#' @param xInd - number of the column in companiesDF with information regarding the latitude
#' @param yInd - number of the column in companiesDF with information regarding the longitude
#' @param empInd - number of the column in companiesDF with numeric data regarding the employment
#' @param categInd - number of the column in companiesDF information about the category of the company
#' @param shp - SpatialPolygonsDataFrame object obtained via loading a shapefile
#'
#' @importFrom graphics plot
#' @importFrom stats dist
#' @import maptools
#' @import rgdal
#' @import sp
#' @import rgeos
#'
#' @export




SPAG <- function(companiesDF, xInd = 1, yInd=2, empInd = 3, categInd = 4, shp){
  # Calculating the distance part of SPAG index

  categories <- unique(companiesDF[,categInd])
  vectorOfEmployment <- companiesDF[,empInd]
  totalEmployment <- sum(vectorOfEmployment)

  ICov <- sapply(categories,
                      function(x){
                        return (sum(companiesDF[companiesDF[,categInd]==x,empInd])/totalEmployment)
                      })

  # calculating base radius for other indexes

 # projekcja<-"+init=epsg:3347"
 # region<-spTransform(shp, CRS(projekcja))


  area <- gArea(shp)

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
  xySP <- SpatialPoints(companiesDF[,c(xInd,yInd)])

  circles <-gBuffer(xySP, quadsegs=150, byid=TRUE, width=vectorOfRadius)
  circlesTotal <- gBuffer(xySP, quadsegs=150, byid=TRUE, width=radiusVectorTotal)



  projekcja<-"+proj=longlat +datum=WGS84"
  region<-spTransform(shp, CRS(projekcja))
  # Coverage Index for all the companies
  IDist<- sapply(categories,
           function(x){
             theoreticalCompanies <- spsample(region, nrow(companiesDF[companiesDF[,categInd]==x,]), type="regular")
             theoreticalDF <- as.data.frame(theoreticalCompanies)
             theoreticalDist<-dist(as.matrix(theoreticalCompanies@coords))
             mean(dist(as.matrix(companiesDF[companiesDF[,categInd]==x,c(xInd,yInd)])))/mean(theoreticalDist)
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

  # wont work when name total is in a category
  names(CategoryArea) <- categories
  CategoryArea$total <- gUnaryUnion(circles)

  unionArea <- unionSpatialPolygons(circles,rep(1,nrow(companiesDF)))
  IOverTotal <- gArea(unionArea)/gArea(circles)


  ISPAG = IDist*IOver*ICov
  IndexDF <- data.frame(categories,IDist, IOver,ICov,ISPAG)

  IndexTotal <- data.frame("Total",IDistTotal,IOverTotal,1,IDistTotal*IOverTotal)

  names(IndexTotal) <- c("categories","IDist","IOver","ICov","ISPAG")
  IndexDF <- rbind(IndexDF,IndexTotal)


  x <- list( map = shp, unionAreaList = CategoryArea, SPAGIndex = IndexDF)
  class(x) <- "SPAG"


  return(x);
}




plot.SPAG <- function(obj, category){

  frameCircle <- obj$unionAreaList[[category]]@bbox
  frameMap <- obj$map@bbox

  frameFinal <- matrix(c(
    min(frameMap[1,1],frameCircle[1,1]),
    max(frameMap[1,2],frameCircle[1,2]),
    min(frameMap[2,1],frameCircle[2,1]),
    max(frameMap[2,2],frameCircle[2,2])
  ), nrow=2, byrow=TRUE)

  plot(obj$map, xlim=c(frameFinal[1,1], frameFinal[1,2]), ylim=c(frameFinal[2,1], frameFinal[2,2]))
  plot(obj$unionAreaList[[category]], border="red",add=TRUE)
}
