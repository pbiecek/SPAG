## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("pbiecek/SPAG")
#  library(SPAG)

## ----eval=FALSE----------------------------------------------------------
#  ?ShapefilePoland
#  ShapefilePoland

## ----eval=FALSE----------------------------------------------------------
#  ?CompaniesPoland
#  CompaniesPoland

## ----eval=FALSE----------------------------------------------------------
#  ?SPAG
#  SPAGIndex <- SPAG(companiesDF = CompaniesPoland, shp = ShapefilePoland)
#  print(SPAGIndex)

## ----eval=FALSE----------------------------------------------------------
#  ggSPAG <- plot(SPAGIndex)
#  ggSPAG

