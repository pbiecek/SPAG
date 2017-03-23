## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("pbiecek/SPAG")

## ---- warning=FALSE, message=FALSE---------------------------------------
library("SPAG")

## ----eval=FALSE----------------------------------------------------------
#  ?ShapefilePoland
#  ShapefilePoland

## ------------------------------------------------------------------------
# ?CompaniesPoland
CompaniesPoland

## ---- message=FALSE, warning=FALSE---------------------------------------
# ?SPAG
SPAGIndex <- SPAG(companiesDF = CompaniesPoland, shp = ShapefilePoland)
print(SPAGIndex)

## ------------------------------------------------------------------------
ggSPAG <- plot(SPAGIndex)
ggSPAG

