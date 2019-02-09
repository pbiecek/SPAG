library(ZPD)
src = polacz()
szkoly = pobierz_szkoly(src)
# odfiltrowujemy jedynie licea ogólnokształcące w 2012 roku w województwie podlaskim
schools = szkoly %>% filter(rok == 2015)
schools <- as.data.frame(schools)
aa <- schools[,c("nazwa_szkoly","adres", "miejscowosc")]

adresy <- list()
for (i in 2096:nrow(schools)) {
  if (i %% 500 == 0) cat(i, "/", nrow(schools), "\n")
  try(
  adresy[[i]] <- SmarterPoland::getGoogleMapsAddress(street = schools$adres[i], city = schools$miejscowosc[i]),
  silent = TRUE
  )
}

lat <- sapply(adresy, function(x) ifelse(length(x)>1, x[1], NA))
lng <- sapply(adresy, function(x) ifelse(length(x)>1, x[2], NA))

schools <- data.frame(schools, lat, lng)

# przestrzenna

load("schools.rda")
(load("dane surowe/matematyka_podstawowa.rda"))

szkolyWyniki <- merge(matematyka_podstawowa, schools, by.x = "id_szkoly", by.y = "id_szkoly")


szkolyWyniki <- szkolyWyniki[,c(1,2,3,4,5,6,13:26)]
szkolyWyniki <- szkolyWyniki[szkolyWyniki$id_szkoly > 0,]  

szkolyWynikiGeo <- szkolyWyniki[!is.na(szkolyWyniki$lat),]

plot(szkolyWyniki$lat, szkolyWyniki$lng)

szkolyWynikiGeoWr <- szkolyWynikiGeo[szkolyWynikiGeo$gmina_szkoly == "Wrocław",]
plot(szkolyWynikiGeoWr$lat, szkolyWynikiGeoWr$lng)
