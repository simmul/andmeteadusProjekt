library(dplyr)

#install.packages('rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

# andmed_alus <- read.csv("gun_violence_2013_2018.csv", sep = ",", header = T)
# andmed <- andmed_alus %>% 
#   filter(state == "New York")

#panin github-i juba filtreeritud faili, et ei oleks liiga suur.
andmed <- read.csv("ny_gun_violence_2013_2018.csv", sep = ",", header = T)

metroo <- read.csv("ny_subway_2010.csv", sep = ",", header = T)

#Arvutame kauguse kahe punkti vahel kasutades nende koordinaate (võttes arvesse maakera kumerust, kuid mitte mägesi ning ellipsoidilist kuju).
#Kuna vahemaad on võrdlemisi lühikesed, ei tohiks see probleemi valmistada.

deg2rad <- function(d) {
  return(d*pi/180)
}

vahemaa <- function(lat1, lon1, lat2, lon2) {
  psi1 <- deg2rad(lat1)
  psi2 <- deg2rad(lat2)
  delta_psi <- deg2rad(lat2 - lat1)
  delta_lambda <- deg2rad(lon2 - lon1)
  a <- sin(delta_psi/2)**2 + cos(psi1)*cos(psi2)*sin(delta_lambda/2)**2
  c <- 2*atan2(sqrt(a), sqrt(1-a))
  d <- 6371*c  #maa keskmine raadius on 6371 km
  return(d)
}

Numextract <- function(string){  #see funktsioon on leitud internetist: http://stla.github.io/stlapblog/posts/Numextract.html
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

metroo_naide <- as.character(metroo[1,4])
metroo_naide

lat1 <- as.numeric(Numextract(metroo_naide))[1]
lat2 <- as.numeric(Numextract(metroo_naide))[2]

andmed_alus %>% 
  filter(vahemaa(lat1, lat2, latitude, longitude) <= 1000) %>%  #leiame tulistamised, mis leidsid aset sellele metroole lähemal kui 1000 km
  nrow()  #tuleb null sellist

metroo <- metroo %>% 
  mutate(the_geom = as.character(the_geom),
         lat = as.numeric(Numextract(the_geom)[seq(1,2*nrow(metroo),2)]),
         lon = as.numeric(Numextract(the_geom)[seq(2,2*nrow(metroo),2)]))

vahemaa(metroo$lat[1],metroo$lon[1],metroo$lat,metroo$lon)

andmed %>% 
  filter(sum(vahemaa(latitude, longitude, metroo$lat, metroo$lon) <= 100) >= 1) %>% 
  nrow()

#----Visualiseerime kõik andmestiku punktid

library(leaflet)
library(geosphere)

m = leaflet() #koordinaatsüsteemis sisse lugemine

m = addTiles(m) #kaardikihtide lisamine

for (i in 1:nrow(andmed)) {
  m = m %>% #punktide lisamine kaardile
    addCircles(andmed$longitude[i], andmed$latitude[i], color = 'red', weight = 3)
}

m = m %>% 
  setView(lng = 0, lat = 50, zoom = 3) %>% 
  addProviderTiles("Esri.WorldImagery") #geoloogiline
m

#Kui vaadata kaarti, siis on nähe, et peaks olema küll tulistamisi ka metroode ümbruses. See tähendab, et peab üle vaatama filtri
#real 40-42.






