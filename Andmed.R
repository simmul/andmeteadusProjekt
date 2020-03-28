library(dplyr)
library(stringr)
library(purrr)

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

numextract <- function(string){ #pattern on leitud internetist: http://stla.github.io/stlapblog/posts/Numextract.html
  list <- str_extract_all(string, "\\-*\\d+\\.*\\d*")
  return(list)
}

metroo <- metroo %>% 
  mutate(the_geom = as.character(the_geom),
         lat = as.numeric(map(numextract(the_geom),2)),
         lon = as.numeric(map(numextract(the_geom),1)))

#---Et aru saada, mis võtta metroo "mõjuala raadiuseks"

#Pane siia argumendiks proovitav raadius 
Info <- function(r) {
  mitu_lähemal_kui_r <- c()
  for (i in 1:nrow(metroo)) {
    mitu <- andmed %>% 
      filter(vahemaa(metroo$lat[i], metroo$lon[i], latitude, longitude) < r) %>% 
      nrow()
    mitu_lähemal_kui_r <- c(mitu_lähemal_kui_r, mitu)
  }
  
  lugeja <- 0
  for (i in 1:nrow(metroo)) {
    for (j in 1:nrow(metroo)) {
      if (i > j & vahemaa(metroo$lat[i], metroo$lon[i], metroo$lat[j], metroo$lon[j]) <= r) {
        lugeja <- lugeja + 1
      }
    }
  }
  cat(
    paste0("Kokkuvõtteks:
  kui võtame r=", r, ", siis tulistamiste arv metroo ümber jaguneb järgnevalt\n\n")
  )
  print(summary(mitu_lähemal_kui_r))
  cat(
    paste0("\n  kusjuures metroosi, millele nii lähedal ei ole toimunud ühtegi tulistamist on ", sum(mitu_lähemal_kui_r == 0),".
  Kuna ei ole väga hea kui üks tulistamine satub mitme metroo raadiusesse (võib arutleda, et siis polegi seal midagi pistmist metrooga), siis tasub tähele panna, et metroo paare, mis satuvad üksteise raadiusesse, on ", lugeja)
  )
}
Info(0.5)

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












