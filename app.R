library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)
library(rvest)
library(lubridate)

#Funktsioonid----

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


less_than_r <- function(list, r) {
  return(list < r)
}

rename_func <- function(string) {    #<--- Ei ole kõige elegantsem lahendus, aga töötab
  if (string == "Street") {
    return("St")
  } else if (string == "Avenue") {
    return("Ave")
  } else if (string == "Parkway") {
    return("Pky")
  } else if (string == "Boulevard") {
    return("Blvd")
  } else if (string == "Road") {
    return("Rd")
  } else if (string == "Clinton-Washington Aves") {
    return("Clinton - Washington Aves")
  } else if (string == "East") {
    return("E")
  } else if (string == "Heights") {
    return("Hts")
  } else if (string == "Highway") {
    return("Hwy")
  } else if (string == "Place") {
    return("Pl")
  } else if (string == "Square") {
    return("Sq")
  } else if (string == "Third") {
    return("3rd")
  } else if (string == "Fifth") {
    return("5th")
  } else if (string == "Seventh") {
    return("7th")
  } else if (string == "Eighth") {
    return("8th")
  } else {
    return("9th")
  }
}

leia_linnaosa <- function(metroo_peatus) {
  l2hend <- str_split(metroo_peatus, " - ")[[1]][1]
  if (metroo_peatus %in% linnaosa$`Name of station`) {
    return(linnaosa$Boroughs[linnaosa$`Name of station` == metroo_peatus][1])
  } else if (l2hend %in% linnaosa$`Name of station`)
    return(linnaosa$Boroughs[linnaosa$`Name of station` == l2hend][1])
  else {
    return()
  }
}

#Andmetöötlus----

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

suppressWarnings(
  current_resolution <- system("wmic path Win32_VideoController get CurrentHorizontalResolution,CurrentVerticalResolution /format:value", intern = TRUE)  %>%
    strsplit("=") %>%
    unlist() %>%
    as.double()
)
current_resolution <- current_resolution[!is.na(current_resolution)]

metroo <- read.csv("ny_subway_2010.csv", sep = ",", header = T) %>% 
  group_by(NAME) %>% 
  summarise(the_geom = the_geom[1]) %>% 
  mutate(the_geom = as.character(the_geom),
         lat = as.numeric(map(numextract(the_geom),2)),
         lon = as.numeric(map(numextract(the_geom),1)))

andmed <- read.csv("ny_gun_violence_2013_2018.csv", sep = ",", header = T) %>% 
  filter(!is.na(latitude),
         city_or_county %in% c("Brooklyn",
                               "Queens",
                               "Breezy Point",
                               "New York",
                               "Far Rockaway",
                               "Corona (Queens)",
                               "Inwood",
                               "Arverne (Queens)",
                               "Lawrence",
                               "Jamaica",
                               "South Richmond Hill",
                               "Saint Albans",
                               "Springfield",
                               "Flushing",
                               "East Elmhurst",
                               "Woodside",
                               "Jackson Heights",
                               "Astoria",
                               "New York (Manhattan)",
                               "Manhattan",
                               "Bronx",
                               "Mount Vernon",
                               "Pelham",
                               "Yonkers",
                               "New Rochelle"
         )
  ) %>% 
  mutate(source = str_split_fixed(sources, "\\|", n = 2)[,1],
         sedel = paste(sep = "<br/>",
                       paste0("<b><a href='",source,"'>",date,"</a></b>"),
                       paste0("Hukkunuid: ", n_killed),
                       paste0("Vigastatuid: ", n_injured)
         ),
         sedel = map(sedel,htmltools::HTML),
         date = ymd(date))

tabelid <- "https://en.wikipedia.org/wiki/New_York_City_Subway_stations" %>%
  read_html() %>%
  html_nodes("table")
linnaosa <- html_table(tabelid[20], fill = T)[[1]] %>% 
  mutate(`Name of station` = str_replace_all(`Name of station`, "Street|Avenue|Parkway|Boulevard|Road|Clinton-Washington Aves|East|Heights|Highway|Place|Square|Third|Fifth|Seventh|Eighth|Ninth", rename_func)) %>% 
  select(-Lines)

metroo <- metroo %>% 
  mutate(borough = as.character(map(NAME, leia_linnaosa)),
         borough = case_when(
           NAME == "103rd St - Corona Plaza" ~ "Queens",
           NAME %in% c("3rd Ave", "Broadway - Lafayette St", "50th St", "59th St - Columbus Circle", "77th St", "79th St", "86th St", "175th St") ~ "Manhattan",
           NAME == "36th St" ~ "Queens",
           borough == "the Bronx" ~ "Bronx",
           T ~ borough
         ))

#Leaflet----

pal <- colorFactor(c("navy", "red"), domain = c(FALSE, TRUE))

subwayIcon <- makeIcon("subway_logo.png", "subway_logo.png", 24, 24)

# r1 < r2, kus r1 on see, kui lähedal olevad tulistamised märgime punasega ja
# r2 on see kui laialt kaarti näitame
r2 <- 5

kaart_metrooNimi <- function(metrooNimi, r1, algus_kp = ymd("2013-01-01"), l6pp_kp = ymd("2018-12-31"), kas_hukkunutega = T) {
  m_lat <- as.numeric(metroo[metroo$NAME == metrooNimi, "lat"])
  m_lon <- as.numeric(metroo[metroo$NAME == metrooNimi, "lon"])
  
  andmed %>% 
    mutate(
      lahedal = vahemaa(latitude, longitude, m_lat, m_lon) < r1
    ) %>% 
    filter(
      vahemaa(latitude, longitude, m_lat, m_lon) < r2,
      date %within% interval(algus_kp, l6pp_kp),
      if (kas_hukkunutega) {n_killed > 0} else {T}
    ) %>%
    leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>% #Võetud RShiny rakenduse näitest https://github.com/CodingTigerTang/NYC_Metro_Vis/blob/master/app.R
    addCircles(~longitude, ~latitude, popup = ~sedel, color = ~pal(as.factor(lahedal))) %>%
    # addMarkers(m_lon, m_lat, label = metrooNimi, icon = subwayIcon) %>% 
    addMarkers(data = metroo, ~lon, ~lat, label = ~NAME, icon = subwayIcon) %>% 
    setView(m_lon, m_lat, zoom = 15)
}

kaart_linnaosaNimi <- function(linnaosaNimi, r1, algus_kp = ymd("2013-01-01"), l6pp_kp = ymd("2018-12-31"), kas_hukkunutega = T) {
  linnaosa_metrood <- metroo %>% filter(borough == linnaosaNimi)
  andmed %>%
    mutate(
      lahedal = ifelse(
        map(map(map2(latitude, longitude, vahemaa, linnaosa_metrood$lat, linnaosa_metrood$lon),less_than_r,r1),sum) > 0,
        TRUE,
        FALSE)
    ) %>%
    filter(
      ifelse(
        map(map(map2(latitude, longitude, vahemaa, linnaosa_metrood$lat, linnaosa_metrood$lon),less_than_r,r2),sum) > 0,
        TRUE,
        FALSE),
      date %within% interval(algus_kp, l6pp_kp),
      if (kas_hukkunutega) {n_killed > 0} else {T}
    ) %>%
    leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addCircles(~longitude, ~latitude, popup = ~sedel, color = ~pal(as.factor(lahedal))) %>%
    addMarkers(data = linnaosa_metrood, ~lon, ~lat, label = ~NAME, icon = subwayIcon)
}

box_plot <- function(nimi, r1, algus_kp = ymd("2013-01-01"), l6pp_kp = ymd("2018-12-31"), kas_hukkunutega = T) {
  linnaosa_metrood <- metroo %>% filter(borough == nimi)
  andmed %>%
    mutate(
      lahedal = ifelse(
        map(map(map2(latitude, longitude, vahemaa, linnaosa_metrood$lat, linnaosa_metrood$lon),less_than_r,r1),sum) > 0,
        TRUE,
        FALSE)
    ) %>%
    filter(
      ifelse(
        map(map(map2(latitude, longitude, vahemaa, linnaosa_metrood$lat, linnaosa_metrood$lon),less_than_r,r2),sum) > 0,
        TRUE,
        FALSE),
      date %within% interval(algus_kp, l6pp_kp),
      if (kas_hukkunutega) {n_killed > 0} else {T}
    ) %>%
    ggplot(aes(x=lahedal)) +
      geom_bar(aes(fill = lahedal)) +
      scale_fill_manual(values = c("navy", "red")) +
      xlab(paste("Palju tulistamisi leidis aset lähemal kui", r1, "kilomeetrit")) + 
      ylab("Kokku") + 
      scale_x_discrete(labels = c("Kaugel", "Lähedal")) +
      theme(legend.position = "none", 
            text = element_text(color = "white"),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank())
}

#Shiny Dashboard----

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "New Yorki metroo ja tulistamised"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Kaart", tabName = "Kaart", icon = icon("dashboard")),
                        menuItem("Animatsioon", tabName = "Animatsioon", icon = icon("tv"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Kaart",
                                bootstrapPage(
                                  leafletOutput("kaart", width = "100%", height = round(0.76*current_resolution[2])),
                                  absolutePanel(top = 150, left = 270, draggable = TRUE, width = "20%",
                                                radioButtons("liik", 
                                                             "Vali kas kaart luuakse peatuse või linnaosa järgi",
                                                             choices = c("Peatus", "Linnaosa")),
                                                conditionalPanel(
                                                  condition = "input.liik == 'Peatus'",
                                                  selectInput("metrooNimi",
                                                              "Peatus",
                                                              choices = metroo$NAME)
                                                ),
                                                conditionalPanel(
                                                  condition = "input.liik == 'Linnaosa'",
                                                  selectInput("linnaosaNimi",
                                                              "Linnaosa",
                                                              choices = c("Manhattan", "Bronx", "Brooklyn", "Queens"))
                                                ),
                                                numericInput("raadius",
                                                             "Vali metroo mõjuraadius (km)",
                                                             value = 0.5,
                                                             min = 0.1,
                                                             max = 1,
                                                             step = 0.1),
                                                sliderInput("KP",
                                                            "Vali sobiv algusaeg",
                                                            min = ymd("2013-01-01"),
                                                            max = ymd("2018-12-31"),
                                                            value = c(ymd("2013-01-01"),ymd("2018-12-31"))),
                                                checkboxInput("kasHukkunutega", 
                                                              "Näita ainult neid tulistamisi, kus oli hukkunuid", 
                                                              FALSE),
                                                actionButton("plot", "Loo kaart")
                                  ),
                                  absolutePanel(bottom = 150, right = 100, draggable = TRUE, width = "10%",
                                                plotOutput("box_plot"))
                                )
                        ),
                        tabItem(tabName = "Animatsioon",
                                bootstrapPage(
                                  leafletOutput("animatsioon", width = "100%", height = round(0.76*current_resolution[2])),
                                  absolutePanel(top = 150, left = 270, draggable = TRUE, width = "20%",
                                                sliderInput("anim_KP",
                                                            "",
                                                            min = ymd("2014-01-01"),
                                                            max = ymd("2017-12-31"),
                                                            value = c(ymd("2014-01-01"),ymd("2014-01-10")),
                                                            animate = animationOptions(interval = 10))
                                  )
                                )
                        )
                      )
                    )
)

server <- function(input, output) { 
  
  kaart <- eventReactive(input$plot, {
    if (input$liik == "Peatus") {
      kaart_metrooNimi(metrooNimi = input$metrooNimi,
                       r1 = input$raadius, algus_kp = input$KP, l6pp_kp = ymd("2018-12-31"), kas_hukkunutega = input$kasHukkunutega)
    } else {
      kaart_linnaosaNimi(linnaosaNimi = input$linnaosaNimi,
                         r1 = input$raadius, algus_kp = input$KP[1], l6pp_kp = input$KP[2], kas_hukkunutega = input$kasHukkunutega)
    }
  })
  
  output$kaart <- renderLeaflet(
    kaart()
  )
  
  output$animatsioon <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      setView(mean(andmed$longitude), mean(andmed$latitude), zoom = 11) 
  })
  
  observeEvent(input$anim_KP, {
    data <- andmed %>%
      mutate(hukkunuga = n_killed>0) %>% 
      filter(date %within% interval(input$anim_KP[1], input$anim_KP[2]))
    
    leafletProxy("animatsioon", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(~longitude, ~latitude, color = ~pal(as.factor(hukkunuga)))
  })
  
  boxplot <- eventReactive(input$plot, {
    if (input$liik != "Peatus") {
       box_plot(nimi = input$linnaosaNimi,
                r1 = input$raadius, algus_kp = input$KP[1], l6pp_kp = input$KP[2], kas_hukkunutega = input$kasHukkunutega)
    }
  })
  
  output$box_plot <- renderPlot({
    boxplot()
  }, bg="transparent")
  
}

shinyApp(ui, server)
