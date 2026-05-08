setwd("C:\\Users\\berna\\OneDrive\\Desktop\\Production\\geoinsights\\code\\Mapping with leaflet")
#load libraries
library(sf)
library(leaflet)
library(janitor)
library(dplyr)

#load data set
Facilities <- read.csv("https://raw.githubusercontent.com/bernardkilonzo-rigor/dataviz/main/data/Health%20facilities%20in%20Kibera.csv")%>%
  clean_names()

#creating a basic interactive map (point distribution map)
leaflet(Facilities)%>%
  addTiles()%>%
  addMarkers(~longitude,
             ~latitude,
             popup = ~name)

#creating color palette
pal <- colorFactor(
  palette = c("#ff9d23","#5b7e3c","#7f2020"),
  domain = Facilities$status
)

#coloring different categories
leaflet(Facilities)%>%
  addProviderTiles("CartoDB.Positron")%>%
  addCircleMarkers(
    ~longitude,
    ~latitude,
    radius = 4,
    color = ~pal(status),
    fillOpacity = 0.6,
    popup = ~paste0("<b>", name, "</b><br>Status: ", status))

#using different icons for different categories
#using awesome icons
icons <- awesomeIcons(
  icon = ifelse(Facilities$status == "operational", "window-close", "user-circle"),
  iconColor = "white",
  markerColor = ifelse(Facilities$status == "operational", "#5b7e3c", "#7f2020")
)

leaflet(Facilities)%>%
  addProviderTiles("CartoDB.Positron") %>%
  addAwesomeMarkers(
    ~longitude,
    ~latitude,
    icon = icons,
    popup = ~paste0("<b>", name, "</b><br>Status: ", status)
  )