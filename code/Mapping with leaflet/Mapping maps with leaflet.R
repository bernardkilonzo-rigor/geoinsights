setwd("C:\\Users\\berna\\OneDrive\\Desktop\\Production\\geoinsights\\code\\Mapping with leaflet")
#load libraries
library(sf)
library(leaflet)
library(janitor)

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
  palette = c("red","blue"),
  domain = Facilities$status
)

#coloring different categories
leaflet(Facilities)%>%
  addProviderTiles("CartoDB.Positron")%>%
  addCircleMarkers(
    ~longitude,
    ~latitude,
    radius = 3,
    color = ~pal(status),
    fillOpacity = 0.4,
    popup = ~paste0("<b>", name, "</b><br>Status: ", status))

#using different icons for different categories
