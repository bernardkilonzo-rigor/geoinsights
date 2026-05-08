setwd("C:\\Users\\berna\\OneDrive\\Desktop\\Production\\geoinsights\\code\\Mapping with leaflet")
#load libraries
library(sf)
library(leaflet)
library(janitor)

#load data set
Facilities <- read.csv("https://raw.githubusercontent.com/bernardkilonzo-rigor/dataviz/main/data/Health%20facilities%20in%20Kibera.csv")%>%
  clean_names()

#creating a basic interactive map
leaflet(Facilities)%>%
  addTiles()%>%
  addMarkers(~longitude, ~latitude, popup = ~name)
