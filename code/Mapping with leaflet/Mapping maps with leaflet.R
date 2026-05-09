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

#using a custom icons
facility_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/main/icons/Location%20icon.png", #your icon file
  iconWidth = 25, #adjust width
  iconHeight = 25 #adjust height
)

leaflet(Facilities)%>%
  addTiles()%>%
  addMarkers(~longitude,
             ~latitude,
             popup = ~paste0("<b>", name, "</b><br>Status: ", status),
             icon = facility_icon)

#using different icons for different categories
#computing facility categories
Facilities <- Facilities%>%
  mutate(
    category = case_when(
      type %in% c("hospital") ~ "hospital",
      type %in% c("health_center","Health clinic", "health_programme") ~ "health_center",
      TRUE ~ "Other"
    )
  )

#using custom icons
icons <- iconList(
  hospital = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/main/icons/hospital.png",
    iconWidth = 24, iconHeight = 24
  ),
  health_center = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/main/icons/health_center.png",
    iconWidth = 24, iconHeight = 24
  ),
  Other = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/main/icons/medic.png",
    iconWidth = 24, iconHeight = 24
  )
)

leaflet(Facilities)%>%
  addTiles()%>%
  addMarkers(~longitude,
             ~latitude,
             popup = ~paste0("<b>", name, "</b><br>category: ", category),
             icon = icons[Facilities$category]
             )

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


