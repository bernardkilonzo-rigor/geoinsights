setwd("C:\\Users\\berna\\OneDrive\\Desktop\\Production\\geoinsights\\code\\Mapping with leaflet")
#load libraries
library(sf)
library(leaflet)
library(janitor)
library(tidyverse)

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
      type %in% c("chemist_dispensing","chemist_over_the_counter") ~ "chemist",
      type %in% c("medical_clinic") ~"clinic",
      TRUE ~ "Other"
    )
  )

#using custom icons
icons <- iconList(
  hospital = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/main/icons/hospital.png",
    iconWidth = 24, iconHeight = 24
  ),
  chemist = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/main/icons/settings.png",
    iconWidth = 24, iconHeight = 24
  ),
  clinic = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/main/icons/clinic.png",
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

#using circle markers with color
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

#create a polygon map with leaflet
#load data set
setwd("C:\\Users\\berna\\OneDrive\\Desktop\\Production\\geoinsights\\code\\Choropleth Map\\haiti_adm2_boundaries")
survey_sample<-read.csv("https://raw.githubusercontent.com/bernardkilonzo-rigor/dataviz/refs/heads/main/data/Survey%20Sample.csv")
adm2_shapefiles<-st_read("hti_boundaries_communes_adm2_cnigs_polygon.shp")

#joining the two data sets
merged_data<-adm2_shapefiles%>%left_join(
  survey_sample, by =c("commune"="Commune"))


