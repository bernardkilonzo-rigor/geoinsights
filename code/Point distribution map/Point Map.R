#load libraries
library(tidyverse)
library(maps)

#load data
conflict_data<-read.csv("https://raw.githubusercontent.com/bernardkilonzo-rigor/geoinsights/refs/heads/main/data/Conflict%20Fatalities.csv")

#load world map data
world_map<-map_data("world")

#get a base map of Africa (by filtering regions)
africa_map <- world_map %>%
  filter(region %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                       "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros",
                       "Congo", "Democratic Republic of the Congo", "Djibouti", "Egypt",
                       "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia",
                       "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho",
                       "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania",
                       "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
                       "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone",
                       "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo",
                       "Tunisia", "Uganda", "Zambia", "Zimbabwe"))


#Creating point distribution map
ggplot()+
  geom_polygon(data = africa_map, aes(x =long, y = lat, group = group),
               fill ="white", color = "gray35", linewidth =0.1)+
  geom_point(data = conflict_data, aes(x = Longitude, y = Latitude, size = Fatalities), alpha = 0.4)+
  coord_quickmap()+
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
