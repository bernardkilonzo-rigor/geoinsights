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
p_map<-ggplot()+
  geom_polygon(data = africa_map, aes(x =long, y = lat, group = group),
               fill ="white", color = "gray35", linewidth =0.1)+
  geom_point(data = conflict_data, aes(x = Longitude, y = Latitude, size = Fatalities), alpha = 0.4)+
  coord_quickmap()+
  labs(title = "Political Conflict in Africa",
       subtitle = "Reported Fatalities by Location",
       caption = "Viz by: Bernard Kilonzo")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "gray95"),
        legend.background = element_rect(fill = "gray95"),
        plot.title = element_text(family = "serif", face = "bold", size = 12, color = "gray20"),
        plot.subtitle = element_text(family = "serif", size = 10, color = "gray20"),
        plot.caption = element_text(family = "serif", face = "italic", size = 9, color = "gray35"))

#saving the plot
ggsave(plot = p_map, filename = "Point_map.png",
       width = 8, height = 6, units = "in", dpi = 300) 
