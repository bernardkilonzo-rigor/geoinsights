#Load libraries
library(tidyverse)
library(sf)
library(paletteer)

#load data
setwd("C:\\Users\\berna\\OneDrive\\Desktop\\Production\\dataviz\\code\\Map\\haiti_adm2_boundaries")
survey_sample<-read.csv("https://raw.githubusercontent.com/bernardkilonzo-rigor/dataviz/refs/heads/main/data/Survey%20Sample.csv")
adm2_shapefiles<-st_read("hti_boundaries_communes_adm2_cnigs_polygon.shp")#download the files and read them from your computer

#joining the two data sets
merged_data<-adm2_shapefiles%>%left_join(
  survey_sample, by =c("commune"="Commune"))

#creating a map (using continuous legend)
map_1<-merged_data%>%ggplot(aes(geometry=geometry, label = commune,
                         fill = Sample.Size))+
  geom_sf(color ="white", linewidth = 0.1)+
  scale_fill_gradient(low = "#fe9f6d", high = "#3b0f70")+
  labs(title = "Survey Sample Size by Commune in Haiti",
       caption = "Viz By: Bernard Kilonzo",
       fill = "Sample Size")+
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "serif", face = "bold",size = 12),
        plot.subtitle = element_text(family = "serif", size = 10,face = "italic" ,color = "gray22"),
        plot.caption = element_text(family = "serif",  size = 8),
        legend.title = element_text(family = "serif", size = 8),
        legend.text = element_text(family = "serif", size = 8))

#saving the plot
ggsave(plot = map_1, filename = "Polygon_map_1.png",
       width = 8, height = 6, units = "in", dpi = 300)

#creating map using discrete legend
#grouping the sample size into bins
merged_data<-merged_data%>%mutate(bins = case_when(
  Sample.Size<=200 ~"100-200",
  Sample.Size<=300 ~"201-300",
  Sample.Size<=400 ~"301-400",
  Sample.Size<=500 ~"401-500"
))

#creating the map (discrete legend)
map_2<-merged_data%>%ggplot(aes(geometry=geometry, label = commune,
                         fill = bins))+
  geom_sf(color ="white", linewidth = 0.1)+
  scale_fill_manual(values = c("100-200"="#fe9f6d","201-300"="#f4665c",
                               "301-400"="#cf4070","401-500"="#9c2e2f"))+
  labs(title = "Survey Sample Size by Commune in Haiti",
       caption = "Viz By: Bernard Kilonzo",
       fill = "Sample Size")+
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "serif", face = "bold",size = 12),
        plot.subtitle = element_text(family = "serif", size = 10,face = "italic" ,color = "gray22"),
        plot.caption = element_text(family = "serif",  size = 8),
        legend.title = element_text(family = "serif", size = 8),
        legend.text = element_text(family = "serif", size = 8))

#saving the plot
ggsave(plot = map_2, filename = "Polygon_map_2.png",
       width = 8, height = 6, units = "in", dpi = 300)
