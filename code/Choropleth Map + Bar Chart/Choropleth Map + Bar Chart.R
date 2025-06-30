#Load libraries
library(tidyverse)
library(sf)
library(paletteer)
library(patchwork)

#load data
setwd("C:\\Users\\berna\\OneDrive\\Desktop\\Production\\dataviz\\code\\Map\\haiti_adm2_boundaries")
survey_sample<-read.csv("https://raw.githubusercontent.com/bernardkilonzo-rigor/dataviz/refs/heads/main/data/Survey%20Sample.csv")
adm2_shapefiles<-st_read("hti_boundaries_communes_adm2_cnigs_polygon.shp")#download the files and read them from your computer
#access shape file data ("https://github.com/bernardkilonzo-rigor/geoinsights/tree/main/data")

#grouping the sample size into bins
survey_sample<-survey_sample%>%mutate(bins = case_when(
  Sample.Size<=150 ~"100-150",
  Sample.Size<=200 ~"151-200",
  Sample.Size<=250 ~"201-250",
  Sample.Size<=300 ~"251-300",
  Sample.Size<=350 ~"301-350",
  Sample.Size<=400 ~"351-400",
  Sample.Size<=450 ~"401-450"
))

#joining the two data sets
merged_data<-adm2_shapefiles%>%left_join(
  survey_sample, by =c("commune"="Commune"))

#creating a bar plot
bplot<-survey_sample%>%group_by(bins)%>%
  summarise(sample =n_distinct(Id.Com))%>%
  ggplot(aes(y = bins, x = sample, fill =bins))+
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = sample),hjust = -0.2, size =2.5,family ="serif",color = "gray25")+
  coord_cartesian(xlim = c(0,40))+
  scale_fill_paletteer_d("rcartocolor::BurgYl")+
  labs(title = "Number of Communes by
       Sample Size Bin")+
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "serif",size = 6, color = "gray25"),
        legend.position = "none",
        plot.title = element_text(family = "serif",face = "bold",size = 8, color = "gray25"))

#creating a choropleth map
map<-merged_data%>%ggplot(aes(geometry=geometry, label = commune,
                         fill = bins))+
  geom_sf(color ="white", linewidth = 0.1)+
  scale_fill_paletteer_d("rcartocolor::BurgYl")+
  labs(title = "Survey Sample Size by Commune in Haiti",
       caption = "Viz By: Bernard Kilonzo",
       fill = "Sample Size")+
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "serif", face = "bold",size = 14, color = "gray25"),
        plot.caption = element_text(family = "serif", face = "italic",color = "gray35",size = 8),
        legend.title = element_text(family = "serif", size = 8),
        legend.text = element_text(family = "serif", size = 8))

#joining the two plots
merged_plot<-map+inset_element(bplot, left = 0.0,bottom = 0.35,right = 0.3,top = 0.78)

#saving the plot
ggsave(plot = merged_plot, filename = "Choropleth_map+barplot.png",
       width = 8, height = 6, units = "in", dpi = 300)
