library(raster)
USA <- getData('GADM', country ='USA', level=0)

#Download Texas spatial data for bays
#install.packages("sf")
library(sf)

setwd("~/spatial_data")
#I got the shape files from the state department website
river <- st_read('./Major_Rivers_dd83/MajorRivers_dd83.shp') 
bays <- st_read('./MajorBays/MajorBays.shp')

#Convert objects to sf objects and points to coordinates
state.sf <- st_as_sf(USA)
river.sf <- st_transform(river, crs = 4326)
bay.sf <- st_transform(bays, crs = 4326)

# Bounding box for cropping the map to a specific region 
box <- c(ymin = 26.0127, ymax = 30.4753, xmin = -99.067, xmax= -92.763)

#limit spatial data to the bounding box
state_crop <- st_crop(state.sf, box)
river_crop <- st_crop(river.sf, box) #I didn't use rivers
bay_crop <- st_crop(bay.sf, box)

#Plot clusters on a map
#install.packages("ggspatial")
library(ggspatial)
library(ggplot2)

map_dat <- read.csv("~/spatial_data/bay_data.csv") #I found the coordinate points for each of my bays online and made a csv file of them 

library(leaflet)
library(tidyverse)
library(sf)
library(ggrepel)
library(tigris)

states <- states()
texas <- subset(states, NAME %in% c("Texas"))
map_dat <- read.csv("~/spatial_data/bay_data.csv")
data.frame(map_dat)
map_dat_sf <- st_as_sf(map_dat, coords = c("Long", "Lat"), crs = 4326)
data.frame()

Map_Theme <- theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 20),
  plot.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 14),
  axis.ticks.length = unit(0.20, "cm"),
  axis.text.y = element_text(size = 14))

#Plot of main map
texascoast <- ggplot()+
  geom_sf(data = state_crop, fill = "gray97") +
  geom_sf(data = bay_crop, fill = "#8FC0C7", alpha = 0.4) +
  geom_sf(data = map_dat_sf, color = "black", size = 1) + 
  geom_text_repel(data = map_dat, aes(x = Long, y = Lat, label = Bay),
                  size = 4,
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.35, "lines"),
                  segment.color = 'black',
                  segment.size = 0.6,
                  force = 10,
                  hjust = -0.5) +
  theme_classic() + Map_Theme+ annotation_north_arrow(location = "bl", height = unit(1.5,"cm"),width = unit(1.5,"cm"),  which_north = "true") + annotation_scale(location = 'br') + labs(
    x = "Longitude",
    y = "Latitude"
  ) 

#inset map
states <- map_data("usa")
texas <- subset(states, region %in% c("Texas"))
gulf <- subset(states, region != "alaska" & region != "hawaii")
mexico_map <- map_data("world", region = "Mexico")



gulfcoast <- ggplot(data = gulf) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray80", color = "black") +
  geom_polygon(data = mexico_map, aes(x = long, y = lat, group = group), fill = "gray80", color = "black") +
  geom_polygon(data = texas, aes(x = long, y = lat, group = group), fill = "NA", color = "red", size = 1)+
  annotate('rect', xmin=-98.1278, ymin=25.9944, xmax=-93.108, ymax=31.111, col = "black", alpha = 0.2, size = 1)+coord_fixed(1.5)+theme_void() +xlab("") + ylab("")

#joining the two maps into one plot
library(cowplot)

fullmap <- ggdraw()+
  draw_plot(texascoast) +
  draw_plot(gulfcoast, x =0.15, y=0.55, width = 0.4, height = 0.4)

fullmap

#ggsave("TXMap_edited.jpg", width = 10, height = 8, units = c("in"), dpi = 300)  
