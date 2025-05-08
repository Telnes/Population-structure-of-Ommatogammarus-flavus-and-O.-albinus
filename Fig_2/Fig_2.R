#install.packages("marmap")

## packages for getting and reading data
library(marmap) # for bathymetric data (readGEBCO.bathy)
library(openxlsx) # for read.xlsx
library(rnaturalearth) # for contours
## data manipulation
library(dplyr) # for %>%
## plotting
library(ggplot2) # for plotting
library(ggrepel) 

## base map
## get borders
russia <- ne_download(scale = 'large', returnclass = "sf", type='lakes', category='physical')
russia2 <- ne_download(scale = 'large', returnclass = "sf", type='rivers_lake_centerlines', category='physical')
## Get bathymetric data
bat <- readGEBCO.bathy("GEBCO_13_Mar_2025_334d111f978b/gebco_2024_n57.0_s51.0_w103.0_e112.0.nc")
## convert into a data frame
bat_xyz <- as.xyz(bat) %>% 
  rename(Longitude=V1,Latitude=V2,Depth=V3) %>%
  filter(Depth < 456)
## and correct for Baikal's latitude (~455 m above the sea level) 
bat_xyz$Depth <- bat_xyz$Depth - 455


coordinates <- read.xlsx("../Supplementary_Materials/Supplementary_tables.xlsx", sheet = 1, startRow = 2)
coordinates$lat <- as.numeric(sapply(X = coordinates$Coordinate, FUN = function(X) {unlist(strsplit(X, split = " "))[1]}))
coordinates$lon <- as.numeric(sapply(X = coordinates$Coordinate, FUN = function(X) {unlist(strsplit(X, split = " "))[3]}))

coordinates$label <- paste0(coordinates$location, "_", coordinates$`Depth,.m`)

coordinates %>% select(location, lon, lat) %>% unique() %>% 
  group_by(location) %>% mutate(mean_lon = mean(lon), mean_lat = mean(lat)) %>%
  select(location, mean_lon, mean_lat) %>% unique() -> place_coordinates
#coordinates2 <- coordinates2[ , -c("Species")]
#coordinates2 <- coordinates2[coordinates2$Species == "O. flavus", ]

#map <- 
ggplot() +
  #  geom_tile(data = bat_xyz, aes(x=Longitude,y=Latitude, fill=Depth), alpha=1) + 
  geom_raster(data = bat_xyz, aes(x=Longitude,y=Latitude, fill=Depth), alpha=1) + 
  geom_sf(data = russia, color = "blue", fill='NA') +
  geom_sf(data = russia2, colour = "blue", linewidth = 0.2, alpha=0.5) + 
  #  geom_point(data = sampling_points, aes(x = lon, y = lat), size=1.5, fill=col) + #+, 
  theme_minimal() + 
  xlab("") + ylab("") + #-> base_map
  coord_sf(xlim = c(103, 111), ylim = c(51.2, 56.2), expand = FALSE) + 
  scale_fill_binned(high = "lightblue1", low = "#000088", breaks=c(0, -250, -500, -750, -1000, -1500), name="Depth, m") +
  geom_point(data = coordinates, aes(x = lon, y = lat), shape=21, size=1.5, color="orange", fill="yellow") + 
  geom_text_repel(data = place_coordinates, aes(label = location, x=mean_lon, y=mean_lat), size=3) #+ 
#coord_quickmap() ## this works when coord_sf does not

ggsave("basemap_raster.svg", device = svg, width=16, height=18, units="cm")

#ggsave("basemap.svg", device = 'svg', width=20, height=20, units="cm")
#ggsave("basemap.pdf", width=20, height=20, units="cm")
#ggsave("basemap.png", width=20, height=20, units="cm", device="png", bg='white')
