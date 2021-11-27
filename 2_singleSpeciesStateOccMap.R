
##load libraries
library(raster) #for processing some spatial data
library(rnaturalearth) #for downloading shapefiles
library(sf) #for processing shapefiles
library(elevatr) #for downloading elevation data
library(dplyr) #to keep things tidy
library(magrittr) #to keep things very tidy
library(ggspatial) #for scale bars and arrows
library(ggplot2) #for tidy plotting
library(ggpubr) #for easy selection of symbols
library(colourpicker) #for easy selection of colors


#load occurrence data and filter by taxon 
points <- read.csv("stigmatopterisPM.csv")

speciesList <- levels(points$scientificName)

taxon <- "Stigmatopteris longicaudata" #specify taxon name here

pointsFiltered <- points %>% filter(scientificName == taxon) %>% droplevels()

pointsFiltered <- st_as_sf(pointsFiltered,coords = c(3,2), 
                           crs= '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84+towgs84=0,0,0')


#load relevant shapefiles
map <- ne_countries(scale = 10, returnclass = "sf")

states <- ne_states(returnclass = "sf")

ocean <- ne_download(scale = 10, type = 'ocean', 
                     category = 'physical', returnclass = 'sf')

rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
                      category = 'physical', returnclass = 'sf')


#set focal area for mapping
focalState <- states %>% filter(name == "Ngöbe Buglé")


#filter records by state
pointsFilteredState <- st_join(pointsFiltered, focalState, 
                               join = st_intersects) %>% 
                               filter(!is.na(name)) %>% 
                               select(scientificName, geometry)

#set map extent
limitState <- st_buffer(focalState, dist = 1) %>% st_bbox()

clipLimitState <- st_buffer(focalState, dist = 2) %>% st_bbox()

limitExtentState <- as(extent(clipLimitState), 'SpatialPolygons')

crs(limitExtentState) <- "+proj=longlat +datum=WGS84 +no_defs"


#obtain elevation data
elev<-get_elev_raster(locations = limitExtentState, z = 6, override_size_check = T)

elevDF<-as.data.frame(elev, xy=TRUE)

colnames(elevDF)<-c("x", "y", "elevation")

elevDF[, 3][elevDF[, 3] < 1500] <- NA #filter only data >=1500m



#set symbol shape, color, and legend position
pch <- 17

fillColor <- colourPicker(numCols = 1)

position <- c(0.70, 0.08)


#plot map
occStateMap <-
  ggplot() +
  geom_tile(data = elevDF, aes(x=x, y=y, fill=elevation), alpha =0.4)+
  scale_fill_gradient(low="#a3a0a0", high= "#000000", 
                      na.value="transparent")+
  geom_sf(data = ocean, color = "black", size = 0.05, fill = "#add8e6")+
  geom_sf(data = rivers, color = "#d7f2fa", size = 0.5)+
  geom_sf(data = states, color = "black", size = 0.05, fill = "#f5f5f5", alpha = 0.5)+
  geom_sf(data = focalState, color = "black", size = 0.15,
          linetype = "dashed", fill = "#acbdac", alpha = 0.2)+
  geom_sf(data = pointsFilteredState, aes(geometry = geometry,
                                          shape = scientificName,
                                          color = scientificName), size = 2)+
  scale_shape_manual(values = pch)+
  scale_color_manual(values = fillColor)+
  coord_sf(
    xlim = c(limitState["xmin"], limitState["xmax"]),
    ylim = c(limitState["ymin"], limitState["ymax"])) +
  labs(x="Longitude", y="Latitude", color = taxon)+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  guides(colour = guide_legend(override.aes = list(size = 3)))+
  theme(legend.position = position,
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background=element_blank(),
        legend.text = element_text(face = "italic"),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.box.background = element_rect(color = "black", size = 0.1,
                                             linetype = "longdash", 
                                             fill = alpha("white", 0.9)))+
  theme(panel.grid.major = element_line(colour = "#c9c9c9", 
                                        linetype = "dashed", 
                                        size = 0.3), 
        panel.background = element_rect(fill = "#f0f8ff"), 
        panel.border = element_rect(fill = NA))+
  guides(fill=FALSE)+
  guides(shape=FALSE)+
  guides(color=guide_legend(override.aes = list(shape = pch, 
                                                fill=fillColor, size =3)))

occStateMap


##save plots

dir.create("plots")

nameSave<-get("taxon") %>% sub("\\s","_",.)

saveDirectory<-paste("plots/",nameSave,sep="")

dir.create(saveDirectory)

fileNameTiff<-paste(nameSave,".tiff",sep="")

fileNamePDF<-paste(nameSave,".PDF",sep="")

ggsave(filename = fileNameTiff, path = saveDirectory, width = 15, height = 10, 
       units = "cm", device = 'tiff', dpi=300)

ggsave(filename = fileNamePDF, path = saveDirectory, width = 15, height = 10, 
       units = "cm", device = 'pdf', dpi=300)

dev.off()
