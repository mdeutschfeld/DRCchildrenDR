library(tidyverse)
#plotting
library(RColorBrewer)
#spatial
library(rgeos)
library(rgdal)


dhs13adults <- read.csv(file="/Volumes/share/2. Projects/1. Current/AdultDHS_EpidemiologicalAnalysis/alladults_v3_clean.csv", header = T)
colnames(dhs13adults) <- tolower(colnames(dhs13adults))

clustdf <- dhs13adults %>% 
  dplyr::select(cluster, latnum, longnum) %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(latnum=mean(latnum), longnum=mean(longnum))
  
## Map the DRC
tol <- 0.05
gadmpath <- "~/Desktop/"
setwd(gadmpath)
DRCcountry <- gSimplify(readOGR(dsn = "~/Desktop/DRC_GADM/COD_adm_shp/", layer = "COD_adm0"), tol, topologyPreserve=TRUE)
DRCprov <- readOGR(dsn = "~/Desktop/DRC_DHS/sdr_subnational_boundaries_2017-11-30/shps/", layer = "sdr_subnational_boundaries2")

kids13 <- subset(ind_full, ind_full$year == "2013")

DRC_PvMapPlotObj <- ggplot() + geom_polygon(data=DRCprov, aes(x=long, y=lat, group=group), colour="#737373", fill="#f7fcf5") +
  geom_point(data=kids13, aes(x=long, y=lat), shape=16, size=1.5, colour="black", fill = "black") +
  scale_size_area() + # set up reasonable scale for point prev
  coord_equal() +
  labs(title="Locations of children included in study", 
       subtitle="DRC province borders outlined in black") +
  theme(panel.border=element_blank()) +
  theme(panel.background=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(legend.title.align=0.5) +
  labs(x=NULL, y=NULL)

plot(DRC_PvMapPlotObj)

