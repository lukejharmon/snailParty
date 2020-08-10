##let's make minimium convex polygons (MCP)
#packages
library(ggmap)
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
library("sf")
library(dplyr)
library(sp)
library(mapview)
library(dplyr)
library(adehabitatHR)
library(scales) 

#import data
MCPYann <- read.csv("DistributionYannickus.csv")
#change to a spatial points DF and add proj string
coordinates(MCPYann) <- c("Longitude", "Latitude")
proj4string(MCPYann) <- CRS("+init=epsg:4326")
MCPYann

#run MCP
MCPYann.mcp <- mcp(MCPYann, percent=100, unout="m2")
#examine output
MCPYann.mcp

#Let's quickly plot our MCPs overtop of the points.
plot(MCPYann, col = as.factor(MCP@data$ID), pch = 16)
plot(MCPYann.mcp, col = alpha(1:5, 0.5), add = TRUE)
