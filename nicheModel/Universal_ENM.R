# GOAL: Do the ENM analysis but make it more universal (so we can use it for any species)

# Load applicable libraries
library(ENMTools)
library(raster)
library(dplyr)

#file <- read.csv("SnailCoords_08_10.csv") # Species are S#
#file <- read.csv("NaesiotusCoords.csv")   # Species and genus
#file <- read.csv("NaesiotusDataNoRepeats_October (1).csv")

file <- read.csv("NaesiotusData_updatedOctober.csv") # Most recent spreadsheet 10/22/20

#colnames(file) <- c("Species", "Latitude", "Longitude", "Collection.ID", "Note") # Not needed for most recent

species_name <- "Naesiotus ochsneri"

# Grab all row numbers that have this species
selection <- which(file$Species == species_name)

# Create new data frame with just that species' info
species_data <- file[selection,]

# Presence lat and long
lat <- species_data$Latitude
lon <- species_data$Longitude
latlong <- cbind.data.frame(lat, lon)

# For now, background points are just points of other species
anti_selection <- which(file$Species != species_name)

# Create data frame with the background species' info
background_data <- file[anti_selection,]

# Background lat and long
lat <- background_data$Latitude
lon <- background_data$Longitude
latlongbg <- cbind.data.frame(lat, lon)

# Make sure the background data does not have presence data points
# Returns all rows from latlongbg WITHOUT a match in latlong
latlongbg <- anti_join(latlongbg, latlong)

# You can get 0.5 minute resolution (~1km^2) WorldClim data if you use a lon and lat for the tile you want
env <- raster::getData('worldclim', var='bio', res=0.5, lon = -90.44, lat = -0.69)

# Crop the environment to the extent we figured out before
env <- crop(env, extent(-90.57163, -90.11387, -0.8867798, -0.3647284))

# Declare the enmtools.species object using the species name and presence points
snail <- enmtools.species(species.name = species_name,
                          presence.points = latlong)

# ENMTools can create a range raster using presence points as a reference
radius <- 5   # Radius for circular buffers to draw around points, in meters
snail$range <- background.raster.buffer(snail$presence.points, radius, mask = env)

# Add the background points to the enmtools.species object
snail$background.points <- latlongbg

# After creating the species object, use check.species to make sure it's set up properly
snail <- check.species(snail)

# Take a look at our fancy new object!
snail

# Check the MDS to choose good variables and prevent colinearity
raster.cor.plot(env)

# Choosing bio2, bio5, bio7
env1 <- env[[c("bio2_32", "bio5_32", "bio7_32")]]

# Build the GLM
snail.glm <- enmtools.glm(species = snail, env = env1, f = pres ~ bio2_32 + bio5_32 + bio7_32)

# Let's take a look!
snail.glm

########
# Can we map the roads on the suitability plot?
########
# Grab just the suitability part
suitable <- snail.glm$suitability
plot(suitable)    # The colors change, but the legend is helpful

# Read roads shape file
roads <- shapefile("roads-line.shp")

# Add it to the map
plot(roads, add = TRUE)

# ISSUE: Roads seem correct when the plot is small, but not when fully zoomed in?
# Could be the 1920x1080 aspect ratio that's stretching the roads

# One thing Kelly and I tried was changing the proj4string for the roads
# object to the same proj4string seen in the suitability plot

# It didn't seem to change anything, but here's the code just in case:
proj4string(roads) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

