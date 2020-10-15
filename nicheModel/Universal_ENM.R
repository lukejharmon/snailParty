# GOAL: Do the ENM analysis but make it more universal (so we can use it for any species)

# Load applicable libraries
library(ENMTools)
library(raster)

file <- read.csv("SnailCoords_08_10.csv") # Species are S#
#file <- read.csv("NaesiotusCoords.csv")   # Species and genus

species_name <- "S12"

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
latbg <- background_data$Latitude
lonbg <- background_data$Longitude
latlongbg <- cbind.data.frame(latbg, lonbg)

# You can get 0.5 minute resolution (~1km^2) WorldClim data if you use a lon and lat for the tile you want
env <- raster::getData('worldclim', var='bio', res=0.5, lon = -90.44, lat = -0.69)

# Crop the environment to the extent we figured out before
env <- crop(env, extent(-90.57163, -90.11387, -0.8867798, -0.3647284))

# Declare the enmtools.species object using the species name and presence points
snail <- enmtools.species(species.name = species_name,
                          presence.points = latlong)

# ENMTools can create a range raster using presence points as a reference
radius <- 0.5   # Radius for circular buffers to draw around points, in meters
snail$range <- background.raster.buffer(snail$presence.points, radius, mask = env)

# Add the background points to the enmtools.species object
snail$background.points <- latlongbg

# After creating the species object, use check.species to make sure it's set up properly
snail <- check.species(snail)

# Take a look at our fancy new object!
snail

# Check the MDS to choose good variables and prevent colinearity
raster.cor.plot(env)

# Choosing bio3, bio5, bio7
env1 <- env[[c("bio3_32", "bio5_32", "bio7_32")]]

# Build the GLM
snail.glm <- enmtools.glm(species = snail, env = env1, f = pres ~ bio3_32 + bio5_32 + bio7_32)

# Let's take a look!
snail.glm
