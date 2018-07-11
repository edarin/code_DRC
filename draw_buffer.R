##### Load libraries
library(rgdal)
library(maptools)
library(dplyr)
library(sp)

##### Set working directory
setwd("/Users/gianluca/") #Gianluca
setwd("/home/edarin/") #Édith
getwd()

##### Import datasets
boundaries <- readOGR("Dropbox/data/in/boundaries/", "boundaries_bandunduKinshasa")

census_res <- readOGR("Dropbox/data/in/eHealth/DRC_micocensus_Final.gdb/", 
                      "residential_buildings")
census_nonres <- readOGR("Dropbox/data/in/eHealth/DRC_micocensus_Final.gdb/", 
                         "non_residential_buildings")

census_clust <- readOGR("Dropbox/data/in/microcensusCluster/", 
                       "Microcensus_cluster_polygons")

# 1. Focus on Bandundu
# Drop Kinshasa
boundaries <- boundaries[boundaries@data$name != 'Kinshasa',]

# Merge the three regions
coords <- coordinates(boundaries)
id <- rep(mean(coords[,1]),3)
boundaries <- unionSpatialPolygons(boundaries, id)
plot(boundaries)

# 2. Merge census data
# Create dummy to track original dataset
census_res@data$res_status = "darkgreen"
census_nonres@data$res_status = "orange"

# Build merged dataset
data <- full_join(census_res@data, census_nonres@data)
coords <- rbind(census_res@coords, census_nonres@coords)
census_full <- SpatialPointsDataFrame(coords, data )
census_full@proj4string <- boundaries@proj4string
# Plot
plot(census_full, col=census_full@data$res_status)# too many points to see both
census_extract =census_full[sample(1:83058,50000, replace = F),]
plot(census_extract,pch =20, col=census_extract@data$res_status, cex=0.3)
plot( boundaries, add=TRUE)


#Transer the attribute from the census_clus to census points - Spatial join here -> https://gis.stackexchange.com/questions/137621/join-spatial-point-data-to-polygons-in-r
