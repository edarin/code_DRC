##### Load libraries

# Function to load and install if required
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rgdal", "maptools", "spatialEco", "dplyr", "sp")
ipak(packages)


##### Set working directory
setwd("/Users/gianluca/Dropbox") #Gianluca
setwd("/home/edarin/") #Édith
setwd("C://Users//ecd1u18/Dropbox") # Edith windows
getwd()

##### Import datasets
boundaries <- readOGR("data/in/boundaries", "boundaries_bandunduKinshasa")

census_res <- readOGR("data/in/eHealth/DRC_micocensus_Final.gdb", 
                      "residential_buildings")
census_nonres <- readOGR("data/in/eHealth/DRC_micocensus_Final.gdb", 
                         "non_residential_buildings")

census_clust <- readOGR("data/in/microcensusCluster", 
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
census_full$index = seq(nrow(census_full))
# Plot
plot(census_full, col=census_full@data$res_status)# too many points to see both
census_extract =census_full[sample(1:83058,50000, replace = F),]
plot(census_extract,pch =20, col=census_extract@data$res_status, cex=0.3)
plot( boundaries, add=TRUE)


#Transer the attribute from the census_clus to census points 
names_cluster = colnames(census_clust@data)


# First step: Spatial merge
census_full = point.in.poly(census_full, census_clust)
census_full@data %>%  filter(is.na(mez_id)) %>% summarise(n())


# second step: Dplyr merge
census_clust$cluster_id = census_clust$mez_id
df=census_full@data %>%  filter(is.na(mez_id)) %>% select(-names_cluster) %>% left_join(census_clust@data)
census_full@data[is.na(census_full$mez_id),] = df
census_full@data %>%  filter(is.na(mez_id))  %>% summarise(n())
census_full@data %>%  filter(is.na(mez_id)) %>% distinct(cluster_id) %>% summarise(n())

census_full@data %>%  filter(is.na(mez_id)) %>% View()

# EXPLORATION SECTION


###### Comparing merging methods
# Understanding the weird cluster_id 
census_full@data %>%  filter(is.na(mez_id))  %>% summarise(n())
census_full@data %>% filter(is.na(mez_id))  %>% group_by(cluster_id) %>% summarise(n()) %>% View()

# Dplyr merge
merge = census_full@data %>% left_join(census_clust@data)
merge %>%  filter(is.na(mez_id))  %>% summarise(n())

# look similarities of merging method
idx.mrg.m= merge %>%  filter(!is.na(mez_id))  %>% select(index) %>% unlist(use.names = FALSE)
idx.spt.m =census_full@data %>%  filter(!is.na(mez_id))  %>% select(index)%>% unlist(use.names = FALSE)
idx.m.intersect = intersect(idx.mrg.m, idx.spt.m)

all.equal(merge[idx.m.intersect,],census_full@data[idx.m.intersect,]) 
        # for merged items same result -> comforting

# Idea: using two steps of merge, first spatial than dplyr

census_full@data = census_full@data %>% left_join(census_clust@data,
                                                  by="cluster_id")
sum(is.na(census_full$mez_id.y))


