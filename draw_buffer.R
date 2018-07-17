##### Load libraries

# Function to load and install if required

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rgdal", "maptools", "spatialEco", 
              "dplyr", "sp", "gpclib", "rgeos",
              'tmap')
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
########
# 1. Handling SpatialPolygon
########

##### 1.1 Focus on Bandundu
# Drop Kinshasa
boundaries <- boundaries[boundaries@data$name != 'Kinshasa',]

# Merge the three regions
coords <- coordinates(boundaries)
id <- rep(median(coords[,1]),3)
boundaries <- unionSpatialPolygons(boundaries, id)
plot(boundaries)

########
# 2. Handling SpatialPoint (the census)
########

##### 2.1 Merge census data

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


##### 2.2 Transer the attribute from the census_clus to census points 
rownames_cluster = colnames(census_clust@data)


# First step: Spatial merge
census_full = point.in.poly(census_full, census_clust)
census_full@data %>%  filter(is.na(mez_id)) %>% summarise(n())


# second step: Dplyr merge
census_clust$cluster_id = census_clust$mez_id
df=census_full@data %>%  filter(is.na(mez_id)) %>% select(-rownames_cluster) %>% left_join(census_clust@data)
census_full@data[is.na(census_full$mez_id),] = df
census_full@data %>%  filter(is.na(mez_id))  %>% summarise(n())
census_full@data %>%  filter(is.na(mez_id)) %>% group_by(cluster_id) %>% summarise(n())


#### 2.3 Drop Kinshasa observations

census_bandundu = census_full
census_bandundu = point.in.poly(census_bandundu, boundaries)
census_bandundu = census_bandundu[!is.na(census_bandundu$poly.ids),]
#clust_bandandu = intersect(census_clust,boundaries) not sure required

n=nrow(census_bandundu@data)
census_bandundu_extract =census_bandundu[sample(1:n,50000, replace = F),]
plot(census_bandundu_extract,pch =20, col=census_bandundu_extract@data$res_status, cex=0.3)
plot( boundaries, add=TRUE)

#########
# 3. Compute coverage
############

#### 3.1 Draw buffer
bandundu_buffer_size = 5 # for sensitivity study

census_bandundu= spTransform(census_bandundu, CRS("+proj=utm +zone=34 +datum=WGS84 +units=m"))
census_clust = spTransform(census_clust, CRS("+proj=utm +zone=34 +datum=WGS84 +units=m"))

bandundu_buffer = gBuffer(census_bandundu, width = bandundu_buffer_size, byid=T)

plot(bandundu_buffer[2,])
plot(census_bandundu[2,])

##### 3.2

# Drop NA
bandundu_buffer = bandundu_buffer[!is.na(bandundu_buffer$mez_id),]
# Comput
compute_completeness = function(cluster_name){

  #select data
  census = bandundu_buffer[which(bandundu_buffer$mez_id == cluster_name),]
  border = census_clust[which(census_clust$mez_id == cluster_name),]
  
  #dissolve buffer
  coords_buff = coordinates(census)
  id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
  census_diss = unionSpatialPolygons(census, id_buff)
  
  #compute proportion
  prop = gArea(census_diss)/gArea(border)

  output= prop
  
  return(output)
}

cluster_names = droplevels(bandundu_buffer@data %>% distinct(mez_id) %>% select(mez_id) %>%  unlist(use.names = FALSE))

complete_prop = vector(length=nrow(census_clust@data))
i=0
for(name in cluster_names){
  complete_prop[i]= compute_completeness(name)
  
  if(i%%10){
    print(i)
  }
  i = i+1
  
}

a=compute_completeness('drc_maindombe_0076')
### Test case: Maidombe 0076
cluster_name='drc_maindombe_0076'
test_case = bandundu_buffer[which(bandundu_buffer$mez_id == cluster_name),]
test_case_border = census_clust[which(census_clust$mez_id == cluster_name),]

# Dissolve
coords_buff = coordinates(test_case)
id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
test_case_diss = unionSpatialPolygons(test_case, id_buff)
prop = gArea(test_case_diss)/gArea(test_case_border)

plot(test_case)
plot(test_case_diss, add=T, border = 'red')


plot(census_bandundu[which(census_bandundu$mez_id == cluster_name),],
     pch=20)

tm_shape(test_case_border) +
  tm_fill()+
  tm_shape(test_case) +
  tm_bubbles(size=0.3, border.lwd = 0.1, alpha=0.5, 
             col='building_type', title.col='Buildings visited',
             palette= "Oranges") +
  tm_legend()+
  tm_layout('Test case: Maidombe 0076')







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


