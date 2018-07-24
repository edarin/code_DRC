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
              "raster", "sp", "gpclib", "rgeos",
              'tmap', 'dplyr')
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

raster_bandundu <- raster('data/in/ornl_settlement/cod_ban_100m_ornl_settlement.tif')

crs_lglat= '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
crs_utm = '+proj=utm +zone=34 +datum=WGS84 +units=m'

########
# 1. Handling SpatialPolygon
########

##### 1.1 Focus on Bandundu
# Drop Kinshasa
boundaries <- boundaries[boundaries@data$name != 'Kinshasa',]
census_clust <- census_clust[census_clust@data$province != 'Kinshasa',]

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
#plot(census_full, col=census_full@data$res_status)# too many points to see both
# census_extract =census_full[sample(1:83058,50000, replace = F),]
# plot(census_extract,pch =20, col=census_extract@data$res_status, cex=0.3)
# plot( boundaries, add=TRUE)


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
census_bandundu$poly.ids = NULL # don't need anymore

n=nrow(census_bandundu@data)
census_bandundu_extract =census_bandundu[sample(1:n,50000, replace = F),]
plot(census_bandundu_extract,pch =20, col=census_bandundu_extract@data$res_status, cex=0.3)
plot( boundaries, add=TRUE)

########
# 3. Add settlement layer
########

raster_bandundu@legend@colortable = c('#ffcccc','#ff0000')
plot(raster_bandundu)
#########
# 3. Compute coverage
############

#### 3.1 Draw buffer
buffer_bandundu_size = 10 # for sensitivity study

census_bandundu= spTransform(census_bandundu, CRS(crs_utm))
census_clust_utm = spTransform(census_clust, CRS(crs_utm))

buffer_bandundu = gBuffer(census_bandundu, width = buffer_bandundu_size, byid=T)

plot(buffer_bandundu[2,])
plot(census_bandundu[2,])
 
##### 3.2

# Drop NA
buffer_bandundu = buffer_bandundu[!is.na(buffer_bandundu$mez_id),]

# Flag cluster without observation
cluster_names = buffer_bandundu@data %>% distinct(mez_id) %>% select(mez_id)
cluster_names$obs = TRUE
cluster_theory = census_clust@data %>% distinct(mez_id) %>% select(mez_id)
spot_missing_cluster = left_join(cluster_theory, cluster_names)
spot_missing_cluster = droplevels(spot_missing_cluster %>% filter(is.na(obs)) %>% select(mez_id) %>% unlist(use.names = FALSE))
cluster_names = droplevels(cluster_names$mez_id)                                 
                                 
# Coverage study
raster_bandundu[is.na(raster_bandundu[])] <- 0 

compute_completeness = function(cluster_name, buffer = buffer_bandundu, plot =FALSE){

  #select data
  census = buffer[which(buffer$mez_id == cluster_name),]
  border = census_clust[which(census_clust$mez_id == cluster_name),]
  border_utm = census_clust_utm[which(census_clust_utm$mez_id == cluster_name),]
  border_lines = SpatialLines(list(Lines(Line(geom(border)[,c('x','y')]), 'border_lines')), proj4string = CRS(crs_lglat))
  
  # find corresponding raster layer
  raster = raster::crop(raster_bandundu, border, snap='out')
  rstz_p <- rasterize(border, raster,field=1,background=0) #by lines
  rstz_l <- rasterize(border_lines, raster,field=1,background=0) # by polygone centroids
  rstz = rstz_l + rstz_p - rstz_l*rstz_p
  mask <- mask(x=raster, mask=rstz, updatevalue = 0, updateNA=T, maskvalue=0)

  # compute denominator
  build_area = sum(mask@data@values, na.rm = T)*100*100

  #dissolve buffer
  coords_buff = coordinates(census)
  id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
  census_diss = unionSpatialPolygons(census, id_buff)
  census_diss_wg = spTransform(census_diss, CRS(crs_lglat))
  
  
  #compute numerator
  raster_surv = raster::crop(raster_bandundu,census_diss_wg, snap='out')
  raster_surv= raster::intersect(raster_surv, mask)
  raster_surv=extend(raster_surv, mask, value =0)
  raster_unsurv= mask - raster_surv
  unsurv = sum(raster_unsurv@data@values==1)*100*100
  
  #compute proportion
  prop = 1-unsurv/build_area
  
  #plot

  if (plot==TRUE) {
    plot(mask)
    plot(border, add =T)
    plot(census_diss_wg, col= 'green', border='green',add=T)
    
  }

  output= prop
  
  return(output)
}


compute_completeness('drc_kwango_0097', plot = T)

  

completed_prop = vector(length=length(cluster_names))
i=1
for(name in cluster_names){
  completed_prop[i]= compute_completeness(name, buffer = buffer_bandundu)
  
  if(i %%20 == 0){
    print(i)
  }
  i = i+1
  
}

summary(completed_prop)
worst_name = as.character(cluster_names[which(completed_prop == min(completed_prop))])
compute_completeness(worst_name, plot=T)

completed_prop = data.frame('cov_prop' = round(completed_prop,2), 'mez_id'=cluster_names)
buffer_bandundu@data = left_join(buffer_bandundu@data, completed_prop)

buffer_bandundu@data= buffer_bandundu@data %>% mutate(cov_grp = ifelse(cov_prop == 1, '100%', ifelse(cov_prop < 0.7, '14%-70%', 
                                                                                 ifelse(cov_prop <0.8, '70%-80%', 
                                                                                        ifelse(cov_prop < 0.9,'80%-90%' , 
                                                                                               ifelse(cov_prop <1,'90%-100%', NA)) ))))
buffer_bandundu$cov_grp= factor(buffer_bandundu$cov_grp, levels=c('14%-70%','70%-80%','80%-90%','90%-100%','100%'))


hist(completed_prop, breaks = 100, col='purple', xlim=c(0,1), main= 'Distribution of coverage proportion' )

tm_shape(boundaries) +
  tm_fill()+
  tm_shape(buffer_bandundu) +
  tm_dots(size=0.1,   
             col='cov_grp', title = 'Coverage percentage', palette='RdPu') +
  tm_legend()+
  tm_layout(title = 'Census coverage \n for each cluster', main.title.position= 'left',
            legend.outside = T, legend.outside.position = 'right',
            title.size	=2
  )

low_complete = buffer_bandundu@data %>% filter(cov_grp == '14%-70%')
low_complete$interviewer_name = trim(low_complete$interviewer_name)
low_complete %>% group_by(cov_prop,interviewer_name)  %>% distinct(cluster_id) %>% summarise(n())




###########
# Compute over sampled buildings
###########



#### sensitivity analysis to buffer size

# 1m
buffer_bandundu_1 = gBuffer(census_bandundu, width = 1, byid=T)

completed_prop_1 = vector(length=length(cluster_names))
i=1
for(name in cluster_names){
  completed_prop_1[i]= compute_completeness(name, buffer_bandundu = buffer_bandundu_1)
  
  if(i %%20 == 0){
    print(i)
  }
  i = i+1
  
}
summary(completed_prop_1)
hist(completed_prop_1, breaks = 10, col='orange' )

#10m
buffer_bandundu_10 = gBuffer(census_bandundu, width = 10, byid=T)

completed_prop_10 = vector(length=length(cluster_names))
i=1
for(name in cluster_names){
  completed_prop_10[i]= compute_completeness(name, buffer_bandundu = buffer_bandundu_10)
  
  if(i %%20 == 0){
    print(i)
  }
  i = i+1
  
}
summary(completed_prop_10)
hist(completed_prop_10, breaks = 50, col='orange' )

# EXPLORATION SECTION


#### Test case: Kwango 0090
cluster_name='drc_kwango_0091'
test_case_census = census_bandundu[which(census_bandundu$mez_id == cluster_name),]
test_case_buff = buffer_bandundu[which(buffer_bandundu$mez_id == cluster_name),]
test_case_border = census_clust[which(census_clust$mez_id == cluster_name),]
test_case_border_utm = census_clust_utm[which(census_clust_utm$mez_id == cluster_name),]

# plot
plot(test_case_buff[1:3,], main= 'Drawing buffer around observations')
plot(test_case_census[1:3,], pch=20, col='orange', add=T)

# Dissolve
coords_buff = coordinates(test_case_buff)
id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
test_case_diss = unionSpatialPolygons(test_case_buff, id_buff)



plot(census_bandundu[which(census_bandundu$mez_id == cluster_name),],
     pch=20)

tm_shape(test_case_border) +
  tm_fill()+
  tm_shape(test_case_census) +
  tm_bubbles(size=0.3, border.lwd = 0.1, alpha=0.5, 
             col='building_type', title.col='Buildings visited',
             palette= "Oranges") +
  tm_legend()+
  tm_layout('Test case: Kwango 0090')

# census
test_case_diss_wg = spTransform(test_case_diss, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))


# Raster data

test_raster = raster::crop(raster_bandundu, test_case_border)
test_rstz <- rasterize(test_case_border, test_raster)   
test_mask <- mask(x=test_raster, mask=test_rstz)

plot(test_raster)
plot(test_rstz)
plot(test_mask)
title(main='Census points in cluster', line=-1)
plot(test_case_border, add=T)
plot(test_case_diss_wg, col= 'green', border='green',add=T)

sum(!is.na(test_raster@data@values))
sum(!is.na(test_mask@data@values))

test_mask[is.na(test_mask[])] <- 0 

a = raster::intersect(test_mask,test_case_diss_wg)
b=extend(a, extent(test_mask))
b[is.na(b[])] <- 0 


c= overlay(test_mask, b, fun= function(x,y){x-y})
plot(b)
plot(test_mask -b)
plot(c)
sum(c@data@values)


for (name in cluster_names) {
  border = census_clust[which(census_clust$mez_id == name),]
  raster = raster::crop(raster_bandundu, border)
  print(sum(is.na(raster@data@values)))
  
}








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




