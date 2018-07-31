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
              'tmap', 'dplyr', 'tictoc', 'gsubfn','doParallel' )
ipak(packages)


##### Set working directory
setwd("/Users/gianluca/Dropbox") #Gianluca
setwd("/home/edarin/") #Édith
setwd("C://Users//ecd1u18/Dropbox") # Edith windows
getwd()

load("Z:/mydocuments/DRC/code_DRC/data.RData")
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

## Do some quality check
# NOn-Residential buildings surveyed
buffer_bandundu@data %>% filter(building_type == 'Non-Residential' & !is.na(population)) %>% summarise(sum(population))

########
# 3. Add settlement layer
########

raster_bandundu@legend@colortable = c('#ffcccc','#ff0000')
plot(raster_bandundu)
#########
# 3. Compute coverage
############

#### 3.1 Draw buffer
buffer_bandundu_size = 5 # for sensitivity study

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

load_data = function(cluster_name, buffer = buffer_bandundu){
  #select data
  #cluster_name = as.character(cluster_names[cluster_name])
  census = buffer[which(buffer$mez_id == cluster_name),]
  border = census_clust[which(census_clust$mez_id == cluster_name),]
  border_lines = SpatialLines(list(Lines(Line(geom(border)[,c('x','y')]), 'border_lines')), proj4string = CRS(crs_lglat))
  
  #select raster and increase resolution
  border_extent = extent(border)+0.5
  raster = raster::crop(raster_bandundu, border_extent) %>% disaggregate(fact=10)
  
  #dissolve buffer
  coords_buff = coordinates(census)
  id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
  census_diss = unionSpatialPolygons(census, id_buff)
  census_diss_wg = spTransform(census_diss, CRS(crs_lglat))
  
  #crop focus aerea
  raster_border = raster::crop(raster, border, snap='out')
  raster_surv = raster::crop(raster,census_diss_wg, snap='out')
  
  output = list(raster_border,raster_surv,border,border_lines, census_diss_wg)
  
  return(output)
  
}


evaluate_coverage = function(raster_border, raster_surv,border, border_lines, census_diss_wg){

  ### SETLLEMEMT COVERAGE
  # find corresponding raster layer
  rstz_p <- rasterize(border, raster_border,field=1,background=0) # by polygone centroids 
  rstz_l <- rasterize(border_lines, raster_border,field=1,background=0) #by lines
  rstz = rstz_l + rstz_p - rstz_l*rstz_p
  mask <- mask(x=raster_border, mask=rstz, updatevalue = 0, updateNA=T, maskvalue=0)
  # compute denominator
  build_area = sum(mask@data@values, na.rm = T)*10*10
  #compute numerator
  raster_surv_ext= raster::intersect(raster_surv, mask) %>% extend(y=mask, value =0)
  raster_unsurv= mask - raster_surv_ext
  unsurv = sum(raster_unsurv@data@values==1)*10*10
  #compute proportion
  under_cov = round(1-unsurv/build_area,2) * 100
  
  output = list(mask,raster_surv, unsurv, under_cov,  build_area)
  return(output)
  
}


plot_census = function(cluster_name, mask, border, 
                       census_diss_wg, cov_acc = NA, under_cov=NA){
  text_title = paste('cluster:',cluster_name)
  text_sub = paste('coverage:',under_cov,
               '% \n supplementary census:',cov_acc, '%',
               sep = ' ')
  plot(mask, main =text_title, sub=text_sub, axes=T)
  plot(border, add =T, main=text)
  plot(census_diss_wg, main=text,col= 'orange', border='orange',add=T)
  
}


evaluate_supp = function(raster_surv, build_area, census_diss_wg, unsurv){
  ### COVERAGE ACCURACY
  #Compute statistic
  rstrz_surv = rasterize(census_diss_wg, raster_surv, mask=T)
  cov_acc= sum(rstrz_surv@data@values==0, na.rm=T)*10*10 %>%
    `/`(build_area-unsurv) * 100
  over_cov= sum(rstrz_surv@data@values==0, na.rm=T)*10*10 %>%
    `/`(build_area) * 100 
  
  output= list(round(cov_acc,2), round(over_cov,2))
  
  return(output)
}
cluster_name = 'drc_maindombe_0059'

evaluate_census = function(cluster_name, plot =FALSE){
  list[raster_border,raster_surv,border,border_lines, census_diss_wg]=load_data(cluster_name)
  list[mask,raster_surv, unsurv, under_cov,  build_area]=evaluate_coverage(raster_border,raster_surv, border, border_lines, census_diss_wg)
  list[cov_acc,over_cov]=evaluate_supp(raster_surv, build_area, census_diss_wg, unsurv)

  ### PLOT
  if (plot==TRUE) {
    plot_census(cluster_name, mask, border, census_diss_wg, cov_acc, under_cov)
  }
  
  output= c(cluster_name, under_cov,cov_acc, over_cov, unsurv, build_area)
  gc()
  return(output)
}

tic('evaluate')
evaluate_census('drc_kwilu_0192', plot=T)
toc()

eval_stat <- matrix(ncol=6, nrow=length(cluster_names))
tic('evaluate')
i=1
for(name in cluster_names){
  eval_stat[i,]= evaluate_census(name)
  i = i+1
  if(i %%20 == 0){
    print(i)
  }
}
toc()

# WIP
evaluate_census_para = function(index, plot =FALSE){
  cluster_name= cluster_names[index]
  print(cluster_name)
  list[raster_border,raster_surv,border,border_lines, census_diss_wg]=load_data(cluster_name)
  print(2)
  list[mask,raster_surv, unsurv, under_cov, build_area]=evaluate_coverage(raster_border,raster_surv, border, border_lines, census_diss_wg)
  list[cov_acc,over_cov]=evaluate_supp(raster_surv, census_diss_wg, unsurv)
  
  ### PLOT
  if (plot==TRUE) {
    plot_census(cluster_name, mask, border, census_diss_wg, cov_acc, under_cov)
  }
  
  output= c(cluster_name, under_cov,cov_acc, over_cov, build_area, unsurv)
  gc()
  return(output)
}
cl <- makeCluster(3, outfile='')
registerDoParallel(cl)
x <- foreach(i = 7:9,
             .combine='cbind',
             .packages=c('raster','sp','maptools')) %dopar% evaluate_census_para(i)
stopCluster(cl)
####

eval_stat= data.frame(eval_stat)
colnames(eval_stat) = c('mez_id','cov_prop',  'cov_acc', 'supp_prop','unsurv', 'build_area')
eval_stat[, -1] = lapply(eval_stat[, -1], FUN= function(x) as.numeric(as.character(x)))
summary(eval_stat[,-1])

# Group

eval_stat= eval_stat %>% mutate(cov_grp = ifelse(cov_prop < 60, '0%-60%', 
                                                                                 ifelse(cov_prop <80, '60%-80%', 
                                                                                        ifelse(cov_prop < 90,'80%-90%' , 
                                                                                               ifelse(cov_prop <101,'90%-100%', NA)) )))
eval_stat$cov_grp= factor(eval_stat$cov_grp, levels=c('0%-60%','60%-80%','80%-90%','90%-100%'))
eval_stat= eval_stat %>% mutate(supp_grp = ifelse(supp_prop == 0, '0%', 
                                                  ifelse(supp_prop > 1, '1%-3%', 
                                                         ifelse(supp_prop > 0.3,'0.4%-1%' , 
                                                                ifelse(supp_prop > 0,'0.03%-0.4%', NA)) )))
eval_stat$supp_grp= factor(eval_stat$supp_grp)


# compute densities

pop = buffer_bandundu@data %>% filter(building_type == 'Residential') %>% group_by(mez_id) %>% select(mez_id,population)%>% summarise(pop=sum(population, na.rm=T))

eval_stat= left_join(eval_stat,pop)
eval_stat = eval_stat %>% mutate(dens_tot = pop*10000/build_area, dens_cov= pop*10000/(build_area-unsurv))
hist(eval_stat$dens_cov, breaks=70, ylim=c(0,30), col=rgb(0,0.9,0.3,0.3), border='white', main='Distribution of density', xlab='person per km^2')
hist(eval_stat$dens_tot, breaks=70, col=rgb(0.9,0,0.2,0.3), add=T,border='white')
text(x=100,y=c(22,25),adj=0, labels = c("-- On surveyed area", "-- On total"), col=c(rgb(0,0.9,0.3,0.5),rgb(0.9,0,0.2,0.5)))



buffer_bandundu@data = left_join(buffer_bandundu@data, eval_stat)


# spot outliers
low_complete = buffer_bandundu@data %>% filter(cov_grp == '0%-60%')
supp_census = buffer_bandundu@data %>% filter(supp_grp == '1%-3%')

summary(eval_stat$supp_prop)
summary(eval_stat$cov_acc)

table(eval_stat$cov_grp)
table(eval_stat$supp_grp)

#graph
hist(eval_stat$cov_prop, breaks = 100, col='purple',  main= 'Distribution of coverage proportion', xlab='Percent of coverage' )

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

par(mfrow=c(2,2))
low_complete_names = low_complete %>% distinct(mez_id) %>% select(mez_id) %>% unlist(use.names = F)
for (name in low_complete_names) {
  evaluate_census(name, buffer = buffer_bandundu, plot = T)
}

par(mfrow=c(2,2))
supp_names = buffer_bandundu@data %>% filter(supp_grp1 == '1%-3%')%>% distinct(mez_id) %>% select(mez_id) %>% unlist(use.names = F)
for (name in supp_names) {
  evaluate_census(name, buffer = buffer_bandundu, plot = T)
}

hist(eval_stat$supp_prop, breaks = 100, col='purple',  main= 'Distribution of supplementary surveyed area proportion', xlab='Percent of total settlement area in cluster' )

tm_shape(boundaries) +
  tm_fill()+
  tm_shape(buffer_bandundu) +
  tm_dots(size=0.1,   
          col='supp_grp1', title = 'Over settlement areas percentage', palette='RdPu') +
  tm_legend()+
  tm_layout(title = 'Supplementary census \n for each cluster', main.title.position= 'left',
            legend.outside = T, legend.outside.position = 'right',
            title.size	=2
  )

###########
# Compute over sampled buildings
###########

# too computer intensive
# tic('dissagregate')
# raster_bandundu_dis = disaggregate(raster_bandundu, fact=10)
# toc()

compute_oversurveyed = function(cluster_name, buffer=buffer_bandundu, plot= FALSE){
  
  #select data
  census = buffer[which(buffer$mez_id == cluster_name),]
  border = census_clust[which(census_clust$mez_id == cluster_name),]

  #dissolve buffer
  coords_buff = coordinates(census)
  id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
  census_diss = unionSpatialPolygons(census, id_buff)
  census_diss_wg = spTransform(census_diss, CRS(crs_lglat))
  
  #
  raster_surv = raster::crop(raster_bandundu,census_diss_wg, snap='out')
  raster_surv = disaggregate(raster_surv, fact=10)
  rstrz = rasterize(census_diss_wg, raster_surv, mask=T)
  stat=sum(rstrz@data@values==0, na.rm=T)*10*10
  
  if (plot==T) {
  text = paste('cluster:',cluster_name, '\n Area unbuilt but surveyed (m^2):',stat,sep = ' ')
  plot(raster_surv, axes=TRUE, main=text)
  plot(border,add=T)
  plot(census_diss_wg, col= 'orange', border='orange',add=T)
  
  }
  return(stat)
  
}

compute_oversurveyed('drc_kwilu_0180', plot=T)

area_unbuild = vector(length=length(cluster_names))
i=1
for(name in cluster_names){
  area_unbuild[i]= compute_oversurveyed(name, buffer = buffer_bandundu)
  if(i %%20 == 0){
    print(i)
  }
  i = i+1
  
}

area_unbuild = data.frame('area_over' = area_unbuild, 'mez_id'=cluster_names)
worst_name = as.character(area_unbuild %>% filter(area_over == max(area_over)) %>% select(mez_id) %>% unlist(use.names = F))
compute_oversurveyed(worst_name, plot=T)
hist(area_unbuild$area_over, breaks=50, col='purple', main='Distribution of unbuilt areas surveyed per cluster (m^2)')
high_area_unbuild = area_unbuild %>% filter(area_over > 1000)

high_area_names = high_area_unbuild %>% distinct(mez_id) %>% select(mez_id) %>% unlist(use.names = F)

for (name in high_area_names) {
  compute_oversurveyed(name, buffer = buffer_bandundu, plot = T)
}

compute_completeness('drc_kwilu_0154', buffer= buffer_bandundu, plot=T)
compute_oversurveyed('drc_kwilu_0154', buffer= buffer_bandundu, plot=T)

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




