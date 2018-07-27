
# buffer_bandundu = polygones of buffer around observations in Bandundu
# census_clust = polygones of each microcensus area
# raster_bandundu = raster of settlement layer


#######
# Test case: Kwilu 0212
#######

cluster_name='drc_kwilu_0192'
test_case_buff = buffer_bandundu[which(buffer_bandundu$mez_id == cluster_name),]
test_case_border = census_clust[which(census_clust$mez_id == cluster_name),]
coords_buff = coordinates(test_case_buff)
id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
test_case_diss = unionSpatialPolygons(test_case_buff, id_buff)
test_case_diss_wg = spTransform(test_case_diss, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

raster_surv = raster::crop(raster_bandundu,test_case_diss_wg, snap='out') %>%
  disaggregate(fact=100)
#buffer_lines = SpatialLines(list(Lines(Line(geom(test_case_diss_wg)[,c('x','y')]), 'test')), proj4string = CRS(crs_lglat))
rstrz = rasterize(test_case_diss_wg, raster_surv, mask=T)
plot(rstrz)
plot(test_case_buff)
plot(raster_surv, axes=TRUE, main='rt')
plot(test_case_border,add=T)
plot(test_case_diss_wg, col= 'green', border='green',add=T)
sum(rstrz@data@values==0, na.rm=T)
plot(test_case_diss_wg)

#test_case_diss_wg@polygons[[1]]@Polygons[[1]]@coords 

# Raster data

a = SpatialLines(list(Lines(Line(geom(test_case_border)[,c('x','y')]), 'test')), proj4string = CRS(crs_lglat))

test = raster::crop(raster_bandundu,test_case_border, snap='out')
fr_l = rasterize(a, test, field=1,background=0)
fr_p <- rasterize(test_case_border, test, field=1,background=0)
fr = fr_l + fr_p - fr_l*fr_p
#fr[is.na(fr[])] <- 0
lr <- mask(x=test, mask=fr, updatevalue = 0, updateNA=T, maskvalue=0)
plot(test)
plot(fr)
plot(lr)
plot(test_case_border,add=T)
plot(test_case_diss_wg, col= 'green', border='green',add=T)






rstrz_surv <- rasterize(test_case_diss_wg, raster_surv, field=1,background=0, update=T, updateValue=0)
mask_surv <- mask(x=raster_surv, mask=rstrz_surv, updatevalue = 1, updateNA=T, maskvalue=1)
plot(raster_surv)
plot(rstrz_surv)
plot(mask_surv)

raster_surv= raster::intersect(raster_surv, lr)
raster_surv = extend(raster_surv, lr, value =0)
plot(raster_surv)
raster_unsurv= lr - raster_surv
unsurv = sum(raster_unsurv@data@values==1)*100*100
plot(raster_unsurv)


fr2 <- rasterize(test_case_diss_wg, raster_surv, field=1)   
lr2 <- mask(x=raster_surv, mask=fr2)

build_area = sum(lr@data@values, na.rm = T)*100*100



# apparently only builded area
# check it for all clusters

for (name in cluster_names) {
  border = census_clust[which(census_clust$mez_id == cluster_name),]
  raster = raster::crop(raster_bandundu, border)
  print(sum(is.na(raster@data@values)))
  
}
# all equals to zero -> apparently there is only build_areas in the clusters
#dissolve buffer
coords_buff = coordinates(buffer_bandundu)
id_buff = cut(coords_buff[,1], range(coords_buff[,1]), include.lowest=TRUE)
buffer_bandundu_diss = unionSpatialPolygons(buffer_bandundu, id_buff)
a= SpatialPointsDataFrame(buffer_bandundu_diss, buffer_bandundu@data)
buffer_bandundu_diss_wg = spTransform(buffer_bandundu_diss, CRS(crs_lglat))

# Convert SpatialPolygons to data frame
df <- as(buffer_bandundu, "data.frame")

# Aggregate and sum desired data attributes by ID list
df.agg <- aggregate(as.numeric(df$number_of_migrants), list(id_buff), sum)
shp.agg <- SpatialPolygonsDataFrame(buffer_bandundu_diss, df.agg)
