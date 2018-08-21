
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("rgdal", "maptools", "spatialEco", 'MASS','tidyr',
              "raster", "sp", "gpclib", "rgeos",
              'tmap', 'dplyr', 'tictoc', 'gsubfn','doParallel' )
ipak(packages)



setwd("C://Users//ecd1u18/Dropbox") # Edith windows


df_resident <- sf::st_read(dsn = "data/in/eHealth/DRC_micocensus_Final.gdb",
                  layer = "residents")
df_hh <- sf::st_read(dsn = "data/in/eHealth/DRC_micocensus_Final.gdb",
                           layer = "household_data")
df_migr <- sf::st_read(dsn = "data/in/eHealth/DRC_micocensus_Final.gdb",
                           layer = "migrants")
census_res_geo <- readOGR("data/in/eHealth/DRC_micocensus_Final.gdb", 
                      "residential_buildings")
census_clust <- readOGR("data/in/microcensusCluster", 
                        "Microcensus_cluster_polygons")
boundaries <- readOGR("data/in/boundaries", "boundaries_bandunduKinshasa")

# select focus data (Bandundu)

cluster_names <- read.csv('data/in/cluster_names.csv', row.names = 1)

census_res_geo <- census_res_geo[census_res_geo$cluster_id %in% cluster_names$mez_id,]
census_res_geo <- census_res_geo[census_res_geo$building_type %in% c('Residential', 'Mixed'),]
df_resident <- droplevels(df_resident[df_resident$cluster_id %in% cluster_names$mez_id,])

building_name <- df_resident %>% distinct(building_id) %>% select(building_id) %>% unlist(use.names = F)

df_hh <- droplevels(df_hh[df_hh$building_id %in% building_name,])
df_migr <- droplevels(df_migr[df_migr$building_id %in% building_name,])

#Cleaning
census_res = census_res_geo@data
census_res =census_res %>% mutate(status=replace(status, status=='Incomplete' & population >0, 'Complete'))
  
## Duplicate analysis
# In resident layer one building appears in two different clusters
# in census data only in one
df_resident %>% group_by(building_id) %>% distinct(cluster_id) %>% filter(n()>1) %>% View()

#Remove false incomplete
# actual completed survey marked as incomplete

#Prestep: knowing how may observations have the same GPS coords
census_res %>% filter(duplicated(latitude,longitude) |duplicated(latitude,longitude, fromLast = T) ) %>% View()


df_incomp <- census_res[census_res$status == 'Incomplete',]
df_comp <- census_res[census_res$status == 'Complete',]

incomp = data.frame(longitude=df_incomp$longitude,
                    latitude=df_incomp$latitude,
                    cluster_incomp = df_incomp$cluster_id,
                    building_incomp=df_incomp$building_id,
                    comments_incomp= df_incomp$comments)
incomp$incomp = TRUE
comp = data.frame(longitude=df_comp$longitude,
                  latitude=df_comp$latitude,
                  cluster_comp=df_comp$cluster_id,
                  building_comp=df_comp$building_id,
                  comments_comp= df_comp$comments)
comp$comp = TRUE

a=full_join(comp,incomp)

id_false_incomp <- as.character(a %>%  filter(comp==TRUE & incomp==TRUE) %>% distinct(building_incomp) %>% select(building_incomp) %>% unlist(use.names = F))
census_res = census_res %>% filter(!building_id %in% id_false_incomp) 

#Remove empty housing
# as shown in the comment
pattern = c('on occup', 'Abandonn', 'vide', 'noccup',
            "existe plus",'En construction','Boutique','habit',
            'Douche', 'Wc', 'detrui', 'toilette', 'Non occup',
            'Cuisine', 'Abondonnee','Deplaement', 'occup','Inau')
census_res =census_res %>% filter(!(status=='Incomplete' & grepl(paste(pattern, collapse = "|"),comments)))
census_res %>% filter(status=='Incomplete') %>% select(comments) %>% View()


census_res_geo = census_res_geo[census_res_geo$building_id %in% census_res$building_id,]



# Incomplete Analysis

#Overview
table(census_res$status)
tm_shape(boundaries) +
  tm_fill()+
  tm_shape(census_res_geo) +
  tm_dots(size=0.1,   
          col='status', title = 'Survey status \n (all building types)', palette = 'PiYG') +
  tm_legend()+
  tm_layout(title = 'drc_maindombe_0048', main.title.position= 'left',
            legend.outside = T, legend.outside.position = 'right')

census_res %>% group_by(cluster_id)
#Plot
exemple <- census_res_geo[census_res_geo$cluster_id =='drc_maindombe_0048',]
exemple_poly <- census_clust[census_clust$mez_id =='drc_maindombe_0048',]


tm_shape(exemple_poly) +
  tm_fill()+
  tm_shape(exemple) +
  tm_dots(size=0.1,   
          col='status', title = 'Survey status \n (all building types)', palette = 'PiYG') +
  tm_legend()+
  tm_layout(title = 'drc_maindombe_0048', main.title.position= 'left',
            legend.outside = T, legend.outside.position = 'right')

#statistics




building_incomp_id <- df_incomp %>% select(building_id, comments,interviewer_name )
building_incomp_id$incomplete = TRUE

df_resident <- left_join(df_resident, building_incomp_id )
df_resident %>% filter(interviewer_name=='Falco') %>% View() # Falco didn't report status as he should
table(df_resident$incomplete)

census_res %>% filter(interviewer_name =='Falco') %>% View()
