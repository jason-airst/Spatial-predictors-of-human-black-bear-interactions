#### R packages ####
library(gamm4)
library(sf)
library(ggplot2)
library(ggeffects)
library(stringr)
library(dplyr)
library(spatialEco)


#### set working directory
setwd("D:/Bear_data")

#### Data from Biodiversity Investigation Reporting system ####
Random_locations <- read.csv("Random_locations.csv")
Human_bear_conflict_locations <- read.csv("Human_bear_conflict_locations.csv")
Bear_non_conflict_locations <- read.csv("Bear_non_conflict_locations.csv")

### Habitat data for a 1 km grid of Nova Scotia
NS_Habitat_1km_grid <- read.csv("NS_Habitat_1km_grid.csv")

#################################################################################################################
#################################################################################################################

## How the habitat data was analyzed for human-bear conflict locations, human bear non-conflict lacations,
## and random locations. The example given is for human-bear conflict locations

## Event location data
Conflict_data<-data.frame(East=Human_bear_conflict_locations$UTME, North = Human_bear_conflict_locations$UTMN, 
                          ID = Human_bear_conflict_locations$Number)


### Tell R to treat location data as a spatial dataset
Shape_file_conflict_data<- st_as_sf(Conflict_data, coords = c("East", "North"),
                                    crs = st_crs(2961)) # CRS should match GIS layer you are trying to compare

#################################################################################################################
## Agricultural data
### geo-database of agriculture land polygons in Nova Scotia
Ag_land <- st_read("ALIP_FINAL.gdb")

### Identity of the closest agricultural land to each of the locations
nearest_ag <- st_nearest_feature(Shape_file_conflict_data, Ag_land) 
nearest_ag<-st_transform(nearest_ag, crs = 2961) # Re-project layer to the same as the location data

### Distance between the location and the nearest agricultural land
#used st_zm() to make data format of coordinates in location and agriculture data comparable 
dist_ag<-st_distance(Shape_file_conflict_data, st_zm(Ag_land)[nearest_ag,], by_element = TRUE) 

### Add agriculture land distances to the original data set
### Used as. numeric to remove meter from the data column
### Divided by 1000 to convert to km
Conflict_data$Agr_Dist<-as.numeric(dist_agri/1000)

#################################################################################################################

## Distance between the location and the nearest surface water
### Geo-database of surface water feature lines in Nova Scotia
Water <- st_read("BASE_Water.gdb", layer = "WA_LINE_10K")
Water<-st_transform(Water, crs = 2961) # Re-project layer to the same as the location data

### Identity of the closest surface water feature to each of the locations
nearest_water <- st_nearest_feature(Shape_file_conflict_data, Water) 

### Distance between the location and the nearest surface water
dist_water<-st_distance(Shape_file_conflict_data, st_zm(Water)[nearest_water,], by_element = TRUE) 

### Add surface water distances to the original data set
### Used as. numeric to remove meter from the data column
### Divided by 1000 to convert to km
Conflict_data$water_Dist<-as.numeric(dist_water/1000)

#################################################################################################################

## Distance between the location and the nearest road
### Geo-database of roads, trails, rail roads lines in Nova Scotia
Road_trail_rail <- st_read("BASE_Road.gdb", layer = "RR_ROAD_LINE_10K")
Road_trail_rail$FEAT_DESC<-factor(Road_trail_rail$FEAT_DESC)

# Roads only
Road<-Road_trail_rail %>%
  filter(str_detect(str_to_lower(FEAT_DESC), "^road")) 
Road<-st_transform(Road, crs = 2961) # Re-project layer to the same as the location data

### Identity of the closest road to each of the locations
nearest_road <- st_nearest_feature(Shape_file_conflict_data, Road) 

### Distance between the location and the nearest road
dist_road<-st_distance(Shape_file_conflict_data, st_zm(Road)[nearest_road,], by_element = TRUE)

### Add aroad distances to the original data set
### Used as. numeric to remove meter from the data column
### Divided by 1000 to convert to km
Conflict_data$dist_road<-as.numeric(dist_road/1000)

#################################################################################################################

## Distance between the location and the nearest forest

### Geo-database of forest inventory polygons for each county
Inventory_Annapolis <- st_read("Forest_inventory/Annapolis/forest.shp")
Inventory_Antigonish <- st_read("Forest_inventory/Antigonish/forest.shp")
Inventory_Cape_Breton <- st_read("Forest_inventory/Cape_Breton/forest.shp")
Inventory_Colchester <- st_read("Forest_inventory/Colchester/forest.shp")
Inventory_Cumberland <- st_read("Forest_inventory/Cumberland/forest.shp")
Inventory_Digby <- st_read("Forest_inventory/Digby/forest.shp")
Inventory_Guysborough_east <- st_read("Forest_inventory/Guysborough_east/forest.shp")
Inventory_Guysborough_west <- st_read("Forest_inventory/Guysborough_west/forest.shp")
Inventory_Halifax_east <- st_read("Forest_inventory/Halifax_east/forest.shp")
Inventory_Halifax_west <- st_read("Forest_inventory/Halifax_west/forest.shp")
Inventory_Hants <- st_read("Forest_inventory/Hants/forest.shp")
Inventory_Inverness <- st_read("Forest_inventory/Inverness/forest.shp")
Inventory_Kings <- st_read("Forest_inventory/Kings/forest.shp")
Inventory_Lunnenburg <- st_read("Forest_inventory/Lunnenburg/forest.shp")
Inventory_pictou <- st_read("Forest_inventory/Pictou/forest.shp")
Inventory_Queens <- st_read("Forest_inventory/Queens/forest.shp")
Inventory_Richmond <- st_read("Forest_inventory/Richmond/forest.shp")
Inventory_Shelburne <- st_read("Forest_inventory/Shelburne/forest.shp")
Inventory_Victoria<- st_read("Forest_inventory/Victoria/forest.shp")
Inventory_Yarmouth<- st_read("Forest_inventory/Yarmouth/forest.shp")

### Making sure they all have the same crs
Inventory_Annapolis<-st_transform(Inventory_Annapolis, crs = 2961)
Inventory_Antigonish<-st_transform(Inventory_Antigonish, crs = 2961)
Inventory_Cape_Breton<-st_transform(Inventory_Cape_Breton, crs = 2961)
Inventory_Colchester<-st_transform(Inventory_Colchester, crs = 2961)
Inventory_Cumberland<-st_transform(Inventory_Cumberland, crs = 2961)
Inventory_Digby<-st_transform(Inventory_Digby, crs = 2961)
Inventory_Guysborough_east<-st_transform(Inventory_Guysborough_east, crs = 2961)
Inventory_Guysborough_west<-st_transform(Inventory_Guysborough_west, crs = 2961)
Inventory_Halifax_east<-st_transform(Inventory_Halifax_east, crs = 2961)
Inventory_Halifax_west<-st_transform(Inventory_Halifax_west, crs = 2961)
Inventory_Hants<-st_transform(Inventory_Hants, crs = 2961)
Inventory_Inverness<-st_transform(Inventory_Inverness, crs = 2961)
Inventory_Kings<-st_transform(Inventory_Kings, crs = 2961)
Inventory_Lunnenburg<-st_transform(Inventory_Lunnenburg, crs = 2961)
Inventory_pictou<-st_transform(Inventory_pictou, crs = 2961)
Inventory_Queens<-st_transform(Inventory_Queens, crs = 2961)
Inventory_Richmond<-st_transform(Inventory_Richmond, crs = 2961)
Inventory_Shelburne<-st_transform(Inventory_Shelburne, crs = 2961)
Inventory_Victoria<-st_transform(Inventory_Victoria, crs = 2961)
Inventory_Yarmouth<-st_transform(Inventory_Yarmouth, crs = 2961)

### Combining forest inventories for counties
Forest_inventory<- bind_rows(list(Inventory_Annapolis, Inventory_Antigonish,Inventory_Cape_Breton,
                                  Inventory_Colchester, Inventory_Cumberland, Inventory_Digby,
                                  Inventory_Guysborough_east, Inventory_Guysborough_west,
                                  Inventory_Halifax_east, Inventory_Halifax_west, Inventory_Hants,
                                  Inventory_Inverness, Inventory_Kings, Inventory_Lunnenburg,
                                  Inventory_pictou, Inventory_Queens, Inventory_Richmond, 
                                  Inventory_Shelburne, Inventory_Victoria, Inventory_Yarmouth))

### Cover type has 4 classes: 0 = non-forest, 2 = coniferous (> 75%), 5 = mixed wood, 8 = deciduous (>75%) 
Forest_inventory$Forest<-ifelse(Forest_inventory$COVER_TYPE != 0, 1, 0)

### Just forest
forest<-subset(Forest_inventory, Forest ==1)

### Identity of the closest forest to each of the locations
nearest_forest<- st_nearest_feature(Shape_file_conflict_data, forest) 

### Distance between the location and the nearest forest
dist_forest<-st_distance(Shape_file_conflict_data, st_zm(forest)[nearest_forest,], by_element = TRUE) 

### Add forest distances to the original data set
### Used as. numeric to remove meter from the data column
### Divided by 1000 to convert to km
Conflict_data$dist_forest<-as.numeric(dist_forest/1000) 

#################################################################################################################

## Forest edge within 1 km
### Geo-database of forest inventory polygons for each county
Inventory_Antigonish <- st_read("Forest_inventory/Antigonish/forest.shp")
Inventory_Colchester <- st_read("Forest_inventory/Colchester/forest.shp")
Inventory_Cumberland <- st_read("Forest_inventory/Cumberland/forest.shp")
Inventory_Guysborough_east <- st_read("Forest_inventory/Guysborough_east/forest.shp")
Inventory_Guysborough_west <- st_read("Forest_inventory/Guysborough_west/forest.shp")
Inventory_Halifax_east <- st_read("Forest_inventory/Halifax_east/forest.shp")
Inventory_Halifax_west <- st_read("Forest_inventory/Halifax_west/forest.shp")
Inventory_Hants <- st_read("Forest_inventory/Hants/forest.shp")
Inventory_pictou <- st_read("Forest_inventory/Pictou/forest.shp")

### Making sure they all have the same crs
Inventory_Antigonish<-st_transform(Inventory_Antigonish, crs = 2961)
Inventory_Colchester<-st_transform(Inventory_Colchester, crs = 2961)
Inventory_Cumberland<-st_transform(Inventory_Cumberland, crs = 2961)
Inventory_Guysborough_east<-st_transform(Inventory_Guysborough_east, crs = 2961)
Inventory_Guysborough_west<-st_transform(Inventory_Guysborough_west, crs = 2961)
Inventory_Halifax_east<-st_transform(Inventory_Halifax_east, crs = 2961)
Inventory_Halifax_west<-st_transform(Inventory_Halifax_west, crs = 2961)
Inventory_Hants<-st_transform(Inventory_Hants, crs = 2961)
Inventory_pictou<-st_transform(Inventory_pictou, crs = 2961)

Inventory_halifax<-st_transform(Inventory_halifax, crs = 2961)
Inventory_Colchester<-st_transform(Inventory_Colchester, crs = 2961)
Inventory_Cumberland<-st_transform(Inventory_Cumberland, crs = 2961)
Inventory_pictou<-st_transform(Inventory_pictou, crs = 2961)

crs(Inventory_Hants)

### Combining forest inventories for counties
Forest_inventory_central <- bind_rows(list(Inventory_Antigonish,Inventory_Colchester
                                           ,Inventory_Cumberland,Inventory_Guysborough_east,
                                           Inventory_Guysborough_west,Inventory_Halifax_east,
                                           Inventory_Halifax_west, Inventory_Hants, Inventory_pictou))

### Cover type has 4 classes: 0 = non-forest, 2 = coniferous (> 75%), 5 = mixed wood, 8 = deciduous (>75%) 
Forest_inventory_central$Forest<-ifelse(Forest_inventory_central$COVER_TYPE != 0, 1, 0)

### Just forest
forest<-subset(Forest_inventory_central, Forest ==1)

### Forest dissolved so that there is no forest type
forest_dissolve<-sf_dissolve(forest)

### Create 1 km buffer around each location
Conflict_buffer_1km<-st_buffer(Shape_file_conflict_data, 1000) # 1000 m = 1 km

### All forest types within 1 km of location
forest_1km<-st_intersection(Conflict_buffer_1km, forest)

### Calculating forest edge values
forest_edge_value<-sf::st_perimeter(forest_1km)

### Connecting location number to forest edge estimate 
forest_edge_location_polygons<-data.frame(ID = forest_1km$ID, Forest_edge=forest_edge_value)

### summing all the polygons for each site
forest_edge_location_sum<-forest_edge_location_polygons%>%
  group_by(ID)%>%
  summarise(forest_edge = sum(Forest_edge/1000)) # divided by 1000 to convert to km

### Adding housing density estimate to the original data set
Conflict_data2<-merge(Conflict_data, forest_edge_location_sum, by = "ID", all.x = T)

#################################################################################################################

## House density at locations
### Geo-database of housing density polygons in Nova Scotia
House_dense<-st_read("Housing_dense_NS.gdb")
House_dense<-st_transform(House_dense, crs = 2961) # Re-project layer to the same as the location data

### Getting housing density estimates for each location
Location_house_dense<-st_intersection(Shape_file_conflict_data, House_dense)

### Connecting location number to housing density estimate 
Location_house_dense2<-data.frame(ID = Location_house_dense$ID, House_dens=Location_house_dense$House_dens)

### Adding housing density estimate to the original data set
Conflict_data2<-merge(Conflict_data, Location_house_dense2, by = "ID", all.x = T)

#################################################################################################################
##################################################################################################################

## Human-bear conflict verses random locations

## Adding a 1/0 column to the data sets
Human_bear_conflict_locations$Event<-1
Random_locations$Event<-0

## combining data sets together
Conflict_random<-rbind(Human_bear_conflict_locations, Random_locations)

## run model
Conflict_model<-gamm4(Event~s(UTME,UTMN)+log(Road_Dist+0.001)+ log(Water_Dist+0.001)
                +poly(House_dens,2)+ log(Dist_Forest+0.001)+log(Forest_edge+0.001)
                +log(Dist_Farm+0.001),random = ~(1|Year)+(1|Month),
                data = Conflict_random, binomial())

## marginal plots (Distance to forests)
Conflict_plot<-ggpredict(Conflict_model,terms = "Dist_Forest[0:20]") # All distance plots show 0 to 10 km
ggplot(Conflict_plot,aes(x,predicted))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high),alpha =0.1)+
  coord_cartesian(ylim = c(0,1))+
  ylab("Likelihood")+ xlab("Distance (km)")+
  theme_bw()

## Predicted values for 1 km provincial grid
Conflict_predict<- predict(Conflict_model$gam, NS_Habitat_1km_grid, type = 'response') 

## turning output into a data frame and adding coordinates to predicted values
Conflict_predict_with_UTM<-data.frame(predict = Conflict_predict,
                                                   grid_size = NS_Habitat_1km_grid$Area,
                                                   Easting = NS_Habitat_1km_grid$UTME,
                                                   Northing = NS_Habitat_1km_grid$UTMN)

##################################################################################################################
### Human-bear non-conflict verses random locations

## Adding a 1/0 column to the data sets
Bear_non_conflict_locations$Event<-1
Random_locations$Event<-0

## combining data sets together
Non_conflict_random<-rbind(Bear_non_conflict_locations, Random_locations)

## run model
Non_conflict_model<-gamm4(Event~s(UTME,UTMN)+log(Road_Dist+0.001)+ log(Water_Dist+0.001)
                    +poly(House_dens,2)+ log(Dist_Forest+0.001)+log(Forest_edge+0.001)
                    +log(Dist_Farm+0.001),random = ~(1|Year)+(1|Month),
                    data = Non_conflict_random, binomial())

## marginal plots (Human density)
Non_Conflict_plot<-ggpredict(Non_conflict_model,terms = "House_dens[0:200]") # estimated from 0 to 200 houses per km?2
ggplot(Non_Conflict_plot,aes(x,predicted))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high),alpha =0.1)+
  coord_cartesian(ylim = c(0,1))+
  ylab("Likelihood")+ xlab("Density (houses/ km^2)")+
  theme_bw()

## Predicted values for 1 km provincial grid
Non_conflict_predict<- predict(Non_conflict_model$gam, NS_Habitat_1km_grid, type = 'response') 

## turning output into a data frame and adding coordinates to predicted values
Non_conflict_predict_with_UTM<-data.frame(predict = Non_conflict_predict,
                                                   grid_size = NS_Habitat_1km_grid$Area,
                                                   Easting = NS_Habitat_1km_grid$UTME,
                                                   Northing = NS_Habitat_1km_grid$UTMN)

##################################################################################################################
### Human-bear conflict verses Human-bear non-conflict 

## Adding a 1/0 column to the data sets
Human_bear_conflict_locations$Event<-1
Bear_non_conflict_locations$Event<-0

## combining data sets together
Conflict_Non_conflict<-rbind(Human_bear_conflict_locations,Bear_non_conflict_locations)

## run model
Interaction_model<-gamm4(Event~s(UTME,UTMN)+log(Road_Dist+0.001)+log(Water_Dist+0.001)
                         +log(House_dens+0.001)+log(Dist_Forest+0.001)
                         +log(Forest_edge +0.001)+log(Dist_Farm+0.001), 
                         random = ~(1|Year) + (1|Month),data = Conflict_Non_conflict, 
                         family = binomial())

## marginal plots (Distance to agricultural land)
Interaction_plot<-ggpredict(Interaction_model,terms = "Dist_Farm[0:20]") # All distance plots show 0 to 10 km
ggplot(Interaction_plot,aes(x,predicted))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high),alpha =0.1)+
  coord_cartesian(ylim = c(0,1))+
  ylab("Likelihood")+ xlab("Distance (km)")+
  theme_bw()
