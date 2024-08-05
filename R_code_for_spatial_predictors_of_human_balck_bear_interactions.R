#### R packages ####
library(gamm4)
library(sf)
library(ggplot2)
library(ggeffects)

#### set working directory
setwd("")

#### Data from Biodiversity Investigation Reporting system ####
Random_locations <- read.csv("Random_locations.csv")
Human_bear_conflict_locations <- read.csv("Human_bear_conflict_locations.csv")
Bear_non_conflict_locations <- read.csv("Bear_non_conflict_locations.csv")

### habitat data for a 1 km grid of Nova Scotia
NS_Habitat_1km_grid <- read.csv("NS_Habitat_1km_grid.csv")

#################################################################################################################
### Example of how to Determine the distance between an event location and the nearest something 

## Event location data
Location_data<-data.frame(East=Random_locations$UTME, North = Random_locations$UTMN)

## Tell R to treat as a spatial data set
Shape_file_location_data<- st_as_sf(Location_data, coords = c("East", "North"),
                                    crs = st_crs(2961)) # CRS should match GIS layer you are trying to compare

## Shapefile/ spatial geo database we are comparing event locations to. In this case location of agriculture land
GIS_file <- st_read("ALIP_FINAL.gdb")

## Identity of the closest ag land to each of the locations
nearest <- st_nearest_feature(Shape_file_location_data, GIS_file) 

## Coordinates of the closest ag land to the locations
loc_join <- cbind( Shape_file_location_data, st_drop_geometry(GIS_file)[nearest,]) 

## Distance between the location and the nearest ag land
dist<-st_distance(Shape_file_location_data, st_zm(GIS_file)[nearest,], by_element = TRUE) 
#used st_zm() to make data format of coordinates in location and agriculture data compatable  

## Add this distances to the original data set
Location_data$Agr_Dist<-dist

##################################################################################################################
### Human-bear conflict verses random locations

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