# Data using in "Spatial predictors of human-black bear (Ursus americanus) interactions in Nova Scotia, Canada" 
## By: Jason I. Airst and Tricia B. Fleming

## All the data collect from Nova Scotia's Biodiversity Investigation Reporting system can be found in the following files:
### 1) Human-bear conflict event locations can be found in Human_bear_conflict_locations.csv
### 2) Human-bear non-conflict event locations can be found in Bear_non_conflict_locations.csv
### 3) Random locations that conflict and non-conflict events were compared to can be found in Random_locations.csv

## The geo-database found in the NS_house_dense.zip folder was used to determine the housing density at event locations. This geo-database was generated using Access Nova Scotia's online property identification system https://novascotia.ca/sns/access/land.asp and was gathered in March 2020.

## The geo-database found in the NS_ALIP_2019.zip folder is based on the 2010 Agricultural Land Identification Project. https://www.novascotia.ca/agri/documents/business-research/AL1000%20Nova%20Scotia.pdf and was last updated in March 2019. 

## the shapefiles used to generate the provincial forest inventory can  be  accessed from the Nova Scotia Forest Inventory website https://novascotia.ca/natr/forestry/gis/forest-inventory.asp. This provides a shapefile for each county. A geo-database that covers the entire province can be found on the Nova Scotia Geodata website https://nsgi.novascotia.ca/gdd/ but be caution against using this data set as it does not process properly without removing several erronious data records.

### Both Distances to forests and forest edge within 1 km were calculated based on this Forest Inventory. 

## The road and surface water feature layer used in modelling came from the Nova Scotia Geodata website https://nsgi.novascotia.ca/gdd/
### Distances from an event to the nearest road was calculated based on the RR_ROAD_LINE_10K layer within the Nova Scotia Topographic Database - Roads, Trails and Rails layer. Only data on roads was used 
### Distances from an event to the nearest surface water feature was calculated based on  the WA_LINE_10K layer within the Nova Scotia Topographic Database - Water Features

## Examples of code used in this analysis can be found in the R file R_code_for_spatial_predictors_of_human_balck_bear_interactions.R
