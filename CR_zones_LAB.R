#Utilization of PrioritizR for identification of Essential Life support
#areas (ELSAs) in Costa Rica
#ecosystem services (Biodiversity intactness, below and above-growth carbon,
#agriculture value, water quality)

###################################################################
#sTEP ONE: Importating all the packeges necessary for the analyses
## Packages for data entry and processing
#install.packages("tidyverse")
library(tidyverse)

# load prioritizr package
#install.packages("prioritizr")
library("prioritizr")
#install.packages("rgdal")
library("rgdal")

#Load sover
#install.packages("slam")
library("slam")
library("gurobi")

###################################################################
#sTEP TWO: Set work directory, which should be where the data is saved
## Set work directory and import data
setwd("C:/Users/Jenny/Dropbox/ELSA pilot - Costa Rica/input data prepared/")

datfolder <- "C:/Users/Jenny/Dropbox/ELSA pilot - Costa Rica/input data original/species/rasters_species_top100/"
raster_files <- list.files(datfolder, pattern=".tif", 
                           all.files=TRUE, full.names=TRUE)
raster_files

#import all raster files in folder using lapply
allrasters <- lapply(raster_files, raster)

#to check the index numbers of all imported raster list elements
allrasters[[1]]

#call single raster element
allrasters[[80]]

#to run a function on an individual raster e.g., plot 
plot(allrasters[[23]])
plot(allrasters[[24]])

#or

species <- stack(allrasters)
names(species)
###################################################################
#sTEP THREE: Importing the data. If the directory is correct, only the full name
#of the layer will suffice to call the raster layer.
#Import data with "raster" function, from the raster package.
#Ecosystem Services:
Biodiversity <- raster("Biodiversity Intactness Index.tif") #Reads raster layers
import_hidrica<- raster("Water Importance.tif")
carbon <- raster("Geo Carbon.tif")
organic_s <- raster("Organic Soil.tif")
water_q<- raster("Quality of the Residual Surface Water.tif")
agricutlrure <- raster("Agriculture Suitability.tif")
#mangrove<- raster("C:/Users/Jenny/Dropbox/ELSA pilot - Costa Rica/input data original/hamilton_mangroves2014_cri.tif")
Mangroves<- raster("Mangroves.tif")
wetlands<-raster("Ramsar Site (proposed) - Wetland.tif")

ESs<-stack(import_hidrica,carbon,organic_s,water_q,Mangroves)
bio<- stack(wetlands,species,bio_corridors)

#Possible cost layer:
HFP <- raster("Human Footprint.tif")

#Important features and protected lands

PA<-raster("Protected Areas.tif")
bio_corridors<-raster("Biological Corridors.tif")
forest_reserv<-raster("Forest Reserve.tif")
T_indigena<- raster("Indigenous Territories.tif")
costa_r<-readOGR("C:/Users/Jenny/Dropbox/ELSA pilot - Costa Rica/input data original/costa_rica_bound/costa_rica.shp") #Reads shapefiles


#Add planning units
PU<-raster("Planning_units_buffer.tif")

## Preparing layers for each zone
Biodiversity2 <-Biodiversity*2
Biodiversity3 <-Biodiversity*0.5
PA2<-PA*2
PA3<-PA*0.5
T_indigena2<-T_indigena*2
T_indigena3<-T_indigena*0.5
bio_corridors2<-bio_corridors*2
bio_corridors3<-bio_corridors*0.5
forest_reserv2<-forest_reserv*2
forest_reserv3<-forest_reserv*0.5
wetlands2<-wetlands*1.5
wetlands3<-wetlands*0.5
import_hidrica2<- import_hidrica*1.5
import_hidrica3<- import_hidrica*0.5
carbon2 <- carbon*1.5
carbon3<-carbon*0.5
organic_s3 <- organic_s*0.75
water_q2<- water_q*1.5
water_q3<- water_q*0.75
agricutlrure1 <- agricutlrure*0
Mangroves3<- Mangroves*0


#Setting up the cost layer: We can either set a cost as land, so prioritizR
#will try to get the most of each feature without surpassing the budget.
#or we can use human foot print. so the cost of creating a PA is 
# the human pressure.

#cost<-HFP #If we want to avoid areas with high human prossure
PU<-(PU/PU) #In this case, cost is the same across the landscape
#Create zonal layers
HFP_in<-HFP<4
plot(HFP_in)
HFP_res<-HFP>4&HFP<20
plot(HFP_res)
HFP_mg<-HFP>20
plot(HFP_mg)

cellStats(PU,"sum")

## Creating budgets based on the total value in each cost layer
budget<-c(53290,53290,53290)

##Create planning units stack with the cost layers

pu1 <- stack(PU, PU,PU)
zn1<-stack(Biodiversity,import_hidrica,carbon ,organic_s ,
           water_q,agricutlrure1 ,Mangroves, wetlands,bio_corridors,
           PA,forest_reserv,T_indigena,species,HFP_in)
zn2<-stack(Biodiversity2,import_hidrica2,carbon2 ,organic_s ,
           water_q2,agricutlrure1 ,Mangroves, wetlands2,bio_corridors2,
           PA2,forest_reserv2,T_indigena2,species,HFP_res)
zn3<-stack(Biodiversity3,import_hidrica3,carbon3 ,organic_s3 ,
           water_q3,agricutlrure ,Mangroves3, wetlands3,bio_corridors3,
           PA3,forest_reserv3,T_indigena3,species,HFP_mg)


### Create Zone file

z2 <- zones("zone 1" = zn1, "zone 2" = zn2,  "zone 3" = zn3)
# each cell value corresponds to the target 10%
t2 <- matrix(NA, ncol = 3, nrow = 113)
t2[1:113, 1] <- 0.1
t2[1:113, 2] <- 0.1
t2[1:113, 3] <- 0.1

t2[113,1]<-0.3
t2[113,2]<-0.1
t2[113,3]<-0.4
t2[113,]


## Matrix for clumping rules.
# print stack
print(pu1)
plot(pu1)

z6 <- diag(3)
z6[1, 2] <- 1
z6[2, 1] <- 1
z6[2, 3] <- 1
z6[3, 2] <- 1

z6
colnames(z6) <- c("zn1","zn2","zn3")
rownames(z6) <- colnames(z6)



p1 <-  problem(pu1, z2, run_checks=FALSE) %>%
  add_max_features_objective(budget) %>%
  add_relative_targets(t2) %>%
  add_binary_decisions()%>%
  add_boundary_penalties(penalty=0.00001, zones = z6)%>%
  add_gurobi_solver()

s1 <- solve(p1, run_checks=FALSE, force=TRUE)
setMinMax(s1)
plot(category_layer(s1), main="solution")

fr <- feature_representation(p1, s1)
