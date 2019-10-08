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
#install.packages("here")
library("here")

library(doParallel)

###################################################################
#sTEP TWO: Set work directory, which should be where the data is saved
## Set work directory and import data
setwd("input data prepared/")

datfolder <- here("rasters_species_top100")
raster_files <- list.files(datfolder, pattern=".tif$", 
                           all.files=TRUE, full.names=TRUE)
raster_files

#import all raster files in folder using lapply

allrasters <- stack(raster_files)

#to check the index numbers of all imported raster list elements
allrasters

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
carbon <- raster("Geo Carbon.tif") / 1000000
organic_s <- raster("Organic Soil.tif") / 1000000
water_q<- raster("Quality of the Residual Surface Water.tif") / 10000
agricutlrure <- raster("Agriculture Suitability.tif")
#mangrove<- raster("C:/Users/Jenny/Dropbox/ELSA pilot - Costa Rica/input data original/hamilton_mangroves2014_cri.tif")
Mangroves<- raster("Mangroves.tif")
wetlands<-raster("Ramsar Site (proposed) - Wetland.tif")
bio_corridors <- raster("Biological Corridors.tif")

ESs<-stack(import_hidrica,carbon,organic_s,water_q,Mangroves)
bio<- stack(wetlands,species,bio_corridors)

#Possible cost layer:
HFP <- raster("Human Footprint.tif")

#Important features and protected lands

PA<-raster("Protected Areas.tif")
# bio_corridors<-raster("Biological Corridors.tif")
forest_reserv<-raster("Forest Reserve.tif")
T_indigena<- raster("Indigenous Territories.tif")
# costa_r<-readOGR("C:/Users/Jenny/Dropbox/ELSA pilot - Costa Rica/input data original/costa_rica_bound/costa_rica.shp") #Reads shapefiles


#Add planning units
PU<-raster("Planning_units_buffer.tif")

## Preparing layers for each zone



###Zone 1: only need to change agriculture value
agricutlrure1 <- agricutlrure*0

###Zone 2: 

Biodiversity2 <-Biodiversity*2
PA2<-PA*2
T_indigena2<-T_indigena*2
bio_corridors2<-bio_corridors*2
forest_reserv2<-forest_reserv*2
import_hidrica2<- import_hidrica*1.5
carbon2 <- carbon*1.5
water_q2<- water_q*1.5
wetlands2<-wetlands*1.5

###Zone 3:

Biodiversity3 <-Biodiversity*0.5
PA3<-PA*0.5
T_indigena3<-T_indigena*0.5
bio_corridors3<-bio_corridors*0.5
forest_reserv3<-forest_reserv*0.5
wetlands3<-wetlands*0.5
import_hidrica3<- import_hidrica*0.5
carbon3<-carbon*0.5
organic_s3 <- organic_s*0.75
water_q3<- water_q*0.75
Mangroves3<- Mangroves*0
species3<-species*0.5


#Setting up the cost layer: We can either set a cost as land, so prioritizR
#will try to get the most of each feature without surpassing the budget.
#or we can use human foot print. so the cost of creating a PA is 
# the human pressure.

#cost<-HFP #If we want to avoid areas with high human prossure
PU<-(PU/PU) #In this case, cost is the same across the landscape
#Create zonal layers
# HFP < 14 ~ 50% of country

HFP_in <- HFP < 14
plot(HFP_in,add=TRUE)
HFP_res <- HFP >= 14 & HFP < 20
plot(HFP_res)
HFP_mg<-HFP<20
plot(HFP_mg)
HFP_BAU<-HFP>=20
plot(HFP_BAU)

cellStats(PU,"sum")

#get country specific target value
count_tar <- function(target = NULL){
  round(cellStats(PU,"sum") / 100 * target, 0)
}


##Create planning units stack with the cost layers

pu1 <- stack(PU, PU,PU,PU)
names(pu1) <- c("zone_1", "zone_2", "zone_3", "zone_4")

##I included in each zone, all three zone layers (low, medium and high HFP)

zn1<-stack(Biodiversity,import_hidrica,carbon ,organic_s ,
           water_q,agricutlrure1 ,Mangroves, wetlands,bio_corridors,
           # PA,forest_reserv,T_indigena,
           HFP_in,HFP_res*0,HFP_mg*0,HFP_BAU*0)
zn2<-stack(Biodiversity2,import_hidrica2,carbon2 ,organic_s ,
           water_q2,agricutlrure1 ,Mangroves, wetlands2,bio_corridors2,
           # PA2,forest_reserv2,T_indigena2,
           HFP_in*0,HFP_res,HFP_mg*0,HFP_BAU*0)
zn3<-stack(Biodiversity3,import_hidrica3,carbon3 ,organic_s3 ,
           water_q3,agricutlrure ,Mangroves3, wetlands3,bio_corridors3,
           # PA3,forest_reserv3,T_indigena3,
           HFP_in*0,HFP_res*0,HFP_mg,HFP_BAU*0)
zn4<-stack(Biodiversity3*0,import_hidrica3*0,carbon3*0 ,organic_s3*0 ,
           water_q3*0,agricutlrure ,Mangroves3*0, wetlands3*0,bio_corridors3*0,
           # PA3*0,forest_reserv3*0,T_indigena3*0,
           HFP_in*0,HFP_res*0,HFP_mg,HFP_BAU)

### Create Zone file

z2 <- zones("zone_1" = zn1, "zone_2" = zn2,  "zone_3" = zn3, "zone_4" = zn4)


## Setting overall targets:
# 
t4 <- tibble::tibble(feature = names(zn1),
                     zone = list(names(pu1))[rep(1, 13)],
                     target = c(rep(0.2, 9), 0.3, 0.2, 0.2,0.3),
                     type = rep("relative", 13))

t4
# ##Problem
# 
# p4 <- problem(pu1, zones("zone_1" = zn1, "zone_2" = zn2, 
#                          "zone_3" = zn3,"zone_4" = zn4,
#                          feature_names = names(zn1))) %>%
#   add_min_set_objective() %>%
#   add_manual_targets(t4) %>%
#   add_binary_decisions()
# 
# s<-solve(p4, force=TRUE) # It was giving me a message saying: Warning in presolve_check.OptimizationProblem(compile(x)) :
#                          #features target values are (relatively) very high
# 
# plot(category_layer(s), main="solution")
# fr <- feature_representation(p4, s)
# fr[15:35,]


## Matrix for clumping rules.
# print stack
# print(pu1)
# plot(pu1)
# 
# z6 <- diag(4)
# z6[1, 2] <- 1
# z6[2, 1] <- 1
# z6[2, 3] <- 1
# z6[3, 2] <- 1
# 
# z6
# colnames(z6) <- c("zone_1", "zone_2", "zone_3", "zone_4")
# rownames(z6) <- colnames(z6)

# parallelization
n_cores <- 12
cl <- makeCluster(n_cores)
registerDoParallel(cl)

p1 <-   problem(pu1, zones("zone_1" = zn1, "zone_2" = zn2, 
                           "zone_3" = zn3,"zone_4" = zn4,
                           feature_names = names(zn1))) %>%
  add_min_set_objective() %>%
  add_manual_targets(t4) %>%
  add_binary_decisions()%>%
  # add_proportion_decisions %>%
  # add_boundary_penalties(penalty = 0.0000001)%>%
  add_gurobi_solver(threads = n_cores)

s1 <- solve(p1, force=TRUE)
setMinMax(s1)
plot(category_layer(s1), main="solution")

fr <- feature_representation(p1, s1)

fr[50:64,]

####################################################################
### Loop to run multiple scenarios of feature representation where
### zones representation of low, medium and high HFP is the same
z2<-zones("zone_1" = zn1, "zone_2" = zn2, 
      "zone_3" = zn3,"zone_4" = zn4,
      feature_names = names(zn1))

result_list <- list()
for(ii in 1:100){
  
  t4 <- tibble::tibble(feature = names(zn1),
                       zone = list(names(pu1))[rep(1, 16)],
                       target = c(rep((0.01*ii), 12), 0.3, 0.2, 0.2,0.3),
                       type = rep("relative", 16))
  
  p2<- problem(pu1, z2) %>%
        add_min_set_objective() %>%
        add_manual_targets(t4) %>%
        add_binary_decisions()%>%
        add_boundary_penalties(0.0001)%>%
        add_gurobi_solver(gap=0.1)
                                 
  s2 <- solve(p2,force=TRUE)
  
  result_list[[ii]] <- s2 
  fr <- feature_representation(p2,s2) 
  pu<- cellStats(s2, "sum") 
  
  if(ii == 1){
    feat_rep_rel <- data.frame(rbind(c(fr$relative_held)))
    feat_rep_abs <- data.frame(rbind(c(fr$absolute_held)))
    planning_u <- data.frame(rbind(c(pu)))
    names(feat_rep_rel) <- fr$feature
    names(feat_rep_abs) <- fr$feature
    names(planning_u)<- c("PU")
  } else {
    feat_rep_rel[ii,] <- rbind(c(fr$relative_held))
    feat_rep_abs[ii,] <- rbind(c(fr$absolute_held))
    planning_u[ii,] <- rbind(c(pu))
  }
  plot(category(s2), main=(paste("Scenario",ii))) 
  print(paste("End of scenario",ii))
  rm(p2, s2, fr,pu)
  
  
}

feat_rep_abs$scenario<- c(1:(nrow(feat_rep_abs)))
feat_rep_rel$scenario<- c(1:(nrow(feat_rep_abs)))
planning_u$scenario<- c(1:(nrow(feat_rep_abs)))

CR_Toff<- full_join(feat_rep_rel,feat_rep_abs,by="scenario")
CR_Toff<- full_join(CR_Toff,planning_u,by="scenario")

write.csv(CR_Toff,"CR_Toff.csv")
CR_Toff

CR_Toff_stack <- stack(result_list)
plot(category_layer(CR_Toff_stack[[1:4]]))

writeRaster(CR_Toff_stack,"CR_Toff_stack.tif", overwrite=TRUE)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                         
# Max utility
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                         


#cost<-HFP #If we want to avoid areas with high human prossure
PU<-(PU/PU) #In this case, cost is the same across the landscape
#Create zonal layers

# HFP < 14 ~ 50% of country
HFP_in <- HFP < 14
HFP_in[HFP_in < 1] <- NA
plot(HFP_in)
HFP_res <- HFP >= 14 & HFP < 20
HFP_res[HFP_res < 1] <- NA
plot(HFP_res)
HFP_mg<-HFP<20
HFP_mg[HFP_mg < 1] <- NA
plot(HFP_mg)
HFP_BAU<-HFP>=20
HFP_BAU[HFP_BAU < 1] <- NA
plot(HFP_BAU)

cellStats(PU,"sum")

#get country specific target value
count_tar <- function(target = NULL){
  round(cellStats(PU,"sum") / 100 * target, 0)
}


##Create planning units stack with the cost layers

pu1 <- stack(HFP_in, HFP_res, HFP_mg, HFP_BAU)
names(pu1) <- c("zone_1", "zone_2", "zone_3", "zone_4")

##I included in each zone, all three zone layers (low, medium and high HFP)

zn1<-stack(Biodiversity,
           import_hidrica,
           carbon,
           organic_s,
           water_q,
           agricutlrure1,
           Mangroves, 
           wetlands,
           bio_corridors)

zn2<-stack(Biodiversity2,
           import_hidrica2,
           carbon2,
           organic_s,
           water_q2,
           agricutlrure1,
           Mangroves, 
           wetlands2,
           bio_corridors2)

zn3<-stack(Biodiversity3,
           import_hidrica3,
           carbon3,
           organic_s3,
           water_q3,
           agricutlrure,
           Mangroves3, 
           wetlands3,
           bio_corridors3)

zn4<-stack(Biodiversity3 * 0,
           import_hidrica3 * 0,
           carbon3 * 0,
           organic_s3 * 0,
           water_q3 * 0,
           agricutlrure,
           Mangroves3 * 0, 
           wetlands3 * 0,
           bio_corridors3 * 0)


### Create Zone file
z2 <- zones("zone_1" = zn1, "zone_2" = zn2,  "zone_3" = zn3, "zone_4" = zn4)

p1 <-   problem(pu1, zones("zone_1" = zn1, "zone_2" = zn2, 
                           "zone_3" = zn3,"zone_4" = zn4,
                           feature_names = names(zn1))) %>%
  add_max_utility_objective(c(count_tar(20), count_tar(5), count_tar(10), 1)) %>%
  add_gurobi_solver(gap = 0, threads = n_cores)

s1 <- solve(p1, force=TRUE)
setMinMax(s1)
plot(category_layer(s1), main="solution")

                         

# clean up
stopCluster(cl)