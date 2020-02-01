library(tidyverse)
library(prioritizr)
library(rgdal)
library(here)
library(doParallel)
library(raster)

#get country specific target value
count_tar <- function(PU = NULL, target = NULL){
  round(cellStats(PU,"sum") / 100 * target, 0)
}


setwd("data")

Biodiversity <- raster("Biodiversity Intactness Index.tif") #Reads raster layers
import_hidrica<- raster("Water Importance.tif")
carbon <- raster("Geo Carbon.tif") / 1000000
organic_s <- raster("Organic Soil.tif") / 1000000
water_q<- raster("Quality of the Residual Surface Water.tif") / 10000
agricutlrure <- raster("Agriculture Suitability.tif")
Mangroves<- raster("Mangroves.tif")
wetlands<-raster("Ramsar Site (proposed) - Wetland.tif")
bio_corridors <- raster("Biological Corridors.tif")

feat_stack <- stack(Biodiversity,
                    import_hidrica,
                    carbon,
                    organic_s,
                    water_q,
                    agricutlrure,
                    Mangroves, 
                    wetlands,
                    bio_corridors)

#Possible cost layer:
HFP <- raster("Human Footprint.tif")

#Protected areas
PA<-raster("Protected Areas.tif")
forest_reserv<-raster("Forest Reserve.tif")
T_indigena<- raster("Indigenous Territories.tif")


# pu
pu0 <- raster("Planning_units_buffer.tif")

# HFP < 14 ~ 50% of country
HFP_in <- HFP < 14
HFP_in[HFP_in < 1] <- NA

HFP_res <- HFP >= 14 & HFP < 20
HFP_res[HFP_res < 1] <- NA

HFP_mg<-HFP<20
HFP_mg[HFP_mg < 1] <- NA

HFP_BAU<-HFP>=20
HFP_BAU[HFP_BAU < 1] <- NA

pu1 <- stack(HFP_in, HFP_res, HFP_mg, HFP_BAU)
names(pu1) <- c("Protect", "Restore", "Manage", "BAU")


# Impacts setup 
zone_1_impacts <- c(1, 1, 1, 1, 1, 0, 1, 1, 1)
zone_2_impacts <- c(2, 1.5, 1.5, 1, 1.5, 0, 1, 1.5, 2)
zone_3_impacts <- c(0.5, 0.5, 0.5, 0.75, 1.5, 1, 0, 0.5, 0.5)
zone_4_impacts <- c(0, 0, 0, 0, 0, 1, 0, 0, 0)

# wgts <- matrix(1, ncol = 4, nrow = nlayers(feat_stack), dimnames = list(c(names(feat_stack)), c(names(pu1))))
wgts <- data.frame(weight = rep(1, nlayers(feat_stack)),
                   row.names = names(feat_stack))

impacts <-data.frame(Protect = zone_1_impacts, 
                     Restore = zone_2_impacts, 
                     Manage = zone_3_impacts, 
                     BAU = zone_4_impacts,
                     row.names = names(feat_stack))


#features
zn1 <- feat_stack * impacts[,"Protect"]
zn2 <- feat_stack * impacts[,"Restore"] 
zn3 <- feat_stack * impacts[,"Manage"] 
zn4 <- feat_stack * impacts[,"BAU"] 

### Create Zone file
zns <- zones("Protect" = zn1, "Restore" = zn2,  "Manage" = zn3, "BAU" = zn4,
             feature_names = names(zn1))

# not used for now
# w1 <- matrix(0, ncol = nlayers(pu1), nrow = nlayers(zn1))                     
# w1[c(2,3,4,5,7),] <- 1

save.image(here("pre_global.RData"))



### Create Zone file
z2 <- zones("zone_1" = zn1, "zone_2" = zn2,  "zone_3" = zn3, "zone_4" = zn4)

p1 <- problem(pu1, zns) %>%
  add_max_utility_objective(c(count_tar(pu0, 20), 
                              count_tar(pu0, 5), 
                              count_tar(pu0, 10), 
                              count_tar(pu0, 65))) %>%
  add_gurobi_solver(gap = 0)

s1 <- solve(p1, force=TRUE)
setMinMax(s1)
plot(category_layer(s1), main="global")


p2 <- problem(pu1, zns) %>%
  add_max_utility_objective(c(count_tar(pu0, 20), 
                              count_tar(pu0, 5), 
                              count_tar(pu0, 10), 
                              count_tar(pu0, 65))) %>%
  add_rsymphony_solver()

s2 <- solve(p2, force=TRUE)
setMinMax(s2)
plot(category_layer(s2), main="global")







model <- list(
  obj = tt$obj(),
  mat = tt$A(),
  dir = tt$sense(),
  rhs = tt$rhs(),
  types = tt$vtype(),
  bounds = list(lower = list(ind = seq_along(tt$lb()), val = tt$lb()),
                upper = list(ind = seq_along(tt$ub()), val = tt$ub())),
  max = isTRUE(tt$modelsense() == "max"))
model$dir <- replace(model$dir, model$dir == "=", "==")
model$types <- replace(model$types, model$types == "S", "C")

gap = 0.1
time_limit = -1
first_feasible = 0
verbose = TRUE
parameters = parameters(
  numeric_parameter("gap", gap, lower_limit = 0),
  integer_parameter("time_limit", time_limit, lower_limit = -1,
                    upper_limit = .Machine$integer.max),
  binary_parameter("first_feasible", first_feasible),
  binary_parameter("verbose", verbose))

p <- as.list(parameters)

p <- list()
p$verbosity <- -1
p <- p[names(p) != "verbose"]
names(p)[which(names(p) == "gap")] <- "gap_limit"
p$first_feasible <- as.logical(p$first_feasible)


x <- do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))

x$solution[x$solution > 1]
