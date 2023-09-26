### Perform a regression analysis to find statistical relationships of the multiple F-value
### from the DLM and possible explanatory variables

library(raster)
library(rgdal)
# library(lattice)
# library(gstat)
# library(ranger)
# library(ggplot2)
# library(tibble)
library(sp)
library(sf)

setwd("~/Dokumente/uni/SoSe23/Pattern Recognition/UE8_portfolio_part2/data")



## Use the following possibly explanatory spatial variables using the terrain function
## from the raster library and the DEM: slope, aspect, roughness, flowdirection, terrain
## ruggedness index (TRI) and topographic position index (TPI)

# this file is the DEM
data = raster("Botswana_Elevation_resample.tif")
# extract spacial variables from DEM
x = terrain(data, opt=c('slope', 'aspect','roughness','flowdir','TRI','TPI')) 
# shows all 6 characteristics ...
plot(x)



## Combine the created raster data sets with the topographic wetness index (TWI),
## vertical distance from the channel network (ACHAN), mean evapotranspiration,
## mean aridity, mean surface temperature

# Create grid 
grids <- as(data, "SpatialGridDataFrame")
names(grids)[1] <- "elev"
slot(slot(grids, "grid"), "cellsize")[1] <- slot(slot(grids, "grid"), "cellsize")[2]
grids$elev <- as(data, "SpatialGridDataFrame")
grids$slope <- as(x$slope, "SpatialGridDataFrame")
grids$aspect <- as(x$aspect, "SpatialGridDataFrame")
grids$tri <- as(x$tri, "SpatialGridDataFrame")
grids$tpi <- as(x$tpi, "SpatialGridDataFrame")
grids$roughness <- as(x$roughness, "SpatialGridDataFrame")
grids$flowdir <- as(x$flowdir, "SpatialGridDataFrame")

# import additional layers:
grids$twi <- readGDAL("twi.asc")          # topographic wetness index (TWI)
grids$achan <- readGDAL("achan.asc")      # vertical distance from the channel network (ACHAN)

ET <- raster("meanET1.tif")               # mean evapotranspiration
ET[ET <= 0.1] <- NA
ET[ET > 10000] <- NA
tmp <- as(ET, "SpatialGridDataFrame")
names(tmp)[1] <- "ET"
grids$ET <- tmp

aridity <- raster("meanAridity1.tif")     # mean aridity
aridity[aridity <= 0.1] <- NA
tmp <- as(aridity, "SpatialGridDataFrame")
names(tmp)[1] <- "aridity"
grids$aridity <- tmp

LST <- raster("meanLST1.tif")             # mean surface temperature
LST[LST <= 0.1] <- NA
tmp <- as(LST, "SpatialGridDataFrame")
names(tmp)[1] <- "LST"
grids$LST <-tmp

# # visualisation
# spplot(grids$ET)



## Build a training and validation data set based on 50.000 randomly selected pixels
## (only complete cases, 50% each)

# import trend results, provided on stud.IP (p-value for F-statistics: "F.tif")
Fvalue <- raster("F.tif")
Fvalue[Fvalue == -999] <- NA
grids$Fstat <- as(Fvalue, "SpatialGridDataFrame")
# Pvalue <- raster("siglag3.tif") # for part III

# create sample for random pixels
set.seed(100)                                 # set seed for re
x <- sample(1:(dim(Fvalue)[1]*dim(Fvalue)[2]), 50000)     
# # for part III - boolean mask is needed, because of classification, siglag3 ....
# sig <- as.data.frame(raster(grids$P))
# sig[sig <= 0.05] <- 0 # significant
# sig[sig > 0.05] <- 1  # non significant


# grids --> data.frame
  Fval <- as.data.frame(raster(grids$Fstat))
  slope <- as.data.frame(raster(grids$slope))
  elev <- as.data.frame(raster(grids$elev))
  twi <- as.data.frame(raster(grids$twi))
  achan <- as.data.frame(raster(grids$achan))
  aspect <- as.data.frame(raster(grids$aspect))
  LST <- as.data.frame(raster(grids$LST))
  ET <- as.data.frame(raster(grids$ET))
  flowdir <- as.data.frame(raster(grids$flowdir))
  tri <- as.data.frame(raster(grids$tri))
  tpi <- as.data.frame(raster(grids$tpi))
  roughness <- as.data.frame(raster(grids$roughness))
  aridity <- as.data.frame(raster(grids$aridity))

  
# create subset vor all characteristics
sub <- as.data.frame(cbind(Fval[x,1],slope[x,1], aspect[x,1], flowdir[x,1], aridity[x,1], ET[x,1], LST[x,1], roughness[x,1],
                           elev[x,1], twi[x,1], achan[x,1], tri[x,1], tpi[x,1]))
names(sub) <- c("Fval","slope","aspect","flowdir","aridity","ET","LST", "Roughness","elev","twi","achan","tri", "tpi")
sub <- sub[complete.cases(sub),]        # throwing out all NA values


# divide subset into half-half training-testing
tmp <- sample(nrow(sub), 0.5 * nrow(sub))
train <- sub[tmp, ]
test <- sub[-tmp, ]



## Run a random forest regression model (ranger function from the ranger library!) for
## the training data set (dependent variable: multiple F-values from the DLM,
## explanatory variables: the formerly created raster stack)

