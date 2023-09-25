### Perform a regression analysis to find statistical relationships of the multiple F-value
### from the DLM and possible explanatory variables

library(raster)
# library(rgdal)
# library(lattice)
# library(gstat)
# library(ranger)
# library(ggplot2)
# library(tibble)

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



## Build a training and validation data set based on 50.000 randomly selected pixels
## (only complete cases, 50% each)

# import trend results, provided on stud.IP (p-value for F-statistics: "F.tif")
Fvalue <- raster("F.tif")
Fvalue[Fvalue == -999] <- NA
grids$Fstat <- as(Fvalue, "SpatialGridDataFrame")


