library(raster)
library(rgdal)
library(lattice)
library(gstat)
library(ranger)
library(ggplot2)
library(tibble)

setwd("U:\\Time Series Analysis\\s7")
datapath = '.\\Botswana'

data = raster(paste(datapath,"Botswana_Elevation_resample.tif"))

x = terrain(data, opt=c('slope', 'aspect','roughness','flowdin')) ## not finished