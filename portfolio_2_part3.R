###################
#### PART III ####
#################

### Perform a regression analysis to find statistical relationships of the multiple F-value
### from the DLM and possible explanatory variables

library(raster)
library(rgdal)
library(ranger)
library(magrittr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(flextable)
# library(lattice)
# library(gstat)
#library(dplyr)
# library(sp)
# library(sf)

setwd("/home/maxim/Documents/coursework/time-series-analysis/data_p2/")

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


grid_plot=grid.arrange(
  spplot(grids$ET, main = "Evapotranspiration"), 
  spplot(grids$aridity, main = "Aridity" ),
  spplot(grids$LST, main = "Mean Surface Temperature" ),
  spplot(grids$twi, main = "Topographic Wetness Index" ),
  spplot(grids$achan, main = "Vertical Distance from Channel Network" ),
  ncol=2,nrow=3)

ggsave("../portfolio2/spplots.png", plot = grid_plot, width = 10, height = 8)

## Build a training and validation data set based on 50.000 randomly selected pixels
## (only complete cases, 50% each)

## (From Part II)
# import trend results, provided on stud.IP (p-value for F-statistics: "F.tif")
#Fvalue <- raster("F.tif")
#Fvalue[Fvalue == -999] <- NA
#grids$Fstat <- as(Fvalue, "SpatialGridDataFrame")
## ----------------------------------------------

## Import pseudo p-value for lag 3
Pvalue <- raster(" 	peudoPvalue_lag3.tif") # for part III
Pvalue[Pvalue == -999] <- NA
grids$pval <- as(Pvalue, "SpatialGridDataFrame")

# create sample for random pixels
set.seed(100)                                 # set seed for re
#x <- sample(1:(dim(Fvalue)[1]*dim(Fvalue)[2]), 50000)



#x <- sample(1:(dim(Fvalue)[1]*dim(Fvalue)[2]), 40000)     

 # for part III - boolean mask is needed, because of classification, siglag3 ....
sig <- as.data.frame(raster(grids$pval))
sig[abs(sig) <= 0.05] <- 0 # significant
sig[abs(sig) > 0.05] <- 1  # non significant



  #Fval <- as.data.frame(raster(grids$Fstat))
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


# create master data frame with all data
  
master = as.data.frame(cbind(sig[,1],slope[,1], aspect[,1], flowdir[,1], aridity[,1], ET[,1], LST[,1], roughness[,1],
                             elev[,1], twi[,1], achan[,1], tri[,1], tpi[,1]))
names(master) <- c("sig","slope","aspect","flowdir","aridity","ET","LST", "Roughness","elev","twi","achan","tri", "tpi")

# throw out all NA values
master = master[complete.cases(master),]



# significance mask

sig0_indices = which(master['sig']==0)
sig1_indices = which(master['sig']==1)

sig0_sample = sample(length(sig0_indices),20000)
sig1_sample = sample(length(sig1_indices),20000)

x = c(sig0_sample,sig1_sample)


# create subset vor all characteristics
sub <- master[x,]


# divide subset into half-half training-testing
tmp <- sample(nrow(sub), 0.5 * nrow(sub))
train <- sub[tmp, ]
test <- sub[-tmp, ]



## Run a random forest regression model (ranger function from the ranger library!) for
## the training data set (dependent variable: multiple F-values from the DLM,
## explanatory variables: the formerly created raster stack)
rf <- ranger(as.factor(sig) ~ ., importance = "impurity",data = train,num.trees=1000)  # classification
pred <- predict(rf, data=test)
rf$prediction.error # should be between 0-1, 0: good, 1:bad
rf$confusion.matrix

## Regress the modelled against observed F-values based on the validation data set.
## Provide an interpretation of the results: can the outcomes of the DLM be linked with
## any environmental variables (analyze the variable importance)?
rf_imp <- as.data.frame(rf$variable.importance)
names(rf_imp) <- "importance"
rf_imp<- rf_imp %>% rownames_to_column("variable")

# X11()
prel <- ggplot(rf_imp, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
print(prel)
ggsave(filename = "../portfolio2/Variable_Importance_part2_step3_result2.png", plot = prel, width = 10, device = "png", dpi = 300)
# dev.print(png, "Variable_Importance_part2_step2_result", width=500)
# dev.off()

set_flextable_defaults(
  font.size = 10, #theme_fun = theme_vanilla,
  padding = 6,
)

ft_confm = flextable(rf$confusion.matrix)
save_as_image(ft_confm,path='../portfolio2/confmatrix.png')


