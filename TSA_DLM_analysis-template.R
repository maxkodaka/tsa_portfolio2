library(raster)
library(rgdal)
library(lattice)
library(gstat)
library(ranger)
library(ggplot2)
library(tibble)
install.packages('pdp')
library(pdp)
library(gridExtra)

setwd("U:\\Time Series Analysis\\Botswana")
#datapath = '.\\Botswana'

# Load the data:

data <- raster("Botswana_Elevation_resample.tif")
#data = raster(paste(datapath,"Botswana_Elevation_resample.tif"))

x <- terrain(data, opt=c('slope', 'aspect','roughness', 'flowdir','TPI','TRI'), unit='degrees')

plot(x)

file.exists('achan.asc')

# Create grid 
grids <- as(data, "SpatialGridDataFrame")
names(grids)[1] <- "elev"
slot(slot(grids, "grid"), "cellsize")[1] <- slot(slot(grids, "grid"), "cellsize")[2]
## import addional layers
# Altitude above channel network in m (achan)
grids$achan <- readGDAL("achan.asc")
# Topographic Wetness Index (twi)
grids$twi <- readGDAL("twi.asc")


# convert elev into SpatialGridDataFrame

grids$elev <- as(data, "SpatialGridDataFrame")
...

## import addional layers: aridity, LST, ET

aridity <- raster("meanAridity1.tif")
aridity[aridity <= 0.1] <- NA
tmp <- as(aridity, "SpatialGridDataFrame")
names(tmp)[1] <- "aridity"
grids$aridity <-tmp

lst = raster('meanLST1.tif')
lst[lst<=0.1] = NA
tmp=as(lst, 'SpatialGridDataFrame')
names(tmp)[1]='LST'
grids$lst = tmp

et = raster('meanET1.tif')
et[et<=0.1] = NA
tmp=as(et, 'SpatialGridDataFrame')
names(tmp)[1]='ET'
grids$et = tmp

### import trend results
Fmult <- raster("F.tif") #Fixed from Fmult.tif
#Rmult <- raster("Rmult.tif")
Fmult[Fmult == -999] <- NA
tmp <- as(Fmult, "SpatialGridDataFrame")
grids$F <-tmp

## visualizsation

grid.arrange(spplot(grids$et),spplot(grids$twi),spplot(grids$aridity),spplot(grids$lst))

## modelling: ranger prediction, 5000 pixels in total, 2/3 training 1/3 test

x <- sample(1:(dim(Trend)[1]*dim(Trend)[2]), 5000) # Where does this 'Trend' come from?

...

rf <- ranger(sig ~ ., importance = "impurity",data = train)
pred <- predict(rf, data=test)

plot(test$sig,pred$predictions)
cor(test$sig,pred$predictions)

# variable importance
# direction of correlation
tmp <- cor(test[,-1], test$sig, use="complete.obs")

tmp[is.na(tmp)] <- 0

#rf$importance <- rf$variable.importance * sign(tmp)
#rf_imp <- as.data.frame(rf$variable.importance * sign(tmp))
rf_imp <- as.data.frame(rf$variable.importance)
names(rf_imp) <- "importance"
rf_imp<- rf_imp %>% rownames_to_column("variable")

windows()
p <- ggplot(rf_imp, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
print(p)

# ranger for classification

sig <- p
sig[sig <= 0.05] <- 0 # significant
sig[sig > 0.05] <- 1  # non significant
...

rf <- ranger(as.factor(sig) ~ ., importance = "impurity",data = train)
pred <- predict(rf, data=test)

# Accuracy assessment
table(test$sig,pred$predictions)
Acc <- caret::confusionMatrix(as.factor(as.character(pred$predictions)),as.factor(as.character(test$sig)), positive="0")



# Partial Dependence Plots
target_variable <- "Fmult"
predictor_variables <- setdiff(names(train), target_variable)
train_data <- train
# Initialize an empty dataframe to hold all the data for the plots
plot_data <- data.frame()

for (variable in predictor_variables) {
  pd <- partial(rf, pred.var = variable, train = train_data, plot = FALSE, prob = FALSE)
  
  
  pd_data <- data.frame(x = pd[[variable]], y = pd$y, variable = variable)
  
  # Add the data for this plot to the main dataframe
  plot_data <- rbind(plot_data, pd_data)
}

# Now create a single plot with one facet per variable
p <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Value", y = "Partial Dependence") +
  theme_minimal()

print(p)

