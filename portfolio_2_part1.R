library(quantmod)
library(forecast)
library(lmtest)
library(caTools)
library(raster)

setwd("/home/maxim/Documents/coursework/time-series-analysis/data_p2/")
ndvi_file = "BOTSWANA_NDVI_USGS"
rain_file = "rs_rain_chirps_Botswana"
dw_out = 'dw_bots.tif'
p_out = 'p_bots.tif'

ndvi = read.ENVI(ndvi_file,headerfile=paste(ndvi_file,'.hdr',sep=''))
rain = read.ENVI(rain_file,headerfile=paste(rain_file,'.hdr',sep=''))

#nrows = nrow(ndvi)
#ncols = ncol(ndvi)
# test subset
nrows = 20
ncols = 20

#diagnostic variable

rsquare_arr = matrix(NA, nrow <- nrows,ncol <- ncols)
pval_arr = matrix(NA, nrow <- nrows,ncol <- ncols)
dw_arr = matrix(NA, nrow <- nrows,ncol <- ncols)

pb = txtProgressBar(title='progress bar',min=0,max =nrows) #run together with loop
for (i in 1:nrows) {
  for (j in 1:ncols) {
    setTxtProgressBar(pb, i, title = round(i / nrow(cube) * 100, 0))
    
    ts_ndvi = ts(ndvi[i,j,], frequency=12) #start=?
    ts_rain = ts(rain[i,j,], frequency=12)
    
    if (var(ts_ndvi) > 0) {
      # temporal decomposition
      ts_decomp_ndvi = stl(ts_ndvi,"periodic") ## To access ts: ts_decomp_ndvi$time.series
      ts_decomp_rain = stl(ts_rain,"periodic")
      
      ts_ndvi_anom <- ts_decomp_ndvi[[1]][,3]
      ts_rain_anom = ts_decomp_rain[[1]][,3]
      
      # Step 1: OLS Regression of NDVI and rainfall anomalies (NOT the original data)
      
      model <- lm(ts_ndvi_anom ~ ts_rain_anom)
      
      # Step 2: fitting an optimal Arima-Model to model residuals (fitting stochastic model to model residuals)
      
      optfit <- auto.arima(model$residuals, max.p=5, max.q = 0)
      
      # Step 3: filter the original series (the anomalies!)
      
      yfiltered <- residuals(Arima(ts_ndvi_anom,model=optfit))
      xfiltered <- residuals(Arima(ts_rain_anom,model=optfit))
      
      # Step 4: setup the distributed lag model
      # build a dataframe with the filtered NDVI and rainfall anomalies and lagged versions of the rainfall data
      lagX1 <- Lag (as.numeric(xfiltered) ,1)
      lagX2 <- Lag (as.numeric(xfiltered) ,2)
      lagX3 <- Lag (as.numeric(xfiltered) ,3)
      lagX4 <- Lag (as.numeric(xfiltered) ,4)
      lagX5 <- Lag (as.numeric(xfiltered) ,5)
      lagX6 <- Lag (as.numeric(xfiltered) ,6) # shift of max 6 months
      
      X <- as.data.frame(cbind(yfiltered, xfiltered, lagX1, 
                               lagX2, lagX3,  lagX4, lagX5, lagX6))
      names(X) <- c("ndvi","Lag0", "Lag1","Lag2","Lag3","Lag4","Lag5", "Lag6")
      
      # Step 5. repeat OLS-regression 
      
      lag_model <- lm(ndvi ~ ., data=X)
      
      tmp = summary(lag_model)
      # Diagnostic
      rsquare_arr[i,j] = tmp$r.squared
      #pval_arr[i,j] = summary(lag_model)$
      dw_arr[i,j] = dwtest(model)$p.value
      #dw_arr[i,j] = dwtest(model)$statistic
      
    }
  }
}
close(pb)

cube = brick(ndvi_file)
dw_raster = raster(dw_arr,xmn=extent(cube)[1],xmx=extent(cube)[2],ymn=extent(cube)[3],ymx=extent(cube)[4],crs=projection(cube))
writeRaster(dw_raster,filename=dw_out,format='GTiff',overwrite="True")