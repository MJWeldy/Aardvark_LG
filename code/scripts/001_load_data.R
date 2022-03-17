library(readxl)
library(tidyverse)
library(raster)
library(sf)

df <- read_excel("data/raw/Aardvark_msat_210715_8locmin_GPSlocs.xlsx",
                 col_types = c("text", "text", "text","numeric", 
                               "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric")
                 )

df <- df[-c(1:4),]
for_pca_mean <- df[,6:43] %>% 
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

#for_pca_kruger <- for_pca[which(df$Region!="NA"),] # delete in a bit
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for_pca_mode <- df[,6:43] %>% 
  mutate_all(~ifelse(is.na(.x), getmode(.x), .x))

#mean(df$MK15955A,na.rm=TRUE)#116.5125
#getmode(df$MK15955A) #117

sp_points <- st_as_sf(df[,1:5], coords = c("Longitude", "Latitude"), 
                      crs = 4326)
sp_points$id <- seq(1:nrow(sp_points))

plot(GLM)
plot(sp_points, add=TRUE)

dist <- GLM
values(dist) <- 1
summary(dist)
#writeRaster(dist, filename = "./data/gis/dist.ascii", format= "ascii", overwrite=TRUE)

GLM <- raster("./data/gis/GLM.tif")
GLM[is.na(GLM)] <- -9999
summary(GLM)
#writeRaster(GLM, filename = "./data/gis/glm.ascii", format= "ascii")

ME <- raster("./data/gis/ME_Resistance.tif")
ME[is.na(ME)] <- -9999
summary(ME)
#writeRaster(ME, filename = "./data/gis/me.ascii", format= "ascii", overwrite=TRUE)

points <- GLM
values(points) <- -9999
kruger_points <- sp_points[sp_points$Region!="NA",]
kruger_points$id <- seq(1:nrow(kruger_points))
x <- rasterize(kruger_points, points, field="id", update=TRUE)
summary(x)
#writeRaster(x, filename = "./data/gis/points.ascii", format= "ascii", overwrite=TRUE)


SA_DEM <- raster("./data/gis/dem250m_mean.tif")
SA_DEM[is.na(SA_DEM)] <- -9999
SA_DEM[SA_DEM==0] <- -9999
summary(SA_DEM)
#writeRaster(SA_DEM, filename = "./data/gis/SA_DEM.ascii", format= "ascii", overwrite=TRUE)


SA_slope <- raster("./data/gis/sa_slope30.tif")
SA_slope[is.na(SA_slope)] <- -9999
summary(SA_slope)
#writeRaster(SA_slope, filename = "./data/gis/SA_slope.ascii", format= "ascii", overwrite=TRUE)

#Crashes Circuitscape.jl I think it is from a computational overflow problem resulting from multiplying small 
#numbers
SA_TPI <- raster("./data/gis/sa_tpi5x5250.tif")
SA_TPI[is.na(SA_TPI)] <- -9999
SA_TPI[SA_TPI==0] <- 0.001
summary(SA_TPI)
#writeRaster(SA_TPI, filename = "./data/gis/SA_TPI.ascii", format= "ascii", overwrite=TRUE)

SA_VRM <- raster("./data/gis/sa_vrm250.tif")
SA_VRM <- SA_VRM*100
SA_VRM[SA_VRM<0.1] <- 0.1
SA_VRM[is.na(SA_VRM)] <- -9999
summary(SA_VRM)
#writeRaster(SA_VRM, filename = "./data/gis/SA_VRM.ascii", format= "ascii", overwrite=TRUE)

SA_MAP_1 <- raster("./data/gis/mean_annual_precip.tif")
SA_MAP_1 <- resample(SA_MAP_1, SA_VRM)
SA_MAP_1 <- (1/SA_MAP_1)*100
SA_MAP_1[is.na(SA_MAP_1)] <- -9999
summary(SA_MAP_1)
#writeRaster(SA_MAP_1, filename = "./data/gis/SA_MAP_1.ascii", format= "ascii", overwrite=TRUE)

SA_MAP_2 <- raster("./data/gis/mean_annual_precip.tif")
SA_MAP_2 <- resample(SA_MAP_2, SA_VRM)
MAP_2_med <- cellStats(SA_MAP_2, function(s,...){median(s, na.rm = TRUE)})
SA_MAP_2 <-abs(SA_MAP_2-MAP_2_med)
SA_MAP_2 <-round(SA_MAP_2, 6)
SA_MAP_2[is.na(SA_MAP_2)] <- -9999
summary(SA_MAP_2)
#writeRaster(SA_MAP_2, filename = "./data/gis/SA_MAP_2.ascii", format= "ascii", overwrite=TRUE)


SA_Tmax <- raster("./data/gis/mean_tmax.tif")
SA_Tmax <- resample(SA_Tmax, SA_VRM)
summary(SA_Tmax)
plot(SA_Tmax)
#writeRaster(SA_Tmax, filename = "./data/gis/SA_Tmax.ascii", format= "ascii", overwrite=TRUE)

dist <- SA_DEM
values(dist) <- 1
summary(dist)
#writeRaster(dist, filename = "./data/gis/SA_dist.ascii", format= "ascii", overwrite=TRUE)

points <- SA_DEM
values(points) <- -9999
SA_points <- st_transform(sp_points,9221)
SA_points$id <- seq(1:nrow(SA_points))
SA_points_raster <- rasterize(SA_points, points, field="id", update=TRUE)
summary(SA_points_raster)
#writeRaster(SA_points_raster, filename = "./data/gis/SA_points.ascii", format= "ascii", overwrite=TRUE)

