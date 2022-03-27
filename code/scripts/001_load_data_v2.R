df <- read_excel("data/raw/Aardvark_msat_210715_8locmin_GPSlocs.xlsx",
                 col_types = c("text", "text", "numeric","text","numeric", 
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

NA_filter_row <- rep(TRUE, nrow(df))
for(i in 1:nrow(df)){
  NA_filter_row[i] <- ifelse(sum(is.na(df[i,7:ncol(df)]))/length(df[i,7:ncol(df)]) > 0.20 , FALSE, TRUE)
}
test <- data.frame(df$Population, NA_filter_row) %>% group_by(df.Population) %>% summarise(count = sum(NA_filter_row))

filter_set <- seq(1:nrow(df))[NA_filter_row]

NA_filter_col <- rep(TRUE, ncol(df))
for(i in 7:ncol(df)){
  NA_filter[i] <- ifelse(sum(is.na(df[,i]))/nrow(df[,i]) > 0.1 , FALSE, TRUE)
}

  
sp_points <- st_as_sf(df[,1:6], coords = c("Longitude", "Latitude"), 
                      crs = 4326)
sp_points$id <- seq(1:nrow(sp_points))


#Prep PCA datasets
for_pca_mean <- df[,7:ncol(df)] %>% 
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

getmode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for_pca_mode <- df[,7:ncol(df)] %>% 
  mutate_all(~ifelse(is.na(.x), getmode(.x), .x))

df_clustered_mode <- df %>% 
  group_by(CombinedPop) %>% 
  mutate_all(~ifelse(is.na(.x), getmode(.x),.x)) %>% 
  ungroup() 
df_clustered_mode[is.na(df_clustered_mode$MK13744A),21] <- getmode(df_clustered_mode[df_clustered_mode$Population == "KrugerNP",21])
df_clustered_mode[is.na(df_clustered_mode$MK13744B),22] <- getmode(df_clustered_mode[df_clustered_mode$Population == "KrugerNP",22])

# Kruger rasters
GLM <- raster("./data/gis/GLM.tif")
ME <- raster("./data/gis/ME_RAW_STAND.tif")
dist <- GLM
values(dist) <- 1

points <- GLM
values(points) <- -9999
kruger_points <- sp_points[sp_points$Region!="NA",]
kruger_points$id <- seq(1:nrow(kruger_points))
x <- rasterize(kruger_points, points, field="id", update=TRUE)
#writeRaster(x, filename = "./data/gis/points.ascii", format= "ascii", overwrite=TRUE)

#SA rasters
SA_DEM <- raster("./data/gis/dem250m_mean.tif")
SA_VRM <- raster("./data/gis/sa_vrm250.tif")
SA_TPI <- raster("./data/gis/sa_tpi5x5250.tif")
SA_MAP <- raster("./data/gis/mean_annual_precip.tif")
SA_MAP_1 <- resample(SA_MAP, SA_VRM)
SA_MAP_2 <- SA_MAP_1
SA_Tmax <- raster("./data/gis/mean_tmax.tif")
SA_Tmax <- resample(SA_Tmax, SA_VRM)
dist <- SA_DEM
values(dist) <- 1
#writeRaster(dist, filename = "./data/gis/SA_dist.ascii", format= "ascii", overwrite=TRUE)

points <- SA_DEM
values(points) <- -9999
SA_points <- st_transform(sp_points,9221)
SA_points$id <- seq(1:nrow(SA_points))
SA_points_raster <- rasterize(SA_points, points, field="id", update=TRUE)
#writeRaster(SA_points_raster, filename = "./data/gis/SA_points.ascii", format= "ascii", overwrite=TRUE)

