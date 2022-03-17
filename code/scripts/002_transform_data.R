alpha <- 1
r_max <- 100

positive_transform <- function(x, r_max, alpha) {
  (x^alpha/cellStats(x, 'max', na.rm=TRUE)^alpha)*(r_max-1) +1
}

negative_transform <- function(x, r_max, alpha) {
  r_max - (x^alpha/cellStats(x, 'max', na.rm=TRUE)^alpha)*(r_max-1)
}

gaussian_transform <- function(x, x_opt, x_SD, r_max) {
  r_max - (r_max-1)*exp( -(((x-x_opt)^2) / (2*(x_SD)^2)) )
}

GLM <- negative_transform(GLM, r_max, alpha)
GLM[is.na(GLM)] <- -9999
#writeRaster(GLM, filename = "./data/gis/glm.ascii", format= "ascii", overwrite=TRUE)

ME <- negative_transform(ME, r_max, alpha)
ME[is.na(ME)] <- -9999
#writeRaster(ME, filename = "./data/gis/me.ascii", format= "ascii", overwrite=TRUE)

x_opt <- cellStats(SA_DEM, function(s,...){median(s, na.rm = TRUE)})
x_SD <- cellStats(SA_MAP_2, 'sd', na.rm=TRUE)
SA_DEM_gaus <- gaussian_transform(SA_DEM, x_opt, x_SD, r_max)
SA_DEM_gaus[is.na(SA_DEM_gaus)] <- -9999
#writeRaster(SA_DEM_gaus, filename = "./data/gis/SA_DEM.ascii", format= "ascii", overwrite=TRUE)

SA_VRM <- positive_transform(SA_VRM, r_max, alpha)
SA_VRM[is.na(SA_VRM)] <- -9999
#writeRaster(SA_VRM, filename = "./data/gis/SA_VRM.ascii", format= "ascii", overwrite=TRUE)

SA_TPI <- positive_transform(abs(SA_TPI), r_max, alpha)
SA_TPI[is.na(SA_TPI)] <- -9999
#writeRaster(SA_TPI, filename = "./data/gis/SA_TPI.ascii", format= "ascii", overwrite=TRUE)

SA_MAP_1 <- negative_transform(SA_MAP_1, r_max, alpha)
SA_MAP_1[is.na(SA_MAP_1)] <- -9999
#writeRaster(SA_MAP_1, filename = "./data/gis/SA_MAP_1.ascii", format= "ascii", overwrite=TRUE)

x_opt <- cellStats(SA_MAP_2, function(s,...){median(s, na.rm = TRUE)})
x_SD <- cellStats(SA_MAP_2, 'sd', na.rm=TRUE)
SA_MAP_2 <- gaussian_transform(SA_MAP_2, x_opt, x_SD, r_max)
SA_MAP_2[is.na(SA_MAP_2)] <- -9999
#writeRaster(SA_MAP_2, filename = "./data/gis/SA_MAP_2.ascii", format= "ascii", overwrite=TRUE)

alpha <- 1
SA_Tmax_lin <- positive_transform(SA_Tmax, r_max, alpha)
#writeRaster(SA_Tmax_lin, filename = "./data/gis/SA_Tmax_lin.ascii", format= "ascii", overwrite=TRUE)

alpha <- 2
SA_Tmax_sq <- positive_transform(SA_Tmax, r_max, alpha)
#writeRaster(SA_Tmax_sq, filename = "./data/gis/SA_Tmax_sq.ascii", format= "ascii", overwrite=TRUE)

x_opt <- 23.47 #From Pantheria DB might not be a good source...
x_SD <- 3
SA_Tmax_gaus <- gaussian_transform(SA_Tmax, x_opt, x_SD, r_max)
#writeRaster(SA_Tmax_gaus, filename = "./data/gis/SA_Tmax_gaus.ascii", format= "ascii", overwrite=TRUE)
