library(ecodist)
library(ResistanceGA)
library(AICcmodavg)
library(MuMIn)
library(ade4)
library(broom.mixed)
library(geosphere)
#mean
pca_sa<-dudi.pca(for_pca_mean,
                     center = TRUE, scale = FALSE, 
                     scann = FALSE, nf = 30)
PCA_DUDI_mean<-pca_sa$li
a_mean <- as.matrix(dist(PCA_DUDI_mean[,1:30], method = "euclidean"))
#mode
pca_sa<-dudi.pca(for_pca_mode,
                     center = TRUE, scale = FALSE, 
                     scann = FALSE, nf = 30)
PCA_DUDI_mode<-pca_sa$li
a_mode <- as.matrix(dist(PCA_DUDI_mode, method = "euclidean"))
#clustered mode
pca_sa<-dudi.pca(df_clustered_mode[,7:ncol(df_clustered_mode)],
                     center = TRUE, scale = FALSE, 
                     scann = FALSE, nf = 30)
PCA_DUDI_mode<-pca_sa$li
a_mode <- as.matrix(dist(PCA_DUDI_mode[,1:30], method = "euclidean"))

#Autoencoder
z <- as.matrix(dist(intermediate_output, method = "euclidean"))

# 
# for_pca_SA <- for_pca[5:nrow(df),]
# pca<-dudi.pca(for_pca_SA,center = TRUE, scale = FALSE, scann = FALSE, nf = 30) #30 axes
# PCA_DUDI<-pca$li
# a<-as.matrix(dist(PCA_DUDI[,1:30], method = "euclidean")) # Using the first 7 PCs
# nrow(PCA_DUDI)
#Estimating the euclidean distances between individuals

#XY<-read.csv("./data/raw/SPATIAL_DATA.csv") #Euclidean Distances of Individuals
#XY<-as.matrix(dist(df[5:nrow(df),4:5], method = "euclidean"))
#XY<-as.matrix(distance(df[5:nrow(df),4:5], method = "euclidean"))

XY <- distm(cbind(df[,6],df[,5]),
            cbind(df[,6],df[,5]), fun = distGeo)/1000

SA_df <- read.delim("./data/clean/SA_VRM/SA_VRM_resistances_3columns.out", 
                    sep =" ", header=FALSE)
names(SA_df) <- c("ID1", "ID2", "VRM")
# SA_df$genetic_distance_mean <- lower(a_mean)
# SA_df$genetic_distance_mode <- lower(a_mode)
# SA_df$genetic_distance_z <- lower(z)
SA_df$dist <- lower(XY)

SA_df <- SA_df[,c(1,2,4,3)]
SA_df$DEM_gaus <- read.delim("./data/clean/SA_DEM/SA_DEM_resistances_3columns.out", 
                             sep =" ", header=FALSE)$V3
SA_df$TPI <- read.delim("./data/clean/SA_TPI/SA_TPI_resistances_3columns.out", 
                            sep =" ", header=FALSE)$V3
SA_df$MAP_lin <- read.delim("./data/clean/SA_MAP_1/SA_MAP_1_resistances_3columns.out", 
                             sep =" ", header=FALSE)$V3
SA_df$MAP_gaus <- read.delim("./data/clean/SA_MAP_2/SA_MAP_2_resistances_3columns.out", 
                            sep =" ", header=FALSE)$V3
SA_df$Tmax_lin <- read.delim("./data/clean/SA_Tmax_lin/SA_Tmax_lin_resistances_3columns.out", 
                             sep =" ", header=FALSE)$V3
SA_df$Tmax_sq <- read.delim("./data/clean/SA_Tmax_sq/SA_Tmax_sq_resistances_3columns.out", 
                             sep =" ", header=FALSE)$V3
SA_df$Tmax_gaus <- read.delim("./data/clean/SA_Tmax_gaus/SA_Tmax_gaus_resistances_3columns.out", 
                            sep =" ", header=FALSE)$V3

delta_cov <- function(raster, points, ID1, ID2){
  tmp <- data.frame(id = seq(1:nrow(sp_points)),
                    val = raster::extract(raster, points))
  target_df <- data.frame(ID1 = ID1, 
                          ID2 = ID2)
  target_df <- left_join(target_df, tmp, by = c("ID1" = "id"))
  target_df <- left_join(target_df, tmp, by = c("ID2" = "id"))
  target_df$delta <- abs(target_df$val.x - target_df$val.y)
  return(target_df$delta)
}
SA_df$delta_elev <- delta_cov(SA_DEM, SA_points, SA_df$ID1, SA_df$ID2)
SA_df$delta_Tmax <- delta_cov(SA_Tmax, SA_points, SA_df$ID1, SA_df$ID2)
SA_df$delta_MAP <- delta_cov(SA_MAP, SA_points, SA_df$ID1, SA_df$ID2)

z_SA_df <- SA_df
#z_SA_df[,3:ncol(z_SA_df)] <- scale(z_SA_df[,3:ncol(z_SA_df)] ,center = TRUE, scale = TRUE)
z_SA_df[,4:ncol(z_SA_df)] <- apply(z_SA_df[4:ncol(z_SA_df)], 2,scale, center = FALSE, scale = TRUE)
library(corrplot)
M = cor(SA_df[,3:5])
corrplot(M, method = 'number')

M = cor(SA_df[,4:ncol(SA_df)])
corrplot(M, method = 'number')

fit_models <- function(genetic_distance, covs, distance, relation, set, return_list){
  SA_df_filter <- cbind(genetic_distance, covs)
  if(relation == "greater than") {
    SA_df_filter <- SA_df_filter[which(SA_df_filter$dist>=distance),]
  } else {
    SA_df_filter <- SA_df_filter[which(SA_df_filter$dist<=distance),]
  }
  Cand_models_SA <- list( )
  Cand_models_SA[[1]] <-lmer(genetic_distance ~ (1 | ID1), 
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[2]] <-lmer(formula = genetic_distance ~ dist + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[3]] <-lmer(formula = genetic_distance ~ delta_elev + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[4]] <-lmer(formula = genetic_distance ~ delta_Tmax + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[5]] <-lmer(formula = genetic_distance ~ delta_MAP + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[6]] <-lmer(formula = genetic_distance ~ VRM + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[7]] <-lmer(formula = genetic_distance ~ DEM_gaus + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[8]] <-lmer(formula = genetic_distance ~ TPI + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[9]] <-lmer(formula = genetic_distance ~ MAP_lin + (1 | ID1),
                             REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[10]] <-lmer(formula = genetic_distance ~ MAP_gaus + (1 | ID1),
                              REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[11]] <-lmer(formula = genetic_distance ~ Tmax_lin + (1 | ID1),
                              REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[12]] <-lmer(formula = genetic_distance ~ Tmax_sq + (1 | ID1),
                              REML=FALSE, data = SA_df_filter)
  Cand_models_SA[[13]] <-lmer(formula = genetic_distance ~ Tmax_gaus + (1 | ID1),
                              REML=FALSE, data = SA_df_filter)
  names<-rbind(#Univariates
    'null','dist','delta_elev','delta_Tmax','delta_MAP',
    'VRM','DEM_gaus','TPI','MAP_lin', 'MAP_gaus', 
    'Tmax_lin','Tmax_sq','Tmax_gaus'
  )
  if( set == "multivariate") {
    Cand_models_SA[[14]] <-lmer(formula = genetic_distance ~ dist + delta_elev + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[15]] <-lmer(formula = genetic_distance ~ dist + delta_Tmax + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[16]] <-lmer(formula = genetic_distance ~ DEM_gaus + delta_Tmax + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[17]] <-lmer(formula = genetic_distance ~ DEM_gaus + delta_MAP + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[18]] <-lmer(formula = genetic_distance ~ VRM + delta_elev + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[19]] <-lmer(formula = genetic_distance ~ VRM + delta_Tmax + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[20]] <-lmer(formula = genetic_distance ~ VRM + delta_MAP + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[21]] <-lmer(formula = genetic_distance ~ MAP_gaus + delta_elev + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    Cand_models_SA[[22]] <-lmer(formula = genetic_distance ~ MAP_gaus + delta_Tmax + (1 | ID1),
                                REML=FALSE, data = SA_df_filter)
    names<-rbind(#Univariates
      'null','dist','delta_elev','delta_Tmax','delta_MAP',
      'VRM','DEM_gaus','TPI','MAP_lin', 'MAP_gaus', 
      'Tmax_lin','Tmax_sq','Tmax_gaus',
      #Multivariate
      'dist + delta_elev','dist + delta_Tmax', #'dist + delta_MAP' #highly correlated
      'DEM_gaus + delta_Tmax','DEM_gaus + delta_MAP',
      'VRM + delta_elev','VRM + delta_Tmax','VRM + delta_MAP',
      'MAP_gaus + delta_elev', 'MAP_gaus + delta_Tmax'
    )
  }
  AIC<-aictab(Cand_models_SA, sort = TRUE,
               second.ord = FALSE, modnames=names)
  if(return_list == TRUE) {
    return(list(AIC, Cand_models_SA))  
  } else {
    return(AIC)
  }
  
}
set.seed(1234)
SA_mean_AIC <- fit_models(lower(a_mean), z_SA_df, 0, "greater than", "multivariate", TRUE)
SA_mode_AIC <- fit_models(lower(a_mode), z_SA_df, 0, "greater than", "multivariate", TRUE)
SA_z_AIC <- fit_models(lower(z), z_SA_df, 0, "greater than", "multivariate", TRUE)
write.csv(SA_mean_AIC[[1]], "./figures/SA_AIC_set_mean.csv")
write.csv(SA_mode_AIC[[1]], "./figures/SA_AIC_set_mode.csv")
write.csv(SA_z_AIC[[1]], "./figures/SA_AIC_set_z.csv")

models <- c(20, 19, 6, 18, 8, 9, 15, 2)
betas <- list()
for(i in 1:length(models)){
  tmp <- tidy(SA_mean_AIC[[2]][[models[i]]],effects="fixed", conf.int=TRUE, conf.method="profile")
  betas[[i]] <- data.frame(term = tmp$term, mean = tmp$estimate, ci_low = tmp$conf.low, ci_high = tmp$conf.high)
}
all_betas <- do.call("rbind", betas)
write.csv(all_betas, "./figures/all_betas.csv")

models <- c(9 ,19, 20, 6, 18, 8, 15, 11, 14, 2)
betas <- list()
for(i in 1:length(models)){
  tmp <- tidy(SA_mode_AIC[[2]][[models[i]]],effects="fixed", conf.int=TRUE, conf.method="profile")
  betas[[i]] <- data.frame(term = tmp$term, mean = tmp$estimate, ci_low = tmp$conf.low, ci_high = tmp$conf.high)
}
all_betas <- do.call("rbind", betas)
write.csv(all_betas, "./figures/all_betas_mode.csv")

models(18, 20, 19, 14, 6, 15, 17, 2)
betas <- list()
for(i in 1:length(models)){
  tmp <- tidy(SA_z_AIC[[2]][[models[i]]],effects="fixed", conf.int=TRUE, conf.method="profile")
  betas[[i]] <- data.frame(term = tmp$term, mean = tmp$estimate, ci_low = tmp$conf.low, ci_high = tmp$conf.high)
}
all_betas <- do.call("rbind", betas)
write.csv(all_betas, "./figures/all_betas_z.csv")