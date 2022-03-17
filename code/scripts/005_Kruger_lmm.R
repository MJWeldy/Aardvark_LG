library(ecodist)
library(ResistanceGA)
library(AICcmodavg)
library(MuMIn)
library(ade4)
library(lme4)
library(geosphere)

#mean
pca_kruger<-dudi.pca(for_pca_mean[which(df$Region!="NA"),],
                     center = TRUE, scale = FALSE, 
                     scann = FALSE, nf = 30)
PCA_DUDI_mean<-pca_kruger$li
a_mean <- as.matrix(dist(PCA_DUDI_mean[,1:30], method = "euclidean"))
#mode
pca_kruger<-dudi.pca(for_pca_mode[which(df$Region!="NA"),],
                     center = TRUE, scale = FALSE, 
                     scann = FALSE, nf = 30)
PCA_DUDI_mode<-pca_kruger$li
a_mode <- as.matrix(dist(PCA_DUDI_mode[,1:30], method = "euclidean"))
#Autoencoder
z <- as.matrix(dist(intermediate_output[which(df$Region!="NA"),], method = "euclidean"))


#Estimating the euclidean distances between individuals
XY <- distm(cbind(df[which(df$Region!="NA"),5],df[which(df$Region!="NA"),4]),
            cbind(df[which(df$Region!="NA"),5],df[which(df$Region!="NA"),4]), 
            fun = distGeo)/1000
#XY<-as.matrix(dist(df[which(df$Region!="NA"),4:5], method = "euclidean"))

# vegan::mantel(a , XY)
# z.mgram <- mgram(lower(a), lower(XY), breaks= 1:40) #, nclass= 40
# plot(z.mgram,  pval = 0.05, xlab = "Distance", ylab = "Mantel r")


id <- To.From.ID(nrow(XY)) #Calculating an index for the individual pairwise distances using a ResistancesGA function

# dist<-as.matrix(read.csv("./data/clean/dist/dist_resistances.csv"))
# colnames(dist) <- NULL
glm<-as.matrix(read.csv("./data/clean/GLM/glm_resistances.csv"))
colnames(glm) <- NULL 
me<-as.matrix(read.csv("./data/clean/ME/ME_resistances.csv"))
colnames(me) <- NULL 

lmm_df <- data.frame(y_mean = lower(a_mean),
                     y_mode = lower(a_mode),
                     y_AE = lower(z),
                     dist = lower(XY),
                     z_dist = scale(lower(XY), center = FALSE, scale = TRUE),
                     GLM = lower(glm),
                     z_GLM = scale(lower(glm), center = FALSE, scale = TRUE),
                     ME = lower(me),
                     z_ME = scale(lower(me), center = FALSE, scale = TRUE),
                     pop = id$pop1,
                     otherpop = id$pop2)

set.seed(1234)
Cand.models_mean <- list( )
Cand.models_mean[[1]] <-lmer(formula = y_mean ~ (1 | pop),
                            REML=FALSE,data = lmm_df)
Cand.models_mean[[2]] <-lmer(formula = y_mean ~ z_dist + (1 | pop),
                            REML=FALSE,data = lmm_df)
Cand.models_mean[[3]] <-lmer(formula = y_mean ~ z_GLM + (1 | pop),
                            REML=FALSE,data = lmm_df)
Cand.models_mean[[4]] <-lmer(formula = y_mean ~ z_ME + (1 | pop),
                            REML=FALSE,data = lmm_df)

Cand.models_mode <- list( )
Cand.models_mode[[1]] <-lmer(formula = y_mode ~ (1 | pop),
                             REML=FALSE,data = lmm_df)
Cand.models_mode[[2]] <-lmer(formula = y_mode ~ z_dist + (1 | pop),
                             REML=FALSE,data = lmm_df)
Cand.models_mode[[3]] <-lmer(formula = y_mode ~ z_GLM + (1 | pop),
                             REML=FALSE,data = lmm_df)
Cand.models_mode[[4]] <-lmer(formula = y_mode ~ z_ME + (1 | pop),
                             REML=FALSE,data = lmm_df)

Cand.models_AE <- list( )
Cand.models_AE[[1]] <-lmer(formula = y_AE ~ (1 | pop),
                             REML=FALSE,data = lmm_df)
Cand.models_AE[[2]] <-lmer(formula = y_AE ~ z_dist + (1 | pop),
                             REML=FALSE,data = lmm_df)
Cand.models_AE[[3]] <-lmer(formula = y_AE ~ z_GLM + (1 | pop),
                             REML=FALSE,data = lmm_df)
Cand.models_AE[[4]] <-lmer(formula = y_AE ~ z_ME + (1 | pop),
                             REML=FALSE,data = lmm_df)
names<-rbind('null','dist','GLM','ME')

(AIC<-aictab(Cand.models_mean, sort = TRUE,
            second.ord = FALSE, modnames=names))
(AIC<-aictab(Cand.models_mode, sort = TRUE,
             second.ord = FALSE, modnames=names))
(AIC<-aictab(Cand.models_AE, sort = TRUE,
             second.ord = FALSE, modnames=names))