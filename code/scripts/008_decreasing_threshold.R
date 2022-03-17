library(lme4)
set.seed(1234)
breaks <- c(1600, 1300, 1200, 1100, 1000, 900, 800, 700, 600, 500, 400, 300,
            200, 150, 100, 90, 80, 70, 60, 50, 40, 30, 20)
results <- list()
for(i in 1:length(breaks)){
  tmp_AIC <- fit_models(lower(a_mean), SA_df, breaks[i], "less than", "univariate")
  results[[i]] <- data.frame(n = nrow(SA_df[which(SA_df$dist<=breaks[i]),]),
                             dist = breaks[i], 
                             covs = tmp_AIC$Modnames,
                             AIC_wt = tmp_AIC$AICWt)
}

decreasing_threshold <- do.call("rbind", results)
head(decreasing_threshold)
(decreasing_plot <- ggplot(decreasing_threshold, aes(x=as.factor(dist), y=covs)) +
  geom_tile(aes(fill = AIC_wt)) +
  scale_fill_distiller(palette = "YlGnBu")+
  theme_minimal()+
  labs(y = "Covariate",
       x = "Upper threshold (km)"))
ggsave("./figures/decreasing_plot.jpg",plot=decreasing_plot,device = "jpg",
       width=7, height=7, units="in",dpi=600)