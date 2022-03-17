library(lme4)
set.seed(1234)
breaks <- c(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 600, 700, 
            800, 900, 1000, 1100, 1200, 1300)
results <- list()
for(i in 1:length(breaks)){
  tmp_AIC <- fit_models(lower(a_mean), SA_df, breaks[i], "greater than", "univariate")
  results[[i]] <- data.frame(n = nrow(SA_df[which(SA_df$dist>=breaks[i]),]),
                             dist = breaks[i], 
                             covs = tmp_AIC$Modnames,
                             AIC_wt = tmp_AIC$AICWt)
}

increasing_threshold <- do.call("rbind", results)
head(increasing_threshold)
(increasing_plot <- ggplot(increasing_threshold, aes(x=as.factor(dist), y=covs)) +
  geom_tile(aes(fill = AIC_wt)) +
  scale_fill_distiller(palette = "YlGnBu")+
  theme_minimal()+
  labs(y = "Covariate",
       x = "Lower threshold (km)"))
ggsave("./figures/increasing_plot.jpg",plot=increasing_plot,device = "jpg",
       width=7, height=7, units="in",dpi=600)