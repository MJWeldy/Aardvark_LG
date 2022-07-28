library(lme4)
set.seed(1234)
breaks <- c(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 600, 700, 
            800, 900, 1000, 1100, 1200)
increasing <- function(response, covs, breaks){
  results <- list()
  for(i in 1:length(breaks)){
    tmp_AIC <- fit_models(response, covs, breaks[i], "greater than", "univariate", FALSE)
    results[[i]] <- data.frame(n = nrow(SA_df[which(SA_df$dist>=breaks[i]),]),
                               dist = breaks[i], 
                               covs = tmp_AIC$Modnames,
                               AIC_wt = tmp_AIC$AICWt)
  }
  increasing_threshold <- do.call("rbind", results)
  (increasing_plot <- ggplot(increasing_threshold, aes(x=as.factor(dist), y=covs)) +
      geom_tile(aes(fill = AIC_wt)) +
      scale_fill_distiller(palette = "YlGnBu")+
      labs(fill = "AIC\nweight")+
      theme_minimal()+
      scale_y_discrete(labels=c(
        "null" = "Null",
        "dist" = "Distance",
        "delta_elev" = "Difference in elevation",
        "delta_Tmax" = "Difference in maximum temperature",
        "delta_MAP" = "Difference in mean annual precipitation",
        "VRM" = "Vector ruggedness measure",
        "TPI" = "Topographic position index",
        "DEM_gaus" = "Elevation Gaussian",
        "MAP_lin" = "Inverse mean annual precipitation",
        "MAP_gaus" = "Mean annual precipitation Gaussian",
        "Tmax_lin" = "Maximum temperature",
        "Tmax_sq" = "Maximum temperature squared",
        "Tmax_gaus" = "Maximum temperature Gaussian"

        ))+
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(y = "",
           x = "Lower threshold (km)"))
  return(increasing_plot)
}

# mean_increasing_plot <- increasing(response = lower(a_mean), covs = z_SA_df, breaks = breaks)
# ggsave("./figures/mean_increasing_plot.jpg",plot=mean_increasing_plot,device = "jpg",
#        width=7, height=3.5, units="in",dpi=600)
mode_increasing_plot <- increasing(response = lower(a_mode), covs = z_SA_df, breaks = breaks)
ggsave("./figures/mode_increasing_plot.jpg",plot=mode_increasing_plot,device = "jpg",
       width=7, height=3.5, units="in",dpi=600)