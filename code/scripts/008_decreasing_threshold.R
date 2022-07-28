library(lme4)
set.seed(1234)
breaks <- c(1600, 1300, 1200, 1100, 1000, 900, 800, 700, 600, 500, 400, 300,
            200, 150, 100, 90, 80, 70, 60, 50, 40, 30, 20)
decreasing <- function(response, covs, breaks){
  results <- list()
  for(i in 1:length(breaks)){
    tmp_AIC <- fit_models(response, covs, breaks[i], "less than", "univariate", FALSE)
    results[[i]] <- data.frame(n = nrow(SA_df[which(SA_df$dist<=breaks[i]),]),
                               dist = breaks[i], 
                               covs = tmp_AIC$Modnames,
                               AIC_wt = tmp_AIC$AICWt)
  }
  
  decreasing_threshold <- do.call("rbind", results)
  (decreasing_plot <- ggplot(decreasing_threshold, aes(x=as.factor(dist), y=covs)) +
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
           x = "Upper threshold (km)"))
  return(decreasing_plot)
}

# mean_decreasing_plot <- decreasing(response = lower(a_mean), covs = z_SA_df, breaks = breaks)
# ggsave("./figures/mean_decreasing_plot.jpg",plot=mean_decreasing_plot,device = "jpg",
#        width=7, height=3.5, units="in",dpi=600)
mode_decreasing_plot <- decreasing(response = lower(a_mode), covs = z_SA_df, breaks = breaks)
ggsave("./figures/mode_decreasing_plot.jpg",plot=mode_decreasing_plot,device = "jpg",
       width=7, height=3.5, units="in",dpi=600)