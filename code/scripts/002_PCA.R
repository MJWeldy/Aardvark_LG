library(ade4)
library(factoextra)
library(adegenet)
set.seed(12)

pca<-dudi.pca(for_pca_mean,
              center = TRUE, scale = FALSE, 
              scann = FALSE, nf = 50)

for(i in 1:38) {
  eig_vals <- pca$eig/sum(pca$eig)
  print(paste0("iter: ", i," ",sum(eig_vals[1:i])))
}

evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p2 <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}
evplot(pca$eig)

pops <- df$Population[filter_set]
pops <- case_when(
  pops == "KenyaMpala" ~ "Kenya Mpala", 
  pops == "AugrabiesNP" ~ "Augrabies NP",
  pops == "CamdebooNP" ~ "Camdeboo NP",
  pops == "FarmArea" ~ "Roggeveld",
  pops == "KarooNP" ~ "Karoo NP",
  pops == "KrugerNP" ~ "Kruger NP",
  pops == "NamaquaNP" ~ "Namaqua NP",
  pops == "TankwaKarooNP" ~ "Tankwa Karoo NP",
  pops == "UppingtonArea" ~ "Upington area",
  pops == "Prieaza" ~ "Prieska area",
  pops == "VictoriaWestArea" ~ "Victoria West area",
  pops == "SwaziMlawulaGR" ~ "Eswatini Mlawula NR",
)
library(ggthemes)
library(viridis)
library(patchwork)
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))
palette=getPalette(12)
(p1 <- ggplot(pca$li, aes(x = Axis1,y = Axis2)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_point(size=2, shape=16, aes(colour=pops))+
    guides(colour=guide_legend(title="Location"))+
    theme_tufte()+
    xlim(-25,25)+
    ylim(-25,25)+
    xlab("Principal component 1")+
    ylab("Principal component 2")+
    #scale_colour_viridis(discrete = TRUE))
    #scale_colour_brewer(getPalette(12)))
    scale_colour_manual(values=palette))
(p2 <- ggplot(pca$li, aes(x = Axis1,y = Axis3)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_point(size=2, shape=16, aes(colour=pops))+
    guides(colour=guide_legend(title="Location"))+
    theme_tufte()+
    xlim(-25,25)+
    ylim(-25,25)+
    xlab("Principal component 1")+
    ylab("Principal component 3")+
    #scale_colour_viridis(discrete = TRUE))
    #scale_colour_brewer(palette = "Spectral"))
    scale_colour_manual(values=palette))
(p3 <- ggplot(pca$li, aes(x = Axis2,y = Axis3)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_point(size=2, shape=16, aes(colour=pops))+
    guides(colour=guide_legend(title="Location"))+
    theme_tufte()+
    xlim(-25,25)+
    ylim(-25,25)+
    xlab("Principal component 2")+
    ylab("Principal component 3")+
    #scale_colour_viridis(discrete = TRUE))
    #scale_colour_brewer(palette = "Spectral"))
    scale_colour_manual(values=palette))
eig_vals = pca$eig/sum(pca$eig)
eig_df <- data.frame(
  eig_num = seq(1:25),
  eig_vals = eig_vals[1:25]
)
(p4 <- ggplot(eig_df, aes(x = as.factor(eig_num),y = eig_vals)) +
    geom_bar(stat="identity")+
    theme_tufte()+
    ylim(0,0.15)+
    xlab("PCA Axis")+
    ylab("Percentage of variance")+
    theme(axis.text.x=element_blank()))
p_sigma <- p1 + p2 + p3 + 
  inset_element(p4, left = 1, bottom = -0.1, right = 2.1, top = 0.2) +
  plot_layout(guides = 'collect') &
  theme(legend.position='right', legend.justification = 'top')
ggsave("./figures/pca.jpg",plot=p_sigma,device = "jpg",
       width=8, height=5, units="in",dpi=600)


(p1 <- ggplot(pca$li, aes(x = Axis1,y = Axis2)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_point(size=2, shape=16, aes(colour=as.factor(df$CombinedPop[filter_set])))+
    guides(colour=guide_legend(title="Location"))+
    theme_tufte()+
    xlim(-25,25)+
    ylim(-25,25)+
    xlab("Principal component 1")+
    ylab("Principal component 2")+
    #scale_colour_viridis(discrete = TRUE))
    scale_colour_brewer(palette = "Spectral"))

(p2 <- ggplot(pca$li, aes(x = Axis1,y = Axis3)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_point(size=2, shape=16, aes(colour=as.factor(df$CombinedPop[filter_set])))+
    guides(colour=guide_legend(title="Location"))+
    theme_tufte()+
    xlim(-25,25)+
    ylim(-25,25)+
    xlab("Principal component 1")+
    ylab("Principal component 3")+
    #scale_colour_viridis(discrete = TRUE))
    scale_colour_brewer(palette = "Spectral"))

(p3 <- ggplot(pca$li, aes(x = Axis2,y = Axis3)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_point(size=2, shape=16, aes(colour=as.factor(df$CombinedPop[filter_set])))+
    guides(colour=guide_legend(title="Location"))+
    theme_tufte()+
    xlim(-25,25)+
    ylim(-25,25)+
    xlab("Principal component 2")+
    ylab("Principal component 3")+
    #scale_colour_viridis(discrete = TRUE))
    scale_colour_brewer(palette = "Spectral"))


p_sigma <- p1 + p2 + p3 + inset_element(p4, left = 0.6, bottom = 0.6, right = 1, top = 1) + plot_layout(guides = 'collect')
ggsave("./figures/pca_structure.jpg",plot=p_sigma,device = "jpg",
       width=8, height=5, units="in",dpi=600)