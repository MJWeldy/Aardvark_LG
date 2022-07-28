library(vegan)
library(ecodist)
library(ggplot2)
library(latex2exp)

df_filter <- df[filter_set,]
XY_filter <- distm(cbind(df_filter[,6],df_filter[,5]),
            cbind(df_filter[,6],df_filter[,5]), fun = distGeo)/1000
XY_filter[XY_filter>100] <- 100
XYs <- cbind(df_filter[,6],df_filter[,5])
breaks <-seq(0, 100, by = 10)
mant_cor <- mantel.correlog(D.eco = a_mean, D.geo= XY_filter, break.pts=breaks,
                            cutoff=FALSE, r.type="pearson", nperm=999, mult="holm", progressive=TRUE
                            )


ecodist::plot(mant_cor, pval = 0.05, xlab = "Distance", ylab = "Mantel r")
plot(mant_cor,alpha=0.05)#xlab="Kilometers",ylab="Mantel correlation",  

df_mant <- as.data.frame(cbind(mant_cor$mantel.res[,1:5]))
df_mant$sig <- ifelse(df_mant[,5]>0.05,"p > 0.05","p < 0.05")

(p <- ggplot(df_mant, aes(x=df_mant[,1], y=df_mant[,3])) +
  geom_point(size=3, shape=15, aes(colour=as.factor(df_mant[,6])))+
  xlab("Kilometers")+
  scale_x_continuous(breaks= seq(0,100,by=10), 
                     labels= c(seq(0,90,by=10), TeX("$\\geq 100~km$")), expand = c(0.13,0))+
  ylab("Mantel correlation")+
  geom_hline(yintercept = 0)+
  ylim(-0.3,0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position=c(.9, .9))+
  guides(colour=guide_legend(title=" "))+
  scale_color_manual(values=c("grey","black")))

ggsave("./figures/Mantel.jpg",plot=p,device = "jpg",
       width=5, height=3.5, units="in",dpi=600)