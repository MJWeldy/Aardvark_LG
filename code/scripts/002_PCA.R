library(ade4)
library(factoextra)
library(adegenet)
set.seed(12)

pca<-dudi.pca(for_pca,center = TRUE, scale = FALSE, scann = FALSE, nf = 30) #30 axes

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

fviz_eig(pca)
fviz_pca_ind(pca,
             #col.ind = "cos2", # Color by the quality of representation
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             habillage = df$Population,
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                axes = c(1,2)
)


fviz_pca_biplot(pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                axes = c(2,3)
)

fviz_pca_biplot(pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                axes = c(1,3)
)

grp <- find.clusters(for_pca, method="kmeans",stat="BIC",n.pca = 30, n.clust = 3, n.iter=1e9,n.start=200, dudi=pca)



par(mfcol=c(3,1))
s.class(pca$li, fac=grp$grp,xax=1, yax=2,
        col=transp(funky(15),.6),
        axesel=FALSE, cstar=0, cpoint=3)
add.scatter.eig(pca$eig[1:30],3,1,2, ratio=.3)
s.class(pca$li, fac=grp$grp,xax=2, yax=3,
        col=transp(funky(15),.6),
        axesel=FALSE, cstar=0, cpoint=3)
add.scatter.eig(pca$eig[1:30],3,2,3, ratio=.3)
s.class(pca$li, fac=grp$grp,xax=1, yax=3,
        col=transp(funky(15),.6),
        axesel=FALSE, cstar=0, cpoint=3)
add.scatter.eig(pca$eig[1:30],3,1,3, ratio=.3)