library(reprtree)

day_Ek    <- 34
day_PBmax <- 24

##--extract best fitting model--########
mod_Ek    <- FITS[["reg"]][["Ek"]][[day_Ek]]
mod_PBmax <- FITS[["reg"]][["PBmax"]][[day_PBmax]]

ss <- sample(1:500,size=6) #choose six random trees from the ensemble of 500

pdf('plots/trees_Ek.pdf',height=12,width=20)
par(mfrow=c(2,3),cex=1.25,mar=c(1,1,1,1))
for(i in 1:6){
  reprtree:::plot.getTree(mod_Ek,k=ss[i],depth=4,digits=5)
}
dev.off()

pdf('plots/trees_PBmax.pdf',height=12,width=20)
par(mfrow=c(2,3),cex=1.25,mar=c(1,1,1,1))
for(i in 1:6){
  reprtree:::plot.getTree(mod_PBmax,k=ss[i],depth=4,digits=5)
}
dev.off()
