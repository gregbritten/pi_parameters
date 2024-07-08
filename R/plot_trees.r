
library(reprtree)

par(mfrow=c(1,1))

par(mfrow=c(2,3))

mod_Ek    <- FITS[["reg"]][["Ek"]][[34]]
mod_PBmax <- FITS[["reg"]][["PBmax"]][[24]]


ss <- sample(1:500,size=6)

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
