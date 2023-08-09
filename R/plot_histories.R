#source('r/process_csvs.r') ##--data processing--###########
source('r/plot_histories.R')

regions     <- c('scot','lab','spac','tas','ice')
vars <- c('par','sst','chl')
ylims <- list(c(-40,40),c(-5,5),c(-3,3))
var_long <- c(expression(Delta*bar('PAR')*'('*italic('t'['i'])*')'),
              expression(Delta*bar('SST')*'('*italic('t'['i'])*')'),
              expression(Delta*bar('Chl')*'('*italic('t'['i'])*')'))
lets <- c('a)','b)','c)','d)','e)','f)')
nms <- numeric(6)
for(i in 1:6) nms[i] <- paste(lets[i],region_long[i])
cols <- c(turbo(4)[c(3,4)],'dark green')


pdf('plots/histories_60.pdf',height=5,width=8)
par(mfrow=c(3,5),mar=c(0,0,0,0),oma=c(4,5,3,3),cex.axis=0.8)
f_plot_histories(D_nc,vars,60,regions)
dev.off()

pdf('plots/histories_365.pdf',height=5,width=8)
par(mfrow=c(3,5),mar=c(0,0,0,0),oma=c(4,5,3,3),cex.axis=0.8)
f_plot_histories(D_nc,vars,365,regions)
dev.off()





