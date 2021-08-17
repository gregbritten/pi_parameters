library(tidyverse)
library(viridis)
library(lubridate)


d <- read_csv('~/dropbox/data/Bouman_et_al_2017_A global dataset of photosynthesis-irradiance parameters for marine phytoplankton/Bouman_2017.csv', skip = 57)
colnames(d) <- c('id','chief_scientist','name','project','station','sample_id','lat','lon','depth','date','chla','alpha',
                 'pbmax','province','comment','ek')
d$month <- month(mdy(d$date))
d$year  <- year(mdy(d$date))
d$day   <- day(mdy(d$date))
d$season <- NA
  d$season[d$month%in%c(12,1,2)]   <- 'winter'
  d$season[d$month%in%c(3,4,5)]    <- 'spring'
  d$season[d$month%in%c(6,7,8)]    <- 'summer'
  d$season[d$month%in%c(9,10,11)]  <- 'fall'

plot(d$alpha,d$pbmax)

provs <- sort(unique(d$province))
nprov <- as.numeric(table(d$province))

Nprovs <- provs[nprov>200]

dtmp <- d[d$province==Nprovs[1],]

par(mfrow=c(2,2))
plot(dtmp$alpha,dtmp$pbmax)
plot(dtmp$chla,dtmp$alpha)
plot(dtmp$chla,dtmp$pbmax)

dtmp <- d[d$province%in%Nprovs,]


cols <- brewer.pal(8, "Dark2")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



ggplot(data=dtmp, aes(x=chla,y=pbmax,color=as.factor(province))) +  
       geom_point(cex=0.2) + 
       scale_colour_brewer(palette="Spectral") + 
        geom_smooth(method="loess", se=F)



ggplot(data=dtmp, aes(x=chla,y=alpha,color=as.factor(province))) +  
  geom_point(cex=0.2) + 
  scale_colour_brewer(palette="Spectral") + 
  geom_smooth(method="lm", se=F)

ggplot(data=d[d$province==15,], aes(x=chla,y=alpha,color=as.factor(season))) +  
  geom_point(cex=0.2) + 
  scale_colour_brewer(palette="Spectral") + 
  geom_smooth(method="loess", se=F)


ggplot(data=d[d$province==1,], aes(x=chla,y=alpha,color=as.factor(month))) +  
  geom_point(cex=0.2) + 
  scale_colour_brewer(palette="Spectral") + 
  geom_smooth(method="loess", se=F)


ggplot(d[d$province==15,], aes(alpha)) +
  geom_density(aes(fill=factor(season)), alpha=0.5)


ggplot(dtmp, aes(pbmax)) +
  geom_density(aes(fill=factor(season)), alpha=0.5)

ggplot(d[d$province==15,], aes(pbmax)) +
  geom_density(aes(fill=factor(season)), alpha=0.5,bw=0.8)



  
  + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")
