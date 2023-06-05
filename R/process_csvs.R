
library(hdf5r)
library(anytime)
library(tidyverse)

dir <- "~/dropbox/working/pi_parameters/mapps_forest/upstream_files" #github repo

mean_files <- file.path(dir,list.files(dir)[grep('mean',list.files(dir))])
sd_files   <- file.path(dir,list.files(dir)[grep('std',list.files(dir))])
count_files<- file.path(dir,list.files(dir)[grep('count',list.files(dir))])

nms <- c(colnames(read.csv(mean_files[1])),'spac','tas','scot','lab','ice')

#D <- array(NA,dim=c(2293,31,30)) 
D <- list() 

for(i in 1:length(mean_files)){
  D[[i]] <- read.csv(mean_files[i])
  D[[i]]$region <- NA
  D[[i]] <- D[[i]] %>% mutate(region = ifelse(lat> -40 & lat<0 & lon> -150 & lon< -70,'spac',
                                              ifelse(lat> -60 & lat< -40 & lon< 160 & lon>130,'tas',
                                                     ifelse(lat> 30 & lat<50 & lon> -70 & lon< -40,'scot',
                                                            ifelse(lat>= 50 & lat<65 & lon> -60 & lon< -40,'lab',
                                                                   ifelse(lat>= 58 & lat<72 & lon> -35 & lon< 0,'ice',NA))))))
}


#C <- list()
#for(i in 1:length(count_files)){
#  C[[i]] <- read.csv(count_files[i])
#  D[[i]]$region <- NA
#  D[[i]] <- D[[i]] %>% mutate(region = ifelse(lat> -40 & lat<0 & lon> -150 & lon< -70,'spac',
#                                              ifelse(lat> -60 & lat< -40 & lon< 160 & lon>130,'tas',
#                                                     ifelse(lat> 30 & lat<50 & lon> -70 & lon< -40,'scot',
#                                                            ifelse(lat>= 50 & lat<65 & lon> -60 & lon< -40,'lab',
#                                                                   ifelse(lat>= 58 & lat<72 & lon> -35 & lon< 0,'ice',NA))))))
#}


