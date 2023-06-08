library(tidyverse)

dir <- "~/dropbox/working/pi_parameters/mapps_forest/indata/lagged_data/" #github repo

lag_files <- list.files(dir,full.names=TRUE)

nms <- c(colnames(read.csv(mean_files[1])),'spac','tas','scot','lab','ice')

#D <- array(NA,dim=c(2293,31,30)) 
L <- list() 

for(i in 1:length(lag_files)){
  L[[i]] <- read.csv(lag_files[i]) %>% 
    mutate(region = ifelse(lat> -40 & lat<0 & lon> -150 & lon< -70,'spac',
                           ifelse(lat> -60 & lat< -40 & lon< 160 & lon>130,'tas',
                                  ifelse(lat> 30 & lat<50 & lon> -70 & lon< -40,'scot',
                                         ifelse(lat>= 50 & lat<65 & lon> -60 & lon< -40,'lab',
                                                ifelse(lat>= 58 & lat<72 & lon> -35 & lon< 0,'ice',NA))))))
}

