
library(hdf5r)
library(anytime)
library(tidyverse)

dir <- "~/dropbox/working/pi_parameters/mapps_forest/upstream_files" #github repo

mean_files <- file.path(dir,list.files(dir)[grep('mean',list.files(dir))])
sd_files   <- file.path(dir,list.files(dir)[grep('std',list.files(dir))])
count_files<- file.path(dir,list.files(dir)[grep('count',list.files(dir))])
