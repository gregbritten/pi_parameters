
library(hdf5r)
library(anytime)
library(tidyverse)

setwd("~/dropbox/working/pi_parameters/mapps_forest/") #github repo

#file names
file_daily   <- H5File$new("indata/mapps_daily_sat.h5", mode="r+")
file_monthly <- H5File$new("indata/mapps_monthly_sat.h5", mode="r+")

#extractor functions
make_dat_daily <- function(file){
  dates <- as.numeric(file[['df']][['axis1']]$read())
  dat   <- as.data.frame(t(file[['df']][['block1_values']]$read()))
  colnames(dat)  <- file[['df']][['block1_items']]$read()
  dat2  <- as.data.frame(t(file[['df']][['block2_values']]$read()))
  colnames(dat2) <- file[['df']][['block2_items']]$read()
  cbind(dates,dat,dat2)
}
make_dat_monthly <- function(file){
  dates <- as.numeric(file[['df']][['axis1']]$read())
  dat   <- as.data.frame(t(file[['df']][['block0_values']]$read()))
  colnames(dat)  <- file[['df']][['block0_items']]$read()
  dat2  <- as.data.frame(t(file[['df']][['block1_values']]$read()))
  colnames(dat2) <- file[['df']][['block1_items']]$read()
  cbind(dates,dat,dat2)
}

#make global dataframes
d_day   <- make_dat_daily(file_daily)
d_month <- make_dat_monthly(file_monthly)

#make nwatlantic dataframes
nwa_d   <- d_day   %>% filter(lat>30 & lat<80 & lon < -30 & lon > -90) 
nwa_m   <- d_month %>% filter(lat>30 & lat<80 & lon < -30 & lon > -90) 

