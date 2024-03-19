# input
# NHANES data
# R4.1.3

# objective of this file
# Convert xpt files to csv files

# models
# NA

rm(list = ls())

# Include Foreign Package To Read SAS Transport Files
library(foreign)
library(dplyr)

# First, download the NHANES files and save it to your hard drive #
# from: https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2015 #

# Create data frame from saved XPT file
## NHANES 2001-2002
path_to_data <- "XX" # To be changed
data_paths <- list.files(path = path_to_data, full.names = T)
data_list <- lapply(data_paths, read.xport)
data_names <- list.files(path = path_to_data, full.names = F) %>% gsub(".XPT", "", ., ignore.case = TRUE) #ignore.case = T

# save as a csv file
path_to_output <- "XX" # To be changed
for (i in 1:length(data_list)) {
  write.csv(data_list[[i]], file = paste0(path_to_output, data_names[i],".csv"))
}

## NHANES 2003-2004
path_to_data <- "XX" # To be changed
data_paths <- list.files(path = path_to_data, full.names = T)
data_list <- lapply(data_paths, read.xport)
data_names <- list.files(path = path_to_data, full.names = F) %>% gsub(".XPT", "", ., ignore.case = TRUE) #ignore.case = T

# save as a csv file
path_to_output <- "XX" # To be changed
for (i in 1:length(data_list)) {
  write.csv(data_list[[i]], file = paste0(path_to_output, data_names[i],".csv"))
}

## NHANES 2005-2006
path_to_data <- "XX" # To be changed
data_paths <- list.files(path = path_to_data, full.names = T)
data_list <- lapply(data_paths, read.xport)
data_names <- list.files(path = path_to_data, full.names = F) %>% gsub(".XPT", "", ., ignore.case = TRUE) #ignore.case = T

# save as a csv file
path_to_output <- "XX" # To be changed
for (i in 1:length(data_list)) {
  write.csv(data_list[[i]], file = paste0(path_to_output, data_names[i],".csv"))
}
