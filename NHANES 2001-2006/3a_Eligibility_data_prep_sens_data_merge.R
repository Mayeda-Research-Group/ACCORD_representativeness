# Input
# NHANES 2001-2002/2003-2004/2005-2006 with necessary variables: 
# nhanes0*0*_sens_with_variables_20240111.csv
# R4.1.3

# Objective of this file
# Count the number of people with type 2 diabetes (t2dm)
# who meet each eligibility criterion
# Weighting: weight for fasting sample (use fasting sample)

# Define T2DM according to previous research (self-reported DM + HbA1c + fasting glucose + medication or insulin)

# models
# NA

rm(list = ls())

# Load packages
library(data.table)
library(dplyr)
library(rlang)

# Set up paths
path_to_data <- "XX" # To be changed
path_to_output <- "XX" # To be changed

# Load the datasets for each wave
## 2001-2002
data0102_pre <- read.csv(paste0(path_to_data,"data_eligibility_sens_2001_20240111.csv"))
data0102_pre <- as.data.table(data0102_pre)
str(data0102_pre)

data0102 <- data0102_pre %>% 
  select(c("SEQN", "RIDAGEYR", "RIAGENDR",
           "LBXGH", "t2dm","DIQ010",
           contains("weak_"),
           "WTSAF2YR", "SDMVPSU", "SDMVSTRA"))

## 2003-2004
data0304_pre <- read.csv(paste0(path_to_data,"data_eligibility_sens_2003_20240111.csv"))
data0304_pre <- as.data.table(data0304_pre)
str(data0304_pre)

data0304 <- data0304_pre %>% 
  select(c("SEQN", "RIDAGEYR", "RIAGENDR",
           "LBXGH", "t2dm","DIQ010",
           contains("weak_"),
           "WTSAF2YR", "SDMVPSU", "SDMVSTRA"))

## 2005-2006
data0506_pre <- read.csv(paste0(path_to_data,"data_eligibility_sens_2005_20240111.csv"))
data0506_pre <- as.data.table(data0506_pre)
str(data0506_pre)

data0506 <- data0506_pre %>% 
  select(c("SEQN", "RIDAGEYR", "RIAGENDR",
           "LBXGH", "t2dm", "DIQ010",
           contains("weak_"),
           "WTSAF2YR", "SDMVPSU", "SDMVSTRA"))

data_merged <- rbind.data.frame(data0102, data0304, data0506)

# Check to make sure there is no overlap in some variable
## SEQN
summary(data0102$SEQN)
summary(data0304$SEQN)
summary(data0506$SEQN)

## SDMVSTRA
summary(data0102$SDMVSTRA)
summary(data0304$SDMVSTRA)
summary(data0506$SDMVSTRA)

# Calculate sampling weight for the merged dataset
data_merged <- data_merged %>% 
  mutate(
    WTSAF_01to06 = WTSAF2YR/3
  ) %>% 
  select(-WTSAF2YR)

# save as a csv file
write.csv(data_merged, paste0(path_to_output,"data_eligibility_sens_2001to2006_20240111.csv"))
