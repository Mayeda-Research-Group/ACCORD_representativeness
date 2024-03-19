# Input
# Data of people with diabetes and eligibility status:
# data_eligibility_20240102.csv
# R4.1.3

# Objective of this file
# Count the number of people with (self-reported) diabetes aged 40+
# who meet each eligibility criterion

# Models
# NA
## Sample Rcode: "Sample_Rcode_weighted_proportion.R"
## https://wwwn.cdc.gov/nchs/nhanes/tutorials/samplecode.aspx

rm(list = ls())
options(digits = 10)

# Load packages
library(data.table)
library(dplyr)
library(survey)

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) { 
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

# Set up paths
path_to_data <- "XX" # To be changed
path_to_output <- "XX" # To be changed

# Load the id list of people with (type 2) diabetes age 20+
dm_elg_pre <- read.csv(paste0(path_to_data,"data_eligibility_20240102.csv"))
str(dm_elg_pre)
dm_elg_pre <- dm_elg_pre %>% select(!starts_with("X")) %>% as.data.table()
str(dm_elg_pre)

# Total number of people with (type 2) diabetes age 20+
dim(dm_elg_pre) # 926*107
table(dm_elg_pre$DIQ010, exclude = NULL) # Self-reported DM: 1/2/3 = Yes/No/Borderline

# Age distribution
summary(dm_elg_pre$RIDAGEYR)
hist(dm_elg_pre$RIDAGEYR, xlab = "Age", main = "Age of people with diabetes")

## Flag age < 40 
dm_elg_pre[RIDAGEYR < 40, age_below40:=1]
dm_elg_pre[is.na(age_below40), age_below40:=0]
table(dm_elg_pre$age_below40, exclude = NULL)

# HbA1c distribution (dm_elg_pre)
summary(dm_elg_pre$LBXGH)
hist(dm_elg_pre$LBXGH, xlab = "HbA1c (%)", main = "HbA1c of people with diabetes")
## Flag to show HbA1c < 7.5% or >11%
dm_elg_pre[LBXGH < 7.5, HbA1c_blw7.5:=1]
dm_elg_pre[is.na(HbA1c_blw7.5), HbA1c_blw7.5:=0]
table(dm_elg_pre$HbA1c_blw7.5, exclude = NULL)
dm_elg_pre[LBXGH > 11.0, HbA1c_abv11:=1]
dm_elg_pre[is.na(HbA1c_abv11), HbA1c_abv11:=0]
table(dm_elg_pre$HbA1c_abv11, exclude = NULL)

dm_elg_pre[LBXGH < 6.0,] %>% nrow() # 163

## Sanity check for HbA1c
table(dm_elg_pre$HbA1c_abv11, dm_elg_pre$HbA1c_blw7.5, exclude = NULL)
table(dm_elg_pre$A2_weak_incld, exclude = NULL)

# Exclude those aged under 40 
## Variables added for cumulative proportion (Table 1) 
dm_elg <- dm_elg_pre[(RIDAGEYR>=40 & LBXGH>=6.0),] %>%
  mutate(., one = 1,
         HbA1c_elg = (A2_weak_incld==1),
         eligible = (weak_inclusion_met==1 & weak_exclusion_met==0),
         elg_A2andA4 = (A2_weak_incld==1 & A4_weak_incld==1),
         elg_A2toA5 = (A2_weak_incld==1 & A4_weak_incld==1 & A5_weak_incld==1),
         elg_A2toA5andB5 = 
           (A2_weak_incld==1 & A4_weak_incld==1 & A5_weak_incld==1 & 
              B5_weak_excld==0),
         elg_A2toA5andB5toB6 = 
           (A2_weak_incld==1 & A4_weak_incld==1 & A5_weak_incld==1 &
              B5_weak_excld==0 & B6_weak_excld==0),
         elg_A2toA5andB5toB7 = 
           (A2_weak_incld==1 & A4_weak_incld==1 & A5_weak_incld==1 &
              B5_weak_excld==0 & B6_weak_excld==0 & B7_weak_excld==0),
         elg_A2toA5andB5toB10 = 
           (A2_weak_incld==1 & A4_weak_incld==1 & A5_weak_incld==1 & 
              B5_weak_excld==0 & B6_weak_excld==0 & B7_weak_excld==0 & 
              B10_weak_excld==0))
dim(dm_elg) # 715*119
856 - nrow(dm_elg) # 141 (the number of those excluded because HbA1c < 6.0%)

# HbA1c distribution (dm_elg)
summary(dm_elg$LBXGH)
hist(dm_elg$LBXGH, xlab = "HbA1c (%)", main = "HbA1c of people with diabetes")
## Flag to show HbA1c < 7.5% or >11%
dm_elg[LBXGH < 7.5, HbA1c_blw7.5:=1]
dm_elg[is.na(HbA1c_blw7.5), HbA1c_blw7.5:=0]
table(dm_elg$HbA1c_blw7.5, exclude = NULL)
dm_elg[LBXGH > 11.0, HbA1c_abv11:=1]
dm_elg[is.na(HbA1c_abv11), HbA1c_abv11:=0]
table(dm_elg$HbA1c_abv11, exclude = NULL)

## Sanity check for HbA1c (dm_elg)
table(dm_elg$HbA1c_abv11, dm_elg$HbA1c_blw7.5, exclude = NULL)
table(dm_elg$A2_weak_incld, exclude = NULL)

# Count eligible sample
nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0), ])
table(dm_elg$eligible, exclude = NULL)

## Define survey design 
# Define survey design for overall dataset
NHANES_svy_pre <- svydesign(data = dm_elg_pre, 
                        ids = ~SDMVPSU,
                        strata = ~SDMVSTRA,
                        weights = ~WTSAFPRP,
                        nest = TRUE)

NHANES_svy <- svydesign(data = dm_elg,
                        ids = ~SDMVPSU,
                        strata = ~SDMVSTRA,
                        weights = ~WTSAFPRP,
                        nest = TRUE)

## Analysis
# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count) 
  p <- svyby(varformula, byformula, design, svymean) 
  outSum <- left_join(select(c,-se), p) 
  outSum
}

# Age distribution
summary(dm_elg$RIDAGEYR)
# sd(dm_elg$RIDAGEYR) Age was top-coded at 80 in NAHNAES

## Weighted (proportion of those aged below 40) (NHANES_svy_pre) 
svytable(~age_below40, NHANES_svy_pre) #0/1 = 35813826.785/4153417.367
svytable(~age_below40, NHANES_svy_pre) %>% sum()
100*4153417.367/sum(svytable(~age_below40, NHANES_svy_pre))

# Gender distribution 1/2 = Male/Female
## Unweighted
table(dm_elg$RIAGENDR, exclude = NULL) #1/2 = 376/339
# 339/nrow(dm_elg) 
## Weighted
svytable(~RIAGENDR, NHANES_svy) #16054657/13662749
svytable(~RIAGENDR, NHANES_svy) %>% sum()
svymean(~RIAGENDR, NHANES_svy)

# Number of people with diabetes who meet the eligibility criteria
# 'weak_inclusion_met==1' AND 'weak_exclusion_met==0'
## Unweighted
nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),]) # 88
## Weighted
svytable(~eligible, NHANES_svy)
svytable(~eligible, NHANES_svy) %>% sum()
svytable(~eligible, NHANES_svy) %>% prop.table()
svyciprop(~eligible, NHANES_svy) # 0.1211732 (95% CI: 0.0820031 0.17548)

Overall <- data.frame(Criterion = "Overall",
                      N = nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),]),
                      Weighted_prop = 100*svyciprop(~eligible, NHANES_svy)[[1]],
                      CI = paste0("(8.2, 17.5)"))

# Among those who met HbA1c level (HbA1c_elg == T)
## Numbers for Figure 1
## Weighted
getSummary(~eligible, ~HbA1c_elg, NHANES_svy) # TRUE -> 0/0.4409805341

## CVD factors (A4 and A5) (among those with HbA1c_elg==TRUE) 
svytable(~A4_weak_incld + A5_weak_incld + HbA1c_elg, NHANES_svy) 
#excluded (A4_weak_incld==0 OR A5_weak_incld==0) -> 465286.8721 + 2089805.8478 + 398964.6548
100*(465286.8721 + 2089805.8478 + 398964.6548)/sum(svytable(~eligible, NHANES_svy)) # 9.94049546

## Age and CVD history (A4) (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + A4_weak_incld, NHANES_svy) #included/excluded -> 5610697.196/2555092.720
100*2555092.720/sum(svytable(~eligible, NHANES_svy)) # 8.597966919

## Risk for CVD events (A5) (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + A5_weak_incld, NHANES_svy) #included/excluded -> 7301538.3892/864251.5269 
100*864251.5269/sum(svytable(~eligible, NHANES_svy)) # Updated on June 7 # 2.908233419

## Eligibility of ACCORD-BP/ACCOD-lipid  (A6) (among those with elg_A2toA5==TRUE)
# svytable(~HbA1c_elg + A6_weak_incld, NHANES_svy) #included/excluded -> 7109553.843/1056236.073
# 100*1056236.073/sum(svytable(~eligible, NHANES_svy)) # 3.554267422
svytable(~elg_A2toA5 + A6_weak_incld, NHANES_svy) #included/excluded -> 4931606.6869/280125.8546
100*280125.8546/sum(svytable(~eligible, NHANES_svy)) # 0.9426322623

## Exclusion criteria (among those with elg_A2toA5==TRUE)
# svytable(~HbA1c_elg + weak_exclusion_met, NHANES_svy) #included/excluded -> 6431214.707/1734575.210 
# 100*1734575.210/sum(svytable(~eligible, NHANES_svy)) # 5.836899834 
svytable(~elg_A2toA5 + weak_exclusion_met, NHANES_svy) #included/excluded -> 3811063.125/1400669.416
100*1400669.416/sum(svytable(~eligible, NHANES_svy)) # 4.713296394

## BMI >= 45 (B5) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B5_weak_excld, NHANES_svy) #included/excluded -> 4837957.5581/373774.9834
100*373774.9834/sum(svytable(~eligible, NHANES_svy)) # 1.25776451

## sCr >= 1.5 (B6) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B6_weak_excld, NHANES_svy) #included/excluded -> 4912611.5103/299121.0312
100*299121.0312/sum(svytable(~eligible, NHANES_svy)) # 1.006551626

## Active liver disease (B7) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B7_weak_excld, NHANES_svy) #included/excluded -> 4884156.3618/327576.1796
100*327576.1796/sum(svytable(~eligible, NHANES_svy)) # 1.10230409

## Heart failure (B10) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B10_weak_excld, NHANES_svy) #included/excluded -> 4535316.0319/676416.5096
100*676416.5096/sum(svytable(~eligible, NHANES_svy)) # 2.276162711


# Calculate prevalence of each eligibility condition
# Inclusion criteria (How many are included by the inclusion criteria)
## Overall ('weak_inclusion_met==1')
### Unweighted
nrow(dm_elg[weak_inclusion_met==1,]) # 124
# nrow(dm_elg[weak_inclusion_met==1,])/nrow(dm_elg) 
### Weighted
svytable(~weak_inclusion_met, NHANES_svy)
svytable(~weak_inclusion_met, NHANES_svy) %>% sum()
# svytable(~weak_inclusion_met, NHANES_svy) %>% prop.table()
svymean(~weak_inclusion_met, NHANES_svy)

## A1/A3/A5: NA (Assumed that all the sample met these criteria)
## A2: HbA1c level
### Unweighted
nrow(dm_elg[A2_weak_incld==1,]) # 224
# nrow(dm_elg[A2_weak_incld==1,])/nrow(dm_elg) 
nrow(dm_elg[LBXGH < 7.5,]) #456
table(dm_elg$HbA1c_blw7.5, exclude = NULL)
nrow(dm_elg[LBXGH > 11.0,]) #35
table(dm_elg$HbA1c_abv11, exclude = NULL)
### Weighted
svytable(~A2_weak_incld, NHANES_svy)
svytable(~A2_weak_incld, NHANES_svy) %>% sum()
svytable(~A2_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A2_weak_incld, NHANES_svy) # 0.27478138

svytable(~HbA1c_blw7.5, NHANES_svy)
svytable(~HbA1c_blw7.5, NHANES_svy) %>% prop.table() # 0.6858826739
svytable(~HbA1c_abv11, NHANES_svy)
svytable(~HbA1c_abv11, NHANES_svy) %>% prop.table() # 0.03933594144
svytable(~HbA1c_abv11+HbA1c_blw7.5, NHANES_svy)
svytable(~A2_weak_incld, NHANES_svy)

svyciprop(~A2_weak_incld, NHANES_svy) # 0.274781 (95% CI: 0.226405 0.3291)

A2_inc <- data.frame(Criterion = "A2",
                     N = nrow(dm_elg[A2_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A2_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(22.6, 32.9)"))

## A4: Age and a history of clinical CVD
### Unweighted
nrow(dm_elg[A4_weak_incld==1,]) # 499
# nrow(dm_elg[A2_weak_incld==1 & A4_weak_incld==1,])
# nrow(dm_elg[A4_weak_incld==1,])/nrow(dm_elg) # 0.XX
### Weighted
svytable(~A4_weak_incld, NHANES_svy)
# svytable(~A4_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A4_weak_incld, NHANES_svy) # 0.68295863

svyciprop(~A4_weak_incld, NHANES_svy) # 0.68295863 (95% CI: 0.623732 0.7368)

A4_inc <- data.frame(Criterion = "A4",
                     N = nrow(dm_elg[A4_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A4_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(62.4, 73.7)"))

## A5: CVD risk 
### Unweighted
nrow(dm_elg[A5_weak_incld==1,]) # 621
### Weighted
svytable(~A5_weak_incld, NHANES_svy)
# svytable(~A5_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A5_weak_incld, NHANES_svy) # 0.87927662

svyciprop(~A5_weak_incld, NHANES_svy) # 0.87927662 (95% CI: 0.828494 0.91654)

A5_inc <- data.frame(Criterion = "A5",
                     N = nrow(dm_elg[A5_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A5_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(82.8, 91.7)"))

## A5-a: History of clinical CVD
### Unweighted
nrow(dm_elg[A5_weak_compA==1,]) # 176
### Weighted
svytable(~A5_weak_compA, NHANES_svy)
# svytable(~A5_weak_compA, NHANES_svy) %>% prop.table()
svymean(~A5_weak_compA, NHANES_svy) # 0.24800565

svyciprop(~A5_weak_compA, NHANES_svy) # 0.24800565 (95% CI: 0.190753 0.31574)

A5_compA_inc <- data.frame(Criterion = "A5_A",
                           N = nrow(dm_elg[A5_weak_compA==1,]),
                           Weighted_prop = 100*svyciprop(~A5_weak_compA, NHANES_svy)[[1]],
                           CI = paste0("(19.1, 31.6)"))

## A5-c: Risk factors of CVD
### Unweighted
nrow(dm_elg[A5_weak_compC==1,]) # 608
### Weighted
svytable(~A5_weak_compC, NHANES_svy)
# svytable(~A5_weak_compC, NHANES_svy) %>% prop.table()
svymean(~A5_weak_compC, NHANES_svy) # 0.86944102

svyciprop(~A5_weak_compC, NHANES_svy) # 0.86944102 (95% CI: 0.818719 0.90757)

A5_compC_inc <- data.frame(Criterion = "A5_C",
                           N = nrow(dm_elg[A5_weak_compC==1,]),
                           Weighted_prop = 100*svyciprop(~A5_weak_compC, NHANES_svy)[[1]],
                           CI = paste0("(81.9, 90.8)"))

## A6: Eligibility for ACCOR-BP or ACCORD-lipid
### Unweighted
nrow(dm_elg[A6_weak_incld==1,]) # 590
### Weighted
svytable(~A6_weak_incld, NHANES_svy)
# svytable(~A6_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A6_weak_incld, NHANES_svy) # 0.82411539

svyciprop(~A6_weak_incld, NHANES_svy) # 0.82411539 (95% CI: 0.768869 0.86842)

A6_inc <- data.frame(Criterion = "A6",
                     N = nrow(dm_elg[A6_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A6_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(76.9, 86.8)"))

# Exclusion criteria (How many are excluded by the exclusion criteria)
## Overall ('weak_exclusion_met==1')
### Unweighted
nrow(dm_elg[weak_exclusion_met==1,]) # 149 (updated on Jan 02, 2024)
nrow(dm_elg[weak_exclusion_met==1,])/nrow(dm_elg) 
### Weighted
svytable(~weak_exclusion_met, NHANES_svy)
# svytable(~weak_exclusion_met, NHANES_svy) %>% prop.table()
svymean(~weak_exclusion_met, NHANES_svy) # 0.1778764

## B1-4/B8-9/B11-17/B19: NA (Assumed that all the sample did NOT meet these criteria)
## B5: BMI
### Unweighted
nrow(dm_elg[B5_weak_excld==1,]) # 45
# nrow(dm_elg[B5_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B5_weak_excld, NHANES_svy)
# svytable(~B5_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B5_weak_excld, NHANES_svy) # 0.061379647

svyciprop(~B5_weak_excld, NHANES_svy) # 0.061379647 (95% CI: 0.0373910 0.09917)

B5_exc <- data.frame(Criterion = "B5",
                     N = nrow(dm_elg[B5_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B5_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(3.7, 9.9)"))

## B6: sCr
nrow(dm_elg[B6_weak_excld==1,]) # 46
# nrow(dm_elg[B6_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B6_weak_excld, NHANES_svy)
# svytable(~B6_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B6_weak_excld, NHANES_svy) # 0.042986287

svyciprop(~B6_weak_excld, NHANES_svy) # 0.042986287 (95% CI: 0.0287553 0.0638)

B6_exc <- data.frame(Criterion = "B6",
                     N = nrow(dm_elg[B6_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B6_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(2.9, 6.4)"))

## B7: Liver function
nrow(dm_elg[B7_weak_excld==1,]) # 33
# nrow(dm_elg[B7_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B7_weak_excld, NHANES_svy)
# svytable(~B7_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B7_weak_excld, NHANES_svy) # 0.031404752

svyciprop(~B7_weak_excld, NHANES_svy) # 0.031404752 (95% CI: 0.0192171 0.05092)

B7_exc <- data.frame(Criterion = "B7",
                     N = nrow(dm_elg[B7_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B7_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(1.9, 5.1)"))

## B10: Heart failure
nrow(dm_elg[B10_weak_excld==1,]) # 49
# nrow(dm_elg[B10_weak_excld==1,])/nrow(dm_elg) # 0.0XXX
### Weighted
svytable(~B10_weak_excld, NHANES_svy)
# svytable(~B10_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B10_weak_excld, NHANES_svy) # 0.066239998

svyciprop(~B10_weak_excld, NHANES_svy) # 0.066239998 (95% CI: 0.0403788 0.10682)

B10_exc <- data.frame(Criterion = "B10",
                     N = nrow(dm_elg[B10_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B10_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(4.0, 10.7)"))

## B18: Current pregnancy
nrow(dm_elg[B18_weak_excld==1,]) # 0
# nrow(dm_elg[B18_weak_excld==1,])/nrow(dm_elg) # 0
### Weighted
svytable(~B18_weak_excld, NHANES_svy)
# svytable(~B18_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B18_weak_excld, NHANES_svy)

svyciprop(~B18_weak_excld, NHANES_svy) # 0 (95% CI: 0 0)

B18_exc <- data.frame(Criterion = "B18",
                      N = nrow(dm_elg[B18_weak_excld==1,]),
                      Weighted_prop = 100*svyciprop(~B18_weak_excld, NHANES_svy)[[1]],
                      CI = paste0("(0, 0)"))


## C1: ACCORD-lipid
### Unweighted
nrow(dm_elg[A6_weak_lipid==1,]) # 458
# nrow(dm_elg[A6_weak_lipid==1,])/nrow(dm_elg) 
### Weighted
svytable(~A6_weak_lipid, NHANES_svy)
# svytable(~A6_weak_lipid, NHANES_svy) %>% prop.table()
svymean(~A6_weak_lipid, NHANES_svy) # 0.63099142

svyciprop(~A6_weak_lipid, NHANES_svy) # 0.63099142 (95% CI: 0.581214 0.67813)

A6_lipid <- data.frame(Criterion = "A6_lipid",
                      N = nrow(dm_elg[A6_weak_lipid==1,]),
                      Weighted_prop = 100*svyciprop(~A6_weak_lipid, NHANES_svy)[[1]],
                      CI = paste0("(58.1, 67.8)"))

## D1: ACCORD-BP
### Unweighted
nrow(dm_elg[A6_weak_bp_comp1==1,]) # 335
# nrow(dm_elg[A6_weak_bp_comp1==1,])/nrow(dm_elg) 
### Weighted
svytable(~A6_weak_bp_comp1, NHANES_svy)
# svytable(~A6_weak_bp_comp1, NHANES_svy) %>% prop.table()
svymean(~A6_weak_bp_comp1, NHANES_svy) # 0.44491129

svyciprop(~A6_weak_bp_comp1, NHANES_svy) # 0.44491129 (95% CI: 0.384031 0.50749)

A6_bp <- data.frame(Criterion = "A6_bp",
                       N = nrow(dm_elg[A6_weak_bp_comp1==1,]),
                       Weighted_prop = 100*svyciprop(~A6_weak_bp_comp1, NHANES_svy)[[1]],
                       CI = paste0("(38.4, 50.7)"))

# Cumulative proportion for Table 1
## A2 (HbA1c)
## A2 and A4 (HbA1c and CVD + age)
# table(dm_elg$elg_A2andA4) #153
svymean(~elg_A2andA4, NHANES_svy) # TRUE: 0.18880172
svyciprop(~elg_A2andA4, NHANES_svy) # 0.188802 (95% CI: 0.137659 0.25336)

A2andA4 <- data.frame(Criterion = "A2andA4",
                    N = nrow(dm_elg[elg_A2andA4==1,]),
                    Weighted_prop = 100*svyciprop(~elg_A2andA4, NHANES_svy)[[1]],
                    CI = paste0("(13.8, 25.3)"))

## A2, A4, and A5 (HbA1c, CVD + age, and CVD risks)
# table(dm_elg$elg_A2toA5) #140
svymean(~elg_A2toA5, NHANES_svy) # TRUE: 0.17537643
svyciprop(~elg_A2toA5, NHANES_svy) # 0.175376 (95% CI: 0.126502 0.23799)

A2toA5 <- data.frame(Criterion = "A2toA5",
                      N = nrow(dm_elg[elg_A2toA5==1,]),
                      Weighted_prop = 100*svyciprop(~elg_A2toA5, NHANES_svy)[[1]],
                      CI = paste0("(12.7, 23.8)"))

## A2, A4, A5, and B5 (HbA1c, CVD + age, CVD risks, and BMI >= 45)
# table(dm_elg$elg_A2toA5andB5) #131
svymean(~elg_A2toA5andB5, NHANES_svy) # TRUE: 0.16279879
svyciprop(~elg_A2toA5andB5, NHANES_svy) # 0.162799 (95% CI: 0.115075 0.22528)

A2toA5andB5 <- data.frame(Criterion = "A2toA5andB5",
                     N = nrow(dm_elg[elg_A2toA5andB5==1,]),
                     Weighted_prop = 100*svyciprop(~elg_A2toA5andB5, NHANES_svy)[[1]],
                     CI = paste0("(11.5, 22.5)"))


## A2, A4, A5, B5, and B6 (HbA1c, CVD + age, CVD risks, BMI >= 45, and sCr >= 1.5)
# table(dm_elg$elg_A2toA5andB5toB6) #120
svymean(~elg_A2toA5andB5toB6, NHANES_svy) # TRUE: 0.15373412
svyciprop(~elg_A2toA5andB5toB6, NHANES_svy) # 0.153734 (95% CI: 0.111294 0.20856)

A2toA5andB5toB6 <- data.frame(Criterion = "A2toA5andB5toB6",
                          N = nrow(dm_elg[elg_A2toA5andB5toB6==1,]),
                          Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB6, NHANES_svy)[[1]],
                          CI = paste0("(11.1, 20.9)"))

## A2, A4, A5, B5, B6, and B7 (HbA1c, CVD + age, CVD risks, BMI >= 45, sCr >= 1.5, and acute liver disease)
# table(dm_elg$elg_A2toA5andB5toB7) #110
svymean(~elg_A2toA5andB5toB7, NHANES_svy) # TRUE: 0.14724831
svyciprop(~elg_A2toA5andB5toB7, NHANES_svy) # 0.147248 (95% CI: 0.105463 0.20185)

A2toA5andB5toB7 <- data.frame(Criterion = "A2toA5andB5toB7",
                              N = nrow(dm_elg[elg_A2toA5andB5toB7==1,]),
                              Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB7, NHANES_svy)[[1]],
                              CI = paste0("(10.5, 20.2)"))

## A2, A4, A5, B5, B6, B7, and B10 (HbA1c, CVD + age, CVD risks, BMI >= 45, sCr >= 1.5, acute liver disease, and heart failure)
# table(dm_elg$elg_A2toA5andB5toB10) #97
svymean(~elg_A2toA5andB5toB10, NHANES_svy) # TRUE: 0.12824347
svyciprop(~elg_A2toA5andB5toB10, NHANES_svy) # 0.12824347 (95% CI: 0.0887159 0.18187)

A2toA5andB5toB10 <- data.frame(Criterion = "A2toA5andB5toB10",
                              N = nrow(dm_elg[elg_A2toA5andB5toB10==1,]),
                              Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB10, NHANES_svy)[[1]],
                              CI = paste0("(8.9, 18.2)"))


## All operational eligibility criteria
# table(dm_elg$eligible) #88
svymean(~eligible, NHANES_svy) # TRUE: 0.12117324

# Save the outputs
elg_count <- rbind.data.frame(Overall, A2_inc, A4_inc, A5_inc,
                              A5_compA_inc, A5_compC_inc,
                              A6_inc, A6_bp, A6_lipid,
                              B5_exc, B6_exc, B7_exc, B10_exc, B18_exc,
                              A2andA4, A2toA5, A2toA5andB5, A2toA5andB5toB6,
                              A2toA5andB5toB7, A2toA5andB5toB10)

# save as a csv file
write.csv(elg_count, paste0(path_to_output,"elg_count_20240102.csv"))