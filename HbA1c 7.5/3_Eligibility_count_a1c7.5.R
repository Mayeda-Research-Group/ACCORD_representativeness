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

## Sanity check for HbA1c
table(dm_elg_pre$HbA1c_abv11, dm_elg_pre$HbA1c_blw7.5, exclude = NULL)
table(dm_elg_pre$A2_weak_incld, exclude = NULL)

# Exclude those aged under 40 (Add HbA1c >= 7.5%)
## Variables added for cumulative proportion (Table 1)
dm_elg <- dm_elg_pre[(RIDAGEYR>=40 & LBXGH>=7.5),] %>%
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
              B5_weak_excld==0 & B6_weak_excld==0 & B7_weak_excld==0 & B10_weak_excld==0))
dim(dm_elg) # 259*119

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
nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0), ]) # 88
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
table(dm_elg$RIAGENDR, exclude = NULL) #1/2 = 135/124
## Weighted
svytable(~RIAGENDR, NHANES_svy) #5194444.045/4140308.005 
svytable(~RIAGENDR, NHANES_svy) %>% sum()
svymean(~RIAGENDR, NHANES_svy)

# Number of people with diabetes who meet the eligibility criteria
# 'weak_inclusion_met==1' AND 'weak_exclusion_met==0'
## Unweighted
nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),]) # 88
# nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),])/nrow(dm_elg) 
# 1 - nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),])/nrow(dm_elg)
## Weighted
svytable(~eligible, NHANES_svy)
svytable(~eligible, NHANES_svy) %>% sum()
svytable(~eligible, NHANES_svy) %>% prop.table()
svyciprop(~eligible, NHANES_svy) # 0.385758 (95% CI: 0.277908 0.50613)

Overall <- data.frame(Criterion = "Overall",
                      N = nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),]),
                      Weighted_prop = 100*svyciprop(~eligible, NHANES_svy)[[1]],
                      CI = paste0("(27.8, 50.6)"))


# Among those who met HbA1c level (HbA1c_elg == T)
## Weighted
getSummary(~eligible, ~HbA1c_elg, NHANES_svy) # TRUE -> 0/0.4409805341

## CVD factors (A4 and A5) (among those with HbA1c_elg==TRUE) (added on Jan 16)
svytable(~A4_weak_incld + A5_weak_incld + HbA1c_elg, NHANES_svy) 
#excluded (A4_weak_incld==0 OR A5_weak_incld==0) -> 465286.87212 + 2089805.84776 + 398964.65481
100*(465286.87212 + 2089805.84776 + 398964.65481)/sum(svytable(~eligible, NHANES_svy)) # 31.64580439

## Age and CVD history (A4) (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + A4_weak_incld, NHANES_svy) #included/excluded -> 5610697.196/2555092.720
100*2555092.720/sum(svytable(~eligible, NHANES_svy)) # 27.37183276

## Risk for CVD events (A5) (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + A5_weak_incld, NHANES_svy) #included/excluded -> 7301538.3892/864251.5269
100*864251.5269/sum(svytable(~eligible, NHANES_svy)) # 9.258430457

## Eligibility of ACCORD-BP/ACCOD-lipid  (A6) (among those with elg_A2toA5==TRUE)
# ## Eligibility of ACCORD-BP/ACCOD-lipid  (A6) (among those with HbA1c_elg==TRUE)
# svytable(~HbA1c_elg + A6_weak_incld, NHANES_svy) #included/excluded -> 7109553.843/1056236.073
# 100*1056236.073/sum(svytable(~eligible, NHANES_svy)) # 11.31509511
svytable(~elg_A2toA5 + A6_weak_incld, NHANES_svy) #included/excluded -> 4931606.6869/280125.8546
100*280125.8546/sum(svytable(~eligible, NHANES_svy)) # 3.00089229

## Exclusion criteria (among those with elg_A2toA5==TRUE)
# ## Exclusion criteria (among those with HbA1c_elg==TRUE)
# svytable(~HbA1c_elg + weak_exclusion_met, NHANES_svy) #included/excluded -> 6431214.707/1734575.210
# 100*1734575.210/sum(svytable(~eligible, NHANES_svy)) # 18.58190984
svytable(~elg_A2toA5 + weak_exclusion_met, NHANES_svy) #included/excluded -> 3811063.125/1400669.416
100*1400669.416/sum(svytable(~eligible, NHANES_svy)) # 15.00489149

## BMI >= 45 (B5) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B5_weak_excld, NHANES_svy) #included/excluded -> 4837957.5581/373774.9834
100*373774.9834/sum(svytable(~eligible, NHANES_svy)) # 4.004123317

## sCr >= 1.5 (B6) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B6_weak_excld, NHANES_svy) #included/excluded -> 4912611.5103/299121.0312
100*299121.0312/sum(svytable(~eligible, NHANES_svy)) # 3.204381108

## Active liver disease (B7) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B7_weak_excld, NHANES_svy) #included/excluded -> 4884156.3618/327576.1796
100*327576.1796/sum(svytable(~eligible, NHANES_svy)) # 3.509211362

## Heart failure (B10) (among those with elg_A2toA5==TRUE)
svytable(~elg_A2toA5 + B10_weak_excld, NHANES_svy) #included/excluded -> 4535316.0319/676416.5096
100*676416.5096/sum(svytable(~eligible, NHANES_svy)) # 7.246218281

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
nrow(dm_elg[LBXGH < 7.5,]) #0
table(dm_elg$HbA1c_blw7.5, exclude = NULL)
nrow(dm_elg[LBXGH > 11.0,]) #35
table(dm_elg$HbA1c_abv11, exclude = NULL)
### Weighted
svytable(~A2_weak_incld, NHANES_svy)
svytable(~A2_weak_incld, NHANES_svy) %>% sum()
svytable(~A2_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A2_weak_incld, NHANES_svy) # 0.87477309

svytable(~HbA1c_blw7.5, NHANES_svy)
svytable(~HbA1c_blw7.5, NHANES_svy) %>% prop.table()
svytable(~HbA1c_abv11, NHANES_svy)
svytable(~HbA1c_abv11, NHANES_svy) %>% prop.table() # 0.1252269078
svytable(~HbA1c_abv11+HbA1c_blw7.5, NHANES_svy)
svytable(~A2_weak_incld, NHANES_svy)

svyciprop(~A2_weak_incld, NHANES_svy) # 0.874773 (95% CI: 0.777898 0.93303)

A2_inc <- data.frame(Criterion = "A2",
                     N = nrow(dm_elg[A2_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A2_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(77.8, 93.3)"))

## A4: Age and a history of clinical CVD
### Unweighted
nrow(dm_elg[A4_weak_incld==1,]) # 174
### Weighted
svytable(~A4_weak_incld, NHANES_svy)
# svytable(~A4_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A4_weak_incld, NHANES_svy) # 0.67279964

svyciprop(~A4_weak_incld, NHANES_svy) # 0.67279964 (95% CI: 0.551005 0.77504)

A4_inc <- data.frame(Criterion = "A4",
                     N = nrow(dm_elg[A4_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A4_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(55.1, 77.5)"))

## A5: CVD risk
### Unweighted
nrow(dm_elg[A5_weak_incld==1,]) # 230
### Weighted
svytable(~A5_weak_incld, NHANES_svy)
# svytable(~A5_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A5_weak_incld, NHANES_svy) # 0.89143548

svyciprop(~A5_weak_incld, NHANES_svy) # 0.89143548 (95% CI: 0.831325 0.93188)

A5_inc <- data.frame(Criterion = "A5",
                     N = nrow(dm_elg[A5_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A5_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(83.1, 93.2)"))

## A5-a: History of clinical CVD
### Unweighted
nrow(dm_elg[A5_weak_compA==1,]) # 64
### Weighted
svytable(~A5_weak_compA, NHANES_svy)
# svytable(~A5_weak_compA, NHANES_svy) %>% prop.table()
svymean(~A5_weak_compA, NHANES_svy) # 0.24228257

svyciprop(~A5_weak_compA, NHANES_svy) # 0.24228257 (95% CI: 0.173054 0.32821)

A5_compA_inc <- data.frame(Criterion = "A5_A",
                           N = nrow(dm_elg[A5_weak_compA==1,]),
                           Weighted_prop = 100*svyciprop(~A5_weak_compA, NHANES_svy)[[1]],
                           CI = paste0("(17.3, 32.8)"))

## A5-c: Risk factors of CVD
### Unweighted
nrow(dm_elg[A5_weak_compC==1,]) # 226
### Weighted
svytable(~A5_weak_compC, NHANES_svy)
# svytable(~A5_weak_compC, NHANES_svy) %>% prop.table()
svymean(~A5_weak_compC, NHANES_svy) # 0.88213568

svyciprop(~A5_weak_compC, NHANES_svy) # 0.88213568 (95% CI: 0.820339 0.92463)

A5_compC_inc <- data.frame(Criterion = "A5_C",
                           N = nrow(dm_elg[A5_weak_compC==1,]),
                           Weighted_prop = 100*svyciprop(~A5_weak_compC, NHANES_svy)[[1]],
                           CI = paste0("(82.0, 92.5)"))

## A6: Eligibility for ACCOR-BP or ACCORD-lipid
### Unweighted
nrow(dm_elg[A6_weak_incld==1,]) # 213
### Weighted
svytable(~A6_weak_incld, NHANES_svy)
# svytable(~A6_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A6_weak_incld, NHANES_svy) # 0.84211656

svyciprop(~A6_weak_incld, NHANES_svy) # 0.84211656 (95% CI: 0.782888 0.88751)

A6_inc <- data.frame(Criterion = "A6",
                     N = nrow(dm_elg[A6_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A6_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(78.3, 88.8)"))

# Exclusion criteria (How many are excluded by the exclusion criteria)
## Overall ('weak_exclusion_met==1')
### Unweighted
nrow(dm_elg[weak_exclusion_met==1,]) # 56
nrow(dm_elg[weak_exclusion_met==1,])/nrow(dm_elg) 
### Weighted
svytable(~weak_exclusion_met, NHANES_svy)
# svytable(~weak_exclusion_met, NHANES_svy) %>% prop.table()
svymean(~weak_exclusion_met, NHANES_svy) # 0.20913609

## B1-4/B8-9/B11-17/B19: NA (Assumed that all the sample did NOT meet these criteria)
## B5: BMI
### Unweighted
nrow(dm_elg[B5_weak_excld==1,]) # 12
# nrow(dm_elg[B5_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B5_weak_excld, NHANES_svy)
# svytable(~B5_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B5_weak_excld, NHANES_svy) # 0.048583802

svyciprop(~B5_weak_excld, NHANES_svy) # 0.048583802 (95% CI: 0.0239191 0.09618)

B5_exc <- data.frame(Criterion = "B5",
                     N = nrow(dm_elg[B5_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B5_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(2.4, 9.6)"))

## B6: sCr
nrow(dm_elg[B6_weak_excld==1,]) # 17
# nrow(dm_elg[B6_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B6_weak_excld, NHANES_svy)
# svytable(~B6_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B6_weak_excld, NHANES_svy) # 0.054264577

svyciprop(~B6_weak_excld, NHANES_svy) # 0.054264577 (95% CI: 0.0280202 0.1025)

B6_exc <- data.frame(Criterion = "B6",
                     N = nrow(dm_elg[B6_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B6_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(2.8, 10.3)"))

## B7: Liver function
nrow(dm_elg[B7_weak_excld==1,]) # 17
# nrow(dm_elg[B7_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B7_weak_excld, NHANES_svy)
# svytable(~B7_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B7_weak_excld, NHANES_svy) # 0.053519091

svyciprop(~B7_weak_excld, NHANES_svy) # 0.053519091 (95% CI: 0.0262504 0.10603)

B7_exc <- data.frame(Criterion = "B7",
                     N = nrow(dm_elg[B7_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B7_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(2.6, 10.6)"))

## B10: Heart failure
nrow(dm_elg[B10_weak_excld==1,]) # 18
# nrow(dm_elg[B10_weak_excld==1,])/nrow(dm_elg) # 0.0XXX
### Weighted
svytable(~B10_weak_excld, NHANES_svy)
# svytable(~B10_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B10_weak_excld, NHANES_svy) # 0.088245479

svyciprop(~B10_weak_excld, NHANES_svy) # 0.088245479 (95% CI: 0.0403791 0.18209)

B10_exc <- data.frame(Criterion = "B10",
                     N = nrow(dm_elg[B10_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B10_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(4.0, 18.2)"))

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
nrow(dm_elg[A6_weak_lipid==1,]) # 173
# nrow(dm_elg[A6_weak_lipid==1,])/nrow(dm_elg) 
### Weighted
svytable(~A6_weak_lipid, NHANES_svy)
# svytable(~A6_weak_lipid, NHANES_svy) %>% prop.table()
svymean(~A6_weak_lipid, NHANES_svy) # 0.65226561

svyciprop(~A6_weak_lipid, NHANES_svy) # 0.65226561 (95% CI: 0.573292 0.72367)

A6_lipid <- data.frame(Criterion = "A6_lipid",
                      N = nrow(dm_elg[A6_weak_lipid==1,]),
                      Weighted_prop = 100*svyciprop(~A6_weak_lipid, NHANES_svy)[[1]],
                      CI = paste0("(57.3, 72.4)"))

## D1: ACCORD-BP
### Unweighted
nrow(dm_elg[A6_weak_bp_comp1==1,]) # 115
# nrow(dm_elg[A6_weak_bp_comp1==1,])/nrow(dm_elg) 
### Weighted
svytable(~A6_weak_bp_comp1, NHANES_svy)
# svytable(~A6_weak_bp_comp1, NHANES_svy) %>% prop.table()
svymean(~A6_weak_bp_comp1, NHANES_svy) # 0.39889862

svyciprop(~A6_weak_bp_comp1, NHANES_svy) # 0.39889862 (95% CI: 0.317967 0.48576)

A6_bp <- data.frame(Criterion = "A6_bp",
                       N = nrow(dm_elg[A6_weak_bp_comp1==1,]),
                       Weighted_prop = 100*svyciprop(~A6_weak_bp_comp1, NHANES_svy)[[1]],
                       CI = paste0("(31.8, 48.6)"))

# Cumulative proportion for Table 1
## A2 (HbA1c)
## A2 and A4 (HbA1c and CVD + age)
# table(dm_elg$elg_A2andA4) #153
svymean(~elg_A2andA4, NHANES_svy) # TRUE: 0.60105476
svyciprop(~elg_A2andA4, NHANES_svy) # 0.60105476 (95% CI: 0.485019 0.70675)

A2andA4 <- data.frame(Criterion = "A2andA4",
                    N = nrow(dm_elg[elg_A2andA4==1,]),
                    Weighted_prop = 100*svyciprop(~elg_A2andA4, NHANES_svy)[[1]],
                    CI = paste0("(48.5, 70.7)"))

## A2, A4, and A5 (HbA1c, CVD + age, and CVD risks)
# table(dm_elg$elg_A2toA5) #140
svymean(~elg_A2toA5, NHANES_svy) # TRUE: 0.55831505
svyciprop(~elg_A2toA5, NHANES_svy) # 0.55831505 (95% CI: 0.447550 0.66357)

A2toA5 <- data.frame(Criterion = "A2toA5",
                      N = nrow(dm_elg[elg_A2toA5==1,]),
                      Weighted_prop = 100*svyciprop(~elg_A2toA5, NHANES_svy)[[1]],
                      CI = paste0("(44.8, 66.4)"))

## A2, A4, A5, and B5 (HbA1c, CVD + age, CVD risks, and BMI >= 45)
# table(dm_elg$elg_A2toA5andB5) #131
svymean(~elg_A2toA5andB5, NHANES_svy) # TRUE: 0.51827382
svyciprop(~elg_A2toA5andB5, NHANES_svy) # 0.51827382 (95% CI: 0.405417 0.6293)

A2toA5andB5 <- data.frame(Criterion = "A2toA5andB5",
                     N = nrow(dm_elg[elg_A2toA5andB5==1,]),
                     Weighted_prop = 100*svyciprop(~elg_A2toA5andB5, NHANES_svy)[[1]],
                     CI = paste0("(40.5, 62.9)"))

## A2, A4, A5, B5, and B6 (HbA1c, CVD + age, CVD risks, BMI >= 45, and sCr >= 1.5)
# table(dm_elg$elg_A2toA5andB5toB6) #120
svymean(~elg_A2toA5andB5toB6, NHANES_svy) # TRUE: 0.48941624
svyciprop(~elg_A2toA5andB5toB6, NHANES_svy) # 0.48941624 (95% CI: 0.387026 0.5927)

A2toA5andB5toB6 <- data.frame(Criterion = "A2toA5andB5toB6",
                          N = nrow(dm_elg[elg_A2toA5andB5toB6==1,]),
                          Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB6, NHANES_svy)[[1]],
                          CI = paste0("(38.7, 59.3)"))

## A2, A4, A5, B5, B6, and B7 (HbA1c, CVD + age, CVD risks, BMI >= 45, sCr >= 1.5, and acute liver disease)
# table(dm_elg$elg_A2toA5andB5toB7) #110
svymean(~elg_A2toA5andB5toB7, NHANES_svy) # TRUE: 0.46876849
svyciprop(~elg_A2toA5andB5toB7, NHANES_svy) # 0.46876849 (95% CI: 0.366153 0.5741)

A2toA5andB5toB7 <- data.frame(Criterion = "A2toA5andB5toB7",
                              N = nrow(dm_elg[elg_A2toA5andB5toB7==1,]),
                              Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB7, NHANES_svy)[[1]],
                              CI = paste0("(36.6, 57.4)"))

## A2, A4, A5, B5, B6, B7, and B10 (HbA1c, CVD + age, CVD risks, BMI >= 45, sCr >= 1.5, acute liver disease, and heart failure)
# table(dm_elg$elg_A2toA5andB5toB10) #99
svymean(~elg_A2toA5andB5toB10, NHANES_svy) # TRUE: 0.40826613
svyciprop(~elg_A2toA5andB5toB10, NHANES_svy) # 0.40826613 (95% CI: 0.299743 0.52654)

A2toA5andB5toB10 <- data.frame(Criterion = "A2toA5andB5toB10",
                              N = nrow(dm_elg[elg_A2toA5andB5toB10==1,]),
                              Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB10, NHANES_svy)[[1]],
                              CI = paste0("(30.0, 52.7)"))

## All operational eligibility criteria
# table(dm_elg$eligible) #88
svymean(~eligible, NHANES_svy) # TRUE: 0.38575791

# Save the outputs
elg_count <- rbind.data.frame(Overall, A2_inc, A4_inc, A5_inc,
                              A5_compA_inc, A5_compC_inc,
                              A6_inc, A6_bp, A6_lipid,
                              B5_exc, B6_exc, B7_exc, B10_exc, B18_exc,
                              A2andA4, A2toA5, A2toA5andB5, A2toA5andB5toB6,
                              A2toA5andB5toB7, A2toA5andB5toB10)

# save as a csv file
write.csv(elg_count, paste0(path_to_output,"elg_count_a1c7.5_20240115.csv"))