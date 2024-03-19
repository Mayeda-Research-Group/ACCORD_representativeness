# Input
# NHANES data (2017-2020pre-pandemic) in csv format:
# NHANES 2017-2018 with necessary variables: 
# nhanes1720prepandemic_with_variables_20240102.csv"
# R4.1.3

# Objective of this file
# Count the number of people with type 2 diabetes (t2dm)
# who meet each eligibility criterion
# Weighting: weight for fasting sample (use fasting sample)

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

# Load the id list of people with self-reported diabetes
nhanes_pre <- read.csv(paste0(path_to_data,
                              "nhanes1720prepandemic_with_variables_20240102.csv"))
nhanes_pre <- as.data.table(nhanes_pre)
str(nhanes_pre)

table(is.na(nhanes_pre$LBXGH), is.na(nhanes_pre$LBXGLU))
nhanes_pre %>% filter(
  !is.na(nhanes_pre$LBXGH) & !is.na(nhanes_pre$LBXGLU)) %>% 
  nrow()

## Define T2DM
## Self-reported DM OR HbA1c >= 6.5% OR fasting blood glucose >= 126 mg/dL OR
## Medication use OR Insulin use
## Age >= 20 to exclude possible T1DM
## Ref: PMID: 34862259/29436384
table(nhanes_pre$DIQ010, exclude = NULL) #1/2/3/9 = Yes/No/Borderline/Don't know
table(nhanes_pre$DIQ050, exclude = NULL) #1/2/9 = Yes/No/Don't know
table(nhanes_pre$DIQ070, exclude = NULL) #1/2/9 = Yes/No/Don't know

nhanes_pre[((DIQ010==1|LBXGH >= 6.5|LBXGLU >= 126|
               DIQ050==1|DIQ070==1) & RIDAGEYR >= 20), t2dm:=1]
table(nhanes_pre$t2dm, exclude = NULL) 
nhanes_pre[is.na(t2dm), t2dm:=0]
table(nhanes_pre$t2dm, exclude = NULL) # T2DM: Yes/No = 1879/13681 

# Limit to people with t2dm
data_pre <- nhanes_pre %>% filter(t2dm==1) %>% as.data.table()
table(data_pre$DIQ010, exclude = NULL) #1/2/3/9 = Yes/No/Borderline/Don't know
table(data_pre$DIQ050, exclude = NULL) #1/2/9 = Yes/No/Don't know
table(data_pre$DIQ070, exclude = NULL) #1/2/9 = Yes/No/Don't know
summary(data_pre$LBXGH)
summary(data_pre$LBXGLU)

# Inclusion criteria
## 1 -> include 0 -> otherwise
data_pre[,A1_weak_incld:=1]
table(data_pre$A1_weak_incld,exclude = NULL)

# Exclude those with missing HbA1c or missing fasting glucose
table(is.na(data_pre$LBXGH)) # TRUE/FALSE = 167/1712
table(is.na(data_pre$LBXGLU)) # TRUE/FALSE = 952/927
table(is.na(data_pre$LBXGH), is.na(data_pre$LBXGLU))
data <- data_pre[(!is.na(data_pre$LBXGH) & !is.na(data_pre$LBXGLU)),]
table(is.na(data$LBXGH))
table(is.na(data$LBXGLU))

data[(LBXGH >= 7.5 & LBXGH <= 11.0),A2_weak_incld:=1]
data[is.na(A2_weak_incld),A2_weak_incld:=0]
table(data$A2_weak_incld,exclude = NULL)

data[,A3_weak_incld:=1]
table(data$A3_weak_incld,exclude = NULL)

## A4-age
table(is.na(data$RIDAGEYR))
data[(RIDAGEYR >= 40 & RIDAGEYR <= 54),A4_weak_age4054:=1]
data[is.na(A4_weak_age4054),A4_weak_age4054:=0]
table(data$A4_weak_age4054,exclude = NULL)

## A4-history of clinical CVD
table(data$MCQ160C, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
table(data$MCQ160D, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
table(data$MCQ160E, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
table(data$MCQ160F, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
data[(MCQ160C == 1| MCQ160D == 1| MCQ160E == 1| MCQ160F == 1), A4_weak_cvd:=1]
data[(MCQ160C == 1| MCQ160D == 1| MCQ160E == 1| MCQ160F == 1), ] %>% nrow() #221
data[is.na(A4_weak_cvd), A4_weak_cvd:=0]
table(data$A4_weak_cvd, exclude = NULL)

## A4-overall
data[((A4_weak_age4054==1 & A4_weak_cvd==1)|(RIDAGEYR >= 55 & RIDAGEYR <= 79)),
     A4_weak_incld:=1]
data[is.na(A4_weak_incld), A4_weak_incld:=0]
table(data$A4_weak_incld, exclude = NULL)

## A5: cardiovascular risk (based on part A and C)
## A5-sub: define component A and C
### component A (history of clinical CVD)
data[, A5_weak_compA:=A4_weak_cvd]
table(data$A5_weak_compA, exclude = NULL)

### component C (presence of at least 2 CVD risk factors)
summary(data$LBDLDL) # LDL-C
summary(data$LBDHDD) # HDL-C
summary(data$BPXODI1) # DBP
summary(data$BPXOSY1) # SBP

# SMQ020: Smoked at least 100 cigarettes  
table(data$SMQ020, exclude = NULL) # 1/2/7/9 = Yes/No/Refused/Don't know
# SMQ040: Do you now smoke cigarettes?  
table(data$SMQ040, exclude = NULL) # 1/2/3/NA = Every data/Some days/Not at all/Missing
xtabs(~SMQ020 + SMQ040, data = data, addNA = TRUE)
(115+30)/nrow(data) # 0.1565875 (prop. of those with current smoking)

summary(data$BMXBMI) # BMI

### mean SBP and DBP
data <- data %>% 
  mutate(meanSBP = rowMeans(.[,c("BPXOSY1", "BPXOSY2", "BPXOSY3")], na.rm=TRUE),
         meanDBP = rowMeans(.[,c("BPXODI1", "BPXODI2", "BPXODI3")], na.rm=TRUE))
summary(data$meanSBP)
summary(data$meanDBP)

### current cigarette smoking
data[(SMQ040 == 1 | SMQ040 == 2), smoking:=1]
data[is.na(smoking), smoking:=0]
table(data$smoking, exclude = NULL)
xtabs(~SMQ040 + smoking, data = data, addNA = TRUE)

### 
summary(data$LBDLDL)
data[((LBDLDL > 130 | is.na(LBDLDL))|lipid_med_count > 0), # Not exclude those with missing LDL value
     A5_weak_compC_ldl:=1] 
data[is.na(A5_weak_compC_ldl), A5_weak_compC_ldl:=0]
table(data$A5_weak_compC_ldl, exclude = NULL)

summary(data$LBDHDD)
data[(RIAGENDR == 1 & LBDHDD < 40)|(RIAGENDR == 2 & LBDHDD < 50)|
       is.na(LBDHDD), # Not exclude those with missing HDL value
     A5_weak_compC_hdl:=1]
data[is.na(A5_weak_compC_hdl), A5_weak_compC_hdl:=0]
table(data$A5_weak_compC_hdl, exclude = NULL)

summary(data$meanSBP)
summary(data$meanDBP)
data[is.na(meanSBP), meanDBP] %>% summary
data[(meanSBP > 140 | meanDBP > 95)|
       (is.na(meanSBP) & is.na(meanDBP))|
       bp_med_count > 0, # Not exclude those with missing BP values
     A5_weak_compC_bp:=1]
data[is.na(A5_weak_compC_bp), A5_weak_compC_bp:=0]
table(data$A5_weak_compC_bp, exclude = NULL)

data[(smoking == 1), A5_weak_compC_smk:=1]
data[is.na(A5_weak_compC_smk), A5_weak_compC_smk:=0]
table(data$A5_weak_compC_smk, exclude = NULL)

# data[(BMXBMI >= 32), A5_weak_compC_bmi:=1] #427
data[(BMXBMI >= 32|is.na(BMXBMI)), A5_weak_compC_bmi:=1] # Not exclude those with missing BMI values
data[is.na(A5_weak_compC_bmi), A5_weak_compC_bmi:=0]
table(data$A5_weak_compC_bmi, exclude = NULL)

data <- data %>% mutate(
  C_sum = A5_weak_compC_ldl +
    A5_weak_compC_hdl +
    A5_weak_compC_bp +
    A5_weak_compC_smk +
    A5_weak_compC_bmi)

data[(C_sum >= 2), A5_weak_compC:=1]
data[is.na(A5_weak_compC), A5_weak_compC:=0]
table(data$A5_weak_compC, exclude = NULL)

xtabs(~C_sum + A5_weak_compC, data = data, addNA = TRUE)

xtabs(~A5_weak_compA + A5_weak_compC, data = data, addNA = TRUE)

## Cardiovascular risk (based on part A and C)
data[(A5_weak_compA==1|A5_weak_compC==1), A5_weak_incld:=1]
data[is.na(A5_weak_incld), A5_weak_incld:=0]
table(data$A5_weak_incld, exclude = NULL)

## A6-lipid
### LDL
summary(data$LBDLDL)
data[(LBDLDL >= 24 & LBDLDL <= 135 & atorvastatin_count >= 1)|
       (LBDLDL >= 32 & LBDLDL <= 133 & simvastatin_count >= 1)|
       (LBDLDL >= 36 & LBDLDL <= 148 & lovastatin_count >= 1)|
       (LBDLDL >= 36 & LBDLDL <= 140 & pravastatin_count >= 1)|
       (LBDLDL >= 46 & LBDLDL <= 140 & fluvastatin_count >= 1)|
       (LBDLDL >= 25 & LBDLDL <= 108 & rosuvastatin_count >= 1)|
       (LBDLDL >= 50 & LBDLDL <= 149 & ezetimibe_count >= 1)|
       (LBDLDL >= 57 & LBDLDL <= 171 & fenofibrate_count >= 1)|
       (LBDLDL >= 54 & LBDLDL <= 162 & niacin_count >= 1)|
       (LBDLDL >= 54 & LBDLDL <= 162 & resin_count >= 1)|
       (LBDLDL >= 60 & LBDLDL <= 180 & lipid_med_count == 0)|
       is.na(LBDLDL), # Not exclude those with missing LDL value
     A6_weak_lipid_ldl:=1]
data[is.na(A6_weak_lipid_ldl), A6_weak_lipid_ldl:=0]
table(data$A6_weak_lipid_ldl, exclude = NULL)

### HDL
summary(data$LBDHDD)
summary(data$RIDRETH1)
data[(LBDHDD < 55 & (RIAGENDR == 2 | RIDRETH1 == 4))|
       (LBDHDD <= 50 & RIAGENDR != 2 & RIDRETH1 != 4)|
       is.na(LBDHDD), # Not exclude those with missing HDL value
     A6_weak_lipid_hdl:=1]
data[is.na(A6_weak_lipid_hdl), A6_weak_lipid_hdl:=0]
table(data$A6_weak_lipid_hdl, exclude = NULL)

### Triglycerides (TG)
summary(data$LBXTR)
data[(LBXTR <= 750 & lipid_med_count == 0)|
       (LBXTR <= 400 & lipid_med_count != 0)|
       is.na(LBXTR), # Not exclude those with missing TG value
     A6_weak_lipid_tg:=1]
data[is.na(A6_weak_lipid_tg), A6_weak_lipid_tg:=0]
table(data$A6_weak_lipid_tg, exclude = NULL)

### LDL + HDL + TG
data <- data %>% mutate(
  lipid_sum = A6_weak_lipid_ldl +
    A6_weak_lipid_hdl +
    A6_weak_lipid_tg)
table(data$lipid_sum)

data[(lipid_sum == 3), A6_weak_lipid:=1]
data[is.na(A6_weak_lipid), A6_weak_lipid:=0]
table(data$A6_weak_lipid, exclude = NULL)

## A6-BP
### BP criteria according to BP medication (comp1)
summary(data$meanSBP)
summary(data$bp_med_count)
data[bp_med_count >= 4, A6_weak_bp_comp1:=0]
data[is.na(A6_weak_bp_comp1) &
       ((meanSBP >= 130 & meanSBP <= 180 & (bp_med_count == 0|bp_med_count == 1))|
          (meanSBP >= 130 & meanSBP <= 170 & bp_med_count == 2)|
          (meanSBP >= 130 & meanSBP <= 160 & bp_med_count == 3)|
          is.na(meanSBP)), # Not exclude those with missing BP values
     A6_weak_bp_comp1:=1]
data[is.na(A6_weak_bp_comp1), A6_weak_bp_comp1:=0]
table(data$A6_weak_bp_comp1, exclude = NULL)

## A6: 
data[(A6_weak_lipid==1|A6_weak_bp_comp1==1), A6_weak_incld:=1]
data[is.na(A6_weak_incld), A6_weak_incld:=0]
table(data$A6_weak_incld, exclude = NULL)

## Count those who meet all the inclusion criteria (A1-6)
### A5 will be considered later--no one will be excluded due to A5 here
data[,inclusion_score_weak := A1_weak_incld + A2_weak_incld + A3_weak_incld + 
       A4_weak_incld + A5_weak_incld + A6_weak_incld]
table(data$inclusion_score_weak, exclude = NULL)
data[inclusion_score_weak==6, weak_inclusion_met := 1]
data[is.na(weak_inclusion_met), weak_inclusion_met:=0]
table(data$weak_inclusion_met, exclude = NULL)
table(data$weak_inclusion_met, data$RIDAGEYR, exclude = NULL)

# Exclusion criteria
## 1 -> exclude 0 -> otherwise

## B1/B2/B3/B4/B8/B9/B11-17/B19: skip (no one will be excluded)
for (i in c(1:4, 8, 9, 11:17, 19)) {
  eval(parse_expr(paste0("data[,B", i, "_weak_excld:=0]")))
}

# table(data$B1_weak_excld, exclude = NULL)
# table(data$B2_weak_excld, exclude = NULL)
# table(data$B3_weak_excld, exclude = NULL)
# table(data$B4_weak_excld, exclude = NULL)
# table(data$B8_weak_excld, exclude = NULL)
# table(data$B9_weak_excld, exclude = NULL)
# table(data$B11_weak_excld, exclude = NULL)
# table(data$B12_weak_excld, exclude = NULL)
# table(data$B13_weak_excld, exclude = NULL)
# table(data$B14_weak_excld, exclude = NULL)
# table(data$B15_weak_excld, exclude = NULL)
# table(data$B16_weak_excld, exclude = NULL)
# table(data$B17_weak_excld, exclude = NULL)
# table(data$B19_weak_excld, exclude = NULL)

## B5 (BMI >= 45)
data[BMXBMI >= 45, B5_weak_excld:=1] # Not exclude those with missing values
data[is.na(B5_weak_excld), B5_weak_excld:=0]
table(data$B5_weak_excld, exclude = NULL)

## B6 (sCr >= 1.5 mg/dL)
summary(data$LBXSCR)
data[LBXSCR >= 1.5,
     B6_weak_excld:=1] # Not exclude those with missing values
data[is.na(B6_weak_excld), B6_weak_excld:=0]
table(data$B6_weak_excld, exclude = NULL)

## B7 (Liver function)
table(data$MCQ160L, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
table(data$MCQ170L, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
summary(data$LBXSATSI)
summary(data$LBXSASSI)
data[((LBXSATSI >= 110 | LBXSASSI >= 96) | MCQ170L == 1),
     B7_weak_excld:=1] # Not exclude those with missing values
data[is.na(B7_weak_excld), B7_weak_excld:=0]
table(data$B7_weak_excld, exclude = NULL)

## B10 (Heart failure) (modified on Jan 02, 2024)
table(data$MCQ160B, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
table(data$CDQ010, exclude = NULL) #1/2/7/9/NA  = Yes/No/Refused/Don't know/Missing
data[((CDQ010==1) & MCQ160B == 1),
     B10_weak_excld:=1] # Not exclude those with missing values
data[is.na(B10_weak_excld), B10_weak_excld:=0]
table(data$B10_weak_excld, exclude = NULL)

## B18 (Current pregnancy)
table(data$RIDEXPRG, exclude = NULL) #1/2/3/NA  = Yes/No/Not certain/Missing
data[RIDEXPRG == 1, B18_weak_excld:=1] # Not exclude those with missing values
data[is.na(B18_weak_excld), B18_weak_excld:=0]
table(data$B18_weak_excld, exclude = NULL)

## Count those who meet at least one exclusion criterion (B1-19)
data[,exclusion_score_weak := 
       B1_weak_excld + B2_weak_excld + B3_weak_excld + 
       B4_weak_excld + B5_weak_excld + B6_weak_excld + 
       B7_weak_excld + B8_weak_excld + B9_weak_excld + 
       B10_weak_excld + B11_weak_excld + B12_weak_excld + 
       B13_weak_excld + B14_weak_excld + B15_weak_excld + 
       B16_weak_excld + B17_weak_excld + B18_weak_excld + B19_weak_excld]
table(data$exclusion_score_weak, exclude = NULL)
data[exclusion_score_weak!=0, weak_exclusion_met := 1]
data[is.na(weak_exclusion_met), weak_exclusion_met:=0]
table(data$weak_exclusion_met, exclude = NULL)
table(data$weak_exclusion_met, data$RIDAGEYR, exclude = NULL)

# save as a csv file
write.csv(data, paste0(path_to_output,"data_eligibility_20240102.csv"))
