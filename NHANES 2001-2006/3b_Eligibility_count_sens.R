# Input
# Data of people with diabetes and eligibility status:
# data_eligibility_sens_2001to2006_20240111.csv
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
dm_elg_pre <- read.csv(paste0(path_to_data,"data_eligibility_sens_2001to2006_20240111.csv"))
dm_elg_pre <- dm_elg_pre %>% select(!starts_with("X")) %>% as.data.table()
str(dm_elg_pre)

# Total number of people with (type 2) diabetes age 20+
dim(dm_elg_pre) # 959*50
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

# Exclude those aged under 40 (Add HbA1c >= 6.0% on June 7)
## Variables for cumulative proportion (Table 1)
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
dim(dm_elg) # 680*62

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
nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0), ]) # 71
table(dm_elg$eligible, exclude = NULL)

## Define survey design 
# Define survey design for overall dataset
NHANES_svy_pre <- svydesign(data = dm_elg_pre,
                        ids = ~SDMVPSU,
                        strata = ~SDMVSTRA,
                        weights = ~WTSAF_01to06,
                        nest = TRUE)

NHANES_svy <- svydesign(data = dm_elg,
                        ids = ~SDMVPSU,
                        strata = ~SDMVSTRA,
                        weights = ~WTSAF_01to06,
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
# sd(dm_elg$RIDAGEYR) Age was top-coded at 85 in NAHNAES

## Weighted (proportion of those aged below 40) (NHANES_svy_pre)
svytable(~age_below40, NHANES_svy_pre) #0/1 = 19099569.364/2182731.722
svytable(~age_below40, NHANES_svy_pre) %>% sum()
100*2182731.722/sum(svytable(~age_below40, NHANES_svy_pre))

# Gender distribution 1/2 = Male/Female
## Unweighted
table(dm_elg$RIAGENDR, exclude = NULL) #1/2 = 344/336
# 336/nrow(dm_elg) 
## Weighted
svytable(~RIAGENDR, NHANES_svy) #7082222.983/6929461.616
svytable(~RIAGENDR, NHANES_svy) %>% sum()
svymean(~RIAGENDR, NHANES_svy)

# Number of people with diabetes who meet the eligibility criteria
# 'weak_inclusion_met==1' AND 'weak_exclusion_met==0'
## Unweighted
nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),]) # 71
# nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),])/nrow(dm_elg) 
# 1 - nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),])/nrow(dm_elg)
## Weighted
svytable(~eligible, NHANES_svy)
svytable(~eligible, NHANES_svy) %>% sum()
svytable(~eligible, NHANES_svy) %>% prop.table()
svyciprop(~eligible, NHANES_svy) # 0.0796722 (95% CI: 0.0559433 0.11227)

Overall <- data.frame(Criterion = "Overall",
                      N = nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0),]),
                      Weighted_prop = 100*svyciprop(~eligible, NHANES_svy)[[1]],
                      CI = paste0("(5.6, 11.2)"))

# Among those who met HbA1c level (HbA1c_elg == T)
## Numbers for Figure 1
## Weighted
getSummary(~eligible, ~HbA1c_elg, NHANES_svy) # TRUE -> 0/0.2932129362
## Age and CVD history (A4) (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + A4_weak_incld, NHANES_svy) #included/excluded -> 2706394.690/1100880.193
100*1100880.193/sum(svytable(~eligible, NHANES_svy)) # 7.856872492

## Risk for CVD events (A5) (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + A5_weak_incld, NHANES_svy) #included/excluded -> 3144960.1046/662314.7787
100*662314.7787/sum(svytable(~eligible, NHANES_svy)) # 4.726874731

## Eligibility of ACCORD-BP/ACCOD-lipid  (A6) (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + A6_weak_incld, NHANES_svy) #included/excluded -> 2952600.9212/854673.9621
100*854673.9621/sum(svytable(~eligible, NHANES_svy)) # 6.099723099

## Exclusion criteria (among those with HbA1c_elg==TRUE)
svytable(~HbA1c_elg + weak_exclusion_met, NHANES_svy) #included/excluded -> 2759273.292/1048001.591
100*1048001.591/sum(svytable(~eligible, NHANES_svy)) # 7.479483167

# Calculate prevalence of each eligibility condition
# Inclusion criteria (How many are included by the inclusion criteria)
## Overall ('weak_inclusion_met==1')
### Unweighted
nrow(dm_elg[weak_inclusion_met==1,]) # 102
# nrow(dm_elg[weak_inclusion_met==1,])/nrow(dm_elg) 
### Weighted
svytable(~weak_inclusion_met, NHANES_svy)
svytable(~weak_inclusion_met, NHANES_svy) %>% sum()
# svytable(~weak_inclusion_met, NHANES_svy) %>% prop.table()
svymean(~weak_inclusion_met, NHANES_svy)

## A1/A3/A5: NA (Assumed that all the sample met these criteria)
## A2: HbA1c level
### Unweighted
nrow(dm_elg[A2_weak_incld==1,]) # 204
# nrow(dm_elg[A2_weak_incld==1,])/nrow(dm_elg) 
nrow(dm_elg[LBXGH < 7.5,]) #426
table(dm_elg$HbA1c_blw7.5, exclude = NULL)
nrow(dm_elg[LBXGH > 11.0,]) #50
table(dm_elg$HbA1c_abv11, exclude = NULL)
### Weighted
svytable(~A2_weak_incld, NHANES_svy)
svytable(~A2_weak_incld, NHANES_svy) %>% sum()
svytable(~A2_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A2_weak_incld, NHANES_svy) # 0.27172142

svytable(~HbA1c_blw7.5, NHANES_svy)
svytable(~HbA1c_blw7.5, NHANES_svy) %>% prop.table() # 0.6768804167
svytable(~HbA1c_abv11, NHANES_svy)
svytable(~HbA1c_abv11, NHANES_svy) %>% prop.table() # 0.05139815994
svytable(~HbA1c_abv11+HbA1c_blw7.5, NHANES_svy)
svytable(~A2_weak_incld, NHANES_svy)

svyciprop(~A2_weak_incld, NHANES_svy) # 0.271721 (95% CI: 0.227699 0.32072)

A2_inc <- data.frame(Criterion = "A2",
                     N = nrow(dm_elg[A2_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A2_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(22.8, 32.1)"))

## A4: Age and a history of clinical CVD
### Unweighted
nrow(dm_elg[A4_weak_incld==1,]) # 456
# nrow(dm_elg[A2_weak_incld==1 & A4_weak_incld==1,])
# nrow(dm_elg[A4_weak_incld==1,])/nrow(dm_elg) # 0.XX
### Weighted
svytable(~A4_weak_incld, NHANES_svy)
# svytable(~A4_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A4_weak_incld, NHANES_svy) # 0.61706667

svyciprop(~A4_weak_incld, NHANES_svy) # 0.61706667 (95% CI: 0.549854 0.68008)

A4_inc <- data.frame(Criterion = "A4",
                     N = nrow(dm_elg[A4_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A4_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(55.0, 68.0)"))

## A5: CVD risk
### Unweighted
nrow(dm_elg[A5_weak_incld==1,]) # 576
### Weighted
svytable(~A5_weak_incld, NHANES_svy)
# svytable(~A5_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A5_weak_incld, NHANES_svy) # 0.86582747

svyciprop(~A5_weak_incld, NHANES_svy) # 0.86582747 (95% CI: 0.826811 0.89715)

A5_inc <- data.frame(Criterion = "A5",
                     N = nrow(dm_elg[A5_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A5_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(82.7, 89.7)"))

## A5-a: History of clinical CVD
### Unweighted
nrow(dm_elg[A5_weak_compA==1,]) # 171
### Weighted
svytable(~A5_weak_compA, NHANES_svy)
# svytable(~A5_weak_compA, NHANES_svy) %>% prop.table()
svymean(~A5_weak_compA, NHANES_svy) # 0.22443961

svyciprop(~A5_weak_compA, NHANES_svy) # 0.24800565 (95% CI: 0.17804 0.27883)

A5_compA_inc <- data.frame(Criterion = "A5_A",
                           N = nrow(dm_elg[A5_weak_compA==1,]),
                           Weighted_prop = 100*svyciprop(~A5_weak_compA, NHANES_svy)[[1]],
                           CI = paste0("(17.8, 27.9)"))

## A5-c: Risk factors of CVD
### Unweighted
nrow(dm_elg[A5_weak_compC==1,]) # 558
### Weighted
svytable(~A5_weak_compC, NHANES_svy)
# svytable(~A5_weak_compC, NHANES_svy) %>% prop.table()
svymean(~A5_weak_compC, NHANES_svy) # 0.84666031

svyciprop(~A5_weak_compC, NHANES_svy) # 0.84666031 (95% CI: 0.805855 0.88016)

A5_compC_inc <- data.frame(Criterion = "A5_C",
                           N = nrow(dm_elg[A5_weak_compC==1,]),
                           Weighted_prop = 100*svyciprop(~A5_weak_compC, NHANES_svy)[[1]],
                           CI = paste0("(80.6, 88.0)"))

## A6: Eligibility for ACCOR-BP or ACCORD-lipid
### Unweighted
nrow(dm_elg[A6_weak_incld==1,]) # 559
### Weighted
svytable(~A6_weak_incld, NHANES_svy)
# svytable(~A6_weak_incld, NHANES_svy) %>% prop.table()
svymean(~A6_weak_incld, NHANES_svy) # 0.78922035

svyciprop(~A6_weak_incld, NHANES_svy) # 0.78922035 (95% CI: 0.742531 0.82939)

A6_inc <- data.frame(Criterion = "A6",
                     N = nrow(dm_elg[A6_weak_incld==1,]),
                     Weighted_prop = 100*svyciprop(~A6_weak_incld, NHANES_svy)[[1]],
                     CI = paste0("(74.3, 82.9)"))

# Exclusion criteria (How many are excluded by the exclusion criteria)
## Overall ('weak_exclusion_met==1')
### Unweighted
nrow(dm_elg[weak_exclusion_met==1,]) # 153
nrow(dm_elg[weak_exclusion_met==1,])/nrow(dm_elg) 
### Weighted
svytable(~weak_exclusion_met, NHANES_svy)
# svytable(~weak_exclusion_met, NHANES_svy) %>% prop.table()
svymean(~weak_exclusion_met, NHANES_svy) # 0.20957212

## B1-4/B8-9/B11-17/B19: NA (Assumed that all the sample did NOT meet these criteria)
## B5: BMI
### Unweighted
nrow(dm_elg[B5_weak_excld==1,]) # 34
# nrow(dm_elg[B5_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B5_weak_excld, NHANES_svy)
# svytable(~B5_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B5_weak_excld, NHANES_svy) # 0.057306198

svyciprop(~B5_weak_excld, NHANES_svy) # 0.057306198 (95% CI: 0.0387234 0.08403)

B5_exc <- data.frame(Criterion = "B5",
                     N = nrow(dm_elg[B5_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B5_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(3.9, 8.4)"))

## B6: sCr
nrow(dm_elg[B6_weak_excld==1,]) # 65
# nrow(dm_elg[B6_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B6_weak_excld, NHANES_svy)
# svytable(~B6_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B6_weak_excld, NHANES_svy) # 0.064600663

svyciprop(~B6_weak_excld, NHANES_svy) # 0.064600663 (95% CI: 0.0430548 0.09585)

B6_exc <- data.frame(Criterion = "B6",
                     N = nrow(dm_elg[B6_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B6_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(4.3, 9.6)"))

## B7: Liver function
nrow(dm_elg[B7_weak_excld==1,]) # 28
# nrow(dm_elg[B7_weak_excld==1,])/nrow(dm_elg) 
### Weighted
svytable(~B7_weak_excld, NHANES_svy)
# svytable(~B7_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B7_weak_excld, NHANES_svy) # 0.059201809

svyciprop(~B7_weak_excld, NHANES_svy) # 0.059201809 (95% CI: 0.0359396 0.09602)

B7_exc <- data.frame(Criterion = "B7",
                     N = nrow(dm_elg[B7_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B7_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(3.6, 9.6)"))

## B10: Heart failure
nrow(dm_elg[B10_weak_excld==1,]) # 57
# nrow(dm_elg[B10_weak_excld==1,])/nrow(dm_elg) # 0.0XXX
### Weighted
svytable(~B10_weak_excld, NHANES_svy)
# svytable(~B10_weak_excld, NHANES_svy) %>% prop.table()
svymean(~B10_weak_excld, NHANES_svy) # 0.073505379

svyciprop(~B10_weak_excld, NHANES_svy) # 0.073505379 (95% CI: 0.0521021 0.10275)

B10_exc <- data.frame(Criterion = "B10",
                     N = nrow(dm_elg[B10_weak_excld==1,]),
                     Weighted_prop = 100*svyciprop(~B10_weak_excld, NHANES_svy)[[1]],
                     CI = paste0("(5.2, 10.3)"))

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
nrow(dm_elg[A6_weak_lipid==1,]) # 414
# nrow(dm_elg[A6_weak_lipid==1,])/nrow(dm_elg) 
### Weighted
svytable(~A6_weak_lipid, NHANES_svy)
# svytable(~A6_weak_lipid, NHANES_svy) %>% prop.table()
svymean(~A6_weak_lipid, NHANES_svy) # 0.58547139

svyciprop(~A6_weak_lipid, NHANES_svy) # 0.58547139 (95% CI: 0.523208 0.64512)

A6_lipid <- data.frame(Criterion = "A6_lipid",
                      N = nrow(dm_elg[A6_weak_lipid==1,]),
                      Weighted_prop = 100*svyciprop(~A6_weak_lipid, NHANES_svy)[[1]],
                      CI = paste0("(52.3, 64.5)"))

## D1: ACCORD-BP
### Unweighted
nrow(dm_elg[A6_weak_bp_comp1==1,]) # 366
# nrow(dm_elg[A6_weak_bp_comp1==1,])/nrow(dm_elg) 
### Weighted
svytable(~A6_weak_bp_comp1, NHANES_svy)
# svytable(~A6_weak_bp_comp1, NHANES_svy) %>% prop.table()
svymean(~A6_weak_bp_comp1, NHANES_svy) # 0.49211729

svyciprop(~A6_weak_bp_comp1, NHANES_svy) # 0.49211729 (95% CI: 0.428685 0.5558)

A6_bp <- data.frame(Criterion = "A6_bp",
                       N = nrow(dm_elg[A6_weak_bp_comp1==1,]),
                       Weighted_prop = 100*svyciprop(~A6_weak_bp_comp1, NHANES_svy)[[1]],
                       CI = paste0("(42.9, 55.6)"))

# Cumulative proportion for Table 1
## A2 (HbA1c)
## A2 and A4 (HbA1c and CVD + age)
# table(dm_elg$elg_A2andA4) #148
svymean(~elg_A2andA4, NHANES_svy) # TRUE: 0.1931527
svyciprop(~elg_A2andA4, NHANES_svy) # 0.1931527 (95% CI: 0.153572 0.24004)

A2andA4 <- data.frame(Criterion = "A2andA4",
                    N = nrow(dm_elg[elg_A2andA4==1,]),
                    Weighted_prop = 100*svyciprop(~elg_A2andA4, NHANES_svy)[[1]],
                    CI = paste0("(15.4, 24.0)"))

## A2, A4, and A5 (HbA1c, CVD + age, and CVD risks)
# table(dm_elg$elg_A2toA5) #124
svymean(~elg_A2toA5, NHANES_svy) # TRUE: 0.16175507
svyciprop(~elg_A2toA5, NHANES_svy) # 0.16175507 (95% CI: 0.125585 0.20589)

A2toA5 <- data.frame(Criterion = "A2toA5",
                      N = nrow(dm_elg[elg_A2toA5==1,]),
                      Weighted_prop = 100*svyciprop(~elg_A2toA5, NHANES_svy)[[1]],
                      CI = paste0("(12.6, 20.6)"))

## A2, A4, A5, and B5 (HbA1c, CVD + age, CVD risks, and BMI >= 45)
# table(dm_elg$elg_A2toA5andB5) #118
svymean(~elg_A2toA5andB5, NHANES_svy) # TRUE: 0.14822632
svyciprop(~elg_A2toA5andB5, NHANES_svy) # 0.14822632 (95% CI: 0.114948 0.18908)

A2toA5andB5 <- data.frame(Criterion = "A2toA5andB5",
                     N = nrow(dm_elg[elg_A2toA5andB5==1,]),
                     Weighted_prop = 100*svyciprop(~elg_A2toA5andB5, NHANES_svy)[[1]],
                     CI = paste0("(11.5, 18.9)"))

## A2, A4, A5, B5, and B6 (HbA1c, CVD + age, CVD risks, BMI >= 45, and sCr >= 1.5)
# table(dm_elg$elg_A2toA5andB5toB6) #102
svymean(~elg_A2toA5andB5toB6, NHANES_svy) # TRUE: 0.12897542
svyciprop(~elg_A2toA5andB5toB6, NHANES_svy) # 0.12897542 (95% CI: 0.0989694 0.1664)

A2toA5andB5toB6 <- data.frame(Criterion = "A2toA5andB5toB6",
                          N = nrow(dm_elg[elg_A2toA5andB5toB6==1,]),
                          Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB6, NHANES_svy)[[1]],
                          CI = paste0("(9.9, 16.6)"))

## A2, A4, A5, B5, B6, and B7 (HbA1c, CVD + age, CVD risks, BMI >= 45, sCr >= 1.5, and acute liver disease)
# table(dm_elg$elg_A2toA5andB5toB7) #97
svymean(~elg_A2toA5andB5toB7, NHANES_svy) # TRUE: 0.12531964
svyciprop(~elg_A2toA5andB5toB7, NHANES_svy) # 0.12531964 (95% CI: 0.0953751 0.16297)

A2toA5andB5toB7 <- data.frame(Criterion = "A2toA5andB5toB7",
                              N = nrow(dm_elg[elg_A2toA5andB5toB7==1,]),
                              Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB7, NHANES_svy)[[1]],
                              CI = paste0("(9.5, 16.3)"))

## A2, A4, A5, B5, B6, B7, and B10 (HbA1c, CVD + age, CVD risks, BMI >= 45, sCr >= 1.5, acute liver disease, and heart failure)
# table(dm_elg$elg_A2toA5andB5toB10) #86
svymean(~elg_A2toA5andB5toB10, NHANES_svy) # TRUE: 0.109151
svyciprop(~elg_A2toA5andB5toB10, NHANES_svy) # 0.109151 (95% CI: 0.0795588 0.14798)

A2toA5andB5toB10 <- data.frame(Criterion = "A2toA5andB5toB10",
                              N = nrow(dm_elg[elg_A2toA5andB5toB10==1,]),
                              Weighted_prop = 100*svyciprop(~elg_A2toA5andB5toB10, NHANES_svy)[[1]],
                              CI = paste0("(8.0, 14.8)"))

## All operational eligibility criteria
# table(dm_elg$eligible) #71
svymean(~eligible, NHANES_svy) # TRUE: 0.079672236

# Save the outputs
elg_count <- rbind.data.frame(Overall, A2_inc, A4_inc, A5_inc,
                              A5_compA_inc, A5_compC_inc,
                              A6_inc, A6_bp, A6_lipid,
                              B5_exc, B6_exc, B7_exc, B10_exc, B18_exc,
                              A2andA4, A2toA5, A2toA5andB5, A2toA5andB5toB6,
                              A2toA5andB5toB7, A2toA5andB5toB10)

# save as a csv file
write.csv(elg_count, paste0(path_to_output,"elg_count_sens_20240113.csv"))