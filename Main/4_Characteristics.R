# Input
# Data of people with diabetes and eligibility status:
# data_eligibility_20240102.csv
# R4.1.3

# Objective of this file
# Compare characteristics among those who were excluded vs those who were not excluded

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
dim(dm_elg_pre) # 926*107

# # Age distribution
# summary(dm_elg_pre$RIDAGEYR)
# hist(dm_elg_pre$RIDAGEYR, xlab = "Age", main = "Age of people with diabetes")

## Flag age < 40 
dm_elg_pre[RIDAGEYR < 40, age_below40:=1]
dm_elg_pre[is.na(age_below40), age_below40:=0]
table(dm_elg_pre$age_below40, exclude = NULL)

# Exclude those aged under 40 
dm_elg <- dm_elg_pre[(RIDAGEYR>=40 & LBXGH>=6.0),] %>%
  mutate(., one = 1,
         HbA1c_elg = (A2_weak_incld==1),
         eligible = (weak_inclusion_met==1 & weak_exclusion_met==0))
dim(dm_elg) # 715*111

# Race and ethnicity 1/2/3/4/5 = Mexican American/Other Hispanic/Non-Hispanic White/Non-Hispanic Black/Other Race
## 1_non_hispanic_white <- Non-Hispanic White
## 2_non_hispanic_black <- Non-Hispanic Black
## 3_hispanic <- Mexican American/Other Hispanic
## 4_other <- Other Race
table(dm_elg$RIDRETH1, exclude = NULL)
dm_elg <- dm_elg %>% mutate(
  race_cat = case_when(
    RIDRETH1 == 3 ~ "1_non_hispanic_white",
    RIDRETH1 == 4 ~ "2_non_hospanic_black",
    RIDRETH1 == 5 ~ "4_other",
    TRUE ~ "3_hispanic"))
xtabs(~RIDRETH1 + race_cat, data = dm_elg, addNA = TRUE)

# Education 1/2/3/4/5
# = Less than 9th grade/9-11th grade (Includes 12th grade with no diploma)/
# High school graduate, GED or equivalent /Some college or AA degree/College graduate or above
table(dm_elg$DMDEDUC2, exclude = NULL)
dm_elg <- dm_elg %>% mutate(
  edu_cat = case_when(
    DMDEDUC2 == 1 ~ "1_less_than_high_school",
    DMDEDUC2 == 2 ~ "1_less_than_high_school",
    DMDEDUC2 == 3 ~ "2_high_school_graduate",
    DMDEDUC2 == 4 ~ "3_some_college",
    DMDEDUC2 == 5 ~ "4_college_graduate_or_above"))
xtabs(~DMDEDUC2 + edu_cat, data = dm_elg, addNA = TRUE)

# Self-rated health 1/2/3/4/5 (HUQ010) 
# = Excellent/Very good/Good/Fair/Poor
table(dm_elg$HUQ010, exclude = NULL)
dm_elg <- dm_elg %>% mutate(
  srh_cat = case_when(
    HUQ010 == 1 ~ "1_excellent",
    HUQ010 == 2 ~ "2_very_good",
    HUQ010 == 3 ~ "3_good",
    HUQ010 == 4 ~ "4_fair",
    HUQ010 == 5 ~ "5_poor"))
xtabs(~HUQ010 + srh_cat, data = dm_elg, addNA = TRUE)

# Health insurance 1/2/7/9 (HIQ011) 
# = Yes/No/Refused/Don't know [Question: Covered by health insurance]
table(dm_elg$HIQ011, exclude = NULL)
dm_elg <- dm_elg %>% mutate(
  isr_cat = case_when(
    HIQ011 == 2 ~ "1_No",
    TRUE ~ "2_other"))
xtabs(~HIQ011 + isr_cat, data = dm_elg, addNA = TRUE)

# Medicaid coverage 4 (HIQ032D) 
# = Covered by Medicaid [Question: Covered by Medicaid]
table(dm_elg$HIQ032D, exclude = NULL)
dm_elg <- dm_elg %>% mutate(
  medicaid_cat = case_when(
    HIQ032D == 4 ~ "1_covered",
    TRUE ~ "2_not_covered"))
xtabs(~HIQ032D + medicaid_cat, data = dm_elg, addNA = TRUE)

# Comorbidities (MCQ160B, A4_weak_cvd) 
table(dm_elg$MCQ160B, exclude = NULL)
table(dm_elg$A4_weak_cvd, exclude = NULL)
dm_elg <- dm_elg %>% mutate(
  CHD_history = case_when(
    MCQ160B == 1 ~ "1_yes",
    TRUE ~ "0_no_or_missing"))
xtabs(~MCQ160B + CHD_history, data = dm_elg, addNA = TRUE)

# Count eligible sample
nrow(dm_elg[(weak_inclusion_met==1 & weak_exclusion_met==0), ]) # 86
table(dm_elg$eligible, exclude = NULL)

# Dataset for those included and one for those excluded
data_included <- dm_elg[eligible==1,]
data_excluded <- dm_elg[eligible==0,]

## Define survey design 
NHANES_svy <- svydesign(data = dm_elg,
                        ids = ~SDMVPSU,
                        strata = ~SDMVSTRA,
                        weights = ~WTSAFPRP,
                        nest = TRUE)

# Define survey design for each dataset
NHANES_svy_included <- svydesign(data = data_included,
                        ids = ~SDMVPSU,
                        strata = ~SDMVSTRA,
                        weights = ~WTSAFPRP,
                        nest = TRUE)

NHANES_svy_excluded <- svydesign(data = data_excluded,
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

# Count unweighted and weighted sample
## Unweighted
nrow(data_included) # 88
nrow(data_excluded) # 627
## Weighted
svytable(~eligible, NHANES_svy) # FALSE/TRUE = 26116451.404/3600954.398
svytable(~eligible, NHANES_svy) %>% prop.table()

obs <- data.frame(Category = "N",
                  Total_weighted = round(sum(svytable(~eligible, NHANES_svy)), digits = 0), 
                  Inc_unweighted = nrow(data_included),
                  Inc_weighted = round(svytable(~eligible, NHANES_svy)[[2]], digits = 0),
                  Exc_unweighted = nrow(data_excluded),
                  Exc_weighted = round(svytable(~eligible, NHANES_svy)[[1]], digits = 0))

# Age distribution
## Unweighted
quantile(data_included$RIDAGEYR)
quantile(data_excluded$RIDAGEYR)

## Weighted
svyquantile(~RIDAGEYR, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)
svyquantile(~RIDAGEYR, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)
svyquantile(~RIDAGEYR, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR
svyquantile(~RIDAGEYR, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR
svyquantile(~RIDAGEYR, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR

age <- data.frame(Category = "Age",
                  Total_weighted = paste0( 
                    round(svyquantile(~RIDAGEYR, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[2], digits = 1), 
                    " [",
                    round(svyquantile(~RIDAGEYR, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[1], digits = 1), 
                    " - ",
                    round(svyquantile(~RIDAGEYR, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[3], digits = 1), 
                    "]"),
                  Inc_unweighted = paste0(round(median(data_included$RIDAGEYR), digits = 1), 
                                          " [", round(quantile(data_included$RIDAGEYR)[[2]], digits = 1),
                                          " - ", round(quantile(data_included$RIDAGEYR)[[4]], digits = 1), "]"),
                  Inc_weighted = paste0(
                    round(svyquantile(~RIDAGEYR, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[2], digits = 1), 
                                        " [",
                    round(svyquantile(~RIDAGEYR, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[1], digits = 1), 
                                        " - ",
                    round(svyquantile(~RIDAGEYR, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[3], digits = 1), 
                    "]"),
                  Exc_unweighted = paste0( round(median(data_excluded$RIDAGEYR), digits = 1), 
                                           " [", round(quantile(data_excluded$RIDAGEYR)[[2]], digits = 1),
                                           " - ", round(quantile(data_excluded$RIDAGEYR)[[4]], digits = 1), "]"),
                  Exc_weighted = paste0(
                    round(svyquantile(~RIDAGEYR, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[2], digits = 1), 
                    " [",
                    round(svyquantile(~RIDAGEYR, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[1], digits = 1), 
                    " - ",
                    round(svyquantile(~RIDAGEYR, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$RIDAGEYR[3], digits = 1), 
                    "]"))


# BMI distribution
## Unweighted
summary(data_included$BMXBMI) # 2 NAs
quantile(data_included$BMXBMI, na.rm = T)
quantile(data_excluded$BMXBMI, na.rm = T)

## Weighted
svyquantile(~BMXBMI, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)
svyquantile(~BMXBMI, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)
svyquantile(~BMXBMI, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$BMXBMI
svyquantile(~BMXBMI, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$BMXBMI
svyquantile(~BMXBMI, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$BMXBMI

BMI <- data.frame(Category = "BMI",
                  Total_weighted = paste0( 
                    round(svyquantile(~BMXBMI, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[2], digits = 1), 
                    " [",
                    round(svyquantile(~BMXBMI, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[1], digits = 1), 
                    " - ",
                    round(svyquantile(~BMXBMI, NHANES_svy, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[3], digits = 1), 
                    "]"),
                  Inc_unweighted = paste0(round(median(data_included$BMXBMI, na.rm = T), digits = 1), 
                                          " [", round(quantile(data_included$BMXBMI, na.rm = T)[[2]], digits = 1),
                                          " - ", round(quantile(data_included$BMXBMI, na.rm = T)[[4]], digits = 1), "]"),
                  Inc_weighted = paste0(
                    round(svyquantile(~BMXBMI, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[2], digits = 1), 
                    " [",
                    round(svyquantile(~BMXBMI, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[1], digits = 1), 
                    " - ",
                    round(svyquantile(~BMXBMI, NHANES_svy_included, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[3], digits = 1), 
                    "]"),
                  Exc_unweighted = paste0( round(median(data_excluded$BMXBMI, na.rm = T), digits = 1), 
                                           " [", round(quantile(data_excluded$BMXBMI, na.rm = T)[[2]], digits = 1),
                                           " - ", round(quantile(data_excluded$BMXBMI, na.rm = T)[[4]], digits = 1), "]"),
                  Exc_weighted = paste0(
                    round(svyquantile(~BMXBMI, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[2], digits = 1), 
                    " [",
                    round(svyquantile(~BMXBMI, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[1], digits = 1), 
                    " - ",
                    round(svyquantile(~BMXBMI, NHANES_svy_excluded, quantiles = c(.25,.5,.75),ci = F)$BMXBMI[3], digits = 1), 
                    "]"))

# Gender distribution 1/2 = Male/Female
## Unweighted
### Included
table(data_included$RIAGENDR, exclude = NULL) #1/2 = 45/43
table(data_included$RIAGENDR, exclude = NULL) %>% prop.table()

table(data_included$RIAGENDR, exclude = NULL)[[2]]
prop.table(table(data_included$RIAGENDR, exclude = NULL))[[2]]

### Excluded
table(data_excluded$RIAGENDR, exclude = NULL) #1/2 = 331/296
table(data_excluded$RIAGENDR, exclude = NULL) %>% prop.table()

table(data_excluded$RIAGENDR, exclude = NULL)[[2]]
prop.table(table(data_excluded$RIAGENDR, exclude = NULL))[[2]]

## Weighted
### Total
svytable(~RIAGENDR, NHANES_svy)
prop.table(svytable(~RIAGENDR, NHANES_svy))

### Included
svytable(~RIAGENDR, NHANES_svy_included)
prop.table(svytable(~RIAGENDR, NHANES_svy_included))

svytable(~RIAGENDR, NHANES_svy_included)[[2]]
prop.table(svytable(~RIAGENDR, NHANES_svy_included))[[2]]

### Excluded
svytable(~RIAGENDR, NHANES_svy_excluded)
prop.table(svytable(~RIAGENDR, NHANES_svy_excluded))

svytable(~RIAGENDR, NHANES_svy_excluded)[[2]]
prop.table(svytable(~RIAGENDR, NHANES_svy_excluded))[[2]]

female <- data.frame(Category = "Female",
                     Total_weighted = paste0(round(svytable(~RIAGENDR, NHANES_svy)[[2]], digits = 0), 
                                           " (",
                                           round(100*prop.table(svytable(~RIAGENDR, NHANES_svy))[[2]], digits = 1),
                                           ")"),
                  Inc_unweighted = paste0(round(table(data_included$RIAGENDR, exclude = NULL)[[2]], digits = 0), 
                                          " (", 
                                          round(100*prop.table(table(data_included$RIAGENDR, exclude = NULL))[[2]], digits = 1),
                                          ")"),
                  Inc_weighted = paste0(round(svytable(~RIAGENDR, NHANES_svy_included)[[2]], digits = 0), 
                                        " (",
                                        round(100*prop.table(svytable(~RIAGENDR, NHANES_svy_included))[[2]], digits = 1),
                                        ")"),
                  Exc_unweighted = paste0(round(table(data_excluded$RIAGENDR, exclude = NULL)[[2]], digits = 0), 
                                          " (", 
                                          round(100*prop.table(table(data_excluded$RIAGENDR, exclude = NULL))[[2]], digits = 1),
                                          ")"),
                  Exc_weighted = paste0(round(svytable(~RIAGENDR, NHANES_svy_excluded)[[2]], digits = 0), 
                                        " (",
                                        round(100*prop.table(svytable(~RIAGENDR, NHANES_svy_excluded))[[2]], digits = 1),
                                        ")"))


# CVD_history
## Unweighted
### Included
table(data_included$A4_weak_cvd, exclude = NULL) #0/1 = 64/24
table(data_included$A4_weak_cvd, exclude = NULL) %>% prop.table()

table(data_included$A4_weak_cvd, exclude = NULL)[[2]]
prop.table(table(data_included$A4_weak_cvd, exclude = NULL))[[2]]

### Excluded
table(data_excluded$A4_weak_cvd, exclude = NULL) #0/1 = 475/152
table(data_excluded$A4_weak_cvd, exclude = NULL) %>% prop.table()

table(data_excluded$A4_weak_cvd, exclude = NULL)[[2]]
prop.table(table(data_excluded$A4_weak_cvd, exclude = NULL))[[2]]

## Weighted
### Total
svytable(~A4_weak_cvd, NHANES_svy)
prop.table(svytable(~A4_weak_cvd, NHANES_svy))

### Included
svytable(~A4_weak_cvd, NHANES_svy_included)
prop.table(svytable(~A4_weak_cvd, NHANES_svy_included))

svytable(~A4_weak_cvd, NHANES_svy_included)[[2]]
prop.table(svytable(~A4_weak_cvd, NHANES_svy_included))[[2]]

### Excluded
svytable(~A4_weak_cvd, NHANES_svy_excluded)
prop.table(svytable(~A4_weak_cvd, NHANES_svy_excluded))

svytable(~A4_weak_cvd, NHANES_svy_excluded)[[2]]
prop.table(svytable(~A4_weak_cvd, NHANES_svy_excluded))[[2]]

CVD_history <- data.frame(Category = "CVD history",
                     Total_weighted = paste0(round(svytable(~A4_weak_cvd, NHANES_svy)[[2]], digits = 0), 
                                             " (",
                                             round(100*prop.table(svytable(~A4_weak_cvd, NHANES_svy))[[2]], digits = 1),
                                             ")"),
                     Inc_unweighted = paste0(round(table(data_included$A4_weak_cvd, exclude = NULL)[[2]], digits = 0), 
                                             " (", 
                                             round(100*prop.table(table(data_included$A4_weak_cvd, exclude = NULL))[[2]], digits = 1),
                                             ")"),
                     Inc_weighted = paste0(round(svytable(~A4_weak_cvd, NHANES_svy_included)[[2]], digits = 0), 
                                           " (",
                                           round(100*prop.table(svytable(~A4_weak_cvd, NHANES_svy_included))[[2]], digits = 1),
                                           ")"),
                     Exc_unweighted = paste0(round(table(data_excluded$A4_weak_cvd, exclude = NULL)[[2]], digits = 0), 
                                             " (", 
                                             round(100*prop.table(table(data_excluded$A4_weak_cvd, exclude = NULL))[[2]], digits = 1),
                                             ")"),
                     Exc_weighted = paste0(round(svytable(~A4_weak_cvd, NHANES_svy_excluded)[[2]], digits = 0), 
                                           " (",
                                           round(100*prop.table(svytable(~A4_weak_cvd, NHANES_svy_excluded))[[2]], digits = 1),
                                           ")"))

# CHF_history
## Unweighted
### Included
table(data_included$CHD_history, exclude = NULL) #0/1 = 83/5
table(data_included$CHD_history, exclude = NULL) %>% prop.table()

table(data_included$CHD_history, exclude = NULL)[[2]]
prop.table(table(data_included$CHD_history, exclude = NULL))[[2]]

### Excluded
table(data_excluded$CHD_history, exclude = NULL) #0/1 = 563/64
table(data_excluded$CHD_history, exclude = NULL) %>% prop.table()

table(data_excluded$CHD_history, exclude = NULL)[[2]]
prop.table(table(data_excluded$CHD_history, exclude = NULL))[[2]]

## Weighted
### Total
svytable(~CHD_history, NHANES_svy)
prop.table(svytable(~CHD_history, NHANES_svy))

### Included
svytable(~CHD_history, NHANES_svy_included)
prop.table(svytable(~CHD_history, NHANES_svy_included))

svytable(~CHD_history, NHANES_svy_included)[[2]]
prop.table(svytable(~CHD_history, NHANES_svy_included))[[2]]

### Excluded
svytable(~CHD_history, NHANES_svy_excluded)
prop.table(svytable(~CHD_history, NHANES_svy_excluded))

svytable(~CHD_history, NHANES_svy_excluded)[[2]]
prop.table(svytable(~CHD_history, NHANES_svy_excluded))[[2]]

CHF_history <- data.frame(Category = "CHF history",
                          Total_weighted = paste0(round(svytable(~CHD_history, NHANES_svy)[[2]], digits = 0), 
                                                  " (",
                                                  round(100*prop.table(svytable(~CHD_history, NHANES_svy))[[2]], digits = 1),
                                                  ")"),
                          Inc_unweighted = paste0(round(table(data_included$CHD_history, exclude = NULL)[[2]], digits = 0), 
                                                  " (", 
                                                  round(100*prop.table(table(data_included$CHD_history, exclude = NULL))[[2]], digits = 1),
                                                  ")"),
                          Inc_weighted = paste0(round(svytable(~CHD_history, NHANES_svy_included)[[2]], digits = 0), 
                                                " (",
                                                round(100*prop.table(svytable(~CHD_history, NHANES_svy_included))[[2]], digits = 1),
                                                ")"),
                          Exc_unweighted = paste0(round(table(data_excluded$CHD_history, exclude = NULL)[[2]], digits = 0), 
                                                  " (", 
                                                  round(100*prop.table(table(data_excluded$CHD_history, exclude = NULL))[[2]], digits = 1),
                                                  ")"),
                          Exc_weighted = paste0(round(svytable(~CHD_history, NHANES_svy_excluded)[[2]], digits = 0), 
                                                " (",
                                                round(100*prop.table(svytable(~CHD_history, NHANES_svy_excluded))[[2]], digits = 1),
                                                ")"))


# Health insurance status 
## Unweighted
### Included
table(data_included$isr_cat, exclude = NULL) #1/2 = 9/79
table(data_included$isr_cat, exclude = NULL) %>% prop.table()

table(data_included$isr_cat, exclude = NULL)[[1]]
prop.table(table(data_included$isr_cat, exclude = NULL))[[1]]

### Excluded
table(data_excluded$isr_cat, exclude = NULL) #1/2 = 70/557
table(data_excluded$isr_cat, exclude = NULL) %>% prop.table()

table(data_excluded$isr_cat, exclude = NULL)[[1]]
prop.table(table(data_excluded$isr_cat, exclude = NULL))[[1]]

## Weighted
### Total
svytable(~isr_cat, NHANES_svy)
prop.table(svytable(~isr_cat, NHANES_svy))

svytable(~isr_cat, NHANES_svy)[[1]]
prop.table(svytable(~isr_cat, NHANES_svy))[[1]]

### Included
svytable(~isr_cat, NHANES_svy_included)
prop.table(svytable(~isr_cat, NHANES_svy_included))

svytable(~isr_cat, NHANES_svy_included)[[1]]
prop.table(svytable(~isr_cat, NHANES_svy_included))[[1]]

### Excluded
svytable(~isr_cat, NHANES_svy_excluded)
prop.table(svytable(~isr_cat, NHANES_svy_excluded))

svytable(~isr_cat, NHANES_svy_excluded)[[1]]
prop.table(svytable(~isr_cat, NHANES_svy_excluded))[[1]]

insurance <- data.frame(Category = "No insurance",
                        Total_weighted = paste0(round(svytable(~isr_cat, NHANES_svy)[[1]], digits = 0), 
                                              " (",
                                              round(100*prop.table(svytable(~isr_cat, NHANES_svy))[[1]], digits = 1),
                                              ")"),
                     Inc_unweighted = paste0(round(table(data_included$isr_cat, exclude = NULL)[[1]], digits = 0), 
                                             " (", 
                                             round(100*prop.table(table(data_included$isr_cat, exclude = NULL))[[1]], digits = 1),
                                             ")"),
                     Inc_weighted = paste0(round(svytable(~isr_cat, NHANES_svy_included)[[1]], digits = 0), 
                                           " (",
                                           round(100*prop.table(svytable(~isr_cat, NHANES_svy_included))[[1]], digits = 1),
                                           ")"),
                     Exc_unweighted = paste0(round(table(data_excluded$isr_cat, exclude = NULL)[[1]], digits = 0), 
                                             " (", 
                                             round(100*prop.table(table(data_excluded$isr_cat, exclude = NULL))[[1]], digits = 1),
                                             ")"),
                     Exc_weighted = paste0(round(svytable(~isr_cat, NHANES_svy_excluded)[[1]], digits = 0), 
                                           " (",
                                           round(100*prop.table(svytable(~isr_cat, NHANES_svy_excluded))[[1]], digits = 1),
                                           ")"))

# Medicaid coverage 
## Unweighted
### Included
table(data_included$medicaid_cat, exclude = NULL) #1/2 = 12/76
table(data_included$medicaid_cat, exclude = NULL) %>% prop.table()

table(data_included$medicaid_cat, exclude = NULL)[[1]]
prop.table(table(data_included$medicaid_cat, exclude = NULL))[[1]]

### Excluded
table(data_excluded$medicaid_cat, exclude = NULL) #1/2 = 95/532
table(data_excluded$medicaid_cat, exclude = NULL) %>% prop.table()

table(data_excluded$medicaid_cat, exclude = NULL)[[1]]
prop.table(table(data_excluded$medicaid_cat, exclude = NULL))[[1]]

## Weighted
### Total
svytable(~medicaid_cat, NHANES_svy)
prop.table(svytable(~medicaid_cat, NHANES_svy))

svytable(~medicaid_cat, NHANES_svy)[[1]]
prop.table(svytable(~medicaid_cat, NHANES_svy))[[1]]

### Included
svytable(~medicaid_cat, NHANES_svy_included)
prop.table(svytable(~medicaid_cat, NHANES_svy_included))

svytable(~medicaid_cat, NHANES_svy_included)[[1]]
prop.table(svytable(~medicaid_cat, NHANES_svy_included))[[1]]

### Excluded
svytable(~medicaid_cat, NHANES_svy_excluded)
prop.table(svytable(~medicaid_cat, NHANES_svy_excluded))

svytable(~medicaid_cat, NHANES_svy_excluded)[[1]]
prop.table(svytable(~medicaid_cat, NHANES_svy_excluded))[[1]]

medicaid <- data.frame(Category = "Medicaid coverage",
                        Total_weighted = paste0(round(svytable(~medicaid_cat, NHANES_svy)[[1]], digits = 0),
                                                " (",
                                                round(100*prop.table(svytable(~medicaid_cat, NHANES_svy))[[1]], digits = 1),
                                                ")"),
                        Inc_unweighted = paste0(round(table(data_included$medicaid_cat, exclude = NULL)[[1]], digits = 0), 
                                                " (", 
                                                round(100*prop.table(table(data_included$medicaid_cat, exclude = NULL))[[1]], digits = 1),
                                                ")"),
                        Inc_weighted = paste0(round(svytable(~medicaid_cat, NHANES_svy_included)[[1]], digits = 0), 
                                              " (",
                                              round(100*prop.table(svytable(~medicaid_cat, NHANES_svy_included))[[1]], digits = 1),
                                              ")"),
                        Exc_unweighted = paste0(round(table(data_excluded$medicaid_cat, exclude = NULL)[[1]], digits = 0), 
                                                " (", 
                                                round(100*prop.table(table(data_excluded$medicaid_cat, exclude = NULL))[[1]], digits = 1),
                                                ")"),
                        Exc_weighted = paste0(round(svytable(~medicaid_cat, NHANES_svy_excluded)[[1]], digits = 0), 
                                              " (",
                                              round(100*prop.table(svytable(~medicaid_cat, NHANES_svy_excluded))[[1]], digits = 1),
                                              ")"))

# Race and ethnicity 1/2/3/4/5 = Mexican American/Other Hispanic/Non-Hispanic White/Non-Hispanic Black/Other Race
## 1_non_hispanic_white <- Non-Hispanic White
## 2_black <- Non-Hispanic Black
## 3_hispanic <- Mexican American/Other Hispanic
## 4_other <- Other Race

## Unweighted
### Included
table(data_included$race_cat, exclude = NULL)
table(data_included$race_cat, exclude = NULL) %>% prop.table()
# table(data_included$race_cat, exclude = NULL)[[2]]
# prop.table(table(data_included$race_cat, exclude = NULL))[[2]]
### Excluded
table(data_excluded$race_cat, exclude = NULL)
table(data_excluded$race_cat, exclude = NULL) %>% prop.table()
# table(data_excluded$race_cat, exclude = NULL)[[2]]
# prop.table(table(data_excluded$race_cat, exclude = NULL))[[2]]

## Weighted
### Total
svytable(~race_cat, NHANES_svy)
prop.table(svytable(~race_cat, NHANES_svy))

### Included
svytable(~race_cat, NHANES_svy_included)
prop.table(svytable(~race_cat, NHANES_svy_included))
# svytable(~race_cat, NHANES_svy_included)[[2]]
# prop.table(svytable(~race_cat, NHANES_svy_included))[[2]]
### Excluded
svytable(~race_cat, NHANES_svy_excluded)
prop.table(svytable(~race_cat, NHANES_svy_excluded))
# svytable(~race_cat, NHANES_svy_excluded)[[2]]
# prop.table(svytable(~race_cat, NHANES_svy_excluded))[[2]]

race_total <- cbind(t(t(table(dm_elg$race_cat, exclude = NULL))), 
                       t(t(round(prop.table(table(dm_elg$race_cat, exclude = NULL))*100, digits = 1))),
                       t(t(round(svytable(~race_cat, NHANES_svy), digits = 0))),
                       t(t(round(prop.table(svytable(~race_cat, NHANES_svy))*100, digits = 1)))) %>% as.data.frame()
names(race_total) <- c("Total unweighted (N)", "Total unweighted (%)",
                          "Total weighted (N)", "Total weighted (%)")

race_included <- cbind(t(t(table(data_included$race_cat, exclude = NULL))),
                       t(t(round(prop.table(table(data_included$race_cat, exclude = NULL))*100, digits = 1))),
                       t(t(round(svytable(~race_cat, NHANES_svy_included), digits = 0))),
                       t(t(round(prop.table(svytable(~race_cat, NHANES_svy_included))*100, digits = 1)))) %>% as.data.frame()
names(race_included) <- c("Included unweighted (N)", "Included unweighted (%)",
                          "Included weighted (N)", "Included weighted (%)")

race_excluded <- cbind(t(t(table(data_excluded$race_cat, exclude = NULL))),
                       t(t(round(prop.table(table(data_excluded$race_cat, exclude = NULL))*100, digits = 1))),
                       t(t(round(svytable(~race_cat, NHANES_svy_excluded), digits = 0))),
                       t(t(round(prop.table(svytable(~race_cat, NHANES_svy_excluded))*100, digits = 1)))) %>% as.data.frame()
names(race_excluded) <- c("Excluded unweighted (N)", "Excluded unweighted (%)",
                          "Excluded weighted (N)", "Excluded weighted (%)")

race_tbl <- cbind(race_total, race_included, race_excluded)

# Education (edu_cat) 
## Unweighted
### Included
table(data_included$edu_cat, exclude = NULL)
table(data_included$edu_cat, exclude = NULL) %>% prop.table()
# table(data_included$edu_cat, exclude = NULL)[[2]]
# prop.table(table(data_included$edu_cat, exclude = NULL))[[2]]
### Excluded
table(data_excluded$edu_cat, exclude = NULL)
table(data_excluded$edu_cat, exclude = NULL) %>% prop.table()
# table(data_excluded$edu_cat, exclude = NULL)[[2]]
# prop.table(table(data_excluded$edu_cat, exclude = NULL))[[2]]

## Weighted
### Total
svytable(~edu_cat, NHANES_svy)
prop.table(svytable(~edu_cat, NHANES_svy))

### Included
svytable(~edu_cat, NHANES_svy_included)
prop.table(svytable(~edu_cat, NHANES_svy_included))
# svytable(~edu_cat, NHANES_svy_included)[[2]]
# prop.table(svytable(~edu_cat, NHANES_svy_included))[[2]]
### Excluded
svytable(~edu_cat, NHANES_svy_excluded)
prop.table(svytable(~edu_cat, NHANES_svy_excluded))
# svytable(~edu_cat, NHANES_svy_excluded)[[2]]
# prop.table(svytable(~edu_cat, NHANES_svy_excluded))[[2]]

edu_total <- cbind(t(t(table(dm_elg$edu_cat, exclude = NULL))),
                      t(t(round(prop.table(table(dm_elg$edu_cat, exclude = NULL))*100, digits = 1))),
                      t(t(round(svytable(~edu_cat, NHANES_svy), digits = 0))),
                      t(t(round(prop.table(svytable(~edu_cat, NHANES_svy))*100, digits = 1)))) %>% as.data.frame()
names(edu_total) <- c("Total unweighted (N)", "Total unweighted (%)",
                         "Total weighted (N)", "Total weighted (%)")

edu_included <- cbind(t(t(table(data_included$edu_cat, exclude = NULL))),
                       t(t(round(prop.table(table(data_included$edu_cat, exclude = NULL))*100, digits = 1))),
                       t(t(round(svytable(~edu_cat, NHANES_svy_included), digits = 0))),
                       t(t(round(prop.table(svytable(~edu_cat, NHANES_svy_included))*100, digits = 1)))) %>% as.data.frame()
names(edu_included) <- c("Included unweighted (N)", "Included unweighted (%)",
                          "Included weighted (N)", "Included weighted (%)")

edu_excluded <- cbind(t(t(table(data_excluded$edu_cat, exclude = NULL))),
                       t(t(round(prop.table(table(data_excluded$edu_cat, exclude = NULL))*100, digits = 1))),
                       t(t(round(svytable(~edu_cat, NHANES_svy_excluded), digits = 0))),
                       t(t(round(prop.table(svytable(~edu_cat, NHANES_svy_excluded))*100, digits = 1)))) %>% as.data.frame()
names(edu_excluded) <- c("Excluded unweighted (N)", "Excluded unweighted (%)",
                          "Excluded weighted (N)", "Excluded weighted (%)")

edu_tbl <- cbind(edu_total, edu_included, edu_excluded)

# Self-rated health (srh_cat) 
## Unweighted
### Included
table(data_included$srh_cat, exclude = NULL)
table(data_included$srh_cat, exclude = NULL) %>% prop.table()
# table(data_included$srh_cat, exclude = NULL)[[2]]
# prop.table(table(data_included$srh_cat, exclude = NULL))[[2]]
### Excluded
table(data_excluded$srh_cat, exclude = NULL)
table(data_excluded$srh_cat, exclude = NULL) %>% prop.table()
# table(data_excluded$srh_cat, exclude = NULL)[[2]]
# prop.table(table(data_excluded$srh_cat, exclude = NULL))[[2]]

## Weighted
### Total
svytable(~srh_cat, NHANES_svy)
prop.table(svytable(~srh_cat, NHANES_svy))

### Included
svytable(~srh_cat, NHANES_svy_included)
prop.table(svytable(~srh_cat, NHANES_svy_included))
# svytable(~srh_cat, NHANES_svy_included)[[2]]
# prop.table(svytable(~srh_cat, NHANES_svy_included))[[2]]
### Excluded
svytable(~srh_cat, NHANES_svy_excluded)
prop.table(svytable(~srh_cat, NHANES_svy_excluded))
# svytable(~srh_cat, NHANES_svy_excluded)[[2]]
# prop.table(svytable(~srh_cat, NHANES_svy_excluded))[[2]]

srh_total <- cbind(t(t(table(dm_elg$srh_cat, exclude = NULL))),
                      t(t(round(prop.table(table(dm_elg$srh_cat, exclude = NULL))*100, digits = 1))),
                      t(t(round(svytable(~srh_cat, NHANES_svy), digits = 0))),
                      t(t(round(prop.table(svytable(~srh_cat, NHANES_svy))*100, digits = 1)))) %>% as.data.frame()
names(srh_total) <- c("Total unweighted (N)", "Total unweighted (%)",
                         "Total weighted (N)", "Total weighted (%)")

srh_included <- cbind(t(t(table(data_included$srh_cat, exclude = NULL))),
                      t(t(round(prop.table(table(data_included$srh_cat, exclude = NULL))*100, digits = 1))),
                      t(t(round(svytable(~srh_cat, NHANES_svy_included), digits = 0))),
                      t(t(round(prop.table(svytable(~srh_cat, NHANES_svy_included))*100, digits = 1)))) %>% as.data.frame()
names(srh_included) <- c("Included unweighted (N)", "Included unweighted (%)",
                         "Included weighted (N)", "Included weighted (%)")

srh_excluded <- cbind(t(t(table(data_excluded$srh_cat, exclude = NULL))),
                      t(t(round(prop.table(table(data_excluded$srh_cat, exclude = NULL))*100, digits = 1))),
                      t(t(round(svytable(~srh_cat, NHANES_svy_excluded), digits = 0))),
                      t(t(round(prop.table(svytable(~srh_cat, NHANES_svy_excluded))*100, digits = 1)))) %>% as.data.frame()
names(srh_excluded) <- c("Excluded unweighted (N)", "Excluded unweighted (%)",
                         "Excluded weighted (N)", "Excluded weighted (%)")

srh_tbl <- cbind(srh_total, srh_included, srh_excluded)

# Creatinie 
## Unweighted
summary(dm_elg$LBXSCR) # 15 NAs
### Included
mean(data_included$LBXSCR, na.rm = T)
sd(data_included$LBXSCR, na.rm = T)
### Excluded
mean(data_excluded$LBXSCR, na.rm = T)
sd(data_excluded$LBXSCR, na.rm = T)

## Weighted
### Total
options(survey.lonely.psu="adjust")
svymean(~LBXSCR, NHANES_svy, na.rm = T) # mean
svyvar(~LBXSCR, NHANES_svy, na.rm = T)
svyvar(~LBXSCR, NHANES_svy, na.rm = T)[[1]]
sqrt(svyvar(~LBXSCR, NHANES_svy, na.rm = T)[[1]]) # SD

### Included
options(survey.lonely.psu="adjust")
svymean(~LBXSCR, NHANES_svy_included, na.rm = T) # mean
svyvar(~LBXSCR, NHANES_svy_included, na.rm = T)
svyvar(~LBXSCR, NHANES_svy_included, na.rm = T)[[1]]
sqrt(svyvar(~LBXSCR, NHANES_svy_included, na.rm = T)[[1]]) # SD

### Excluded
svymean(~LBXSCR, NHANES_svy_excluded, na.rm = T) # mean
svyvar(~LBXSCR, NHANES_svy_excluded, na.rm = T)
svyvar(~LBXSCR, NHANES_svy_excluded, na.rm = T)[[1]]
sqrt(svyvar(~LBXSCR, NHANES_svy_excluded, na.rm = T)[[1]]) # SD

creatinie <- data.frame(Category = "Serum creatinine",
                        Total_weighted = paste0(round(svymean(~LBXSCR, NHANES_svy, na.rm = T)[[1]], digits = 1),
                                                " (",
                                                round(sqrt(svyvar(~LBXSCR, NHANES_svy, na.rm = T)[[1]]), digits = 1),
                                                ")"),
                      Inc_unweighted = paste0(round(mean(data_included$LBXSCR, na.rm = T), digits = 1),
                                              " (",
                                              round(sd(data_included$LBXSCR, na.rm = T), digits = 1),
                                              ")"),
                      Inc_weighted = paste0(round(svymean(~LBXSCR, NHANES_svy_included, na.rm = T)[[1]], digits = 1),
                                            " (",
                                            round(sqrt(svyvar(~LBXSCR, NHANES_svy_included, na.rm = T)[[1]]), digits = 1),
                                            ")"),
                      Exc_unweighted = paste0(round(mean(data_excluded$LBXSCR, na.rm = T), digits = 1),
                                              " (",
                                              round(sd(data_excluded$LBXSCR, na.rm = T), digits = 1),
                                              ")"),
                      Exc_weighted = paste0(round(svymean(~LBXSCR, NHANES_svy_excluded, na.rm = T)[[1]], digits = 1),
                                            " (",
                                            round(sqrt(svyvar(~LBXSCR, NHANES_svy_excluded, na.rm = T)[[1]]), digits = 1),
                                            ")"))

# LDL 
## Unweighted
summary(dm_elg$LBDLDL) # 20 NAs
### Included
mean(data_included$LBDLDL, na.rm = T)
sd(data_included$LBDLDL, na.rm = T)
### Excluded
mean(data_excluded$LBDLDL, na.rm = T)
sd(data_excluded$LBDLDL, na.rm = T)

## Weighted
### Total
options(survey.lonely.psu="adjust")
svymean(~LBDLDL, NHANES_svy, na.rm = T) # mean
svyvar(~LBDLDL, NHANES_svy, na.rm = T)
svyvar(~LBDLDL, NHANES_svy, na.rm = T)[[1]]
sqrt(svyvar(~LBDLDL, NHANES_svy, na.rm = T)[[1]]) # SD

### Included
options(survey.lonely.psu="adjust")
svymean(~LBDLDL, NHANES_svy_included, na.rm = T) # mean
svyvar(~LBDLDL, NHANES_svy_included, na.rm = T)
svyvar(~LBDLDL, NHANES_svy_included, na.rm = T)[[1]]
sqrt(svyvar(~LBDLDL, NHANES_svy_included, na.rm = T)[[1]]) # SD

### Excluded
svymean(~LBDLDL, NHANES_svy_excluded, na.rm = T) # mean
svyvar(~LBDLDL, NHANES_svy_excluded, na.rm = T)
svyvar(~LBDLDL, NHANES_svy_excluded, na.rm = T)[[1]]
sqrt(svyvar(~LBDLDL, NHANES_svy_excluded, na.rm = T)[[1]]) # SD

LDL_C <- data.frame(Category = "LDL_C",
                    Total_weighted = paste0(round(svymean(~LBDLDL, NHANES_svy, na.rm = T)[[1]], digits = 1),
                                            " (",
                                            round(sqrt(svyvar(~LBDLDL, NHANES_svy, na.rm = T)[[1]]), digits = 1),
                                            ")"),
                        Inc_unweighted = paste0(round(mean(data_included$LBDLDL, na.rm = T), digits = 1),
                                                " (",
                                                round(sd(data_included$LBDLDL, na.rm = T), digits = 1),
                                                ")"),
                        Inc_weighted = paste0(round(svymean(~LBDLDL, NHANES_svy_included, na.rm = T)[[1]], digits = 1),
                                              " (",
                                              round(sqrt(svyvar(~LBDLDL, NHANES_svy_included, na.rm = T)[[1]]), digits = 1),
                                              ")"),
                        Exc_unweighted = paste0(round(mean(data_excluded$LBDLDL, na.rm = T), digits = 1),
                                                " (",
                                                round(sd(data_excluded$LBDLDL, na.rm = T), digits = 1),
                                                ")"),
                        Exc_weighted = paste0(round(svymean(~LBDLDL, NHANES_svy_excluded, na.rm = T)[[1]], digits = 1),
                                              " (",
                                              round(sqrt(svyvar(~LBDLDL, NHANES_svy_excluded, na.rm = T)[[1]]), digits = 1),
                                              ")"))

# HDL 
## Unweighted
summary(dm_elg$LBDHDD) # 10 NAs
### Included
mean(data_included$LBDHDD, na.rm = T)
sd(data_included$LBDHDD, na.rm = T)
### Excluded
mean(data_excluded$LBDHDD, na.rm = T)
sd(data_excluded$LBDHDD, na.rm = T)

## Weighted
### Total
options(survey.lonely.psu="adjust")
svymean(~LBDHDD, NHANES_svy, na.rm = T) # mean
svyvar(~LBDHDD, NHANES_svy, na.rm = T)
svyvar(~LBDHDD, NHANES_svy, na.rm = T)[[1]]
sqrt(svyvar(~LBDHDD, NHANES_svy, na.rm = T)[[1]]) # SD

### Included
options(survey.lonely.psu="adjust")
svymean(~LBDHDD, NHANES_svy_included, na.rm = T) # mean
svyvar(~LBDHDD, NHANES_svy_included, na.rm = T)
svyvar(~LBDHDD, NHANES_svy_included, na.rm = T)[[1]]
sqrt(svyvar(~LBDHDD, NHANES_svy_included, na.rm = T)[[1]]) # SD

### Excluded
svymean(~LBDHDD, NHANES_svy_excluded, na.rm = T) # mean
svyvar(~LBDHDD, NHANES_svy_excluded, na.rm = T)
svyvar(~LBDHDD, NHANES_svy_excluded, na.rm = T)[[1]]
sqrt(svyvar(~LBDHDD, NHANES_svy_excluded, na.rm = T)[[1]]) # SD

HDL_C <- data.frame(Category = "HDL_C",
                    Total_weighted = paste0(round(svymean(~LBDHDD, NHANES_svy, na.rm = T)[[1]], digits = 1),
                                            " (",
                                            round(sqrt(svyvar(~LBDHDD, NHANES_svy, na.rm = T)[[1]]), digits = 1),
                                            ")"),
                    Inc_unweighted = paste0(round(mean(data_included$LBDHDD, na.rm = T), digits = 1),
                                            " (",
                                            round(sd(data_included$LBDHDD, na.rm = T), digits = 1),
                                            ")"),
                    Inc_weighted = paste0(round(svymean(~LBDHDD, NHANES_svy_included, na.rm = T)[[1]], digits = 1),
                                          " (",
                                          round(sqrt(svyvar(~LBDHDD, NHANES_svy_included, na.rm = T)[[1]]), digits = 1),
                                          ")"),
                    Exc_unweighted = paste0(round(mean(data_excluded$LBDHDD, na.rm = T), digits = 1),
                                            " (",
                                            round(sd(data_excluded$LBDHDD, na.rm = T), digits = 1),
                                            ")"),
                    Exc_weighted = paste0(round(svymean(~LBDHDD, NHANES_svy_excluded, na.rm = T)[[1]], digits = 1),
                                          " (",
                                          round(sqrt(svyvar(~LBDHDD, NHANES_svy_excluded, na.rm = T)[[1]]), digits = 1),
                                          ")"))

# Save the outputs
chr_tbl <- rbind.data.frame(obs, age, female, 
                            BMI,
                            creatinie, LDL_C, HDL_C,
                            CVD_history, CHF_history,
                            insurance, medicaid)
race_tbl
edu_tbl
srh_tbl

# save as a csv file
write.csv(chr_tbl, paste0(path_to_output,"characteristics_20240102.csv"))
write.csv(race_tbl, paste0(path_to_output,"race_20240102.csv"))
write.csv(edu_tbl, paste0(path_to_output,"edu_20240102.csv"))
write.csv(srh_tbl, paste0(path_to_output,"srh_20240102.csv"))