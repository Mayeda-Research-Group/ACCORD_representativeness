# Input
# NHANES data (2017-2020pre-pandemic) in csv format:
# Output
# Variables to be used
# nhanes1720prepandemic_with_variables_20240102.csv
# R4.1.3

# Objective of this file
# Prepare one file that includes necessary variables for the project
# Weighting: weight for fasting sample

# Define T2DM according to previous research 
# (self-reported DM + HbA1c + fasting glucose + insulin use + medication use)

# models
# NA

rm(list = ls())

# Load packages
library(data.table)
library(dplyr)
library(rlang)
library(stringr)

# Set up paths
path_to_data <- "XX" # To be changed
path_to_output <- "XX" # To be changed

# Extract necessary variables for the eligibility criteria
## Load the data
### HbA1c
ghb_data_pre <- read.csv(paste0(path_to_data,"P_GHB.csv"))
str(ghb_data_pre) # SEQN/LBXGH
ghb_data <- ghb_data_pre %>% select(c("SEQN","LBXGH")) %>% as.data.table()
str(ghb_data)

### Self-reported DM
dmq_data_pre <- read.csv(paste0(path_to_data,"P_DIQ.csv"))
str(dmq_data_pre)
dmq_data <- dmq_data_pre %>% select(c("SEQN", "DIQ010", "DIQ050", "DIQ070")) %>%
  as.data.table()
str(dmq_data)

### Fasting glucose
fgl_data_pre <- read.csv(paste0(path_to_data,"P_GLU.csv"))
str(fgl_data_pre)
fgl_data <- fgl_data_pre %>% select(c("SEQN", "WTSAFPRP", "LBXGLU")) %>%
  as.data.table()
str(fgl_data)

### Age, gender, pregnancy status, sampling weight, and education
demo_data_pre <- read.csv(paste0(path_to_data,"P_DEMO.csv"))
str(demo_data_pre) 
demo_data <- demo_data_pre %>%
  select(c("SEQN","RIDAGEYR", "RIAGENDR", "RIDRETH1","RIDEXPRG", "DMDEDUC2",
           "SDMVPSU", "SDMVSTRA")) %>%
  as.data.table()
str(demo_data)

### self-reported health (HUQ010 in P_HUQ.xpt) 
slh_data_pre <- read.csv(paste0(path_to_data,"P_HUQ.csv"))
str(slh_data_pre)
slh_data <- slh_data_pre %>%
  select(c("SEQN", "HUQ010")) %>%
  as.data.table()
str(slh_data)

### health insurance (HIQ011 in P_HIQ.xpt) 
### Medicaid coverage (HIQ032D in P_HIQ.xpt) 
isr_data_pre <- read.csv(paste0(path_to_data,"P_HIQ.csv"))
str(isr_data_pre)
isr_data <- isr_data_pre %>%
  select(c("SEQN", "HIQ011", "HIQ032D")) %>%
  as.data.table()
str(isr_data)

### Albumin creatinine ratio (URDACT in P_ALB_CR.xpt) 
cre_alb_data_pre <- read.csv(paste0(path_to_data,"P_ALB_CR.csv"))
str(cre_alb_data_pre)
cre_alb_data <- cre_alb_data_pre %>%
  select(c("SEQN", "URDACT")) %>%
  as.data.table()
str(cre_alb_data)

### Medical conditions
med_data_pre <- read.csv(paste0(path_to_data,"P_MCQ.csv"))
str(med_data_pre) 
med_data <- med_data_pre %>%
  select(c("SEQN", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E","MCQ160F",
           "MCQ220","MCQ230A","MCQ230B","MCQ230C","MCQ230D",
           "MCQ160L", "MCQ170L")) %>%  
  as.data.table()
str(med_data)

### LDL-C
ldl_data_pre <- read.csv(paste0(path_to_data,"P_TRIGLY.csv"))
str(ldl_data_pre) 
ldl_data <- ldl_data_pre %>%
  select(c("SEQN", "LBDLDL", "LBXTR")) %>%
  as.data.table()
str(ldl_data)

### HDL-C
hdl_data_pre <- read.csv(paste0(path_to_data,"P_HDL.csv"))
str(hdl_data_pre) 
hdl_data <- hdl_data_pre %>%
  select(c("SEQN", "LBDHDD")) %>%
  as.data.table()
str(hdl_data)

### Blood pressure
bp_data_pre <- read.csv(paste0(path_to_data,"P_BPXO.csv"))
str(bp_data_pre) 
bp_data <- bp_data_pre %>%
  select(c("SEQN", 
           paste0("BPXOSY",1:3), paste0("BPXODI",1:3))) %>%
  as.data.table()
str(bp_data)

### Current cigarette smoking
smk_data_pre <- read.csv(paste0(path_to_data,"P_SMQ.csv"))
str(smk_data_pre) 
smk_data <- smk_data_pre %>%
  select(c("SEQN", "SMQ020", "SMQ040")) %>% 
  as.data.table()
str(smk_data)

### BMI
bmi_data_pre <- read.csv(paste0(path_to_data,"P_BMX.csv"))
str(bmi_data_pre) 
bmi_data <- bmi_data_pre %>%
  select(c("SEQN", "BMXBMI")) %>%
  as.data.table()
str(bmi_data)

### Serum creatinine/AST/ALT
kdn_lvr_data_pre <- read.csv(paste0(path_to_data,"P_BIOPRO.csv"))
str(kdn_lvr_data_pre) 
kdn_lvr_data <- kdn_lvr_data_pre %>%
  select(c("SEQN", "LBXSCR",
           "LBXSATSI", "LBXSASSI")) %>%
  as.data.table()
str(kdn_lvr_data)

### Symptoms of heart failure
cardio_data_pre <- read.csv(paste0(path_to_data,"P_CDQ.csv"))
str(cardio_data_pre) 
cardio_data <- cardio_data_pre %>%
  # select(c("SEQN", paste0("CDQ00",1:3))) %>%
  select(c("SEQN", "CDQ010")) %>% # Jan 02, 2024
  as.data.table()
str(cardio_data)

### Lipid-lowering medication (questionnaire)
bpq_data_pre <- read.csv(paste0(path_to_data,"P_BPQ.csv"))
str(bpq_data_pre) 
bpq_data <- bpq_data_pre %>%
  select(c("SEQN", "BPQ100D")) %>%
  as.data.table()
str(bpq_data)

### Prescription drug
prescrpt_data_pre <- read.csv(paste0(path_to_data,"P_RXQ_RX.csv"))
str(prescrpt_data_pre)
prescrpt_data <- prescrpt_data_pre %>%
  select(c("SEQN", "RXDUSE", "RXDDRGID", "RXDDRUG", "RXDCOUNT")) %>%
  as.data.table()
str(prescrpt_data)
prescrpt_data[is.na(RXDCOUNT), RXDCOUNT:=0]

# RXDUSE (Taken prescription medicine, past month) 
# 1/2/7/9 = Yes/No/Refused/Don't know
table(prescrpt_data$RXDUSE) 
xtabs(~RXDUSE + RXDCOUNT, data = prescrpt_data_pre, addNA = TRUE)
table(prescrpt_data$RXDDRUG) # N's of Unknow/Refused/Don't know: 20/46/156
(20+46+256)/nrow(prescrpt_data) # 0.009768825
table(prescrpt_data[RXDDRUG=="", RXDUSE]) # RXDUSE for those with empty RXDDRUG

### Prescription drug codebook
prescrpt_data_code <- read.csv(paste0(path_to_data,"RXQ_DRUG.csv"))
str(prescrpt_data_code)

### Bp-lowering medication list
bp_med_list <- prescrpt_data_code %>% filter(
  (RXDDCI1A == 40 & RXDDCI1B == 42)|(RXDDCI2A == 40 & RXDDCI2B == 42)|
    (RXDDCI3A == 40 & RXDDCI3B == 42)|(RXDDCI4A == 40 & RXDDCI4B == 42)|
    (RXDDCI1A == 40 & RXDDCI1B == 482)|(RXDDCI2A == 40 & RXDDCI2B == 482)|
    (RXDDCI3A == 40 & RXDDCI3B == 482)|(RXDDCI4A == 40 & RXDDCI4B == 482)|
    (RXDDCI1A == 40 & RXDDCI1B == 56)|(RXDDCI2A == 40 & RXDDCI2B == 56)|
    (RXDDCI3A == 40 & RXDDCI3B == 56)|(RXDDCI4A == 40 & RXDDCI4B == 56)|
    (RXDDCI1A == 40 & RXDDCI1B == 49)|(RXDDCI2A == 40 & RXDDCI2B == 49)|
    (RXDDCI3A == 40 & RXDDCI3B == 49)|(RXDDCI4A == 40 & RXDDCI4B == 49)|
    (RXDDCI1A == 40 & RXDDCI1B == 48)|(RXDDCI2A == 40 & RXDDCI2B == 48)|
    (RXDDCI3A == 40 & RXDDCI3B == 48)|(RXDDCI4A == 40 & RXDDCI4B == 48)|
    (RXDDCI1A == 40 & RXDDCI1B == 47)|(RXDDCI2A == 40 & RXDDCI2B == 47)|
    (RXDDCI3A == 40 & RXDDCI3B == 47)|(RXDDCI4A == 40 & RXDDCI4B == 47)|
    (RXDDCI1A == 40 & RXDDCI1B == 44)|(RXDDCI2A == 40 & RXDDCI2B == 44)|
    (RXDDCI3A == 40 & RXDDCI3B == 44)|(RXDDCI4A == 40 & RXDDCI4B == 44)|
    (RXDDCI1A == 40 & RXDDCI1B == 53)|(RXDDCI2A == 40 & RXDDCI2B == 53)|
    (RXDDCI3A == 40 & RXDDCI3B == 53)|(RXDDCI4A == 40 & RXDDCI4B == 53)|
    (RXDDCI1A == 40 & RXDDCI1B == 340)|(RXDDCI2A == 40 & RXDDCI2B == 340)|
    (RXDDCI3A == 40 & RXDDCI3B == 340)|(RXDDCI4A == 40 & RXDDCI4B == 340)|
    (RXDDCI1A == 40 & RXDDCI1B == 342)|(RXDDCI2A == 40 & RXDDCI2B == 342)|
    (RXDDCI3A == 40 & RXDDCI3B == 342)|(RXDDCI4A == 40 & RXDDCI4B == 342)|
    (RXDDCI1A == 40 & RXDDCI1B == 55)|(RXDDCI2A == 40 & RXDDCI2B == 55)|
    (RXDDCI3A == 40 & RXDDCI3B == 55)|(RXDDCI4A == 40 & RXDDCI4B == 55)
) %>% select("RXDDRGID")

### BP-lowering count
prescrpt_data <- prescrpt_data %>% mutate(
  bp_lowering_flag = ifelse(RXDDRGID %in% bp_med_list$RXDDRGID, 1, 0)
)
table(prescrpt_data$bp_lowering_flag)

prescrpt_bp_count <- prescrpt_data %>% group_by(SEQN) %>% 
  summarise(bp_med_count = sum(bp_lowering_flag))
table(prescrpt_bp_count$bp_med_count)


### Lipid-lowering medication
prescrpt_data <- prescrpt_data %>% mutate(
  # atorvastatin = ifelse(RXDDRUG == "ATORVASTATIN", 1, 0),
  atorvastatin = ifelse(str_detect(RXDDRUG, pattern = "ATORVASTATIN"), 1, 0),
  simvastatin = ifelse(str_detect(RXDDRUG, pattern = "SIMVASTATIN"), 1, 0),
  lovastatin = ifelse(str_detect(RXDDRUG, pattern = "LOVASTATIN"), 1, 0),
  pravastatin = ifelse(str_detect(RXDDRUG, pattern = "PRAVASTATIN"), 1, 0),
  fluvastatin = ifelse(str_detect(RXDDRUG, pattern = "FLUVASTATIN"), 1, 0),
  rosuvastatin = ifelse(str_detect(RXDDRUG, pattern = "ROSVASTATIN"), 1, 0),
  ezetimibe = ifelse(str_detect(RXDDRUG, pattern = "EZETIMIBE"), 1, 0),
  fenofibrate = ifelse(str_detect(RXDDRUG, pattern = "FENOFIBRATE"), 1, 0),
  niacin = ifelse(str_detect(RXDDRUG, pattern = "NIACIN"), 1, 0),
  resin = ifelse(str_detect(RXDDRUG, pattern = "RESIN"), 1, 0)
)

### Lipid-lowering count
prescrpt_lipid_count <- prescrpt_data %>% group_by(SEQN) %>% 
  summarise(atorvastatin_count = sum(atorvastatin),
            simvastatin_count = sum(simvastatin),
            lovastatin_count = sum(lovastatin),
            pravastatin_count = sum(pravastatin),
            fluvastatin_count = sum(fluvastatin),
            rosuvastatin_count = sum(rosuvastatin),
            ezetimibe_count = sum(ezetimibe),
            fenofibrate_count = sum(fenofibrate),
            niacin_count = sum(niacin),
            resin_count = sum(resin))

prescrpt_lipid_count <- prescrpt_lipid_count %>% 
  mutate(lipid_med_count = 
           atorvastatin_count +
           simvastatin_count +
           lovastatin_count +
           pravastatin_count +
           fluvastatin_count +
           rosuvastatin_count +
           ezetimibe_count +
           fenofibrate_count +
           niacin_count +
           resin_count)
table(prescrpt_lipid_count$lipid_med_count)

## Merge the datasets
data_pre <- dmq_data %>% left_join(.,ghb_data, by = "SEQN") %>% 
  left_join(.,fgl_data, by = "SEQN") %>% 
  full_join(.,demo_data, by = "SEQN") %>% # modified on Nov 21
  left_join(.,slh_data, by = "SEQN") %>% 
  left_join(.,isr_data, by = "SEQN") %>% 
  left_join(.,cre_alb_data, by = "SEQN") %>% 
  left_join(.,med_data, by = "SEQN") %>% 
  left_join(.,ldl_data, by = "SEQN") %>% 
  left_join(.,hdl_data, by = "SEQN") %>% 
  left_join(.,bp_data, by = "SEQN") %>% 
  left_join(.,smk_data, by = "SEQN") %>% 
  left_join(.,bmi_data, by = "SEQN") %>% 
  left_join(.,kdn_lvr_data, by = "SEQN") %>% 
  left_join(.,cardio_data, by = "SEQN") %>% 
  left_join(.,bpq_data, by = "SEQN") %>% 
  left_join(.,prescrpt_bp_count, by = "SEQN") %>% 
  left_join(.,prescrpt_lipid_count, by = "SEQN")
data_pre <- as.data.table(data_pre)
dim(data_pre)

# save as a csv file
write.csv(data_pre, paste0(path_to_output,
                           "nhanes1720prepandemic_with_variables_20240102.csv"))

