##################################################
# obligatory to append to the top of each script #
renv::activate(project = here::here("..")) #
source(here::here("..", "_common.R")) #
##################################################

# Immunogenicity Tables

# Reload clean_data
base::load(here::here("data_clean", attr(config,"config"), "params.Rdata"))
source(here::here("code", "make_functions.R"))

library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Read in original data
dat <- dat_proc

# The stratified random cohort for immunogenicity

ds_s <- dat %>%
################################################################
  dplyr::filter(!!as.name(config.cor$ph1)==1) %>%
  mutate(
    raceC = as.character(race),
    ethnicityC = case_when(EthnicityHispanic==1 ~ "Hispanic or Latino",
                           EthnicityHispanic==0 & EthnicityNotreported==0 & 
                             EthnicityUnknown==0 ~ "Not Hispanic or Latino",
                           EthnicityNotreported==1 | 
                             EthnicityUnknown==1 ~ "Not reported and unknown "),
    RaceEthC = case_when(
      WhiteNonHispanic==1 ~ "White Non-Hispanic ",
      TRUE ~ raceC
    ),
    MinorityC = case_when(
      MinorityInd == 1 ~ "Communities of Color",
      MinorityInd == 0 ~ "White Non-Hispanic"
    ),
    HighRiskC = ifelse(HighRiskInd == 1, "At-risk", "Not at-risk"),
    AgeC = ifelse(is.na(Senior), ifelse(age.geq.65 == 1, labels.age[2], labels.age[1]), ifelse(Senior == 1, labels.age[2], labels.age[1])),
    SexC = ifelse(Sex == 1, "Female", "Male"),
    AgeRiskC = paste(AgeC, HighRiskC),
    AgeSexC = paste(AgeC, SexC),
    AgeMinorC = ifelse(is.na(MinorityC), NA, paste(AgeC, MinorityC)),
    `Baseline SARS-CoV-2` = factor(ifelse(Bserostatus == 1, "Positive", "Negative"),
                      levels = c("Negative", "Positive")
    ),
    Arm = factor(ifelse(Trt == 1, "Vaccine", "Placebo"), 
                levels = c("Vaccine", "Placebo")),
    demo.stratum.ordered=case_when(!is.na(demo.stratum) ~ as.numeric(demo.stratum), 
                                   age.geq.65 == 1 ~ 7, 
                                   age.geq.65 == 0 & HighRiskInd==1 ~ 8,
                                   age.geq.65 == 0 & HighRiskInd==0 ~ 9), 
    AgeRisk1 = ifelse(AgeC==labels.age[1], AgeRiskC, NA),
    AgeRisk2 = ifelse(AgeC==labels.age[2], AgeRiskC, NA),
    All = "All participants"
  ) 

if(study_name %in% c("ENSEMBLE", "MockENSEMBLE", "PREVENT19")){
  ds_s <- ds_s %>% 
    mutate(CountryC = labels.countries.ENSEMBLE[Country+1],
           URMC = case_when(URMforsubcohortsampling == 1 & Country ==0 ~ "Communities of Color",
                            URMforsubcohortsampling == 0 & Country ==0 ~ "White Non-Hispanic", 
                            TRUE ~ as.character(NA)),
           AgeURM = case_when(is.na(URMC) ~ as.character(NA), 
                              TRUE ~ paste(AgeC, URMC)),
           demo.stratum.ordered=demo.stratum,
           HIVC = c("Positive", "Negative")[2-HIVinfection],
           BMI = case_when(max(BMI, na.rm=T) < 5 ~ labels.BMI[BMI],
                           BMI>=30 ~ "Obese BMI $\\geq$ 30", 
                           BMI>=25 ~ "Overweight 25 $\\leq$ BMI < 30",
                           BMI>=18.5 ~ "Normal 18.5 $\\leq$ BMI < 25",
                           BMI<18.5 ~ "Underweight BMI < 18.5")
           )
} 

if(study_name %in% c("ENSEMBLE", "MockENSEMBLE")){
  ds_s <- ds_s %>% 
    mutate(RegionC = labels.regions.ENSEMBLE[Region+1])
} 

if(study_name %in% c("PROFISCOV")){
  ds_s <- ds_s %>% 
    mutate(URMC = case_when(URMforsubcohortsampling == 1 ~ "Communities of Color",
                            URMforsubcohortsampling == 0 ~ "White Non-Hispanic", 
                            TRUE ~ as.character(NA)),
           AgeURM = case_when(is.na(URMC) ~ as.character(NA), 
                              TRUE ~ paste(AgeC, URMC)),
           demo.stratum.ordered=demo.stratum,
           HIVC = c("Positive", "Negative")[2-HIVinfection],
           BMI = case_when(max(BMI, na.rm=T) < 5 ~ labels.BMI[BMI],
                           BMI>=30 ~ "Obese BMI $\\geq$ 30", 
                           BMI>=25 ~ "Overweight 25 $\\leq$ BMI < 30",
                           BMI>=18.5 ~ "Normal 18.5 $\\leq$ BMI < 25",
                           BMI<18.5 ~ "Underweight BMI < 18.5")
    )
}
# Step2: Responders, % >=2FR, % >=4FR, % >=2lloq, % >=4lloq
# Post baseline visits

pos.cutoffs <- assay_metadata$pos.cutoff
names(pos.cutoffs) <- assay_metadata$assay
ds <- getResponder(ds_s, times=grep("Day", times, value=T), 
                   assays=assays, pos.cutoffs = pos.cutoffs)

subgrp <- c(
  All = "All participants", 
  AgeC = "Age",
  BMI="BMI",
  HighRiskC = "Risk for Severe Covid-19",
  AgeRiskC = "Age, Risk for Severe Covid-19",
  AgeRisk1 = paste0(labels.age[1], ", Risk for Severe Covid-19"),
  AgeRisk2 = paste0(labels.age[2], ", Risk for Severe Covid-19"),
  SexC = "Sex", 
  AgeSexC = "Age, sex",
  ethnicityC = "Hispanic or Latino ethnicity", 
  RaceEthC = "Race",
  MinorityC = "Underrepresented Minority Status",
  AgeMinorC = "Age, Communities of color",
  URMC = "Underrepresented Minority Status in the U.S.",
  AgeURM = "Age, Underrepresented Minority Status in the U.S.",
  CountryC = "Country",
  HIVC = "HIV Infection"
)

grplev <- c("", labels.age, "At-risk", "Not at-risk", 
            paste(labels.age[1], c("At-risk", "Not at-risk")),
            paste(labels.age[2], c("At-risk", "Not at-risk")),
            "Male", "Female", 
            paste(labels.age[1], c("Female", "Male")),
            paste(labels.age[2], c("Female", "Male")),
            "Hispanic or Latino", "Not Hispanic or Latino", "Not reported and unknown ", 
            "White Non-Hispanic ", "Black or African American", "Asian", 
            "American Indian or Alaska Native", 
            "Native Hawaiian or Other Pacific Islander", 
            "Multiracial", "Other", "Not reported and unknown",  
            labels.minor, 
            paste(labels.age[1], labels.minor),  
            paste(labels.age[2], labels.minor),
            labels.countries.ENSEMBLE,
            "Negative", "Positive")

names(grplev) <- c("All participants", grplev[-1])

# path for tables
save.results.to <- here::here("data_clean")
if (!dir.exists(save.results.to))  dir.create(save.results.to)
save.results.to <- paste0(here::here("data_clean"), "/", attr(config,"config"))

if (!dir.exists(save.results.to))  dir.create(save.results.to)
print(paste0("save.results.to equals ", save.results.to))

save(ds, assays, assays_col, labels_all, subgrp, grplev, tlf,
     file = file.path(save.results.to, "ds_all.Rdata"))

