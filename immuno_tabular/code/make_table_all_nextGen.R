##################################################
# obligatory to append to the top of each script #
renv::activate(project = here::here("..")) #
source(here::here("..", "_common.R")) #
##################################################

# Immunogenicity Tables

# Reload clean_data
source(here::here("code", "make_functions_nextGen.R"))
library(survey)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
# For stratum with 1 ppt
options(survey.lonely.psu="adjust")


# To select which tables are included in the report.
# Also to modify the headers, footers, etc. for each table

randomsubcohort <- case_when(study_name=="COVE" ~ "This table summarizes the 
      random subcohort, which was randomly sampled from the per-protocol cohort. The 
      sampling was stratified by 24 strata defined by enrollment characteristics: Assigned 
      treatment arm $\\\\times$ Baseline SARS-CoV-2 naïve vs. non-naïve status 
      (defined by serostatus and NAAT testing) $\\\\times$ Randomization strata 
      (Age < 65 and at-risk, Age < 65 and not at-risk, Age $\\\\geq 65)\\\\times$ 
      Communities of color (Yes/No) defined by White Non-Hispanic vs. all 
      others (following the primary COVE trial paper).",
                             
                             study_name=="ENSEMBLE" ~ "This table summarizes characteristics of 
      per-protocol participants in the immunogenicity subcohort, which was randomly 
      sampled from the study cohort. The sampling was The sampling was stratified by 
      strata defined by enrollment characteristics: Assigned randomization arm $\\\\times$ 
      Baseline SARS-CoV-2 seronegative vs. seropositive $\\\\times$ Randomization strata. 
      The U.S. subcohort includes 8 baseline demographic strata; the Latin America 
      and South Africa subcohorts each include 4 baseline demographic strata.",
                             
                             study_name=="PREVENT19" ~ "This table summarizes characteristics of 
      per-protocol participants in the immunogenicity subcohort, which was randomly 
      sampled from the study cohort. The sampling was The sampling was stratified by 
      strata defined by enrollment characteristics: Assigned randomization arm $\\\\times$ 
      Baseline SARS-CoV-2 seronegative vs. seropositive $\\\\times$ Randomization strata. 
      The U.S. subcohort includes 8 baseline demographic strata; the Mexico subcohort includes 2 baseline demographic strata.",
                             
                             TRUE~ "This table summarizes characteristics of 
      per-protocol participants in the immunogenicity subcohort, which was randomly 
      sampled from the study cohort.")

tlf <-
  list(
    tab_dm = list(
      table_header = paste0("Demographics of participants with ",
                            "serum, nasal fluid, and saliva antibody data "[!grepl("AB", config.cor$ph1)],
                            "T-cell data "[grepl("AB", config.cor$ph1)], 
                            "for the Investigational and Comparator Vaccine arms (",
                            "RIS, RIS-sera, RIS-nasal, or RIS-saliva"[!grepl("AB", config.cor$ph1)],
                            "T-cell data"[grepl("AB", config.cor$ph1)],
                            ")"), 
      table_footer = randomsubcohort,
      deselect = "subgroup",
      pack_row = "subgroup",
      header_above1 = c(" "=1, "Sera" = 3, "Nasal Fluid" = 3, "Saliva" = 3),
      col1="5cm"
    ),
    
    tab_dm_A = list(
      table_header = paste0("Demographics of Track A RIS participants with ",
                            "serum, nasal fluid, and saliva antibody data "[!grepl("AB", config.cor$ph1)],
                            "T-cell data "[grepl("AB", config.cor$ph1)], 
                            "for the Investigational and Comparator Vaccine arms (",
                            "Track A RIS-sera, Track A RIS-nasal, or Track A RIS-saliva"[!grepl("AB", config.cor$ph1)],
                            "Track A T-cell data"[grepl("AB", config.cor$ph1)],
                            ")"),
      table_footer = randomsubcohort,
      deselect = "subgroup",
      pack_row = "subgroup",
      header_above1 = c(" "=1, "Sera" = 3, "Nasal Fluid" = 3, "Saliva" = 3),
      col1="5cm"
    ),
    
    tab_strtm1 = list(
      table_header = "Sample Sizes of Random Subcohort Strata for Measuring Antibody Markers",
      deselect = "Arm",
      pack_row = "Arm"
    ),
    
    tab_strtm2 = list(
      table_header = "Sample Sizes of Random Subcohort Strata for Measuring Antibody Markers",
      deselect = "Arm",
      pack_row = "Arm"
    ),
    
    tab_bind1 = list(
      table_header = "Percentage of responders for binding antibody
      markers",
      table_footer = c(
        "Binding Antibody Responders are defined as participants who had baseline
      values below the lower limit of detection (LLOQ) with detectable concentration 
      above the assay LLOQ, or as participants with baseline values above the LLOQ 
      with a 4-fold increase in concentration.",
        "Percentages are calculated for the whole per-protocol group/subgroup, 
      using inverse probability weighting."),
      loop = "subgroup",
      group_table_col = c("Rx", "Group", "Baseline", "Visit", "N", "Marker"),
      deselect = "subgroup",
      pack_row = "subgroup"
    ),
    
    tab_bind2 = list(
      table_header = "Percentage of responders for binding antibody markers",
      table_footer = c(
      "Binding Antibody Responders are defined as participants who had baseline
      values below the lower limit of detection (LLOQ) with detectable concentration 
      above the assay LLOQ, or as participants with baseline values above the LLOQ 
      with a 4-fold increase in concentration.",
      "Percentages are calculated for the whole per-protocol group/subgroup, 
      using inverse probability weighting."),
      loop = "subgroup",
      group_table_col = c("Rx", "Group", "Baseline", "Visit", "N", "Marker"),
      deselect = "subgroup",
      pack_row = "subgroup"),
    
    tab_pseudo = list(
      table_header = "Percentage of responders for ID50 pseudo-virus neutralization antibody markers",
      table_footer = c(
        "Neutralization Responders are defined as participants who had baseline
        values below the lower limit of detection (LLOQ) with detectable
        ID50 neutralization titer above the assay LLOQ, or as participants with
        baseline values above the LLOQ with a 4-fold increase in ID50.",
        "Percentages are calculated for the whole per-protocol group/subgroup, 
        using inverse probability weighting."
    
      ),
      loop = "subgroup",
      group_table_col = c("Rx", "Group", "Baseline", "Visit", "N", "Marker"),
      deselect = "subgroup",
      pack_row = "subgroup"),
    
    tab_t = list(
      table_header = "Percentage of responders for T-cell markers",
      table_footer = c(
        "T cell marker positivity is based on Fisher’s exact test and control for FDR at 1% over all participants."
      ),
      loop = "subgroup",
      group_table_col = c("Rx", "Group", "Baseline", "Visit", "N", "Marker"),
      deselect = "subgroup",
      pack_row = "subgroup"),
    
    tab_gm = list(
      table_header = ifelse(grepl("AB", config.cor$ph1), "Geometric mean responses (GMRs)", "Geometric mean titers (GMTs) and geometric mean
      concentrations (GMCs)"),
      table_footer = "",
      loop = "subgroup",
      group_table_col = c("Rx", "Group", "Baseline", "Visit", "N", "Marker"),
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="7cm"),
    
    tab_gmr = list(
      table_header = ifelse(grepl("AB", config.cor$ph1), "Geometric mean responses (GMRRs) between post-vaccinations/pre-vaccination", "Geometric mean titer ratios (GMTRs) or geometric mean
      concentration ratios (GMCRs) between post-vaccinations/pre-vaccination"),
      table_footer = " ",
      loop = "subgroup",
      group_table_col = c("Rx", "Group", "Baseline", "Visit", "N", "Marker"),
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="2cm"),
    
    tab_rgmt = list(
      table_header = "The ratios of GMTs/GMCs/GMRs between groups",
      table_footer = " ",
      loop = "subgroup",
      pack_row = "subgroup",
      deselect = "subgroup",
      group_table_col = c("subgroup","Rx", "Baseline", "Visit"),
      col1="4cm"),
    
    tab_rrdiff = list(
      table_header = "Differences in the responder rates",
      table_footer = "Percentages are calculated for the whole per-protocol 
      group/subgroup, using inverse probability weighting.",
      loop = "subgroup",
      pack_row = "subgroup",
      group_table_col = c( "Group", "Baseline","Visit", "Marker"),
      deselect = "subgroup",
      col1="4cm"),
    
    tab_pos = list(
      table_header = "Antibody levels in the baseline SARS-CoV-2
      positive per-protocol cohort (vaccine vs. placebo)",
      table_footer = "Percentages are calculated for the whole 
      per-protocol group/subgroup, using inverse probability weighting.",
      col_name = c("Visit", "Marker", "N", "Resp rate", "GMT/GMC", "N",
                   "Resp rate", "GMT/GMC", "Resp Rate\nDifference", "GMTR/\nGMCR"),
      header_above1 = c(" "=2, "Vaccine" = 3, "Placebo" = 3, "Comparison" = 2),
      header_above2 = c(" "=2, "Baseline SARS-CoV-2 Positive" = 8),
      col1="1cm")  )

# Depends on the Incoming data
# if(include_bindN & !"bindN" %in% assays & study_name!="PROFISCOV"){
# assays <- sort(c("bindN", assays))
assays <- assay_metadata$assay
# }

labels.age <- case_when(study_name %in% c("ENSEMBLE", "MockENSEMBLE") ~ c("Age 18 - 59", "Age $\\geq$ 60"), 
                        TRUE~ c("Age $<$ 65", "Age $\\geq$ 65"))

labels.minor <- c("Communities of Color", "White Non-Hispanic")

labels.BMI <- c("Underweight BMI < 18.5", "Normal 18.5 $\\leq$ BMI < 25", 
                "Overweight 25 $\\leq$ BMI < 30", "Obese BMI $\\geq$ 30")

# labels.time <- labels.time[times]
times <- c("B", "Day31", "Delta31overB", "Day91", "Delta91overB", "Day181", "Delta181overB", "Day366", "Delta366overB")
labels.time <- c("Day 01", sapply(c(31, 91, 181, 366), sprintf, fmt=c("Day %s", "D%s fold-rise over D01")))
names(labels.time) <- times
  

# hacky fix
labels.assays.short <- labels.assays.short.tabular[assays]

# redefines what is in _common.R to use shorter names
labels.assays.long <- data.frame (purrr::imap_dfc(labels.assays.short, ~ paste0(labels.assays.short[.y], ": ", labels.time)))
rownames(labels.assays.long) <- names(labels.time)

visits <- names(labels.time)[!grepl("Delta", names(labels.time))]
assays_col <- as.vector(outer(visits, assays, paste0))

labels.assays <- expand.grid(
  time = rownames(labels.assays.long),
  marker = colnames(labels.assays.long),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    label.long = labels.assays.long[time, marker],
    label.short = sapply(labels.assays.short, as.character)[marker],
    Marker = strsplit(as.character(label.long), ": ", fixed = T)[[1]][1],
    Visit = strsplit(as.character(label.long), ": ", fixed = T)[[1]][2],
    colname = paste0(time, marker)
  )

resp.lb <- expand.grid(
  time = visits, marker = assays,
  ind = c("Resp", "FR2", "FR4", "2lloq", "4lloq", "2llod", "4llod"), stringsAsFactors = F
) %>%
  mutate(Ind = case_when(
    ind == "FR2" ~ "% 2-Fold Rise",
    ind == "FR4" ~ "% 4-Fold Rise",
    ind == "Resp" ~ "Responder",
    ind == "2lloq" ~ "% Greater than 2xLLOQ",
    ind == "4lloq" ~ "% Greater than 4xLLOQ",
    ind == "2llod" ~ "% Greater than 2xLLOD",
    ind == "4llod" ~ "% Greater than 4xLLOD"
  )) 

labels_all <- full_join(labels.assays, resp.lb, by = c("time", "marker")) %>% 
  mutate(mag_cat = colname, resp_cat = paste0(colname, ind))


###################################################
#             Generating the Tables               #
###################################################

# Setup empty tables 
for (i in names(tlf)){
  assign(i, NULL)
}


### Table 1. Demographics 
# Output: tab_dm
# Select the covariates to be summarised.
# num_v are columns from ds_long;
# cat_v are rows of `subgroup`


# Read in original data
dat <- dat_proc

# The stratified random cohort for immunogenicity

ds_s <- dat %>%
  ################################################################
# dplyr::filter(ph1==1) %>%
  mutate(
    raceC = "", #as.character(race),
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
    Arm = factor(ifelse(Trt == 1, "Investigational Vaccine", "Comparator Vaccine"), 
                 levels = c("Investigational Vaccine", "Comparator Vaccine")),
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

if (grepl("AB", config.cor$ph1)) {
  ds <- ds_s
  names(ds) <- gsub("_resp", "Resp", names(ds))
} else {
  ds <- getResponder(ds_s, times=grep("Day", times, value=T), 
                     assays=assay_metadata$assay, pos.cutoffs = pos.cutoffs)
  
}

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
            "Multiracial", 
            "Two or More Races",
            "Other", "Not reported and unknown",  
            labels.minor, 
            paste(labels.age[1], labels.minor),  
            paste(labels.age[2], labels.minor),
            labels.countries.ENSEMBLE,
            "Negative", "Positive")

names(grplev) <- c("All participants", grplev[-1])


char_lev <- c(labels.age, "Mean $\\pm$ SD (Range)", "Mean (Range)","Mean $\\pm$ SD",
              "Female","Male", "White", "Black or African American",
              "Asian", "American Indian or Alaska Native",
              "Native Hawaiian or Other Pacific Islander", "Multiracial",
              "Other", "Not reported and unknown", 
              "White Non-Hispanic", "Communities of Color",
              "Hispanic or Latino","Not Hispanic or Latino",
              "Not reported and unknown ","At-risk","Not at-risk",
              paste(labels.age[1],"At-risk"), paste(labels.age[1], "Not at-risk"), 
              paste(labels.age[2],"At-risk"), paste(labels.age[2], "Not at-risk"),
              paste(labels.age[2], ""), "URM", "Non-URM", labels.countries.ENSEMBLE,
              "Negative", "Positive", labels.BMI, "")


if (study_name %in% c("COVE")) {
  num_v1 <- c("Age") # Summaries - Mean & Range
  num_v2 <- c("BMI") # Summaries - Mean & St.d
  cat_v <- c("AgeC", "SexC", "raceC", "ethnicityC", "HighRiskC", "AgeRiskC", "MinorityC")
} else if (study_name %in% c("NextGen_Mock", "VaxArt_Mock")) {
    num_v1 <- c("Age", "BMI") # Summaries - Mean & Range
    num_v2 <- c("BMI") # Summaries - Mean & St.d
    #cat_v <- c("AgeC", "SexC", "raceC", "ethnicityC", "AgeRiskC")
    cat_v <- c("AgeC", "SexC", "ethnicityC", "AgeRiskC")
    
} else { #if (study_name %in% c("ENSEMBLE", "PREVENT19")) {
  num_v1 <- c("Age") # Summaries - Mean & Range
  num_v2 <- NULL # Summaries - Mean & St.d
  cat_v <- c("AgeC", "SexC", "raceC", "ethnicityC", 
             "HighRiskC", "AgeRiskC", "URMC",  "CountryC", "HIVC", "BMI")
  
  if (study_name %in% c("PROFISCOV")) {
    cat_v <- c("AgeC", "SexC", "raceC", "ethnicityC", 
               "HighRiskC", "AgeRiskC", "URMC", "HIVC", "BMI")
  }
} 

if (!grepl("AB", config.cor$ph1)) {
  # nasal, saliva, sera end with "a" or "l"
  samples_col <- assays_col[substr(assays_col, nchar(assays_col), nchar(assays_col)) %in% c("a", "l")]

  Bsmpl <- ds %>% 
   select(Ptid, all_of(samples_col)) %>% 
   pivot_longer(cols = all_of(samples_col)) %>% 
   filter(!is.na(value)) %>% 
   left_join(labels_all %>% filter(ind=="Resp"), by=c("name"="colname")) %>% 
   mutate(sample=case_when(grepl("sera", name) ~ "sera", 
                           grepl("saliva", name) ~ "saliva", 
                           grepl("nasal", name) ~ "nasal"),
          value=1) %>% 
   distinct(Ptid, time, sample, value) %>% 
   pivot_wider(id_cols=Ptid, names_from=c(time, sample), values_from = value) 
  
  ds <- left_join(ds, Bsmpl)
  
  ds_smpl <- bind_rows(ds %>% filter(B_sera==1) %>% mutate(Bsample="sera"),
                       ds %>% filter(B_saliva==1) %>% mutate(Bsample="saliva"),
                       ds %>% filter(B_nasal==1) %>% mutate(Bsample="nasal"))
  
} else {
  ds <- ds %>% mutate(Bsample="PBMC")
}


subgrp_add <- c("Bsample")

ds_long_ttl <- ds_smpl %>%
  dplyr::filter(!!as.name(paste0("ph2.", "AB."[grepl("AB", config.cor$ph1)], "immuno"))==1) %>% 
  bind_rows(mutate(., Arm="Total")) %>% 
  bind_rows(mutate(., Bsample="RIS")) %>% 
  mutate(AgeRiskC = ifelse(grepl("$\\geq$ 65", AgeRiskC, fixed=T), "Age $\\geq$ 65 ", AgeRiskC)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(all_of(c(num_v1, num_v2, cat_v)), names_to="subgroup", values_to="subgroup_cat")

# Calculate % for categorical covariates
dm_cat <- inner_join(
  ds_long_ttl %>%
    group_by(Arm, across(all_of(subgrp_add)), subgroup, subgroup_cat) %>%
    summarise(n = n(), .groups = 'drop'),
  ds_long_ttl %>%
    group_by(Arm, across(all_of(subgrp_add)), subgroup) %>%
    summarise(N = n(), .groups = 'drop'),
  by = c("Arm", subgrp_add, "subgroup")
) %>%
  mutate(pct = n / N,
         rslt1 = sprintf("%s (%.1f%%)", n, n / N * 100), 
         rslt2 = sprintf("%s/%s = %.1f%%", n, N, n / N * 100)) %>% 
  dplyr::filter(as.character(subgroup) %in% cat_v)


# Calculate mean and range for numeric covariates
dm_num <- ds_long_ttl %>%
  dplyr::filter(subgroup %in% c(num_v1, num_v2)) %>% 
  mutate(subgroup_cat=as.numeric(subgroup_cat)) %>%
  group_by(Arm, across(all_of(subgrp_add)), subgroup) %>%
  summarise(
    min = min(subgroup_cat, na.rm = T), 
    max = max(subgroup_cat, na.rm = T),
    mean = mean(subgroup_cat, na.rm = T),
    sd = sd(subgroup_cat, na.rm = T), 
    rslt0 = sprintf("%.1f $\\pm$ %.1f (%.1f, %.1f)", sd, mean, min, max),
    rslt1 = sprintf("%.1f (%.1f, %.1f)", mean, min, max),
    rslt2 = sprintf("%.1f $\\pm$ %.1f", mean, sd),
    N = n(),
    .groups = 'drop'
  ) %>% 
  mutate(subgroup_cat = case_when(subgroup %in% num_v1 & subgroup %in% num_v2 ~ "Mean $\\pm$ SD (Range)",
                                  subgroup %in% num_v1 ~ "Mean (Range)",
                                  subgroup %in% num_v2 ~ "Mean $\\pm$ SD"),
         subgroup=ifelse(subgroup=="Age", "AgeC", subgroup))


tab_dm <- bind_rows(dm_cat, dm_num) %>%
  mutate(rslt = case_when(subgroup %in% cat_v ~ rslt1,
                          subgroup %in% num_v1 & subgroup %in% num_v2 ~ rslt0, 
                          subgroup %in% num_v1 ~ rslt1,
                          subgroup %in% num_v2 ~ rslt2)) %>%
  mutate(subgroup=ifelse(subgroup %in% c("MinorityC", "raceC"), "RaceEthC", subgroup)) %>% 
  dplyr::filter(subgroup_cat %in% char_lev) %>% 
  inner_join(ds_long_ttl %>% 
               distinct(Arm, Bsample, Ptid) %>% 
               group_by(Arm, Bsample) %>%
               summarise(tot = n()),
             by = c("Arm", subgrp_add)) %>% 
  mutate(Bsample=factor(ifelse(Bsample=="RIS", "RIS", paste0("RIS-", Bsample)), levels = c("RIS", "RIS-sera", "RIS-nasal", "RIS-saliva"))) %>%
  pivot_wider(id_cols = c(subgroup, subgroup_cat, !!subgrp_add),
              names_from = c(Arm), 
              names_sort = T,
              values_from = c(rslt)) %>%
  mutate(Characteristics = factor(subgroup_cat, levels=char_lev),
         subgroup=factor(subgrp[subgroup], levels=subgrp)) %>%
  arrange(Bsample, subgroup, Characteristics) 


tab_dm <- tab_dm %>% 
  select(c(Bsample, subgroup, Characteristics,`Investigational Vaccine`, 
           `Comparator Vaccine`, Total)) %>% 
  select_if(~ !all(is.na(.)))

# tlf$tab_dm$col_name <- gsub("sera_|nasal_|saliva_", "", names(tab_dm))


ds_long_A <- ds_smpl %>% 
  dplyr::filter(Track=="A") %>% 
  bind_rows(mutate(., Arm="Total")) %>% 
  bind_rows(mutate(., Bsample="RIS")) %>% 
  mutate(AgeRiskC = ifelse(grepl("$\\geq$ 65", AgeRiskC, fixed=T), "Age $\\geq$ 65 ", AgeRiskC)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(all_of(c(num_v1, num_v2, cat_v)), names_to="subgroup", values_to="subgroup_cat")

# Calculate % for categorical covariates
dm_cat_A <- inner_join(
  ds_long_A %>%
    group_by(Arm, across(all_of(subgrp_add)), subgroup, subgroup_cat) %>%
    summarise(n = n(), .groups = 'drop'),
  ds_long_A %>%
    group_by(Arm, across(all_of(subgrp_add)), subgroup) %>%
    summarise(N = n(), .groups = 'drop'),
  by = c("Arm", subgrp_add, "subgroup")
) %>%
  mutate(pct = n / N,
         rslt1 = sprintf("%s (%.1f%%)", n, n / N * 100), 
         rslt2 = sprintf("%s/%s = %.1f%%", n, N, n / N * 100)) %>% 
  dplyr::filter(as.character(subgroup) %in% cat_v)


# Calculate mean and range for numeric covariates
dm_num_A <- ds_long_A %>%
  dplyr::filter(subgroup %in% c(num_v1, num_v2)) %>% 
  mutate(subgroup_cat=as.numeric(subgroup_cat)) %>%
  group_by(Arm, across(all_of(subgrp_add)), subgroup) %>%
  summarise(
    min = min(subgroup_cat, na.rm = T), 
    max = max(subgroup_cat, na.rm = T),
    mean = mean(subgroup_cat, na.rm = T),
    sd = sd(subgroup_cat, na.rm = T), 
    rslt1 = sprintf("%.1f (%.1f, %.1f)", mean, min, max),
    rslt2 = sprintf("%.1f $\\pm$ %.1f", mean, sd),
    N = n(),
    .groups = 'drop'
  ) %>% 
  mutate(subgroup_cat = case_when(subgroup %in% num_v1 ~ "Mean (Range)",
                                  subgroup %in% num_v2 ~ "Mean $\\pm$ SD"),
         subgroup=ifelse(subgroup=="Age", "AgeC", subgroup))



tab_dm_A <- bind_rows(dm_cat_A, dm_num_A) %>%
  mutate(rslt = case_when(subgroup %in% cat_v ~ rslt1,
                          subgroup %in% num_v1 ~ rslt1,
                          subgroup %in% num_v2 ~ rslt2)) %>%
  mutate(subgroup=ifelse(subgroup %in% c("MinorityC", "raceC"), "RaceEthC", subgroup)) %>% 
  dplyr::filter(subgroup_cat %in% char_lev) %>% 
  inner_join(ds_long_A %>% 
               distinct(Arm, across(all_of(subgrp_add)),  Ptid) %>% 
               group_by(Arm, across(all_of(subgrp_add))) %>%
               summarise(tot = n()),
             by = c("Arm", subgrp_add)) %>% 
  mutate(Arm = paste0(Arm, "\n(N = ", tot, ")"), subgroup=subgrp[subgroup],
         Bsample=factor(Bsample, levels = c("sera", "nasal", "saliva"))) %>%
  pivot_wider(id_cols = c(subgroup, subgroup_cat),
              names_from = c(!!subgrp_add, Arm), 
              names_sort = T,
              values_from = c(rslt)) %>%
  mutate(Characteristics = factor(subgroup_cat, levels=char_lev),
         subgroup=factor(subgroup, levels=subgrp)) %>%
  arrange(subgroup, Characteristics)


tab_dm_A <- tab_dm_A %>% 
  select_at(c("subgroup", "Characteristics", 
              grep("sera_Investigational" ,names(.), value = T),
              grep("sera_Comparator" ,names(.), value = T),
              grep("sera_Total" ,names(.), value = T),
              grep("nasal_Investigational" ,names(.), value = T),
              grep("nasal_Comparator" ,names(.), value = T),
              grep("nasal_Total" ,names(.), value = T),
              grep("saliva_Investigational" ,names(.), value = T),
              grep("saliva_Comparator" ,names(.), value = T),
              grep("saliva_Total" ,names(.), value = T))) %>% 
  select_if(~ !all(is.na(.)))

tlf$tab_dm_A$col_name <- gsub("sera_|nasal_|saliva_", "", names(tab_dm_A))


print("Done with table 1") 


### Table 2. Responder Rates & Proportions of Magnitudes >= 2FR, 4FR
# For each binding antibody marker, the estimated percentage of participants
# defined as responders, and with concentrations >= 2x LLOQ or >=
# 4 x LLOQ, will be provided with the corresponding 95% CIs
# 
# Output: tab_bind


# Variables used for stratification in the tables
# subgroup: SAP Table 6: Baseline Subgroups
# Arm and Baseline: Assigned treatment Arms * Baseline SARS-CoV-2-19 Status
# Group: Category in each subgroup

sub.by <- c("Arm")
# resp.v <- grep("Resp|2lloq|4lloq|FR2|FR4", names(ds), value = T) 
resp.v <- grep("Resp", names(ds), value = T) 
resp.v <- resp.v[sapply(resp.v, function(x)!all(is.na(ds[, x])))]

if (study_name%in%c("COVE")) {
  subs <- c("All", "AgeC", "HighRiskC", "AgeRiskC", "AgeRisk1", "AgeRisk2", "SexC",
            "AgeSexC", "ethnicityC", "RaceEthC", "MinorityC", "AgeMinorC")
} else if (study_name%in%c("NextGen_Mock")) {
  subs <- c("All", "AgeC", "HighRiskC", "SexC", #"AgeRiskC", "AgeRisk1", "AgeRisk2", 
            "AgeSexC", "ethnicityC", "AgeMinorC")
} else if (study_name %in% c("ENSEMBLE", "PREVENT19")) {
  subs <- c("All", "AgeC", "HIVC", "CountryC", "HighRiskC", "AgeRiskC", "AgeRisk1", "AgeRisk2", "SexC",
            "AgeSexC", "ethnicityC", "RaceEthC", "URMC"[0 %in% ds$Region], "AgeURM"[0 %in% ds$Region])
} else if (study_name %in% c("PROFISCOV")) {
  subs <- c("All", "AgeC", "HIVC", "HighRiskC", "AgeRiskC", "AgeRisk1", "AgeRisk2", "SexC",
            "AgeSexC", "ethnicityC", "RaceEthC", "URMC", "AgeURM")
}

rpcnt <- get_rr(dat=ds, v=resp.v, subs=subs, sub.by=sub.by, strata="tps.stratum",
                weights=paste0("wt.", "AB."[grepl("AB", config.cor$ph1)], "immuno"), 
                subset=paste0("ph2.", "AB."[grepl("AB", config.cor$ph1)], "immuno"))

tab_rr <- rpcnt %>% 
  dplyr::filter(!subgroup %in% c("AgeRisk1", "AgeRisk2") & Visit != "Day 1" & Group %in% names(grplev)) %>% 
  mutate(subgroup=factor(subgrp[subgroup], levels=subgrp), Group=factor(grplev[Group], levels=grplev)) %>% 
  pivot_wider(
    id_cols = c(subgroup, Group, Arm, Marker, Visit, N),
    names_from = Ind, values_from = rslt) %>% 
  arrange(subgroup, Group, Visit, Arm, Marker)

if(any(grepl("bind", assays))){
  
  if (all(c("% Greater than 2xLLOQ", "% Greater than 4xLLOQ") %in% names(tab_rr))){
    tab_bind1 <- tab_rr %>% 
      dplyr::filter(Marker %in% labels_all$Marker[grep("bind", labels_all$marker)]) %>% 
      select(subgroup, Group, Visit, Arm, Marker, N, Responder, 
             c("% Greater than 2xLLOQ", "% Greater than 4xLLOQ"))
  }
  
  tab_bind2 <- tab_rr %>% 
    dplyr::filter(Marker %in% labels_all$Marker[grep("bind", labels_all$marker)]) %>% 
    select(subgroup, Group, Visit, Arm, Marker, N, Responder)
}

print("Done with table2") 

# Table 3 & 4. For the ID50 pseudo-virus & MN50 WT live virus neutralization 
# antibody marker, the estimated percentage of participants defined as 
# responders, participants with % 2-Fold Rise (2FR), and participants with 4-fold 
# rise (4FR) will be provided with the corresponding 95% CIs 
# 
# Output: tab_pseudo & tab_wt

if(any(grepl("pseudoneutid50", assays))){
tab_pseudo <- tab_rr %>% 
  dplyr::filter(Marker %in% labels_all$Marker[grep("pseudoneutid50", labels_all$marker)]) %>% 
  select(subgroup, Group, Visit, Arm, `Baseline SARS-CoV-2`, Marker, N, Responder
         #,`% 2-Fold Rise`, `% 4-Fold Rise`
         )
}else{
  tab_pseudo <- NULL
}

if(any(grepl("T4|T8", assays))){
tab_t <- tab_rr %>% 
  dplyr::filter(Marker %in% labels_all$Marker[grep("T4|T8", labels_all$marker)]) %>% 
  select(subgroup, Group, Visit, Arm, `Baseline SARS-CoV-2`, Marker, N, Responder
        # ,`% 2-Fold Rise`, `% 4-Fold Rise`
        )
}else{
  tab_t <- NULL
}
print("Done with table3 & 4") 

# Table 5. Geometric mean titers (GMTs) and geometric mean concentrations (GMCs)
# will be summarized along with their 95% CIs using the t-distribution
# approximation of log-transformed concentrations/titers (for each of the 5
# Spike-targeted marker types including pseudovirus-nAb ID50 and ID80
# and WT live virus-nAb MN50, as well as for binding Ab to N).
# 
# Output: tab_gm
gm.v <- c(assays_col, grep("Delta", names(ds), value = T))
gm.v <- gm.v[!grepl('cat', gm.v)]
gm.v <- gm.v[sapply(gm.v, function(x)!all(is.na(ds[, x])))]
rgm <- get_gm(dat=ds, v=gm.v, subs=subs, sub.by=sub.by, strata="tps.stratum",
              weights=paste0("wt.", "AB."[grepl("AB", config.cor$ph1)], "immuno"), 
              subset=paste0("ph2.", "AB."[grepl("AB", config.cor$ph1)], "immuno"))

tab_gm <- rgm %>% 
  dplyr::filter(!subgroup %in% c("AgeRisk1", "AgeRisk2") & !grepl("Delta", mag_cat) & Group %in% names(grplev)) %>% 
  mutate(subgroup=factor(subgrp[subgroup], levels=subgrp), Group=factor(grplev[Group], levels=grplev)) %>% 
  arrange(subgroup, Group, Visit, Arm, `Baseline SARS-CoV-2`, Marker) %>% 
  select(subgroup, Group, Visit, Arm, `Baseline SARS-CoV-2`, Marker, N, `GMT/GMC`) 

print("Done with table5") 

### Table 6. GMTRs/GMCRs will be summarized with 95% CI (t-distribution 
# approximation) for any post-baseline values compared to baseline, and
# post-Day 57 values compared to Day 57
# 
# Output: tab_gmr

gmr_gm <- inner_join(
  tab_gm %>% 
    dplyr::filter(Visit == "Day 01") %>% 
    select(-Visit) %>% 
    rename(`Baseline\nGMT/GMC` = `GMT/GMC`),
  tab_gm %>% 
    dplyr::filter(Visit != "Day 01") %>% 
    rename(`Post Baseline\nGMT/GMC` = `GMT/GMC`),
  by = c("subgroup", "Arm", "Group", "Baseline SARS-CoV-2", "N", "Marker")) %>% 
  dplyr::filter(!subgroup %in% c("AgeRisk1", "AgeRisk2")) %>% 
  mutate(Visit = paste0(gsub("ay ", "", Visit), " fold-rise over D01"))

tab_gmr <- rgm %>% 
  dplyr::filter(grepl("overB", mag_cat)) %>%
  mutate(`GMTR/GMCR`=sprintf("%.2f\n(%.2f, %.2f)", 10^mag, 10^ci_l, 10^ci_u),
         subgroup=factor(subgrp[subgroup], levels=subgrp), Group=factor(grplev[Group], levels=grplev)) %>% 
  select(subgroup, Group, Arm, `Baseline SARS-CoV-2`, Visit, N, Marker, `GMTR/GMCR`) %>% 
  inner_join(
    gmr_gm,
    c("subgroup", "Group", "Arm", "Baseline SARS-CoV-2", "Visit", "N", "Marker")) %>% 
  arrange(subgroup, Group, Visit, Arm, `Baseline SARS-CoV-2`, Marker) %>% 
  mutate(Arm=gsub("vaccine", "", Arm)) %>% 
  select(subgroup, Group, Visit, Arm, #`Baseline SARS-CoV-2`, 
         Marker, N,  
         `Baseline\nGMT/GMC`, `Post Baseline\nGMT/GMC`, `GMTR/GMCR`) 

print("Done with table6") 

### Table 7. The ratios of GMTs/GMCs will be estimated between groups with the
# two-sided 95% CIs calculated using t-distribution approximation of 
# log-transformed titers/concentrations
# Output: tab_rgmt
# 
# Ratios of GMT/GMC between subgroups among vacinees


comp_lev <- c("Comparator vaccine", "Investigational vaccine", 
              labels.age[2:1],
              "Male", "Female")

groups <- c("AgeC", "SexC")


mag_groups <- assays_col
mag_groups <- mag_groups[sapply(mag_groups, function(x)!all(is.na(ds[,x])))]

rgmt <- get_rgmt(ds, mag_groups, groups, comp_lev=comp_lev, 
                 sub.by, "tps.stratum", paste0("wt.", "AB."[grepl("AB", config.cor$ph1)], "immuno"), 
                 paste0("ph2.", "AB."[grepl("AB", config.cor$ph1)], "immuno"))

rgmt_ <- get_rgmt(ds, mag_groups, "Arm", comp_lev=c("Comparator vaccine", "Investigational vaccine"), 
                  c("All", "`Baseline SARS-CoV-2`"), "tps.stratum", 
                  paste0("wt.", "AB."[grepl("AB", config.cor$ph1)], "immuno"), 
                  paste0("ph2.", "AB."[grepl("AB", config.cor$ph1)], "immuno"))

rgmt_ <- rgmt_ %>% mutate(subgroup="All", Arm="-")

rgmt_gm <- rgm %>% 
  dplyr::filter(!grepl("Delta", mag_cat) & Group %in% names(grplev)) %>% 
  mutate(subgroup=factor(subgrp[subgroup], levels=subgrp), Group=factor(grplev[Group], levels=grplev)) %>% 
  dplyr::filter(subgroup %in% subgrp[groups]) %>% 
  mutate(groupn = 2-match(Group, comp_lev)%%2) %>% 
  pivot_wider(id_cols = c(subgroup, Arm, `Baseline SARS-CoV-2`, Visit, Marker),
              names_from = groupn, values_from = `GMT/GMC`, 
              names_prefix = "Group")

rgmt_gm_ <- rgm %>% 
  dplyr::filter(!grepl("Delta", mag_cat) & Group=="All participants") %>% 
  mutate(subgroup=factor(subgrp[subgroup], levels=subgrp), Group=factor(grplev[Group], levels=grplev)) %>% 
  mutate(groupn = 2-match(Arm, comp_lev)%%2, Arm="-") %>% 
  pivot_wider(id_cols = c(subgroup, Arm, `Baseline SARS-CoV-2`, Visit, Marker),
              names_from = groupn, values_from = `GMT/GMC`, 
              names_prefix = "Group")


tab_rgmt <- inner_join(bind_rows(rgmt_gm_, rgmt_gm), 
                       bind_rows(rgmt_, rgmt) %>% mutate(subgroup=factor(subgrp[subgroup], levels=subgrp)),  
                       c("Baseline SARS-CoV-2", "Arm", "subgroup", "Marker", "Visit")) %>% 
  rename(`Group 1 vs 2` = comp, 
         `Group 1 GMT/GMC` = `Group1`, 
         `Group 2 GMT/GMC` = `Group2`) %>% 
  select(`Group 1 vs 2`, subgroup, Visit, Arm, 
         # `Baseline SARS-CoV-2`, 
         Marker, 
         `Group 1 GMT/GMC`, `Group 2 GMT/GMC`, `Ratios of GMT/GMC`) %>% 
  arrange(subgroup, Visit, Arm, 
          # `Baseline SARS-CoV-2`, 
          Marker)

print("Done with table7") 

### Table 8. The differences in the responder rates, 2FRs, 4FRs between groups 
# will be computed along with the two-sided 95% CIs by the Wilson-Score
# method without continuity correction (Newcombe, 1998).
# Output: tab_rrdiff

tab_rrdiff <- bind_rows(
                        rpcnt %>%
                          dplyr::filter(subgroup=="All") %>%
                          mutate(Group=Arm, Arm="-"),
                        # rpcnt %>% 
                        #   dplyr::filter(subgroup=="All") %>% 
                        #   mutate(Group=`Baseline SARS-CoV-2`, `Baseline SARS-CoV-2`="-"),
                        rpcnt)%>% 
  dplyr::filter(subgroup %in% c(groups, "All") & grepl("Resp|FR2|FR4",resp_cat)) %>% 
  mutate(groupn = 2-match(Group, c(comp_lev, "Vaccine", "Placebo", "Positive", "Negative"))%%2) %>% 
  pivot_wider(id_cols = c(subgroup, Arm, `Baseline SARS-CoV-2`, Marker, Visit, Ind),
              names_from = groupn, values_from = c(response, ci_l, ci_u), names_sep = "") %>% 
  full_join(distinct(rgmt, subgroup, comp)) %>% 
  mutate(Comparison = case_when(Arm=="-"~"Comparator vaccine vs Investigational vaccine",
                          `Baseline SARS-CoV-2`=="-"~"Positive vs Negative",
                          TRUE~comp),
         subgroup = factor(case_when(Arm=="-" ~ "Arm",
                           `Baseline SARS-CoV-2`=="-" ~ "Baseline SARS-CoV-2",
                            TRUE~subgrp[subgroup]), levels=c("Arm", "Baseline SARS-CoV-2", subgrp)),
         Estimate = response1-response2,
         ci_l = Estimate-sqrt((response1-ci_l1)^2+(response2-ci_u2)^2),
         ci_u = Estimate+sqrt((response1-ci_u1)^2+(response2-ci_l2)^2),
         rslt = ifelse(is.na(Estimate), "-", 
                       sprintf("%s\n(%s, %s)", round(Estimate,2), round(ci_l,2), round(ci_u,2)))) %>%
  dplyr::filter(!is.na(Comparison)) %>%
  pivot_wider(id_cols=c(Comparison, subgroup, `Baseline SARS-CoV-2`, Arm, Visit, Marker), 
              names_from = Ind, values_from = rslt) %>%
  select(Comparison, subgroup, `Baseline SARS-CoV-2`, Arm, Visit, Marker, 
         `Responder Rate Difference`=Responder#, `% 2-Fold Rise`, `% 4-Fold Rise`
         ) %>% 
  arrange(subgroup, Visit, `Baseline SARS-CoV-2`, Marker, Comparison) 

print("Done with table8") 

# renameing for ICS

if(grepl("AB", config.cor$ph1)){
  names(tab_gm) <- gsub("GMT/GMC", "GMR", names(tab_gm))
  names(tab_gmr) <- gsub("GMT/GMC", "GMR", names(tab_gmr))
  names(tab_gmr) <- gsub("GMTR/GMCR", "GMRR", names(tab_gmr))
  names(tab_rgmt) <- gsub("GMT/GMC", "GMR", names(tab_rgmt))
}




print("Done with all tables") 

save.results.to <- here::here("output")
if (!dir.exists(save.results.to))  dir.create(save.results.to)
save.results.to <- paste0(here::here("output"), "/", attr(config,"config"))

if (!dir.exists(save.results.to))  dir.create(save.results.to)
print(paste0("save.results.to equals ", save.results.to))

save(list = c("tlf", names(tlf)), file = file.path(save.results.to, sprintf("Tables%s.Rdata", COR)))

