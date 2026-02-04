##################################################
# obligatory to append to the top of each script #
renv::activate(project = here::here(".."))
source(here::here("..", "_common.R")) #
##################################################

library(survey)
library(tidyverse)
library(PropCIs)
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
# For stratum with 1 ppt
options(survey.lonely.psu="adjust")
source(here::here("code", "make_functions_nextGen.R"))


###################################################
#                  Parameters                     #
###################################################
# To select which tables are included in the report.
# Also to modify the headers and footers for each table.


tlf <-
  list(

    tab_strtm1 = NULL,
	
    tab_case_p = list(
      table_header = "Positivity frequencies and geometric means at D01 and D31, 
      stratified by non-cases and vaccination-proximal cases for the Investigational Vaccine arm and serum samples (ccIAS-sera). 
      Point and 95\\% CI estimates use IPS weights. Immune markers IgG bAb against Spike Index, Delta AY.4, BA.5, BA.2.86, XBB.1.5, JN.1, KP.2, KP.3, LB.1, 
      Spike cross-reactivity score, and IgG bAb against N Index. Positivity frequency is the percentage of values > LLOQ. 
      For each entry, the number with positivity and unweighted positivity frequency is reported, 
      along with the weighted positivity frequency (Posit. Rate results based on weighted frequency). 
      AU, Arbitrary Units. bAb, binding antibodies. CI, confidence interval. N, Nucleocapsid protein.",
      

      table_footer =c(),
      col_name = c("Marker", "N", "Posit. Rate", "GM", "N",
                   "Posit. Rate", "GM", "Posit. Rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Proximal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9), 

    tab_gmr_p = list(
      table_header = "Vaccine-response frequencies and geometric mean fold-rise at D31 over D01, 
      stratified by non-cases and vaccination-proximal cases for the Investigational Vaccine arm and serum samples (ccIAS-sera). 
      Point and 95\\% CI estimates use IPS weights. Immune markers IgG bAb against Spike Index, Delta AY.4, BA.5, BA.2.86, XBB.1.5, JN.1, KP.2, KP.3, LB.1, 
      Spike cross-reactivity score, and IgG bAb against N Index. Vaccine-responder frequency is NA for the Spike cross-reactivity score. 
      Vaccine-response at D31 occurs if the D01 value is < LLOQ at D01 and becomes $\\geq$ 4 times 
      LLOQ at the post vaccination time point, or if the D01 value is $\\geq$ LLOQ and < ULOQ at D01 and becomes $\\geq$ 4 times higher post vaccination. 
      Also, a D01 value $\\geq$ ULOQ implies a missing value, and a D01 value < ULOQ and post vaccination value $\\geq$ ULOQ implies a positive response even if 
      the fold-change between D01 and the ULOQ is < 4. 
      For each entry, the number with vaccine-response and unweighted vaccine-response frequency is reported, 
      along with the weighted frequency (Resp Rate results based on weighted frequency). AU, Arbitrary Units. bAb, binding antibodies. CI, confidence interval. N, Nucleocapsid protein.",
      
      table_footer =c(),
      col_name = c("Marker", "N", "Resp rate", "GM", "N",
                   "Resp rate", "GM", "Resp rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Proximal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9), 
    
    
    tab_case_d = list(
      table_header = "Positivity frequencies and geometric means at D01 and D31, 
      stratified by non-cases and vaccination-distal cases for the Investigational Vaccine arm and serum samples (ccIAS-sera). 
      Point and 95\\% CI estimates use IPS weights. Immune markers IgG bAb against Spike Index, Delta AY.4, BA.5, BA.2.86, XBB.1.5, JN.1, KP.2, KP.3, LB.1, 
      Spike cross-reactivity score, and IgG bAb against N Index. Positivity frequency is the percentage of values > LLOQ. 
      For each entry, the number with positivity and unweighted positivity frequency is reported, 
      along with the weighted positivity frequency (Posit. Rate results based on weighted frequency). 
      AU, Arbitrary Units. bAb, binding antibodies. CI, confidence interval. N, Nucleocapsid protein.",
      table_footer =c(),
      col_name = c("Marker", "N", "Posit. Rate", "GM", "N",
                   "Posit. Rate", "GM", "Posit. Rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Distal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9), 
    
    tab_gmr_d = list(
      table_header = "Vaccine-response frequencies and geometric mean fold-rise at D31 over D01, 
      stratified by non-cases and vaccination-distal cases for the Investigational Vaccine arm and serum samples (ccIAS-sera). 
      Point and 95\\% CI estimates use IPS weights. Immune markers IgG bAb against Spike Index, Delta AY.4, BA.5, BA.2.86, XBB.1.5, JN.1, KP.2, KP.3, LB.1, 
      Spike cross-reactivity score, and IgG bAb against N Index. Vaccine-responder frequency is NA for the Spike cross-reactivity score. 
      Vaccine-response at D31 occurs if the D01 value is < LLOQ at D01 and becomes $\\geq$ 4 times 
      LLOQ at the post vaccination time point, or if the D01 value is $\\geq$ LLOQ and < ULOQ at D01 and becomes $\\geq$ 4 times higher post vaccination. 
      Also, a D01 value $\\geq$ ULOQ implies a missing value, and a D01 value < ULOQ and post vaccination value $\\geq$ ULOQ implies a positive response even if 
      the fold-change between D01 and the ULOQ is < 4. 
      For each entry, the number with vaccine-response and unweighted vaccine-response is reported, 
      along with the weighted frequency (Resp Rate results based on weighted frequency). AU, Arbitrary Units. bAb, binding antibodies. CI, confidence interval. N, Nucleocapsid protein.",
      
      table_footer =c(),
      col_name = c("Marker", "N", "Resp rate", "GM", "N",
                   "Resp rate", "GM", "Resp Rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Distal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9), 
    
    
    
    tab_case_all_p = list(
      table_header = "Vaccine-responder frequencies at D31 and response frequencies at D01 and D31, and vaccine-responder frequencies at D31 and geometric mean fold-rise at D31 over D01,  
      stratified by non-cases and vaccination-proximal cases for the 
      Investigational Vaccine arm and all samples. Point and 95\\% CI estimates use IPS weights. 
      Vaccine-responder frequency is NA for the Spike cross-reactivity score. Response frequency is the 
      percentage of values > LLOQ. Vaccine-response at a post-baseline time point occurs if the D01 value is < LLOQ at D01 and becomes $\\geq$ 4 times 
      LLOQ at the post vaccination time point, or if the D01 value is $\\geq$ LLOQ and < ULOQ at D01 and becomes $\\geq$ 4 times higher post vaccination. 
      Also, a D01 value $\\geq$ ULOQ implies a missing value, and a D01 value < ULOQ and post vaccination value $\\geq$ ULOQ implies a positive response even if 
      the fold-change between D01 and the ULOQ is < 4. 
      For each entry, the number with vaccine-response and unweighted positivity frequency is reported, 
      along with the weighted frequency (Resp Rate results based on weighted frequency). AU, Arbitrary Units. bAb, binding antibodies. 
      CI, confidence interval. N, Nucleocapsid protein.",
      
      table_footer =c(),
      col_name = c("Marker", "N", "Posit. Rate", "GM", "N",
                   "Posit. Rate", "GM", "Posit. Rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Proximal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9),
    
    tab_gmr_all_p = list(
      table_header = "Vaccine response frequencies and geometric means at D01 and D31, and vaccine-responder frequencies at D31 and geometric mean fold-rise at D31 over D01, 
      stratified by non-cases and vaccination-proximal cases for the Investigational Vaccine arm and serum samples 
      (ccIAS-sera). Point and 95\\% CI estimates use IPS weights. Immune markers IgG bAb against Spike Index, Delta AY.4, BA.5, BA.2.86, XBB.1.5, JN.1, KP.2, KP.3, LB.1, 
      Spike cross-reactivity score, and IgG bAb against N Index. Vaccine-responder frequency is NA for the Spike cross-reactivity score. 
      Vaccine-response at D31 occurs if the D01 value is < LLOQ at D01 and becomes $\\geq$ 4 times 
      LLOQ at the post vaccination time point, or if the D01 value is $\\geq$ LLOQ and < ULOQ at D01 and becomes $\\geq$ 4 times higher post vaccination. 
      Also, a D01 value $\\geq$ ULOQ implies a missing value, and a D01 value < ULOQ and post vaccination value $\\geq$ ULOQ implies a positive response even if 
      the fold-change between D01 and the ULOQ is < 4. 
      For each entry, the number with vaccine-response and unweighted vaccine-response is reported, 
      along with the weighted frequency (Resp Rate results based on weighted frequency). AU, Arbitrary Units. bAb, binding antibodies. CI, confidence interval. N, Nucleocapsid protein.",
      
      table_footer =c(),
      col_name = c("Marker", "N", "Resp rate", "GM", "N",
                   "Resp rate", "GM", "Resp Rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Proximal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9), 
    
    
    tab_case_all_d = list(
      table_header = "Vaccine-responder frequencies at D31 and response frequencies at D01 and D31, and vaccine-responder frequencies at D31 and geometric mean fold-rise at D31 over D01,   
      stratified by non-cases and vaccination-distal cases for the 
      Investigational Vaccine arm and all samples. Point and 95\\% CI estimates use IPS weights. 
      Vaccine-responder frequency is NA for the Spike cross-reactivity score. Response frequency is the 
      percentage of values > LLOQ. Vaccine-response at a post-baseline time point occurs if the D01 value is < LLOQ at D01 and becomes $\\geq$ 4 times 
      LLOQ at the post vaccination time point, or if the D01 value is $\\geq$ LLOQ and < ULOQ at D01 and becomes $\\geq$ 4 times higher post vaccination. 
      Also, a D01 value $\\geq$ ULOQ implies a missing value, and a D01 value < ULOQ and post vaccination value $\\geq$ ULOQ implies a positive response even if 
      the fold-change between D01 and the ULOQ is < 4. 
      For each entry, the number with vaccine-response and unweighted positivity frequency is reported, 
      along with the weighted frequency (Resp Rate results based on weighted frequency). AU, Arbitrary Units. bAb, binding antibodies. 
      CI, confidence interval. N, Nucleocapsid protein.",
      
      table_footer =c(),
      col_name = c("Marker", "N", "Posit. Rate", "GM", "N",
                   "Posit. Rate", "GM", "Posit. Rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Distal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9),
    
    
    tab_gmr_all_d = list(
      table_header = "Vaccine response frequencies and geometric means at D01 and D31, and vaccine-responder frequencies at D31 and geometric mean fold-rise at D31 over D01, 
      stratified by non-cases and vaccination-distal cases for the Investigational Vaccine arm and serum samples 
      (ccIAS-sera). Point and 95\\% CI estimates use IPS weights. Immune markers IgG bAb against Spike Index, Delta AY.4, BA.5, BA.2.86, XBB.1.5, JN.1, KP.2, KP.3, LB.1, 
      Spike cross-reactivity score, and IgG bAb against N Index. Vaccine-responder frequency is NA for the Spike cross-reactivity score. 
      Vaccine-response at D31 occurs if the D01 value is < LLOQ at D01 and becomes $\\geq$ 4 times 
      LLOQ at the post vaccination time point, or if the D01 value is $\\geq$ LLOQ and < ULOQ at D01 and becomes $\\geq$ 4 times higher post vaccination. 
      Also, a D01 value $\\geq$ ULOQ implies a missing value, and a D01 value < ULOQ and post vaccination value $\\geq$ ULOQ implies a positive response even if 
      the fold-change between D01 and the ULOQ is < 4. 
      For each entry, the number with vaccine-response and unweighted vaccine-response is reported, 
      along with the weighted frequency (Resp Rate results based on weighted frequency). AU, Arbitrary Units. bAb, binding antibodies. CI, confidence interval. N, Nucleocapsid protein.",
      
      table_footer =c(),
      col_name = c("Marker", "N", "Resp rate", "GM", "N",
                   "Resp rate", "GM", "Resp Rate\nDifference", "GMR"),
      header_above1 = c(" "=1, "Vaccination-Distal Cases" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      pack_row="Visit", 
      deselect="Visit",
      col1="4cm",
      font_size=9)
    
    
  )


assays <- assay_metadata$assay    
timepoints <- config$timepoints

labels.age <-  c("Age $<$ 65", "Age $\\geq$ 65")

labels.minor <- c("Communities of Color", "White Non-Hispanic")

labels.BMI <- c("Underweight BMI < 18.5", "Normal 18.5 $\\leq$ BMI < 25", 
                "Overweight 25 $\\leq$ BMI < 30", "Obese BMI $\\geq$ 30")

# labels.time <- labels.time[times]
times <- c("B", "Day31", "Delta31overB", "Day91", "Delta91overB", "Day181", "Delta181overB", "Day366", "Delta366overB")
labels.time <- c("Day 01", sapply(c(31, 91, 181, 366), sprintf, fmt=c("Day %s", "D%s fold-rise over D01")))
names(labels.time) <- times


labels.assays.short <- labels.assays.short.tabular[assays]

# redefines what is in _common.R to use shorter names
labels.assays.long <- data.frame (purrr::imap_dfc(labels.assays.short, ~ paste0(labels.assays.short[.y], ": ", labels.time)))
rownames(labels.assays.long) <- names(labels.time)

visits <- names(labels.time)#[!grepl("Delta", names(labels.time))]
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
  # ind = c("Resp", "FR2", "FR4", "2lloq", "4lloq", "2llod", "4llod"), 
  ind = "Resp", 
  stringsAsFactors = F
) %>%
  mutate(Ind = case_when(
    # ind == "FR2" ~ "% 2-Fold Rise",
    # ind == "FR4" ~ "% 4-Fold Rise",
    ind == "Resp" ~ "Responder"
    # ind == "2lloq" ~ "% Greater than 2xLLOQ",
    # ind == "4lloq" ~ "% Greater than 4xLLOQ",
    # ind == "2llod" ~ "% Greater than 2xLLOD",
    # ind == "4llod" ~ "% Greater than 4xLLOD"
  )) 

labels_all <- full_join(labels.assays, resp.lb, by = c("time", "marker")) %>% 
  mutate(mag_cat = colname, resp_cat = paste0(colname, ind))



###################################################
#                Clean the Data                   #
###################################################

### Table 1. Demographics 
# Output: tab_dm
# Select the covariates to be summarised.
# num_v are columns from ds_long;
# cat_v are rows of `subgroup`
# Setup empty tables 

# dat.mock was made in _common.R
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

# Step2: Responders
# Post baseline visits
pos.cutoffs <- assay_metadata$pos.cutoff
names(pos.cutoffs) <- assay_metadata$assay

# if (grepl("AB", config.cor$ph1)) {
  ds <- ds_s
  names(ds) <- gsub("_resp", "Resp", names(ds))
# } else {
  ds <- getResponder(ds_s, times=c("B", grep("Day", times, value=T)), 
                     assays=assay_metadata$assay, pos.cutoffs = pos.cutoffs)
# }

if(grepl("tcell", COR)){
  ds <- ds %>% select(!contains("Resp", ignore.case = F))
  names(ds) <- gsub("_resp", "Resp", names(ds))
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

###################################################
#             Generating the Tables               #
###################################################
# Setup empty tables 

for (i in names(tlf)){
  assign(i, NULL)
}


# Cases & Non-cases



# Generate a full table with all the estimates: response rates, GMT, GMTR, etc.
# (Per Peter's email Feb 5, 2021)
# Cases vs Non-cases

  mrks <- c("bindSpike_IgG_%s", "bindSpike_IgG_%s_delta_AY.4", "bindSpike_IgG_%s_BA.5", "bindSpike_IgG_%s_BA.2.86",
            "bindSpike_IgG_%s_XBB.1.5", "bindSpike_IgG_%s_JN.1", "bindSpike_IgG_%s_KP.2", "bindSpike_IgG_%s_KP.3",
            "bindSpike_IgG_%s_LB.1", "bindN_IgG_%s", "bindSpike_IgG_%s_mdw")
  
  allmrks <- sprintf(mrks, "sera")
  allmrks_lev <- assay_metadata$assay_label_short[match(allmrks, assay_metadata$assay)]
  
  
  tab_case.v <- apply(expand.grid(c("B", "Day31", "Delta31overB"), allmrks), 1, paste, collapse="")
  # mag.v <- intersect(mag.v, names(ds))
  # mag.v <- mag.v[sapply(mag.v, function(x)!all(is.na(ds[, x])))]
  # # mag.v <- mag.v[substr(mag.v, nchar(mag.v)-2, nchar(mag.v))!="cat" & !grepl("FR2|FR4|Resp", mag.v)]
  # 
  # resp.v <- paste0(mag.v, "Resp")
  # resp.v <- intersect(resp.v, names(ds))
  # resp.v <- resp.v[sapply(resp.v, function(x)!all(is.na(ds[, x])))]
  
  
  mag.v <- apply(expand.grid(c("B", "Day31"), assay_metadata$assay), 1, paste, collapse="")
  mag.v <- intersect(mag.v, names(ds))
  mag.v <- mag.v[sapply(mag.v, function(x)!all(is.na(ds[, x])))]
  # mag.v <- mag.v[substr(mag.v, nchar(mag.v)-2, nchar(mag.v))!="cat" & !grepl("FR2|FR4|Resp", mag.v)]
  Delta31 <- gsub("Day31", "Delta31overB", mag.v[substr(mag.v, 1, 5)=="Day31"])
  
  resp.v <- paste0(mag.v, "Resp")
  resp.v <- intersect(resp.v, names(ds))
  resp.v <- resp.v[sapply(resp.v, function(x)!all(is.na(ds[, x])))]
  
  subs <- "Case"
  comp.p <- c("Vaccination-Proximal Cases", "Non-Cases")
  comp.d <- c("Vaccination-Distal Cases", "Non-Cases")
  sub.by <- c("Arm")
  
  ds.i <- filter(ds, !!as.name(config.cor$ph1)==1) %>% 
    mutate(!!sym(config.cor$ph2) := as.logical(!!as.name(config.cor$ph2)))
  
  
  # Per-protocol is defined by ph1.D15
  ds_p <- ds.i %>%
    mutate(Case = factor(
      case_when(Perprotocol==1 & !!as.name(config.cor$ph2)==1 & 
                  !!as.name(config.cor$EventIndPrimary)==1 & 
                  !!as.name(config.cor$EventTimePrimary) >= 7 & !!as.name(config.cor$EventTimePrimary) <= 181 ~ "Vaccination-Proximal Cases",
                Perprotocol==1 & !!as.name(config.cor$ph2)==1 & 
                  !!as.name(config.cor$EventIndPrimary)==1 & 
                  !!as.name(config.cor$EventTimePrimary) > 181 ~ "Vaccination-Distal Cases",
                
                Perprotocol==1 & !!as.name(config.cor$ph2)==1 &  
                  AnyinfectionD1==0 ~ "Non-Cases"),
      
      levels = c("Vaccination-Proximal Cases", "Non-Cases"))
    )
  
  
  ds_d <- ds.i %>%
    mutate(Case = factor(
      case_when(Perprotocol==1 & !!as.name(config.cor$ph2)==1 & 
                  !!as.name(config.cor$EventIndPrimary)==1 & 
                  !!as.name(config.cor$EventTimePrimary) >= 7 & !!as.name(config.cor$EventTimePrimary) <= 181 ~ "Vaccination-Proximal Cases",
                
              Perprotocol==1 & !!as.name(config.cor$ph2)==1 & 
                  !!as.name(config.cor$EventIndPrimary)==1 & 
                  !!as.name(config.cor$EventTimePrimary) > 181 ~ "Vaccination-Distal Cases",
                
                Perprotocol==1 & !!as.name(config.cor$ph2)==1 &  
                  AnyinfectionD1==0 ~ "Non-Cases"),
      
      levels = c("Vaccination-Distal Cases", "Non-Cases"))
    )
  
  
  rpcnt_case_p <- get_rr(dat=ds_p, v=resp.v, subs=subs, sub.by=sub.by, strata=config.cor$WtStratum,
                  weights=config.cor$wt, subset=config.cor$ph2)
  
  rgm_case_p  <- get_gm(ds_p, c(mag.v, Delta31), subs, sub.by, strata=config.cor$WtStratum, weights=config.cor$wt, subset=config.cor$ph2) 
  rgmt_case_p <- get_rgmt(ds_p, mag.v, subs, comp_lev=comp.p, sub.by, strata=config.cor$WtStratum, weights=config.cor$wt, subset=config.cor$ph2) 
  
  

  
  rrdiff_case_p <- rpcnt_case_p %>% 
    # dplyr::filter(subgroup %in% subs & grepl("Resp",resp_cat)) %>% 
    mutate(groupn = 2-match(Group, comp.p)%%2) %>%
    pivot_wider(id_cols = c(subgroup, Arm, Visit, Marker, Ind),
                names_from = groupn, values_from = c(response, ci_l, ci_u), names_sep = "") 
  
  responseNA <- setdiff(as.vector(outer(c("response", "ci_l", "ci_u"), 1:2, paste0)), names(rrdiff_case_p))
  rrdiff_case_p[, responseNA] <- NA
  
  rrdiff_case_p <- rrdiff_case_p %>% 
    mutate(Estimate = response1-response2,
           ci_l = Estimate-sqrt((response1-ci_l1)^2+(response2-ci_u2)^2),
           ci_u = Estimate+sqrt((response1-ci_u1)^2+(response2-ci_l2)^2),
           rrdiff = ifelse(!is.na(Estimate), 
                           sprintf("%s\n(%s, %s)", round(Estimate, 3), round(ci_l, 3), round(ci_u, 3)),
                           "-")) 
  
  print("Done with table6")
  
  tab_case_p <- full_join(rpcnt_case_p %>% mutate(rslt = sprintf("%s %.1f%%\n%.1f%% (%.1f%%, %.1f%%)", Np, Np/N*100, response*100, ci_l*100, ci_u*100)), 
                          rgm_case_p,
                        by = c("Group", "Arm", "N", "Marker", "Visit", "subgroup")) %>% 
    pivot_wider(id_cols = c(Arm, Marker, Visit, subgroup, mag_cat),
                names_from = Group, 
                values_from = c(N, rslt, `GMT/GMC`)) %>% 
    full_join(rrdiff_case_p, by = c("Arm", "Marker", "Visit")) %>% 
    full_join(rgmt_case_p, by = c("Arm", "Marker", "Visit", "mag_cat"))
  
  if(length(comp_NA <- setdiff(comp.p, rpcnt_case_p$Group))!=0){
    tab_case_p <- tab_case_p %>% 
      mutate(!!paste0("N_", comp_NA) := 0, 
             !!paste0("rslt_", comp_NA) := "-",
             !!paste0("GM_", comp_NA) :="-",
             `Ratios of GM`=replace_na(`Ratios of GM`, "-"))
  }else{
    tab_case_p <- tab_case_p %>% 
      mutate_at(vars(starts_with("N_")), replace_na, replace=0) %>% 
      mutate_at(vars(starts_with("rslt_")), replace_na, replace="-") %>% 
      mutate_at(vars(starts_with("GMT/GMC_")), replace_na, replace="-") %>% 
      mutate(`Ratios of GM`=replace_na(`Ratios of GMT/GMC`, "-"))
  }
  
  tab_case_all_p <- tab_case_p %>% 
    rowwise() %>% 
    mutate(Bsample=unlist(strsplit(trimws(Marker), " "))[1],
           Bsample=factor(Bsample, levels= c("Serum", "Nasal", "Saliva", "CD4+", "CD8+")),
           Visit=factor(Visit, levels=c("Day 01", "Day 31", "D31 fold-rise over D01"))) %>% 
    arrange(Visit, Arm, Bsample, Marker) %>% 
    filter(!mag_cat%in%tab_case.v) %>%
    filter(Arm=="Investigational Vaccine") %>% 
    select(Visit, Marker, `N_Vaccination-Proximal Cases`, `rslt_Vaccination-Proximal Cases`, 
           `GMT/GMC_Vaccination-Proximal Cases`, `N_Non-Cases`, `rslt_Non-Cases`, `GMT/GMC_Non-Cases`,  
           rrdiff, `Ratios of GM`) 
  
  

tab_case_p <- tab_case_p %>% 
  mutate(Marker=factor(Marker, levels=allmrks_lev), 
         Visit=factor(Visit, levels=c("Day 01", "Day 31", "D31 fold-rise over D01"))) %>% 
  filter(mag_cat%in%tab_case.v) %>% 
  arrange(Visit, Arm, Marker) %>% 
  filter(Arm=="Investigational Vaccine") %>% 
  select(Visit, Marker, `N_Vaccination-Proximal Cases`, `rslt_Vaccination-Proximal Cases`, 
         `GMT/GMC_Vaccination-Proximal Cases`, `N_Non-Cases`, `rslt_Non-Cases`, `GMT/GMC_Non-Cases`,  
         rrdiff, `Ratios of GM`)


tab_case_gmr_p <- tab_case_p %>% 
  filter(Visit=="D31 fold-rise over D01")

tab_case_p <- tab_case_p %>% 
  filter(Visit!="tab_case_p <- tab_case_p")

tab_gmr_all_p <- tab_case_all_p %>% 
  filter(Visit=="D31 fold-rise over D01")

tab_case_all_p <- tab_case_all_p %>% 
  filter(Visit!="D31 fold-rise over D01")


rpcnt_case_d <- get_rr(dat=ds_d, v=resp.v, subs=subs, sub.by=sub.by, strata=config.cor$WtStratum,
                       weights=config.cor$wt, subset=config.cor$ph2)

rgm_case_d  <- get_gm(ds_d, c(mag.v, Delta31), subs, sub.by, strata=config.cor$WtStratum, weights=config.cor$wt, subset=config.cor$ph2) 
rgmt_case_d <- get_rgmt(ds_d, mag.v, subs, comp_lev=comp.d, sub.by, strata=config.cor$WtStratum, weights=config.cor$wt, subset=config.cor$ph2) 




rrdiff_case_d <- rpcnt_case_d %>% 
  # dplyr::filter(subgroup %in% subs & grepl("Resp",resp_cat)) %>% 
  mutate(groupn = 2-match(Group, comp.d)%%2) %>%
  pivot_wider(id_cols = c(subgroup, Arm, Visit, Marker, Ind),
              names_from = groupn, values_from = c(response, ci_l, ci_u), names_sep = "") 

responseNA <- setdiff(as.vector(outer(c("response", "ci_l", "ci_u"), 1:2, paste0)), names(rrdiff_case_d))
rrdiff_case_d[, responseNA] <- NA

rrdiff_case_d <- rrdiff_case_d %>% 
  mutate(Estimate = response1-response2,
         ci_l = Estimate-sqrt((response1-ci_l1)^2+(response2-ci_u2)^2),
         ci_u = Estimate+sqrt((response1-ci_u1)^2+(response2-ci_l2)^2),
         rrdiff = ifelse(!is.na(Estimate), 
                         sprintf("%s\n(%s, %s)", round(Estimate, 3), round(ci_l, 3), round(ci_u, 3)),
                         "-")) 

print("Done with table6")

tab_case_d <- full_join(rpcnt_case_d, rgm_case_d,
                        by = c("Group", "Arm", "N", "Marker", "Visit", "subgroup")) %>% 
  pivot_wider(id_cols = c(Marker, Arm, Visit, subgroup, mag_cat),
              names_from = Group, 
              values_from = c(N, rslt, `GMT/GMC`)) %>% 
  full_join(rrdiff_case_d, by = c("Arm", "Marker", "Visit")) %>% 
  full_join(rgmt_case_d, by = c("Arm", "Marker", "Visit", "mag_cat")) 

if(length(comp_NA <- setdiff(comp.p, rpcnt_case_d$Group))!=0){
  tab_case_d <- tab_case_d %>% 
    mutate(!!paste0("N_", comp_NA) := 0, 
           !!paste0("rslt_", comp_NA) := "-",
           !!paste0("GM_", comp_NA) :="-",
           `Ratios of GM`=replace_na(`Ratios of GMT/GMC`, "-"))
}else{
  tab_case_d <- tab_case_d %>% 
    mutate_at(vars(starts_with("N_")), replace_na, replace=0) %>% 
    mutate_at(vars(starts_with("rslt_")), replace_na, replace="-") %>% 
    mutate_at(vars(starts_with("GM_")), replace_na, replace="-") %>% 
    mutate(`Ratios of GM`=replace_na(`Ratios of GMT/GMC`, "-"))
}

tab_case_all_d <- tab_case_d %>% 
  rowwise() %>% 
  mutate(Bsample=unlist(strsplit(trimws(Marker), " "))[1],
         Bsample=factor(Bsample, levels= c("Serum", "Nasal", "Saliva", "CD4+", "CD8+")),
         Visit=factor(Visit, levels=c("Day 01", "Day 31", "D31 fold-rise over D01"))) %>% 
  arrange(Visit, Arm, Bsample, Marker) %>% 
  filter(!mag_cat%in%tab_case.v) %>% 
  filter(Arm=="Investigational Vaccine") %>% 
  select(Visit, Marker, `N_Vaccination-Distal Cases`, `rslt_Vaccination-Distal Cases`, 
         `GMT/GMC_Vaccination-Distal Cases`, `N_Non-Cases`, `rslt_Non-Cases`, `GMT/GMC_Non-Cases`,  
         rrdiff, `Ratios of GM`) 


tab_case_d <- tab_case_d %>% 
  mutate(Marker=factor(Marker, levels=allmrks_lev),
         Visit=factor(Visit, levels=c("Day 01", "Day 31", "D31 fold-rise over D01"))) %>% 
  filter(mag_cat%in%tab_case.v) %>% 
  arrange(Visit, Arm, Marker) %>% 
  filter(Arm=="Investigational Vaccine") %>% 
  select(Visit, Marker, `N_Vaccination-Distal Cases`, `rslt_Vaccination-Distal Cases`, 
         `GMT/GMC_Vaccination-Distal Cases`, `N_Non-Cases`, `rslt_Non-Cases`, `GMT/GMC_Non-Cases`,  
         rrdiff, `Ratios of GM`) 


tab_case_gmr_d <- tab_case_d %>% 
  filter(Visit=="D31 fold-rise over D01")

tab_case_d <- tab_case_d %>% 
  filter(Visit!="tab_case_p <- tab_case_p")

tab_gmr_all_d <- tab_case_all_d %>% 
  filter(Visit=="D31 fold-rise over D01")

tab_case_all_d <- tab_case_all_d %>% 
  filter(Visit!="D31 fold-rise over D01")


print("Done with all tables")

# path for tables
save.results.to <- here::here("output")
if (!dir.exists(save.results.to))  dir.create(save.results.to)
save.results.to <- paste0(here::here("output"), "/", attr(config,"config"))

if (!dir.exists(save.results.to))  dir.create(save.results.to)
print(paste0("save.results.to equals ", save.results.to))

save(list = c("tlf", names(tlf)), file = file.path(save.results.to, sprintf("Tables%s.Rdata", ifelse(exists("COR"), gsub("D15", "D22", COR), ""))))
