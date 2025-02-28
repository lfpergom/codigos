rm(list=ls()) #cleaning the environment


# This script generate the tables of the descriptive statistics, the frequency nd proportion tables with the LGA,
# gender, age group and IDP status. 


setwd('C:/Users/luisf/Downloads/')  #calling the master directory

# Charging necessary packages
if (!require(pacman)) install.packages("pacman") 
library(pacman)
install.packages("openxlsx")
library(openxlsx)
install.packages("ggplot2")
library(ggplot2)
install.packages("labelled")
library(labelled)
install.packages("reshape2")  
library(reshape2)
library(dplyr)
library(tidyr)

p_load("stringr", "data.table" ,"tidyverse","dplyr", "srvyr")


# loading Nigeria data
load("D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/data/nigeria_data.RData")
ls()
print(nigeria_data)
# setting directory for descriptives
setwd("D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/descriptives")



# generating the idp dummy identifier

nigeria_data <- nigeria_data %>%
  mutate(idp = if_else(!is.na(reason_leave), 1, 0),
         idp = factor(idp, levels = c(0, 1), labels = c("Non-IDP", "IDP")))

print(nigeria_data$idp)

# proportions of idp
idp_prop <- table(nigeria_data$idp)
idp_prop <- prop.table(idp_prop)
names(idp_prop) <- c("non-idp", "idp")
print(idp_prop)


# generating idp host dummy variable

print(nigeria_data$idp_hosting)
nigeria_data$idp_hosting_d <- ifelse(nigeria_data$idp_hosting != "NA", 1, 0)
print(nigeria_data$idp_hosting_d)
nigeria_data$idp_hosting_d[is.na(nigeria_data$idp_hosting_d)] <- 0
print(nigeria_data$idp_hosting_d)


idp_hosting_dfreq <- nigeria_data %>%
  group_by(idp_hosting_d) %>%
  summarise(Frequency = n())

print(idp_hosting_dfreq)

idphost_prop <- table(nigeria_data$idp_hosting_d)
idphost_prop <- prop.table(idphost_prop)
print(idphost_prop)


nigeria_data <- nigeria_data %>%
  mutate(idp_st = case_when(
    idp == 1 & idp_hosting_d == 0 ~ "IDP",
    idp == 0 & idp_hosting_d == 1 ~ "Host",
    idp == 0 & idp_hosting_d == 0 ~ "Non IDP",
    idp == 1 & idp_hosting_d == 1 ~ "IDP host",
    TRUE ~ "Other"  
  ))

nigeria_data$idp_st <- factor(nigeria_data$idp_st)

print(nigeria_data$idp_st)
freq_table_idp_st <- table(nigeria_data$idp_st)
print(freq_table_idp_st)

# number of displacement for IDPs


ftm_table <- nigeria_data %>%
  group_by(ftm_howmany) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = round(Frequency / sum(Frequency), 3))

print(ftm_table)

# recode of schooling

schooling_table <- nigeria_data %>%
  group_by(schooling) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = round(Frequency / sum(Frequency), 3))

print(schooling_table)

nigeria_data <- nigeria_data %>%
  mutate(schooling_gr = case_when(
    schooling == "None" ~ "None",
    schooling %in% c("Primary finished", "Some primary") ~ "Primary",
    schooling %in% c("Secondary", "Some Secondary") ~ "Secondary",
    schooling == "Tertiary" ~ "Tertiary",
    TRUE ~ NA_character_  # Keep NA for missing values
  ))

# Convert to factor for better ordering
nigeria_data$schooling_gr <- factor(nigeria_data$schooling_gr, 
                                         levels = c("None", "Primary", "Secondary", "Tertiary"))

table(nigeria_data$schooling_gr, useNA = "ifany")

# disability variables

print(nigeria_data$wgq_seeing_0)
print(nigeria_data$wgq_seeing_1)

# hh head with a disability

nigeria_data <- nigeria_data %>%
  mutate(disability_d_pers = if_else(
    wgq_seeing_0 == "No difficulty" & 
      wgq_hearing_0 == "No difficulty" & 
      wgq_walking_0 == "No difficulty" & 
      wgq_remembering_0 == "No difficulty" & 
      wgq_selfcare_0 == "No difficulty" & 
      wgq_language_0 == "No difficulty", 
    0, 1))

# label the variable
nigeria_data$disability_d_pers = factor(nigeria_data$disability_d_pers, 
                           levels = c(0, 1), 
                           labels = c("No Disability", "With Disability"))


# Disability in teh HH - somebody in the HH with a disability
nigeria_data <- nigeria_data %>%
  mutate(disability_d_hh = if_else(
    wgq_seeing_1 == "No difficulty" & 
      wgq_hearing_1 == "No difficulty" & 
      wgq_walking_1 == "No difficulty" & 
      wgq_remembering_1 == "No difficulty" & 
      wgq_selfcare_1 == "No difficulty" & 
      wgq_language_1 == "No difficulty", 
    0, 1))

# label the variable
nigeria_data$disability_d_hh = factor(nigeria_data$disability_d_hh, 
                           levels = c(0, 1), 
                           labels = c("No Disability", "With Disability"))



freq_table_disability <- table(nigeria_data$disability_d_hh)
print(freq_table_disability)


# create and label the dummy variable for idp_hosting
nigeria_data <- nigeria_data %>%
  mutate(idp_hosting_d = if_else(idp_hosting == "None", 0, 1),
         idp_hosting_d = factor(idp_hosting_d, 
                                    levels = c(0, 1), 
                                    labels = c("No Host", "Host")))


freq_table_idp_hosting <- table(nigeria_data$idp_hosting_d)
print(freq_table_idp_hosting)

# basic descriptives

freq_table_1 <- table(nigeria_data$gender)
nigeria_data$gender_d <- ifelse(nigeria_data$gender == "Female", 1, 0)
freq_table_1p <- table(nigeria_data$gender_d)
freq_table_1p <- prop.table(freq_table_1p)
names(freq_table_1p) <- c("Male", "Female")
print(freq_table_1p)


variables_dem <- c("idp", "idp_st", "country", "LGA", "urban", "sampling", "screening_1", "age", "age_gr", "gender", "language", "born", "state_or", "LGA_or", "ward_or", "ward_or_1", "live_now", "LGA_now", "ward_now", "ward_now_1", "year", "month", "reason_leave", "urban_previous", "factor_to_move", "ftm_disaster", "ftm_conflict", "ftm_livelihoods", "ftm_accessland", "ftm_accessservices", "ftm_foodinsecurity", "ftm_caterscarcity", "ftm_environmental_fc", "ftm_no", "ftm_multipletimes", "ftm_howmany", "placebf_affecteddisasaster", "placebf_affecteddinsecurity", "placebf_notaffordable", "placebf_evicted", "placebf_notlivelihoods", "placebf_other", "placebf_pna", "placebf_reasontomove", "idp_awareness", "idp_hosting", "idphosting_sinceyr", "idphosting_sincemonth", "idphosting_howmeet", "schooling", "numdep_05", "numdep_614", "numdep_1517", "numdep1864", "numdep_65") 
washingtonq <- c("wgq_seeing_0", "wgq_seeing_1", "wgq_hearing_0", "wgq_hearing_1", "wgq_walking_0", "wgq_walking_1", "wgq_remembering_0", 	"wgq_remembering_1", 	"wgq_selfcare_0", 	"wgq_selfcare_1", 	"wgq_language_0", 	"wgq_language_1")
housing <- c("housing_D1", 	"housing_D1_2", 	"housing_D1_3", 	"housing_D2_1", 	"housing_D2_2", 	"housing_D3", 	"housing_D3_2", 	"housing_D3_3", 	"housing_D4_1", 	"housing_D4_2", 	"housing_D5", 	"housing_D6a_1", 	"housing_D6a_2", 	"housing_D6a_3", 	"housing_D6a_4", 	"housing_D6a_5", 	"housing_D6a_6", 	"housing_D6a_7", 	"housing_D6a_8", 	"housing_D6a_9", 	"housing_D6a_10", 	"housing_D6a_11", 	"housing_D6a_12", 	"housing_D6a_98", 	"housing_D6a_99", 	"housing_D6b_1", 	"housing_D6b_2", 	"housing_D6b_3", 	"housing_D6b_4", 	"housing_D6b_5", 	"housing_D6b_6", 	"housing_D6b_7", 	"housing_D6b_8", 	"housing_D6b_9", 	"housing_D6b_10", 	"housing_D6b_11", 	"housing_D6b_12", 	"housing_D6b_98", 	"housing_D6b_99", 	"housing_D13", 	"housing_D13_2", 	"housing_D13_3", 	"housing_D13a", 	"housing_D13b", 	"housing_D14", 	"housing_D15a_1", 	"housing_D15a_2", 	"housing_D15a_3", 	"housing_D15a_4", 	"housing_D15a_5", 	"housing_D15a_6", 	"housing_D15a_7", 	"housing_D15a_8", 	"housing_D15a_9", 	"housing_D15a_10", 	"housing_D15a_11", 	"housing_D15a_12", 	"housing_D15a_98", 	"housing_D15a_99", 	"housing_D15b_1", 	"housing_D15b_2", 	"housing_D15b_3", 	"housing_D15b_4", 	"housing_D15b_5", 	"housing_D15b_6", 	"housing_D15b_7", 	"housing_D15b_8", 	"housing_D15b_9", 	"housing_D15b_10", 	"housing_D15b_11", 	"housing_D15b_12", 	"housing_D15b_98", 	"housing_D15b_99", 	"housing_D16", 	"housing_D17_1_1", 	"housing_D17_1_2", 	"housing_D17_1_3", 	"housing_D17_1_4", 	"housing_D17_1_5", 	"housing_D17_1_6", 	"housing_D17_1_99", 	"housing_D18_1_1", 	"housing_D18_1_2", 	"housing_D18_1_3")
security <- c("security_E1", 	"security_E2a_1", 	"security_E2a_2", 	"security_E2a_3", 	"security_E2a_4", 	"security_E2a_5", 	"security_E2a_6", 	"security_E2a_7", 	"security_E2a_8", 	"security_E2a_99", 	"security_E2b_1", 	"security_E2b_2", 	"security_E2b_3", 	"security_E2b_4", 	"security_E2b_5", 	"security_E2b_6", 	"security_E2b_7", 	"security_E2b_8", 	"security_E2b_99", 	"security_E3", 	"security_E4_1", 	"security_E4_2", 	"security_E4_3", 	"security_E4_4", 	"security_E4_5", 	"security_E4_6", 	"security_E4_7", 	"security_E4_8", 	"security_E4_9", 	"security_E4_10", 	"security_E4_11", 	"security_E4_99", 	"security_E7", 	"security_E2a_0", 	"security_E2a.1", 	"security_E2a_1_SPECIFY", 	"security_E15", 	"security_E16a_1", 	"security_E16a_2", 	"security_E16a_3", 	"security_E16a_4", 	"security_E16a_5", 	"security_E16a_6", 	"security_E16a_7", 	"security_E16a_8", 	"security_E16a_99", 	"security_E16b_1", 	"security_E16b_2", 	"security_E16b_3", 	"security_E16b_4", 	"security_E16b_5", 	"security_E16b_6", 	"security_E16b_7", 	"security_E16b_8", 	"security_E16b_99", 	"security_E3_H", 	"security_E4_H_1", 	"security_E4_H_2", 	"security_E4_H_3", 	"security_E4_H_4", 	"security_E4_H_5", 	"security_E4_H_6", 	"security_E4_H_7", 	"security_E4_H_8", 	"security_E4_H_9", 	"security_E4_H_10", 	"security_E4_H_11", 	"security_E4_H_99", 	"security_E7_H", 	"security_E16a_0", 	"security_E16a.1", 	"security_E16a_1_SPECIFY")
education_idp_pI <- c("education_F1a", 	"education_F1b", 	"education_idp_MRK_IDP_CHILDREN", 	"education_idp_F1c_1", 	"education_idp_F1c_2", 	"education_idp_F1c_3", 	"education_idp_F1c_4", 	"education_idp_F1c_5", 	"education_idp_F3_1", 	"education_idp_F3_2", 	"education_idp_F4_1", 	"education_idp_F4_2", 	"education_idp_F5_1", 	"education_idp_F5_2", 	"education_idp_F6_1_1", 	"education_idp_F6_1_2", 	"education_idp_F6_1_3", 	"education_idp_F6_1_4", 	"education_idp_F6_1_5", 	"education_idp_F6_1_6", 	"education_idp_F6_1_7", 	"education_idp_F6_1_8", 	"education_idp_F6_1_9", 	"education_idp_F6_1_10", 	"education_idp_F6_1_11", 	"education_idp_F6_1_12", 	"education_idp_F6_1_13", 	"education_idp_F6_1_14", 	"education_idp_F6_1_98", 	"education_idp_F6_1_99", 	"education_idp_F6_2_1", 	"education_idp_F6_2_2", 	"education_idp_F6_2_3", 	"education_idp_F6_2_4", 	"education_idp_F6_2_5", 	"education_idp_F6_2_6", 	"education_idp_F6_2_7", 	"education_idp_F6_2_8", 	"education_idp_F6_2_9", 	"education_idp_F6_2_10", 	"education_idp_F6_2_11", 	"education_idp_F6_2_12", 	"education_idp_F6_2_13", 	"education_idp_F6_2_14", 	"education_idp_F6_2_98", 	"education_idp_F6_2_99", 	"education_idp_F7_1", 	"education_idp_F7_2", 	"education_idp_F8_1_1", 	"education_idp_F8_1_2", 	"education_idp_F8_2_1", 	"education_idp_F8_2_2", 	"education_idp_F8_3_1", 	"education_idp_F8_3_2", 	"education_idp_F8a_1_1", 	"education_idp_F8a_1_2", 	"education_idp_F8a_1_3", 	"education_idp_F8a_1_4", 	"education_idp_F8a_1_5", 	"education_idp_F8a_1_6", 	"education_idp_F8a_1_98", 	"education_idp_F8a_1_99", 	"education_idp_F8a_2_1", 	"education_idp_F8a_2_2", 	"education_idp_F8a_2_3", 	"education_idp_F8a_2_4", 	"education_idp_F8a_2_5", 	"education_idp_F8a_2_6", 	"education_idp_F8a_2_98", 	"education_idp_F8a_2_99", 	"education_idp_F8a_1_98Others", 	"education_idp_F8a_2_98Others", 	"education_idp_F81_1", 	"education_idp_F81_2", 	"education_idp_F81a_1", 	"education_idp_F81a_2", 	"education_idp_F9_1", 	"education_idp_F9_2", 	"education_idp_F10_1_1", 	"education_idp_F10_1_2", 	"education_idp_F10_1_3", 	"education_idp_F10_1_4", 	"education_idp_F10_1_5", 	"education_idp_F10_1_6", 	"education_idp_F10_1_7", 	"education_idp_F10_1_8", 	"education_idp_F10_1_9", 	"education_idp_F10_1_10", 	"education_idp_F10_1_11", 	"education_idp_F10_1_12", 	"education_idp_F10_1_13", 	"education_idp_F10_1_98", 	"education_idp_F10_1_99")
education_idp_pII <- c("education_idp_F10_2_1", 	"education_idp_F10_2_2", 	"education_idp_F10_2_3", 	"education_idp_F10_2_4", 	"education_idp_F10_2_5", 	"education_idp_F10_2_6", 	"education_idp_F10_2_7", 	"education_idp_F10_2_8", 	"education_idp_F10_2_9", 	"education_idp_F10_2_10", 	"education_idp_F10_2_11", 	"education_idp_F10_2_12", 	"education_idp_F10_2_13", 	"education_idp_F10_2_98", 	"education_idp_F10_2_99", 	"education_idp_F11_1", 	"education_idp_F11_2", 	"education_idp_F12_1", 	"education_idp_F12_2", 	"education_idp_F12_1_1", 	"education_idp_F12_1_2", 	"education_idp_F12_2_1", 	"education_idp_F12_2_2", 	"education_idp_F12_3_1", 	"education_idp_F12_3_2", 	"education_idp_F12a_1_1", 	"education_idp_F12a_1_2", 	"education_idp_F12a_1_3", 	"education_idp_F12a_1_4", 	"education_idp_F12a_1_5", 	"education_idp_F12a_1_6", 	"education_idp_F12a_1_98", 	"education_idp_F12a_1_99", 	"education_idp_F12a_2_1", 	"education_idp_F12a_2_2", 	"education_idp_F12a_2_3", 	"education_idp_F12a_2_4", 	"education_idp_F12a_2_5", 	"education_idp_F12a_2_6", 	"education_idp_F12a_2_98", 	"education_idp_F12a_2_99", 	"education_idp_F12a_1_98Others", 	"education_idp_F12a_2_98Others", 	"education_idp_F12b_1_1", 	"education_idp_F12b_1_2", 	"education_idp_F12b_1_3", 	"education_idp_F12b_1_4", 	"education_idp_F12b_1_98", 	"education_idp_F12b_1_99", 	"education_idp_F12b_2_1", 	"education_idp_F12b_2_2", 	"education_idp_F12b_2_3", 	"education_idp_F12b_2_4", 	"education_idp_F12b_2_98", 	"education_idp_F12b_2_99", 	"education_idp_F13_1", 	"education_idp_F13_2", 	"education_idp_F14a_1_1", 	"education_idp_F14a_1_2", 	"education_idp_F14a_1_3", 	"education_idp_F14a_1_4", 	"education_idp_F14a_1_5", 	"education_idp_F14a_1_6", 	"education_idp_F14a_1_7", 	"education_idp_F14a_1_8", 	"education_idp_F14a_1_9", 	"education_idp_F14a_1_10", 	"education_idp_F14a_1_99", 	"education_idp_F14a_2_1", 	"education_idp_F14a_2_2", 	"education_idp_F14a_2_3", 	"education_idp_F14a_2_4", 	"education_idp_F14a_2_5", 	"education_idp_F14a_2_6", 	"education_idp_F14a_2_7", 	"education_idp_F14a_2_8", 	"education_idp_F14a_2_9", 	"education_idp_F14a_2_10", 	"education_idp_F14a_2_99", 	"education_idp_F14b_1_1", 	"education_idp_F14b_1_2", 	"education_idp_F14b_1_3", 	"education_idp_F14b_1_4", 	"education_idp_F14b_1_5", 	"education_idp_F14b_1_6", 	"education_idp_F14b_1_7", 	"education_idp_F14b_1_8", 	"education_idp_F14b_1_9", 	"education_idp_F14b_1_10", 	"education_idp_F14b_1_99", 	"education_idp_F14b_2_1", 	"education_idp_F14b_2_2", 	"education_idp_F14b_2_3", 	"education_idp_F14b_2_4", 	"education_idp_F14b_2_5", 	"education_idp_F14b_2_6", 	"education_idp_F14b_2_7", 	"education_idp_F14b_2_8", 	"education_idp_F14b_2_9", 	"education_idp_F14b_2_10", 	"education_idp_F14b_2_99", 	"education_idp_F25a", 	"education_idp_F25b")
education_host_pI <- c("education_host_MRK_HOST_CHILDREN", 	"education_host_F25c_1", 	"education_host_F25c_2", 	"education_host_F25c_3", 	"education_host_F25c_4", 	"education_host_F25c_5", 	"education_host_F26_1", 	"education_host_F26_2", 	"education_host_F15_1", 	"education_host_F15_2", 	"education_host_F15_2_1", 	"education_host_F15_2_2", 	"education_host_F16_1_1", 	"education_host_F16_1_2", 	"education_host_F16_1_3", 	"education_host_F16_1_4", 	"education_host_F16_1_5", 	"education_host_F16_1_6", 	"education_host_F16_1_7", 	"education_host_F16_1_8", 	"education_host_F16_1_9", 	"education_host_F16_1_10", 	"education_host_F16_1_11", 	"education_host_F16_1_12", 	"education_host_F16_1_13", 	"education_host_F16_1_14", 	"education_host_F16_1_98", 	"education_host_F16_1_99", 	"education_host_F16_2_1", 	"education_host_F16_2_2", 	"education_host_F16_2_3", 	"education_host_F16_2_4", 	"education_host_F16_2_5", 	"education_host_F16_2_6", 	"education_host_F16_2_7", 	"education_host_F16_2_8", 	"education_host_F16_2_9", 	"education_host_F16_2_10", 	"education_host_F16_2_11", 	"education_host_F16_2_12", 	"education_host_F16_2_13", 	"education_host_F16_2_14", 	"education_host_F16_2_98", 	"education_host_F16_2_99", 	"education_host_F17_1", 	"education_host_F17_2", 	"education_host_F18_1_1", 	"education_host_F18_1_2", 	"education_host_F18_2_1", 	"education_host_F18_2_2", 	"education_host_F18_3_1", 	"education_host_F18_3_2", 	"education_host_F18a_1_1", 	"education_host_F18a_1_2", 	"education_host_F18a_1_3", 	"education_host_F18a_1_4", 	"education_host_F18a_1_5", 	"education_host_F18a_1_6", 	"education_host_F18a_1_98", 	"education_host_F18a_1_99", 	"education_host_F18a_2_1", 	"education_host_F18a_2_2", 	"education_host_F18a_2_3", 	"education_host_F18a_2_4", 	"education_host_F18a_2_5", 	"education_host_F18a_2_6", 	"education_host_F18a_2_98", 	"education_host_F18a_2_99", 	"education_host_F18a_1_98Other", 	"education_host_F18a_2_98Other", 	"education_host_F181_1", 	"education_host_F181_2", 	"education_host_F181a_1", 	"education_host_F181a_2", 	"education_host_F19_1", 	"education_host_F19_2", 	"education_host_F20_1_1", 	"education_host_F20_1_2", 	"education_host_F20_1_3", 	"education_host_F20_1_4", 	"education_host_F20_1_5", 	"education_host_F20_1_6", 	"education_host_F20_1_7", 	"education_host_F20_1_8", 	"education_host_F20_1_9", 	"education_host_F20_1_10", 	"education_host_F20_1_11", 	"education_host_F20_1_12", 	"education_host_F20_1_13", 	"education_host_F20_1_98", 	"education_host_F20_1_99"	)
education_host_pII <- c("education_host_F20_2_1", 	"education_host_F20_2_2", 	"education_host_F20_2_3", 	"education_host_F20_2_4", 	"education_host_F20_2_5", 	"education_host_F20_2_6", 	"education_host_F20_2_7", 	"education_host_F20_2_8", 	"education_host_F20_2_9", 	"education_host_F20_2_10", 	"education_host_F20_2_11", 	"education_host_F20_2_12", 	"education_host_F20_2_13", 	"education_host_F20_2_98", 	"education_host_F20_2_99", 	"education_host_F27_1", 	"education_host_F27_2", 	"education_host_F21_1_1", 	"education_host_F21_1_2", 	"education_host_F21_2_1", 	"education_host_F21_2_2", 	"education_host_F21_3_1", 	"education_host_F21_3_2", 	"education_host_F21a_1_1", 	"education_host_F21a_1_2", 	"education_host_F21a_1_3", 	"education_host_F21a_1_4", 	"education_host_F21a_1_5", 	"education_host_F21a_1_6", 	"education_host_F21a_1_98", 	"education_host_F21a_1_99", 	"education_host_F21a_2_1", 	"education_host_F21a_2_2", 	"education_host_F21a_2_3", 	"education_host_F21a_2_4", 	"education_host_F21a_2_5", 	"education_host_F21a_2_6", 	"education_host_F21a_2_98", 	"education_host_F21a_2_99", 	"education_host_F21a_1_98Others", 	"education_host_F21a_2_98Others", 	"education_host_F21b_1_1", 	"education_host_F21b_1_2", 	"education_host_F21b_1_3", 	"education_host_F21b_1_4", 	"education_host_F21b_1_98", 	"education_host_F21b_1_99", 	"education_host_F21b_2_1", 	"education_host_F21b_2_2", 	"education_host_F21b_2_3", 	"education_host_F21b_2_4", 	"education_host_F21b_2_98", 	"education_host_F21b_2_99", 	"education_host_F22_1", 	"education_host_F22_2", 	"education_host_F23a_1_1", 	"education_host_F23a_1_2", 	"education_host_F23a_1_3", 	"education_host_F23a_1_4", 	"education_host_F23a_1_5", 	"education_host_F23a_1_6", 	"education_host_F23a_1_7", 	"education_host_F23a_1_8", 	"education_host_F23a_1_9", 	"education_host_F23a_1_10", 	"education_host_F23a_1_99", 	"education_host_F23a_2_1", 	"education_host_F23a_2_2", 	"education_host_F23a_2_3", 	"education_host_F23a_2_4", 	"education_host_F23a_2_5", 	"education_host_F23a_2_6", 	"education_host_F23a_2_7", 	"education_host_F23a_2_8", 	"education_host_F23a_2_9", 	"education_host_F23a_2_10", 	"education_host_F23a_2_99", 	"education_host_F23b_1_1", 	"education_host_F23b_1_2", 	"education_host_F23b_1_3", 	"education_host_F23b_1_4", 	"education_host_F23b_1_5", 	"education_host_F23b_1_6", 	"education_host_F23b_1_7", 	"education_host_F23b_1_8", 	"education_host_F23b_1_9", 	"education_host_F23b_1_10", 	"education_host_F23b_1_99", 	"education_host_F23b_2_1", 	"education_host_F23b_2_2", 	"education_host_F23b_2_3", 	"education_host_F23b_2_4", 	"education_host_F23b_2_5", 	"education_host_F23b_2_6", 	"education_host_F23b_2_7", 	"education_host_F23b_2_8", 	"education_host_F23b_2_9", 	"education_host_F23b_2_10", 	"education_host_F23b_2_99")
livelihoods <- c("livelihhods_G01", 	"livelihhods_G02", 	"livelihhods_G01_1", 	"livelihhods_G01_bis", 	"livelihhods_G02_1", 	"livelihhods_G3_1", 	"livelihhods_G3_2", 	"livelihhods_G4", 	"livelihhods_G4_1", 	"livelihhods_G40", 	"livelihhods_G7_1", 	"livelihhods_G7_2", 	"livelihhods_G7_3", 	"livelihhods_G7_4", 	"livelihhods_G7_98", 	"livelihhods_G7_99", 	"livelihhods_G1", 	"livelihhods_G2", 	"livelihhods_G1_1", 	"livelihhods_G1_bis", 	"livelihhods_G2_1", 	"livelihhods_G11", 	"livelihhods_G13_1", 	"livelihhods_G13_2", 	"livelihhods_G13_3", 	"livelihhods_G13_4", 	"livelihhods_G13_98", 	"livelihhods_G13_99", 	"livelihhods_G15", 	"livelihhods_G17", 	"livelihhods_G18", 	"livelihhods_G38", 	"livelihhods_G39", 	"livelihhods_G40_1", 	"livelihhods_G42", 	"livelihhods_G43", 	"livelihhods_G45", 	"livelihhods_G46_1", 	"livelihhods_G46_2", 	"livelihhods_G46_3", 	"livelihhods_G46_4", 	"livelihhods_G46_5", 	"livelihhods_G46_6", 	"livelihhods_G46_99", 	"livelihhods_AG10_1", 	"livelihhods_AG10_2", 	"livelihhods_AG10_3", 	"livelihhods_AG11", 	"livelihhods_AG12_1", 	"livelihhods_AG12_2", 	"livelihhods_AG12_3", 	"livelihhods_AG12_4", 	"livelihhods_AG12_5", 	"livelihhods_AG12_6", 	"livelihhods_AG12_7", 	"livelihhods_AG12_99", 	"livelihhods_AG13", 	"livelihhods_AG14_1", 	"livelihhods_AG14_2", 	"livelihhods_AG14_3", 	"livelihhods_AG15_1", 	"livelihhods_AG15_2", 	"livelihhods_AG15_3", 	"livelihhods_AG15_4", 	"livelihhods_AG15_5", 	"livelihhods_AG15_6", 	"livelihhods_AG15_7", 	"livelihhods_AG15_8", 	"livelihhods_AG15_9", 	"livelihhods_AG15_10", 	"livelihhods_AG15_99", 	"livelihhods_AG16_1", 	"livelihhods_AG16_2", 	"livelihhods_AG16_3", 	"livelihhods_G19", 	"livelihhods_G20", 	"livelihhods_G19_1", 	"livelihhods_G19_bis", 	"livelihhods_G20_4", 	"livelihhods_G20_1_1", 	"livelihhods_G20_1_2", 	"livelihhods_G20_2", 	"livelihhods_G20_3", 	"livelihhods_G21", 	"livelihhods_G23_1", 	"livelihhods_G23_2", 	"livelihhods_G23_3", 	"livelihhods_G23_4", 	"livelihhods_G23_98", 	"livelihhods_G23_99", 	"livelihhods_G25", 	"livelihhods_G26", 	"livelihhods_G25_1", 	"livelihhods_G25_bis", 	"livelihhods_G26_1", 	"livelihhods_G27", 	"livelihhods_G29_1", 	"livelihhods_G29_2", 	"livelihhods_G29_3", 	"livelihhods_G29_4", 	"livelihhods_G29_98", 	"livelihhods_G29_99", 	"livelihhods_G31", 	"livelihhods_G33", 	"livelihhods_G34", 	"livelihhods_G38_H", 	"livelihhods_G39_H", 	"livelihhods_G40_H", 	"livelihhods_G42_H", 	"livelihhods_G43_H", 	"livelihhods_G45_H", 	"livelihhods_G46_H_1", 	"livelihhods_G46_H_2", 	"livelihhods_G46_H_3", 	"livelihhods_G46_H_4", 	"livelihhods_G46_H_5", 	"livelihhods_G46_H_6", 	"livelihhods_G46_H_99", 	"livelihhods_AG10_H_1", 	"livelihhods_AG10_H_2", 	"livelihhods_AG10_H_3", 	"livelihhods_AG11_H", 	"livelihhods_AG12_H_1", 	"livelihhods_AG12_H_2", 	"livelihhods_AG12_H_3", 	"livelihhods_AG12_H_4", 	"livelihhods_AG12_H_5", 	"livelihhods_AG12_H_6", 	"livelihhods_AG12_H_7", 	"livelihhods_AG12_H_99", 	"livelihhods_AG13_H", 	"livelihhods_AG14_H_1", 	"livelihhods_AG14_H_2", 	"livelihhods_AG14_H_3", 	"livelihhods_AG15_H_1", 	"livelihhods_AG15_H_2", 	"livelihhods_AG15_H_3", 	"livelihhods_AG15_H_4", 	"livelihhods_AG15_H_5", 	"livelihhods_AG15_H_6", 	"livelihhods_AG15_H_7", 	"livelihhods_AG15_H_8", 	"livelihhods_AG15_H_9", 	"livelihhods_AG15_H_10", 	"livelihhods_AG15_H_99", 	"livelihhods_AG16_H_1", 	"livelihhods_AG16_H_2", 	"livelihhods_AG16_H_3")
health <- c("health_H1", 	"health_H2a_1", 	"health_H2a_2", 	"health_H2a_3", 	"health_H2a_4", 	"health_H2a_5", 	"health_H2a_6", 	"health_H2a_99", 	"health_H2b_1", 	"health_H2b_2", 	"health_H2b_3", 	"health_H2b_4", 	"health_H2b_5", 	"health_H2b_6", 	"health_H2b_99", 	"health_H222", 	"health_H223", 	"health_H224", 	"health_H225", 	"health_H226", 	"health_H227", 	"health_H228", 	"health_H3", 	"health_H4a_1", 	"health_H4a_2", 	"health_H4a_3", 	"health_H4a_4", 	"health_H4a_5", 	"health_H4a_6", 	"health_H4a_7", 	"health_H4a_8", 	"health_H4a_9", 	"health_H4a_99", 	"health_H4ba_1", 	"health_H4ba_2", 	"health_H4ba_3", 	"health_H4ba_4", 	"health_H4ba_5", 	"health_H4ba_6", 	"health_H4ba_7", 	"health_H4ba_8", 	"health_H4ba_99", 	"health_H5", 	"health_H8", 	"health_H6", 	"health_H9", 	"health_H7", 	"health_H10", 	"health_H11_1", 	"health_H12a_1", 	"health_H12a_2", 	"health_H12a_3", 	"health_H12a_4", 	"health_H12a_5", 	"health_H12a_6", 	"health_H12a_99", 	"health_H12b_1", 	"health_H12b_2", 	"health_H12b_3", 	"health_H12b_4", 	"health_H12b_5", 	"health_H12b_6", 	"health_H12b_99", 	"health_H222_H", 	"health_H223_H", 	"health_H224_H", 	"health_H225_H", 	"health_H226_H", 	"health_H227_H", 	"health_H228_H", 	"health_H13", 	"health_H14a_1", 	"health_H14a_2", 	"health_H14a_3", 	"health_H14a_4", 	"health_H14a_5", 	"health_H14a_6", 	"health_H14a_7", 	"health_H14a_8", 	"health_H14a_9", 	"health_H14a_99", 	"health_H14b_1", 	"health_H14b_2", 	"health_H14b_3", 	"health_H14b_4", 	"health_H14b_5", 	"health_H14b_6", 	"health_H14b_7", 	"health_H14b_8", 	"health_H14b_9", 	"health_H17", 	"health_H18", 	"health_H19", 	"health_H20", 	"health_H21", 	"health_H22")
closing <- c("closing_I0_1", 	"closing_I0_2_1", 	"closing_I0_2_2", 	"closing_I0_2_3", 	"closing_I0_2_4", 	"closing_I0_2_98", 	"closing_I0_2_99", 	"closing_I0_5_1", 	"closing_I0_5_2", 	"closing_I0_5_3", 	"closing_I0_5_4", 	"closing_I0_5_5", 	"closing_I0_5_6", 	"closing_I0_6_1", 	"closing_I0_6_2", 	"closing_I0_6_3", 	"closing_I0_6_4", 	"closing_I0_6_5", 	"closing_I0_6_6", 	"closing_I0_6_99", 	"closing_I0_7_1", 	"closing_I0_7_2", 	"closing_I0_7_3", 	"closing_I0_7_4", 	"closing_I0_7_5", 	"closing_I0_7_99", 	"closing_I0_8", 	"closing_I1")


################################################################################
# variables analysis by sector

variable_groups <- list(
  Demographics = variables_dem,
  WashingtonGroup = washingtonq,
  Housing = housing,
  Security = security,
  Health = health,
  Closing = closing,
  Education_IDP_pI = education_idp_pI,
  Education_IDP_pII = education_idp_pII,
  Education_Host_pI = education_host_pI,
  Education_Host_pII = education_host_pII,
  Livelihoods = livelihoods
)

################################################################################
# here is where the loop to generate the descriptives start, it is necessary to 
# run every time from this point in order to have an excel workbook at the end. 

# create a new Excel workbook
wb <- createWorkbook()

# define the disaggregation categories
disaggregation_vars <- list(
  "Total" = NULL,  # Overall Total
  "IDP" = "idp",
  "Gender" = "gender",
  "Age" = "age_gr",
  "Num. of displacement" = "ftm_howmany",
  "Hosting Status" = "idp_hosting_d",
  "Education" = "schooling_gr",
  "Disability (Personal)" = "disability_d_pers",
  "Disability (Household)" = "disability_d_hh",
  "Language" = "language",
  "Location" = "LGA"
)

# loop through each variable group

for (group_name in names(variable_groups)) {
  # Extract the variables for the current group
  variables <- variable_groups[[group_name]]
  
  # add a worksheet for the current group
  addWorksheet(wb, group_name)
  
  # start row counter for this sheet
  start_row <- 1
  
  # process each variable in the group
  for (var in variables) {
    # Skip variables not in the dataset or entirely NA
    if (!var %in% colnames(nigeria_data) || all(is.na(nigeria_data[[var]]))) {
      print(paste("Skipping variable:", var, "- it is missing or empty"))
      next
    }
    
    # retrieve the label for the variable
    variable_label <- var_label(nigeria_data[[var]])
    if (is.null(variable_label)) {
      variable_label <- paste("Label for", var, "not found")  # Default label if missing
    }
    
    # combine the variable name with the label
    label_with_var_name <- paste(variable_label, "(Variable:", var, ")")
    
    # create a data frame to store aggregated numbers in wide format
    aggregate_results <- data.frame(Variable = unique(nigeria_data[[var]]))
    
    ### compute Aggregates for Each Disaggregation Category
    for (disagg_name in names(disaggregation_vars)) {
      disagg_var <- disaggregation_vars[[disagg_name]]
      
      # skip disaggregations that do not exist in nigeria_data 
      if (!is.null(disagg_var) && !disagg_var %in% colnames(nigeria_data)) {
        print(paste("Skipping disaggregation:", disagg_name, "- Column", disagg_var, "not found"))
        next
      }
      
      if (is.null(disagg_var)) {
        # compute total without further disaggregation
        agg_table <- nigeria_data %>%
          group_by(across(all_of(var))) %>%
          summarise(Count = n(), .groups = "drop")
      } else {
        # compute disaggregated counts
        agg_table <- nigeria_data %>%
          group_by(across(all_of(var)), across(all_of(disagg_var))) %>%
          summarise(Count = n(), .groups = "drop")
      
        # convert to data frame (if needed)
        agg_table <- as.data.frame(agg_table)
        
        # pivot to wide format
        agg_table <- reshape2::dcast(agg_table, as.formula(paste(var, "~", disagg_var)), 
                                     value.var = "Count", fill = 0)
      }
      
      # ensuring "Variable" column exists in `agg_table`
      if (!"Variable" %in% colnames(agg_table)) {
        colnames(agg_table)[1] <- "Variable"  # Rename first column to "Variable"
      }
      
      # ensuring "Variable" column exists in `aggregate_results`
      if (!"Variable" %in% colnames(aggregate_results)) {
        colnames(aggregate_results)[1] <- "Variable"  # Rename first column to "Variable"
      }
      
      # merge the new column into aggregate_results
      aggregate_results <- left_join(aggregate_results, agg_table, by = "Variable")
    }
    
    ### Compute Summary Statistics (Only if at least one is valid)
    # This is necessary in order to have a cleaner output. One caveat is that  
    # for variables where the answer is recorded as an option, R is not capable
    # of estimating the stats; i.e., it is necessary to do it manually. This happens
    # for some cost related variables in different sectors.
    
    if (is.numeric(nigeria_data[[var]]) && any(!is.na(nigeria_data[[var]]))) {  
      mean_value <- mean(nigeria_data[[var]], na.rm = TRUE)
      median_value <- median(nigeria_data[[var]], na.rm = TRUE)
      min_value <- min(nigeria_data[[var]], na.rm = TRUE)
      max_value <- max(nigeria_data[[var]], na.rm = TRUE)
      std_dev_value <- sd(nigeria_data[[var]], na.rm = TRUE)  # Compute standard deviation
      total_observations <- sum(!is.na(nigeria_data[[var]]))
      
      # create a summary stats table only if meaningful stats exist
      summary_stats_df <- data.frame(
        Statistic = c("Mean", "Median", "Min", "Max", "Standard Deviation", "Total Observations"),
        Value = c(mean_value, median_value, min_value, max_value, std_dev_value, total_observations)
      )
    } else {
      summary_stats_df <- NULL  # Mark as NULL to skip writing
    }
    
    # write the variable label with the variable name
    writeData(wb, sheet = group_name, x = data.frame(Label = label_with_var_name), 
              startCol = 1, startRow = start_row, colNames = FALSE)
    
    # adjust start_row for the disaggregation table
    start_row <- start_row + 2  
    
    # write the disaggregation table in wide format
    writeData(wb, sheet = group_name, x = aggregate_results, 
              startCol = 1, startRow = start_row, colNames = TRUE, rowNames = FALSE)
    
    # adjust start_row
    start_row <- start_row + nrow(aggregate_results) + 3
    
    # write summary statistics table only if available
    if (!is.null(summary_stats_df)) {
      writeData(wb, sheet = group_name, x = summary_stats_df, 
                startCol = 1, startRow = start_row, colNames = TRUE, rowNames = FALSE)
      start_row <- start_row + nrow(summary_stats_df) + 3  # Adjust for next variable
    }
  }
}


saveWorkbook(wb, "RAW_descrptives.xlsx", overwrite = TRUE)

print("All results have been saved to 'RAW_descrptives.xlsx'")