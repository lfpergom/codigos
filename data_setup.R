rm(list=ls()) #cleaning the environment

setwd('C:/Users/luisf/Downloads/')  #calling the master directory

# Charging necessary packages
if (!require(pacman)) install.packages("pacman") 
library(pacman)
install.packages("openxlsx")
library(openxlsx)
library(dplyr)
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("labelled")
library(labelled)

p_load("stringr", "data.table" ,"tidyverse","dplyr", "srvyr")

# data input

nigeria_data <- read.xlsx("D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/descargas/Project IDMC Socio Economic Study Data_Final 22_01_2025.xlsx", sheet = 1) 
seq_along(nigeria_data)
custom_glimpse <- function(nigdata) { #to know the variables in the dataframe
  data.frame(
    col_name = colnames(nigdata),
    col_index = 1:ncol(nigdata),
    col_class = sapply(nigdata, class),
    row.names = NULL
  )
}

# data revision

custom_glimpse(nigeria_data)

names(nigeria_data)

# renaming variables (there is a first set of variables with clearer names, as the analysis advance 
# some variables will have a clearer name as well. All of the variables with their definitive names 
# will be included in a codebook)


nigeria_data <- nigeria_data %>% rename(id = Respondent_Serial,
                                        country = S3,
                                        LGA = S4,
                                        urban = URBANITY,
                                        sampling = S4_SAMPLING,
                                        screening_1 = A100,
                                        age = A20.1,
                                        age_gr = A20.2,
                                        gender = A25,
                                        language = A51,
                                        born = A1,
                                        state_or = A2a,
                                        LGA_or = A2a_NEW,
                                        ward_or = A2a_NEW_1,
                                        ward_or_1 = A2a_NEW_2,
                                        live_now = A3a,
                                        LGA_now = A3a_NEW,
                                        ward_now = A3a_NEW_1,
                                        ward_now_1 = A3a_NEW_2,
                                        year = A5Year,
                                        month = A5Month,
                                        reason_leave = A7,
                                        urban_previous = A2C,
                                        factor_to_move = A8,
                                        ftm_disaster = A8_bis1,
                                        ftm_conflict = A8_bis2,
                                        ftm_livelihoods = A8_bis3,
                                        ftm_accessland = A8_bis4,
                                        ftm_accessservices = A8_bis5,
                                        ftm_foodinsecurity = A8_bis6,
                                        ftm_caterscarcity = A8_bis7,
                                        ftm_environmental_fc = A8_bis8,
                                        ftm_no = A8_bis9,
                                        ftm_multipletimes = C4_1,
                                        ftm_howmany = C4,
                                        placebf_affecteddisasaster = C4_2_1,
                                        placebf_affecteddinsecurity = C4_2_2,
                                        placebf_notaffordable = C4_2_3,
                                        placebf_evicted = C4_2_4,
                                        placebf_notlivelihoods = C4_2_5,
                                        placebf_other = C4_2_6,
                                        placebf_pna = C4_2_7,
                                        placebf_reasontomove = A8a,
                                        idp_awareness = A14,
                                        #= TYPE_RECODE,
                                        idp_hosting = C7,
                                        #= HOSTING_RECODE,
                                        idphosting_sinceyr = C8_1_1,
                                        idphosting_sincemonth = C8_1_2,
                                        idphosting_howmeet = C9,
                                        schooling = A30,
                                        numdep_05 = A101_1,
                                        numdep_614 = A101_2,
                                        numdep_1517 = A101_3,
                                        numdep1864 = A101_4,
                                        numdep_65 = A101_5,
                                        #= HAVE_CHILDREN,
                                        wgq_seeing_0 = A52_1,
                                        wgq_seeing_1 = A52_2,
                                        wgq_hearing_0 = A53_1,
                                        wgq_hearing_1 = A53_2,
                                        wgq_walking_0 = A54_1,
                                        wgq_walking_1 = A54_2,
                                        wgq_remembering_0 = A55_1,
                                        wgq_remembering_1 = A55_2,
                                        wgq_selfcare_0 = A56_1,
                                        wgq_selfcare_1 = A56_2,
                                        wgq_language_0 = A57_1,
                                        wgq_language_1 = A57_2,
                                        housing_D1 = D1,
                                        housing_D1_2 = D1_2,
                                        housing_D1_3 = D1_3,
                                        housing_D2_1 = D2_1,
                                        housing_D2_2 = D2_2,
                                        housing_D3 = D3,
                                        housing_D3_2 = D3_2,
                                        housing_D3_3 = D3_3,
                                        housing_D4_1 = D4_1,
                                        housing_D4_2 = D4_2,
                                        housing_D5 = D5,
                                        housing_D6a_1 = D6a_1,
                                        housing_D6a_2 = D6a_2,
                                        housing_D6a_3 = D6a_3,
                                        housing_D6a_4 = D6a_4,
                                        housing_D6a_5 = D6a_5,
                                        housing_D6a_6 = D6a_6,
                                        housing_D6a_7 = D6a_7,
                                        housing_D6a_8 = D6a_8,
                                        housing_D6a_9 = D6a_9,
                                        housing_D6a_10 = D6a_10,
                                        housing_D6a_11 = D6a_11,
                                        housing_D6a_12 = D6a_12,
                                        housing_D6a_98 = D6a_98,
                                        housing_D6a_99 = D6a_99,
                                        housing_D6b_1 = D6b_1,
                                        housing_D6b_2 = D6b_2,
                                        housing_D6b_3 = D6b_3,
                                        housing_D6b_4 = D6b_4,
                                        housing_D6b_5 = D6b_5,
                                        housing_D6b_6 = D6b_6,
                                        housing_D6b_7 = D6b_7,
                                        housing_D6b_8 = D6b_8,
                                        housing_D6b_9 = D6b_9,
                                        housing_D6b_10 = D6b_10,
                                        housing_D6b_11 = D6b_11,
                                        housing_D6b_12 = D6b_12,
                                        housing_D6b_98 = D6b_98,
                                        housing_D6b_99 = D6b_99,
                                        housing_D13 = D13,
                                        housing_D13_2 = D13_2,
                                        housing_D13_3 = D13_3,
                                        housing_D13a = D13a,
                                        housing_D13b = D13b,
                                        housing_D14 = D14,
                                        housing_D15a_1 = D15a_1,
                                        housing_D15a_2 = D15a_2,
                                        housing_D15a_3 = D15a_3,
                                        housing_D15a_4 = D15a_4,
                                        housing_D15a_5 = D15a_5,
                                        housing_D15a_6 = D15a_6,
                                        housing_D15a_7 = D15a_7,
                                        housing_D15a_8 = D15a_8,
                                        housing_D15a_9 = D15a_9,
                                        housing_D15a_10 = D15a_10,
                                        housing_D15a_11 = D15a_11,
                                        housing_D15a_12 = D15a_12,
                                        housing_D15a_98 = D15a_98,
                                        housing_D15a_99 = D15a_99,
                                        housing_D15b_1 = D15b_1,
                                        housing_D15b_2 = D15b_2,
                                        housing_D15b_3 = D15b_3,
                                        housing_D15b_4 = D15b_4,
                                        housing_D15b_5 = D15b_5,
                                        housing_D15b_6 = D15b_6,
                                        housing_D15b_7 = D15b_7,
                                        housing_D15b_8 = D15b_8,
                                        housing_D15b_9 = D15b_9,
                                        housing_D15b_10 = D15b_10,
                                        housing_D15b_11 = D15b_11,
                                        housing_D15b_12 = D15b_12,
                                        housing_D15b_98 = D15b_98,
                                        housing_D15b_99 = D15b_99,
                                        housing_D16 = D16,
                                        housing_D17_1_1 = D17_1_1,
                                        housing_D17_1_2 = D17_1_2,
                                        housing_D17_1_3 = D17_1_3,
                                        housing_D17_1_4 = D17_1_4,
                                        housing_D17_1_5 = D17_1_5,
                                        housing_D17_1_6 = D17_1_6,
                                        housing_D17_1_99 = D17_1_99,
                                        housing_D18_1_1 = D18_1_1,
                                        housing_D18_1_2 = D18_1_2,
                                        housing_D18_1_3 = D18_1_3,
                                        security_E1 = E1,
                                        security_E2a_1 = E2a_1,
                                        security_E2a_2 = E2a_2,
                                        security_E2a_3 = E2a_3,
                                        security_E2a_4 = E2a_4,
                                        security_E2a_5 = E2a_5,
                                        security_E2a_6 = E2a_6,
                                        security_E2a_7 = E2a_7,
                                        security_E2a_8 = E2a_8,
                                        security_E2a_99 = E2a_99,
                                        security_E2b_1 = E2b_1,
                                        security_E2b_2 = E2b_2,
                                        security_E2b_3 = E2b_3,
                                        security_E2b_4 = E2b_4,
                                        security_E2b_5 = E2b_5,
                                        security_E2b_6 = E2b_6,
                                        security_E2b_7 = E2b_7,
                                        security_E2b_8 = E2b_8,
                                        security_E2b_99 = E2b_99,
                                        security_E3 = E3,
                                        security_E4_1 = E4_1,
                                        security_E4_2 = E4_2,
                                        security_E4_3 = E4_3,
                                        security_E4_4 = E4_4,
                                        security_E4_5 = E4_5,
                                        security_E4_6 = E4_6,
                                        security_E4_7 = E4_7,
                                        security_E4_8 = E4_8,
                                        security_E4_9 = E4_9,
                                        security_E4_10 = E4_10,
                                        security_E4_11 = E4_11,
                                        security_E4_99 = E4_99,
                                        security_E7 = E7,
                                        security_E2a_0 = E2a_0,
                                        security_E2a.1 = E2a.1,
                                        security_E2a_1_SPECIFY = E2a_1_SPECIFY,
                                        security_E15 = E15,
                                        security_E16a_1 = E16a_1,
                                        security_E16a_2 = E16a_2,
                                        security_E16a_3 = E16a_3,
                                        security_E16a_4 = E16a_4,
                                        security_E16a_5 = E16a_5,
                                        security_E16a_6 = E16a_6,
                                        security_E16a_7 = E16a_7,
                                        security_E16a_8 = E16a_8,
                                        security_E16a_99 = E16a_99,
                                        security_E16b_1 = E16b_1,
                                        security_E16b_2 = E16b_2,
                                        security_E16b_3 = E16b_3,
                                        security_E16b_4 = E16b_4,
                                        security_E16b_5 = E16b_5,
                                        security_E16b_6 = E16b_6,
                                        security_E16b_7 = E16b_7,
                                        security_E16b_8 = E16b_8,
                                        security_E16b_99 = E16b_99,
                                        security_E3_H = E3_H,
                                        security_E4_H_1 = E4_H_1,
                                        security_E4_H_2 = E4_H_2,
                                        security_E4_H_3 = E4_H_3,
                                        security_E4_H_4 = E4_H_4,
                                        security_E4_H_5 = E4_H_5,
                                        security_E4_H_6 = E4_H_6,
                                        security_E4_H_7 = E4_H_7,
                                        security_E4_H_8 = E4_H_8,
                                        security_E4_H_9 = E4_H_9,
                                        security_E4_H_10 = E4_H_10,
                                        security_E4_H_11 = E4_H_11,
                                        security_E4_H_99 = E4_H_99,
                                        security_E7_H = E7_H,
                                        security_E16a_0 = E16a_0,
                                        security_E16a.1 = E16a.1,
                                        security_E16a_1_SPECIFY = E16a_1_SPECIFY,
                                        education_F1a = F1a,
                                        education_F1b = F1b,
                                        education_idp_MRK_IDP_CHILDREN = MRK_IDP_CHILDREN,
                                        education_idp_F1c_1 = F1c_1,
                                        education_idp_F1c_2 = F1c_2,
                                        education_idp_F1c_3 = F1c_3,
                                        education_idp_F1c_4 = F1c_4,
                                        education_idp_F1c_5 = F1c_5,
                                        education_idp_F3_1 = F3_1,
                                        education_idp_F3_2 = F3_2,
                                        education_idp_F4_1 = F4_1,
                                        education_idp_F4_2 = F4_2,
                                        education_idp_F5_1 = F5_1,
                                        education_idp_F5_2 = F5_2,
                                        education_idp_F6_1_1 = F6_1_1,
                                        education_idp_F6_1_2 = F6_1_2,
                                        education_idp_F6_1_3 = F6_1_3,
                                        education_idp_F6_1_4 = F6_1_4,
                                        education_idp_F6_1_5 = F6_1_5,
                                        education_idp_F6_1_6 = F6_1_6,
                                        education_idp_F6_1_7 = F6_1_7,
                                        education_idp_F6_1_8 = F6_1_8,
                                        education_idp_F6_1_9 = F6_1_9,
                                        education_idp_F6_1_10 = F6_1_10,
                                        education_idp_F6_1_11 = F6_1_11,
                                        education_idp_F6_1_12 = F6_1_12,
                                        education_idp_F6_1_13 = F6_1_13,
                                        education_idp_F6_1_14 = F6_1_14,
                                        education_idp_F6_1_98 = F6_1_98,
                                        education_idp_F6_1_99 = F6_1_99,
                                        education_idp_F6_2_1 = F6_2_1,
                                        education_idp_F6_2_2 = F6_2_2,
                                        education_idp_F6_2_3 = F6_2_3,
                                        education_idp_F6_2_4 = F6_2_4,
                                        education_idp_F6_2_5 = F6_2_5,
                                        education_idp_F6_2_6 = F6_2_6,
                                        education_idp_F6_2_7 = F6_2_7,
                                        education_idp_F6_2_8 = F6_2_8,
                                        education_idp_F6_2_9 = F6_2_9,
                                        education_idp_F6_2_10 = F6_2_10,
                                        education_idp_F6_2_11 = F6_2_11,
                                        education_idp_F6_2_12 = F6_2_12,
                                        education_idp_F6_2_13 = F6_2_13,
                                        education_idp_F6_2_14 = F6_2_14,
                                        education_idp_F6_2_98 = F6_2_98,
                                        education_idp_F6_2_99 = F6_2_99,
                                        education_idp_F7_1 = F7_1,
                                        education_idp_F7_2 = F7_2,
                                        education_idp_F8_1_1 = F8_1_1,
                                        education_idp_F8_1_2 = F8_1_2,
                                        education_idp_F8_2_1 = F8_2_1,
                                        education_idp_F8_2_2 = F8_2_2,
                                        education_idp_F8_3_1 = F8_3_1,
                                        education_idp_F8_3_2 = F8_3_2,
                                        education_idp_F8a_1_1 = F8a_1_1,
                                        education_idp_F8a_1_2 = F8a_1_2,
                                        education_idp_F8a_1_3 = F8a_1_3,
                                        education_idp_F8a_1_4 = F8a_1_4,
                                        education_idp_F8a_1_5 = F8a_1_5,
                                        education_idp_F8a_1_6 = F8a_1_6,
                                        education_idp_F8a_1_98 = F8a_1_98,
                                        education_idp_F8a_1_99 = F8a_1_99,
                                        education_idp_F8a_2_1 = F8a_2_1,
                                        education_idp_F8a_2_2 = F8a_2_2,
                                        education_idp_F8a_2_3 = F8a_2_3,
                                        education_idp_F8a_2_4 = F8a_2_4,
                                        education_idp_F8a_2_5 = F8a_2_5,
                                        education_idp_F8a_2_6 = F8a_2_6,
                                        education_idp_F8a_2_98 = F8a_2_98,
                                        education_idp_F8a_2_99 = F8a_2_99,
                                        education_idp_F8a_1_98Others = F8a_1_98Others,
                                        education_idp_F8a_2_98Others = F8a_2_98Others,
                                        education_idp_F81_1 = F81_1,
                                        education_idp_F81_2 = F81_2,
                                        education_idp_F81a_1 = F81a_1,
                                        education_idp_F81a_2 = F81a_2,
                                        education_idp_F9_1 = F9_1,
                                        education_idp_F9_2 = F9_2,
                                        education_idp_F10_1_1 = F10_1_1,
                                        education_idp_F10_1_2 = F10_1_2,
                                        education_idp_F10_1_3 = F10_1_3,
                                        education_idp_F10_1_4 = F10_1_4,
                                        education_idp_F10_1_5 = F10_1_5,
                                        education_idp_F10_1_6 = F10_1_6,
                                        education_idp_F10_1_7 = F10_1_7,
                                        education_idp_F10_1_8 = F10_1_8,
                                        education_idp_F10_1_9 = F10_1_9,
                                        education_idp_F10_1_10 = F10_1_10,
                                        education_idp_F10_1_11 = F10_1_11,
                                        education_idp_F10_1_12 = F10_1_12,
                                        education_idp_F10_1_13 = F10_1_13,
                                        education_idp_F10_1_98 = F10_1_98,
                                        education_idp_F10_1_99 = F10_1_99,
                                        education_idp_F10_2_1 = F10_2_1,
                                        education_idp_F10_2_2 = F10_2_2,
                                        education_idp_F10_2_3 = F10_2_3,
                                        education_idp_F10_2_4 = F10_2_4,
                                        education_idp_F10_2_5 = F10_2_5,
                                        education_idp_F10_2_6 = F10_2_6,
                                        education_idp_F10_2_7 = F10_2_7,
                                        education_idp_F10_2_8 = F10_2_8,
                                        education_idp_F10_2_9 = F10_2_9,
                                        education_idp_F10_2_10 = F10_2_10,
                                        education_idp_F10_2_11 = F10_2_11,
                                        education_idp_F10_2_12 = F10_2_12,
                                        education_idp_F10_2_13 = F10_2_13,
                                        education_idp_F10_2_98 = F10_2_98,
                                        education_idp_F10_2_99 = F10_2_99,
                                        education_idp_F11_1 = F11_1,
                                        education_idp_F11_2 = F11_2,
                                        education_idp_F12_1 = F12_1,
                                        education_idp_F12_2 = F12_2,
                                        education_idp_F12_1_1 = F12_1_1,
                                        education_idp_F12_1_2 = F12_1_2,
                                        education_idp_F12_2_1 = F12_2_1,
                                        education_idp_F12_2_2 = F12_2_2,
                                        education_idp_F12_3_1 = F12_3_1,
                                        education_idp_F12_3_2 = F12_3_2,
                                        education_idp_F12a_1_1 = F12a_1_1,
                                        education_idp_F12a_1_2 = F12a_1_2,
                                        education_idp_F12a_1_3 = F12a_1_3,
                                        education_idp_F12a_1_4 = F12a_1_4,
                                        education_idp_F12a_1_5 = F12a_1_5,
                                        education_idp_F12a_1_6 = F12a_1_6,
                                        education_idp_F12a_1_98 = F12a_1_98,
                                        education_idp_F12a_1_99 = F12a_1_99,
                                        education_idp_F12a_2_1 = F12a_2_1,
                                        education_idp_F12a_2_2 = F12a_2_2,
                                        education_idp_F12a_2_3 = F12a_2_3,
                                        education_idp_F12a_2_4 = F12a_2_4,
                                        education_idp_F12a_2_5 = F12a_2_5,
                                        education_idp_F12a_2_6 = F12a_2_6,
                                        education_idp_F12a_2_98 = F12a_2_98,
                                        education_idp_F12a_2_99 = F12a_2_99,
                                        education_idp_F12a_1_98Others = F12a_1_98Others,
                                        education_idp_F12a_2_98Others = F12a_2_98Others,
                                        education_idp_F12b_1_1 = F12b_1_1,
                                        education_idp_F12b_1_2 = F12b_1_2,
                                        education_idp_F12b_1_3 = F12b_1_3,
                                        education_idp_F12b_1_4 = F12b_1_4,
                                        education_idp_F12b_1_98 = F12b_1_98,
                                        education_idp_F12b_1_99 = F12b_1_99,
                                        education_idp_F12b_2_1 = F12b_2_1,
                                        education_idp_F12b_2_2 = F12b_2_2,
                                        education_idp_F12b_2_3 = F12b_2_3,
                                        education_idp_F12b_2_4 = F12b_2_4,
                                        education_idp_F12b_2_98 = F12b_2_98,
                                        education_idp_F12b_2_99 = F12b_2_99,
                                        education_idp_F13_1 = F13_1,
                                        education_idp_F13_2 = F13_2,
                                        education_idp_F14a_1_1 = F14a_1_1,
                                        education_idp_F14a_1_2 = F14a_1_2,
                                        education_idp_F14a_1_3 = F14a_1_3,
                                        education_idp_F14a_1_4 = F14a_1_4,
                                        education_idp_F14a_1_5 = F14a_1_5,
                                        education_idp_F14a_1_6 = F14a_1_6,
                                        education_idp_F14a_1_7 = F14a_1_7,
                                        education_idp_F14a_1_8 = F14a_1_8,
                                        education_idp_F14a_1_9 = F14a_1_9,
                                        education_idp_F14a_1_10 = F14a_1_10,
                                        education_idp_F14a_1_99 = F14a_1_99,
                                        education_idp_F14a_2_1 = F14a_2_1,
                                        education_idp_F14a_2_2 = F14a_2_2,
                                        education_idp_F14a_2_3 = F14a_2_3,
                                        education_idp_F14a_2_4 = F14a_2_4,
                                        education_idp_F14a_2_5 = F14a_2_5,
                                        education_idp_F14a_2_6 = F14a_2_6,
                                        education_idp_F14a_2_7 = F14a_2_7,
                                        education_idp_F14a_2_8 = F14a_2_8,
                                        education_idp_F14a_2_9 = F14a_2_9,
                                        education_idp_F14a_2_10 = F14a_2_10,
                                        education_idp_F14a_2_99 = F14a_2_99,
                                        education_idp_F14b_1_1 = F14b_1_1,
                                        education_idp_F14b_1_2 = F14b_1_2,
                                        education_idp_F14b_1_3 = F14b_1_3,
                                        education_idp_F14b_1_4 = F14b_1_4,
                                        education_idp_F14b_1_5 = F14b_1_5,
                                        education_idp_F14b_1_6 = F14b_1_6,
                                        education_idp_F14b_1_7 = F14b_1_7,
                                        education_idp_F14b_1_8 = F14b_1_8,
                                        education_idp_F14b_1_9 = F14b_1_9,
                                        education_idp_F14b_1_10 = F14b_1_10,
                                        education_idp_F14b_1_99 = F14b_1_99,
                                        education_idp_F14b_2_1 = F14b_2_1,
                                        education_idp_F14b_2_2 = F14b_2_2,
                                        education_idp_F14b_2_3 = F14b_2_3,
                                        education_idp_F14b_2_4 = F14b_2_4,
                                        education_idp_F14b_2_5 = F14b_2_5,
                                        education_idp_F14b_2_6 = F14b_2_6,
                                        education_idp_F14b_2_7 = F14b_2_7,
                                        education_idp_F14b_2_8 = F14b_2_8,
                                        education_idp_F14b_2_9 = F14b_2_9,
                                        education_idp_F14b_2_10 = F14b_2_10,
                                        education_idp_F14b_2_99 = F14b_2_99,
                                        education_idp_F25a = F25a,
                                        education_idp_F25b = F25b,
                                        education_host_MRK_HOST_CHILDREN = MRK_HOST_CHILDREN,
                                        education_host_F25c_1 = F25c_1,
                                        education_host_F25c_2 = F25c_2,
                                        education_host_F25c_3 = F25c_3,
                                        education_host_F25c_4 = F25c_4,
                                        education_host_F25c_5 = F25c_5,
                                        education_host_F26_1 = F26_1,
                                        education_host_F26_2 = F26_2,
                                        education_host_F15_1 = F15_1,
                                        education_host_F15_2 = F15_2,
                                        education_host_F15_2_1 = F15_2_1,
                                        education_host_F15_2_2 = F15_2_2,
                                        education_host_F16_1_1 = F16_1_1,
                                        education_host_F16_1_2 = F16_1_2,
                                        education_host_F16_1_3 = F16_1_3,
                                        education_host_F16_1_4 = F16_1_4,
                                        education_host_F16_1_5 = F16_1_5,
                                        education_host_F16_1_6 = F16_1_6,
                                        education_host_F16_1_7 = F16_1_7,
                                        education_host_F16_1_8 = F16_1_8,
                                        education_host_F16_1_9 = F16_1_9,
                                        education_host_F16_1_10 = F16_1_10,
                                        education_host_F16_1_11 = F16_1_11,
                                        education_host_F16_1_12 = F16_1_12,
                                        education_host_F16_1_13 = F16_1_13,
                                        education_host_F16_1_14 = F16_1_14,
                                        education_host_F16_1_98 = F16_1_98,
                                        education_host_F16_1_99 = F16_1_99,
                                        education_host_F16_2_1 = F16_2_1,
                                        education_host_F16_2_2 = F16_2_2,
                                        education_host_F16_2_3 = F16_2_3,
                                        education_host_F16_2_4 = F16_2_4,
                                        education_host_F16_2_5 = F16_2_5,
                                        education_host_F16_2_6 = F16_2_6,
                                        education_host_F16_2_7 = F16_2_7,
                                        education_host_F16_2_8 = F16_2_8,
                                        education_host_F16_2_9 = F16_2_9,
                                        education_host_F16_2_10 = F16_2_10,
                                        education_host_F16_2_11 = F16_2_11,
                                        education_host_F16_2_12 = F16_2_12,
                                        education_host_F16_2_13 = F16_2_13,
                                        education_host_F16_2_14 = F16_2_14,
                                        education_host_F16_2_98 = F16_2_98,
                                        education_host_F16_2_99 = F16_2_99,
                                        education_host_F17_1 = F17_1,
                                        education_host_F17_2 = F17_2,
                                        education_host_F18_1_1 = F18_1_1,
                                        education_host_F18_1_2 = F18_1_2,
                                        education_host_F18_2_1 = F18_2_1,
                                        education_host_F18_2_2 = F18_2_2,
                                        education_host_F18_3_1 = F18_3_1,
                                        education_host_F18_3_2 = F18_3_2,
                                        education_host_F18a_1_1 = F18a_1_1,
                                        education_host_F18a_1_2 = F18a_1_2,
                                        education_host_F18a_1_3 = F18a_1_3,
                                        education_host_F18a_1_4 = F18a_1_4,
                                        education_host_F18a_1_5 = F18a_1_5,
                                        education_host_F18a_1_6 = F18a_1_6,
                                        education_host_F18a_1_98 = F18a_1_98,
                                        education_host_F18a_1_99 = F18a_1_99,
                                        education_host_F18a_2_1 = F18a_2_1,
                                        education_host_F18a_2_2 = F18a_2_2,
                                        education_host_F18a_2_3 = F18a_2_3,
                                        education_host_F18a_2_4 = F18a_2_4,
                                        education_host_F18a_2_5 = F18a_2_5,
                                        education_host_F18a_2_6 = F18a_2_6,
                                        education_host_F18a_2_98 = F18a_2_98,
                                        education_host_F18a_2_99 = F18a_2_99,
                                        education_host_F18a_1_98Other = F18a_1_98Other,
                                        education_host_F18a_2_98Other = F18a_2_98Other,
                                        education_host_F181_1 = F181_1,
                                        education_host_F181_2 = F181_2,
                                        education_host_F181a_1 = F181a_1,
                                        education_host_F181a_2 = F181a_2,
                                        education_host_F19_1 = F19_1,
                                        education_host_F19_2 = F19_2,
                                        education_host_F20_1_1 = F20_1_1,
                                        education_host_F20_1_2 = F20_1_2,
                                        education_host_F20_1_3 = F20_1_3,
                                        education_host_F20_1_4 = F20_1_4,
                                        education_host_F20_1_5 = F20_1_5,
                                        education_host_F20_1_6 = F20_1_6,
                                        education_host_F20_1_7 = F20_1_7,
                                        education_host_F20_1_8 = F20_1_8,
                                        education_host_F20_1_9 = F20_1_9,
                                        education_host_F20_1_10 = F20_1_10,
                                        education_host_F20_1_11 = F20_1_11,
                                        education_host_F20_1_12 = F20_1_12,
                                        education_host_F20_1_13 = F20_1_13,
                                        education_host_F20_1_98 = F20_1_98,
                                        education_host_F20_1_99 = F20_1_99,
                                        education_host_F20_2_1 = F20_2_1,
                                        education_host_F20_2_2 = F20_2_2,
                                        education_host_F20_2_3 = F20_2_3,
                                        education_host_F20_2_4 = F20_2_4,
                                        education_host_F20_2_5 = F20_2_5,
                                        education_host_F20_2_6 = F20_2_6,
                                        education_host_F20_2_7 = F20_2_7,
                                        education_host_F20_2_8 = F20_2_8,
                                        education_host_F20_2_9 = F20_2_9,
                                        education_host_F20_2_10 = F20_2_10,
                                        education_host_F20_2_11 = F20_2_11,
                                        education_host_F20_2_12 = F20_2_12,
                                        education_host_F20_2_13 = F20_2_13,
                                        education_host_F20_2_98 = F20_2_98,
                                        education_host_F20_2_99 = F20_2_99,
                                        education_host_F27_1 = F27_1,
                                        education_host_F27_2 = F27_2,
                                        education_host_F21_1_1 = F21_1_1,
                                        education_host_F21_1_2 = F21_1_2,
                                        education_host_F21_2_1 = F21_2_1,
                                        education_host_F21_2_2 = F21_2_2,
                                        education_host_F21_3_1 = F21_3_1,
                                        education_host_F21_3_2 = F21_3_2,
                                        education_host_F21a_1_1 = F21a_1_1,
                                        education_host_F21a_1_2 = F21a_1_2,
                                        education_host_F21a_1_3 = F21a_1_3,
                                        education_host_F21a_1_4 = F21a_1_4,
                                        education_host_F21a_1_5 = F21a_1_5,
                                        education_host_F21a_1_6 = F21a_1_6,
                                        education_host_F21a_1_98 = F21a_1_98,
                                        education_host_F21a_1_99 = F21a_1_99,
                                        education_host_F21a_2_1 = F21a_2_1,
                                        education_host_F21a_2_2 = F21a_2_2,
                                        education_host_F21a_2_3 = F21a_2_3,
                                        education_host_F21a_2_4 = F21a_2_4,
                                        education_host_F21a_2_5 = F21a_2_5,
                                        education_host_F21a_2_6 = F21a_2_6,
                                        education_host_F21a_2_98 = F21a_2_98,
                                        education_host_F21a_2_99 = F21a_2_99,
                                        education_host_F21a_1_98Others = F21a_1_98Others,
                                        education_host_F21a_2_98Others = F21a_2_98Others,
                                        education_host_F21b_1_1 = F21b_1_1,
                                        education_host_F21b_1_2 = F21b_1_2,
                                        education_host_F21b_1_3 = F21b_1_3,
                                        education_host_F21b_1_4 = F21b_1_4,
                                        education_host_F21b_1_98 = F21b_1_98,
                                        education_host_F21b_1_99 = F21b_1_99,
                                        education_host_F21b_2_1 = F21b_2_1,
                                        education_host_F21b_2_2 = F21b_2_2,
                                        education_host_F21b_2_3 = F21b_2_3,
                                        education_host_F21b_2_4 = F21b_2_4,
                                        education_host_F21b_2_98 = F21b_2_98,
                                        education_host_F21b_2_99 = F21b_2_99,
                                        education_host_F22_1 = F22_1,
                                        education_host_F22_2 = F22_2,
                                        education_host_F23a_1_1 = F23a_1_1,
                                        education_host_F23a_1_2 = F23a_1_2,
                                        education_host_F23a_1_3 = F23a_1_3,
                                        education_host_F23a_1_4 = F23a_1_4,
                                        education_host_F23a_1_5 = F23a_1_5,
                                        education_host_F23a_1_6 = F23a_1_6,
                                        education_host_F23a_1_7 = F23a_1_7,
                                        education_host_F23a_1_8 = F23a_1_8,
                                        education_host_F23a_1_9 = F23a_1_9,
                                        education_host_F23a_1_10 = F23a_1_10,
                                        education_host_F23a_1_99 = F23a_1_99,
                                        education_host_F23a_2_1 = F23a_2_1,
                                        education_host_F23a_2_2 = F23a_2_2,
                                        education_host_F23a_2_3 = F23a_2_3,
                                        education_host_F23a_2_4 = F23a_2_4,
                                        education_host_F23a_2_5 = F23a_2_5,
                                        education_host_F23a_2_6 = F23a_2_6,
                                        education_host_F23a_2_7 = F23a_2_7,
                                        education_host_F23a_2_8 = F23a_2_8,
                                        education_host_F23a_2_9 = F23a_2_9,
                                        education_host_F23a_2_10 = F23a_2_10,
                                        education_host_F23a_2_99 = F23a_2_99,
                                        education_host_F23b_1_1 = F23b_1_1,
                                        education_host_F23b_1_2 = F23b_1_2,
                                        education_host_F23b_1_3 = F23b_1_3,
                                        education_host_F23b_1_4 = F23b_1_4,
                                        education_host_F23b_1_5 = F23b_1_5,
                                        education_host_F23b_1_6 = F23b_1_6,
                                        education_host_F23b_1_7 = F23b_1_7,
                                        education_host_F23b_1_8 = F23b_1_8,
                                        education_host_F23b_1_9 = F23b_1_9,
                                        education_host_F23b_1_10 = F23b_1_10,
                                        education_host_F23b_1_99 = F23b_1_99,
                                        education_host_F23b_2_1 = F23b_2_1,
                                        education_host_F23b_2_2 = F23b_2_2,
                                        education_host_F23b_2_3 = F23b_2_3,
                                        education_host_F23b_2_4 = F23b_2_4,
                                        education_host_F23b_2_5 = F23b_2_5,
                                        education_host_F23b_2_6 = F23b_2_6,
                                        education_host_F23b_2_7 = F23b_2_7,
                                        education_host_F23b_2_8 = F23b_2_8,
                                        education_host_F23b_2_9 = F23b_2_9,
                                        education_host_F23b_2_10 = F23b_2_10,
                                        education_host_F23b_2_99 = F23b_2_99,
                                        livelihhods_G01 = G01,
                                        livelihhods_G02 = G02,
                                        livelihhods_G01_1 = G01_1,
                                        livelihhods_G01_bis = G01_bis,
                                        livelihhods_G02_1 = G02_1,
                                        livelihhods_G3_1 = G3_1,
                                        livelihhods_G3_2 = G3_2,
                                        livelihhods_G4 = G4,
                                        livelihhods_G4_1 = G4_1,
                                        livelihhods_G40 = G40,
                                        livelihhods_G7_1 = G7_1,
                                        livelihhods_G7_2 = G7_2,
                                        livelihhods_G7_3 = G7_3,
                                        livelihhods_G7_4 = G7_4,
                                        livelihhods_G7_98 = G7_98,
                                        livelihhods_G7_99 = G7_99,
                                        livelihhods_G1 = G1,
                                        livelihhods_G2 = G2,
                                        livelihhods_G1_1 = G1_1,
                                        livelihhods_G1_bis = G1_bis,
                                        livelihhods_G2_1 = G2_1,
                                        livelihhods_G11 = G11,
                                        livelihhods_G13_1 = G13_1,
                                        livelihhods_G13_2 = G13_2,
                                        livelihhods_G13_3 = G13_3,
                                        livelihhods_G13_4 = G13_4,
                                        livelihhods_G13_98 = G13_98,
                                        livelihhods_G13_99 = G13_99,
                                        livelihhods_G15 = G15,
                                        livelihhods_G17 = G17,
                                        livelihhods_G18 = G18,
                                        livelihhods_G38 = G38,
                                        livelihhods_G39 = G39,
                                        livelihhods_G40_1 = G40_1,
                                        livelihhods_G42 = G42,
                                        livelihhods_G43 = G43,
                                        livelihhods_G45 = G45,
                                        livelihhods_G46_1 = G46_1,
                                        livelihhods_G46_2 = G46_2,
                                        livelihhods_G46_3 = G46_3,
                                        livelihhods_G46_4 = G46_4,
                                        livelihhods_G46_5 = G46_5,
                                        livelihhods_G46_6 = G46_6,
                                        livelihhods_G46_99 = G46_99,
                                        livelihhods_AG10_1 = AG10_1,
                                        livelihhods_AG10_2 = AG10_2,
                                        livelihhods_AG10_3 = AG10_3,
                                        livelihhods_AG11 = AG11,
                                        livelihhods_AG12_1 = AG12_1,
                                        livelihhods_AG12_2 = AG12_2,
                                        livelihhods_AG12_3 = AG12_3,
                                        livelihhods_AG12_4 = AG12_4,
                                        livelihhods_AG12_5 = AG12_5,
                                        livelihhods_AG12_6 = AG12_6,
                                        livelihhods_AG12_7 = AG12_7,
                                        livelihhods_AG12_99 = AG12_99,
                                        livelihhods_AG13 = AG13,
                                        livelihhods_AG14_1 = AG14_1,
                                        livelihhods_AG14_2 = AG14_2,
                                        livelihhods_AG14_3 = AG14_3,
                                        livelihhods_AG15_1 = AG15_1,
                                        livelihhods_AG15_2 = AG15_2,
                                        livelihhods_AG15_3 = AG15_3,
                                        livelihhods_AG15_4 = AG15_4,
                                        livelihhods_AG15_5 = AG15_5,
                                        livelihhods_AG15_6 = AG15_6,
                                        livelihhods_AG15_7 = AG15_7,
                                        livelihhods_AG15_8 = AG15_8,
                                        livelihhods_AG15_9 = AG15_9,
                                        livelihhods_AG15_10 = AG15_10,
                                        livelihhods_AG15_99 = AG15_99,
                                        livelihhods_AG16_1 = AG16_1,
                                        livelihhods_AG16_2 = AG16_2,
                                        livelihhods_AG16_3 = AG16_3,
                                        livelihhods_G19 = G19,
                                        livelihhods_G20 = G20,
                                        livelihhods_G19_1 = G19_1,
                                        livelihhods_G19_bis = G19_bis,
                                        livelihhods_G20_4 = G20_4,
                                        livelihhods_G20_1_1 = G20_1_1,
                                        livelihhods_G20_1_2 = G20_1_2,
                                        livelihhods_G20_2 = G20_2,
                                        livelihhods_G20_3 = G20_3,
                                        livelihhods_G21 = G21,
                                        livelihhods_G23_1 = G23_1,
                                        livelihhods_G23_2 = G23_2,
                                        livelihhods_G23_3 = G23_3,
                                        livelihhods_G23_4 = G23_4,
                                        livelihhods_G23_98 = G23_98,
                                        livelihhods_G23_99 = G23_99,
                                        livelihhods_G25 = G25,
                                        livelihhods_G26 = G26,
                                        livelihhods_G25_1 = G25_1,
                                        livelihhods_G25_bis = G25_bis,
                                        livelihhods_G26_1 = G26_1,
                                        livelihhods_G27 = G27,
                                        livelihhods_G29_1 = G29_1,
                                        livelihhods_G29_2 = G29_2,
                                        livelihhods_G29_3 = G29_3,
                                        livelihhods_G29_4 = G29_4,
                                        livelihhods_G29_98 = G29_98,
                                        livelihhods_G29_99 = G29_99,
                                        livelihhods_G31 = G31,
                                        livelihhods_G33 = G33,
                                        livelihhods_G34 = G34,
                                        livelihhods_G38_H = G38_H,
                                        livelihhods_G39_H = G39_H,
                                        livelihhods_G40_H = G40_H,
                                        livelihhods_G42_H = G42_H,
                                        livelihhods_G43_H = G43_H,
                                        livelihhods_G45_H = G45_H,
                                        livelihhods_G46_H_1 = G46_H_1,
                                        livelihhods_G46_H_2 = G46_H_2,
                                        livelihhods_G46_H_3 = G46_H_3,
                                        livelihhods_G46_H_4 = G46_H_4,
                                        livelihhods_G46_H_5 = G46_H_5,
                                        livelihhods_G46_H_6 = G46_H_6,
                                        livelihhods_G46_H_99 = G46_H_99,
                                        livelihhods_AG10_H_1 = AG10_H_1,
                                        livelihhods_AG10_H_2 = AG10_H_2,
                                        livelihhods_AG10_H_3 = AG10_H_3,
                                        livelihhods_AG11_H = AG11_H,
                                        livelihhods_AG12_H_1 = AG12_H_1,
                                        livelihhods_AG12_H_2 = AG12_H_2,
                                        livelihhods_AG12_H_3 = AG12_H_3,
                                        livelihhods_AG12_H_4 = AG12_H_4,
                                        livelihhods_AG12_H_5 = AG12_H_5,
                                        livelihhods_AG12_H_6 = AG12_H_6,
                                        livelihhods_AG12_H_7 = AG12_H_7,
                                        livelihhods_AG12_H_99 = AG12_H_99,
                                        livelihhods_AG13_H = AG13_H,
                                        livelihhods_AG14_H_1 = AG14_H_1,
                                        livelihhods_AG14_H_2 = AG14_H_2,
                                        livelihhods_AG14_H_3 = AG14_H_3,
                                        livelihhods_AG15_H_1 = AG15_H_1,
                                        livelihhods_AG15_H_2 = AG15_H_2,
                                        livelihhods_AG15_H_3 = AG15_H_3,
                                        livelihhods_AG15_H_4 = AG15_H_4,
                                        livelihhods_AG15_H_5 = AG15_H_5,
                                        livelihhods_AG15_H_6 = AG15_H_6,
                                        livelihhods_AG15_H_7 = AG15_H_7,
                                        livelihhods_AG15_H_8 = AG15_H_8,
                                        livelihhods_AG15_H_9 = AG15_H_9,
                                        livelihhods_AG15_H_10 = AG15_H_10,
                                        livelihhods_AG15_H_99 = AG15_H_99,
                                        livelihhods_AG16_H_1 = AG16_H_1,
                                        livelihhods_AG16_H_2 = AG16_H_2,
                                        livelihhods_AG16_H_3 = AG16_H_3,
                                        health_H1 = H1,
                                        health_H2a_1 = H2a_1,
                                        health_H2a_2 = H2a_2,
                                        health_H2a_3 = H2a_3,
                                        health_H2a_4 = H2a_4,
                                        health_H2a_5 = H2a_5,
                                        health_H2a_6 = H2a_6,
                                        health_H2a_99 = H2a_99,
                                        health_H2b_1 = H2b_1,
                                        health_H2b_2 = H2b_2,
                                        health_H2b_3 = H2b_3,
                                        health_H2b_4 = H2b_4,
                                        health_H2b_5 = H2b_5,
                                        health_H2b_6 = H2b_6,
                                        health_H2b_99 = H2b_99,
                                        health_H222 = H222,
                                        health_H223 = H223,
                                        health_H224 = H224,
                                        health_H225 = H225,
                                        health_H226 = H226,
                                        health_H227 = H227,
                                        health_H228 = H228,
                                        health_H3 = H3,
                                        health_H4a_1 = H4a_1,
                                        health_H4a_2 = H4a_2,
                                        health_H4a_3 = H4a_3,
                                        health_H4a_4 = H4a_4,
                                        health_H4a_5 = H4a_5,
                                        health_H4a_6 = H4a_6,
                                        health_H4a_7 = H4a_7,
                                        health_H4a_8 = H4a_8,
                                        health_H4a_9 = H4a_9,
                                        health_H4a_99 = H4a_99,
                                        health_H4ba_1 = H4ba_1,
                                        health_H4ba_2 = H4ba_2,
                                        health_H4ba_3 = H4ba_3,
                                        health_H4ba_4 = H4ba_4,
                                        health_H4ba_5 = H4ba_5,
                                        health_H4ba_6 = H4ba_6,
                                        health_H4ba_7 = H4ba_7,
                                        health_H4ba_8 = H4ba_8,
                                        health_H4ba_99 = H4ba_99,
                                        health_H5 = H5,
                                        health_H8 = H8,
                                        health_H6 = H6,
                                        health_H9 = H9,
                                        health_H7 = H7,
                                        health_H10 = H10,
                                        health_H11_1 = H11_1,
                                        health_H12a_1 = H12a_1,
                                        health_H12a_2 = H12a_2,
                                        health_H12a_3 = H12a_3,
                                        health_H12a_4 = H12a_4,
                                        health_H12a_5 = H12a_5,
                                        health_H12a_6 = H12a_6,
                                        health_H12a_99 = H12a_99,
                                        health_H12b_1 = H12b_1,
                                        health_H12b_2 = H12b_2,
                                        health_H12b_3 = H12b_3,
                                        health_H12b_4 = H12b_4,
                                        health_H12b_5 = H12b_5,
                                        health_H12b_6 = H12b_6,
                                        health_H12b_99 = H12b_99,
                                        health_H222_H = H222_H,
                                        health_H223_H = H223_H,
                                        health_H224_H = H224_H,
                                        health_H225_H = H225_H,
                                        health_H226_H = H226_H,
                                        health_H227_H = H227_H,
                                        health_H228_H = H228_H,
                                        health_H13 = H13,
                                        health_H14a_1 = H14a_1,
                                        health_H14a_2 = H14a_2,
                                        health_H14a_3 = H14a_3,
                                        health_H14a_4 = H14a_4,
                                        health_H14a_5 = H14a_5,
                                        health_H14a_6 = H14a_6,
                                        health_H14a_7 = H14a_7,
                                        health_H14a_8 = H14a_8,
                                        health_H14a_9 = H14a_9,
                                        health_H14a_99 = H14a_99,
                                        health_H14b_1 = H14b_1,
                                        health_H14b_2 = H14b_2,
                                        health_H14b_3 = H14b_3,
                                        health_H14b_4 = H14b_4,
                                        health_H14b_5 = H14b_5,
                                        health_H14b_6 = H14b_6,
                                        health_H14b_7 = H14b_7,
                                        health_H14b_8 = H14b_8,
                                        health_H14b_9 = H14b_9,
                                        health_H17 = H17,
                                        health_H18 = H18,
                                        health_H19 = H19,
                                        health_H20 = H20,
                                        health_H21 = H21,
                                        health_H22 = H22,
                                        closing_I0_1 = I0_1,
                                        closing_I0_2_1 = I0_2_1,
                                        closing_I0_2_2 = I0_2_2,
                                        closing_I0_2_3 = I0_2_3,
                                        closing_I0_2_4 = I0_2_4,
                                        closing_I0_2_98 = I0_2_98,
                                        closing_I0_2_99 = I0_2_99,
                                        closing_I0_5_1 = I0_5_1,
                                        closing_I0_5_2 = I0_5_2,
                                        closing_I0_5_3 = I0_5_3,
                                        closing_I0_5_4 = I0_5_4,
                                        closing_I0_5_5 = I0_5_5,
                                        closing_I0_5_6 = I0_5_6,
                                        closing_I0_6_1 = I0_6_1,
                                        closing_I0_6_2 = I0_6_2,
                                        closing_I0_6_3 = I0_6_3,
                                        closing_I0_6_4 = I0_6_4,
                                        closing_I0_6_5 = I0_6_5,
                                        closing_I0_6_6 = I0_6_6,
                                        closing_I0_6_99 = I0_6_99,
                                        closing_I0_7_1 = I0_7_1,
                                        closing_I0_7_2 = I0_7_2,
                                        closing_I0_7_3 = I0_7_3,
                                        closing_I0_7_4 = I0_7_4,
                                        closing_I0_7_5 = I0_7_5,
                                        closing_I0_7_99 = I0_7_99,
                                        closing_I0_8 = I0_8,
                                        closing_I1 = I1,
                                        
)

names(nigeria_data)

var_label(nigeria_data$id) <- "Serial number"
var_label(nigeria_data$country) <- "Please select your country"
var_label(nigeria_data$LGA) <- "Please select your LGA"
var_label(nigeria_data$urban) <- "Is this an urban area or rural?"
var_label(nigeria_data$sampling) <- "What sampling method was used to recruit this participant?"
var_label(nigeria_data$screening_1) <- "Can I speak with the person in this house who is most knowledgeable about household income and expenditure please?"
var_label(nigeria_data$age) <- "How old are you?"
var_label(nigeria_data$age_gr) <- "Age Groupings"
var_label(nigeria_data$gender) <- "What is the respondent's gender?"
var_label(nigeria_data$language) <- "What is the main language you speak at home?"
var_label(nigeria_data$born) <- "Were you born in ........?"
var_label(nigeria_data$state_or) <- "Which state do you originally come from / call home?"
var_label(nigeria_data$LGA_or) <- "Please select LGA of origin"
var_label(nigeria_data$ward_or) <- "Please select WARD of origin"
var_label(nigeria_data$ward_or_1) <- "Please select WARD of origin"
var_label(nigeria_data$live_now) <- "Where do you live now?"
var_label(nigeria_data$LGA_now) <- "Please select your LGA"
var_label(nigeria_data$ward_now) <- "Please select your WARD"
var_label(nigeria_data$ward_now_1) <- "Please select your WARD"
var_label(nigeria_data$year) <- "YEAR"
var_label(nigeria_data$month) <- "MONTH"
var_label(nigeria_data$reason_leave) <- "Did you leave your previous home for reasons out of your control (e.g. conflict, disasters, violence, etc.)?"
var_label(nigeria_data$urban_previous) <- "Would you say that where you used to live, in ............, is:"
var_label(nigeria_data$factor_to_move) <- "What was the main factor that forced you to leave your home and move to where you are now?"
var_label(nigeria_data$ftm_disaster) <- "Yes, disasters"
var_label(nigeria_data$ftm_conflict) <- "Yes, conflict, violence and/or insecurity"
var_label(nigeria_data$ftm_livelihoods) <- "Yes, lack of livelihoods and/or work"
var_label(nigeria_data$ftm_accessland) <- "Yes, lack of access to land"
var_label(nigeria_data$ftm_accessservices) <- "Yes, lack of access to services"
var_label(nigeria_data$ftm_foodinsecurity) <- "Yes, food insecurity"
var_label(nigeria_data$ftm_caterscarcity) <- "Yes, water scarcity"
var_label(nigeria_data$ftm_environmental_fc) <- "Yes, environmental factors and/or climate change"
var_label(nigeria_data$ftm_no) <- "No"
var_label(nigeria_data$ftm_multipletimes) <- "In the last three years have you been forced to move multiple times after leaving your home?"
var_label(nigeria_data$ftm_howmany) <- "In the last three years, how many times have you been forced to leave your home?"
var_label(nigeria_data$placebf_affecteddisasaster) <- "The place where I was staying was affected by disasters/at risk of disasters"
var_label(nigeria_data$placebf_affecteddinsecurity) <- "I continued to be at risk of violence and insecurity in the place where I was staying"
var_label(nigeria_data$placebf_notaffordable) <- "I could not afford to stay there"
var_label(nigeria_data$placebf_evicted) <- "I was evicted from the place where I was staying"
var_label(nigeria_data$placebf_notlivelihoods) <- "There were no livelihood opportunities in the area"
var_label(nigeria_data$placebf_other) <- "Other"
var_label(nigeria_data$placebf_pna) <- "Prefer not to answer"
var_label(nigeria_data$placebf_reasontomove) <- "You mentioned you were forced to leave your previous home for reasons out of your control (e.g. conflict, disasters, violence, etc.)?"
var_label(nigeria_data$idp_awareness) <- "Are you aware of people who have had to leave their homes to come to the ......... region for reasons out of their control (e.g. conflict, disasters, violence, etc.)?"

var_label(nigeria_data$idp_hosting) <- "Do you currently host any internally displaced persons? How many people are you currently hosting?"

var_label(nigeria_data$idphosting_sinceyr) <- "YEAR"
var_label(nigeria_data$idphosting_sincemonth) <- "MONTH"
var_label(nigeria_data$idphosting_howmeet) <- "How did you meet them?"
var_label(nigeria_data$schooling) <- "What is the highest level of education you have received?"
var_label(nigeria_data$numdep_05) <- "Number of children 0-5 Yrs.___"
var_label(nigeria_data$numdep_614) <- "Number of children 6- 14Yrs.____"
var_label(nigeria_data$numdep_1517) <- "Number of children 15-17 Yrs. ___"
var_label(nigeria_data$numdep1864) <- "Number of adults 18-64 Yrs. ____"
var_label(nigeria_data$numdep_65) <- "Number of elderly 65 or over Yrs. ____"

var_label(nigeria_data$wgq_seeing_0) <- "'Me, personally' :"
var_label(nigeria_data$wgq_seeing_1) <- "Another member of my household :"
var_label(nigeria_data$wgq_hearing_0) <- "'Me, personally' :"
var_label(nigeria_data$wgq_hearing_1) <- "Another member of my household :"
var_label(nigeria_data$wgq_walking_0) <- "'Me, personally' :"
var_label(nigeria_data$wgq_walking_1) <- "Another member of my household :"
var_label(nigeria_data$wgq_remembering_0) <- "'Me, personally' :"
var_label(nigeria_data$wgq_remembering_1) <- "Another member of my household :"
var_label(nigeria_data$wgq_selfcare_0) <- "'Me, personally' :"
var_label(nigeria_data$wgq_selfcare_1) <- "Another member of my household :"
var_label(nigeria_data$wgq_language_0) <- "'Me, personally' :"
var_label(nigeria_data$wgq_language_1) <- "Another member of my household :"
var_label(nigeria_data$housing_D1) <- "Before you were displaced, did your household:"
var_label(nigeria_data$housing_D1_2) <- "Did you have any documentation or agreement which shows that you owned your own home?"
var_label(nigeria_data$housing_D1_3) <- "Did you have any documentation or agreement which shows that you rented your own home?"
var_label(nigeria_data$housing_D2_1) <- "How much rent did you pay each month?"
var_label(nigeria_data$housing_D2_2) <- "How much money do you think your home was worth before you were displaced?"
var_label(nigeria_data$housing_D3) <- "Currently, does your household:"
var_label(nigeria_data$housing_D3_2) <- "Do you have any documentation or agreement which shows that you own your current home?"
var_label(nigeria_data$housing_D3_3) <- "Do you have any documentation or agreement which shows that you rent your current home?"
var_label(nigeria_data$housing_D4_1) <- "How much do you pay for rent each month?"
var_label(nigeria_data$housing_D4_2) <- "How much did you pay for your new home? This includes whether you bought it, or had to build it yourself"
var_label(nigeria_data$housing_D5) <- "How satisfied are you with your current housing conditions compared to your housing conditions before you were displaced?"
var_label(nigeria_data$housing_D6a_1) <- "Less security of tenure/higher risk of eviction"
var_label(nigeria_data$housing_D6a_2) <- "More expensive"
var_label(nigeria_data$housing_D6a_3) <- "Less physical security/located in a less safe area"
var_label(nigeria_data$housing_D6a_4) <- "Less privacy"
var_label(nigeria_data$housing_D6a_5) <- "Less protection against rain, cold or heat"
var_label(nigeria_data$housing_D6a_6) <- "More crowded"
var_label(nigeria_data$housing_D6a_7) <- "Less access to basic services (electricity, gas, water)"
var_label(nigeria_data$housing_D6a_8) <- "Less access to livelihoods"
var_label(nigeria_data$housing_D6a_9) <- "Poorer sanitary conditions"
var_label(nigeria_data$housing_D6a_10) <- "Less accessible for people with disabilities / limited mobility"
var_label(nigeria_data$housing_D6a_11) <- "More exposed to hazards / disaster risks"
var_label(nigeria_data$housing_D6a_12) <- "Less appropriate to their culture"
var_label(nigeria_data$housing_D6a_98) <- "Other"
var_label(nigeria_data$housing_D6a_99) <- "Prefer not to answer"
var_label(nigeria_data$housing_D6b_1) <- "Improved security of tenure / lower risk of eviction"
var_label(nigeria_data$housing_D6b_2) <- "More affordable"
var_label(nigeria_data$housing_D6b_3) <- "Improved security/located in a safer area"
var_label(nigeria_data$housing_D6b_4) <- "More privacy"
var_label(nigeria_data$housing_D6b_5) <- "Better protection against rain, cold or heat"
var_label(nigeria_data$housing_D6b_6) <- "Less crowded"
var_label(nigeria_data$housing_D6b_7) <- "Improved access to basic services (electricity, gas, water)"
var_label(nigeria_data$housing_D6b_8) <- "Improved access to livelihoods"
var_label(nigeria_data$housing_D6b_9) <- "Cleaner / Improved sanitary conditions"
var_label(nigeria_data$housing_D6b_10) <- "More accessible for people with disabilities / limited mobility"
var_label(nigeria_data$housing_D6b_11) <- "Less exposed to hazards / disaster risks"
var_label(nigeria_data$housing_D6b_12) <- "More appropriate to their culture"
var_label(nigeria_data$housing_D6b_98) <- "Other"
var_label(nigeria_data$housing_D6b_99) <- "Prefer not to answer"
var_label(nigeria_data$housing_D13) <- "Does your household:"
var_label(nigeria_data$housing_D13_2) <- "Do you have any documentation or agreement which shows that you own your current home?"
var_label(nigeria_data$housing_D13_3) <- "Do you have any documentation or agreement which shows that you rent your current home?"
var_label(nigeria_data$housing_D13a) <- "How much rent do you pay each month?"
var_label(nigeria_data$housing_D13b) <- "How much money do you think your home is worth?"
var_label(nigeria_data$housing_D14) <- "How satisfied are you with your current housing conditions compared to your housing conditions before people who had to leave their home for reasons out of their control arrived in ......?"
var_label(nigeria_data$housing_D15a_1) <- "Less security of tenure/higher risk of eviction"
var_label(nigeria_data$housing_D15a_2) <- "More expensive"
var_label(nigeria_data$housing_D15a_3) <- "Less physical security/located in a less safe area"
var_label(nigeria_data$housing_D15a_4) <- "Less privacy"
var_label(nigeria_data$housing_D15a_5) <- "Less protection against rain, cold or heat"
var_label(nigeria_data$housing_D15a_6) <- "More crowded"
var_label(nigeria_data$housing_D15a_7) <- "Less access to basic services (electricity, gas, water)"
var_label(nigeria_data$housing_D15a_8) <- "Less access to livelihoods"
var_label(nigeria_data$housing_D15a_9) <- "Poorer sanitary conditions"
var_label(nigeria_data$housing_D15a_10) <- "Less accessible for people with disabilities / limited mobility"
var_label(nigeria_data$housing_D15a_11) <- "More exposed to hazards / disaster risks"
var_label(nigeria_data$housing_D15a_12) <- "Less appropriate to their culture"
var_label(nigeria_data$housing_D15a_98) <- "Other"
var_label(nigeria_data$housing_D15a_99) <- "Prefer not to answer"
var_label(nigeria_data$housing_D15b_1) <- "Improved security of tenure / lower risk of eviction"
var_label(nigeria_data$housing_D15b_2) <- "More affordable"
var_label(nigeria_data$housing_D15b_3) <- "Improved security/located in a safer area"
var_label(nigeria_data$housing_D15b_4) <- "More privacy"
var_label(nigeria_data$housing_D15b_5) <- "Better protection against rain, cold or heat"
var_label(nigeria_data$housing_D15b_6) <- "Less crowded"
var_label(nigeria_data$housing_D15b_7) <- "Improved access to basic services (electricity, gas, water)"
var_label(nigeria_data$housing_D15b_8) <- "Improved access to livelihoods"
var_label(nigeria_data$housing_D15b_9) <- "Cleaner / Improved sanitary conditions"
var_label(nigeria_data$housing_D15b_10) <- "More accessible for people with disabilities / limited mobility"
var_label(nigeria_data$housing_D15b_11) <- "Less exposed to hazards / disaster risks"
var_label(nigeria_data$housing_D15b_12) <- "More appropriate to their culture"
var_label(nigeria_data$housing_D15b_98) <- "Other"
var_label(nigeria_data$housing_D15b_99) <- "Prefer not to answer"
var_label(nigeria_data$housing_D16) <- "Have you had to face any additional expenses for housing since people who had to leave their home for reasons out of their control arrived in .......?"
var_label(nigeria_data$housing_D17_1_1) <- "Increase in rent"
var_label(nigeria_data$housing_D17_1_2) <- "Increase in utility bills (electricity, gas, water...)"
var_label(nigeria_data$housing_D17_1_3) <- "Buying additional food, supplies or furniture for IDPs (if sharing a home with them)"
var_label(nigeria_data$housing_D17_1_4) <- "Increase in the price of food"
var_label(nigeria_data$housing_D17_1_5) <- "Increase in the price of goods or furniture"
var_label(nigeria_data$housing_D17_1_6) <- "Other"
var_label(nigeria_data$housing_D17_1_99) <- "Prefer not to answer"
var_label(nigeria_data$housing_D18_1_1) <- "On average, per: Week"
var_label(nigeria_data$housing_D18_1_2) <- "On average, per: Month"
var_label(nigeria_data$housing_D18_1_3) <- "On average, per: Year"
var_label(nigeria_data$security_E1) <- "Compared to before you left your home, do you now feel:"
var_label(nigeria_data$security_E2a_1) <- "Poorer housing conditions"
var_label(nigeria_data$security_E2a_2) <- "Less presence of security personnel"
var_label(nigeria_data$security_E2a_3) <- "Further away from family/community members"
var_label(nigeria_data$security_E2a_4) <- "Further away from basic services"
var_label(nigeria_data$security_E2a_5) <- "Higher risk of disasters"
var_label(nigeria_data$security_E2a_6) <- "Higher risk of physical attack"
var_label(nigeria_data$security_E2a_7) <- "Higher risk of discrimination/harassment"
var_label(nigeria_data$security_E2a_8) <- "Higher crime rates"
var_label(nigeria_data$security_E2a_99) <- "Prefer not to answer"
var_label(nigeria_data$security_E2b_1) <- "Better housing conditions"
var_label(nigeria_data$security_E2b_2) <- "Greater presence of security personnel"
var_label(nigeria_data$security_E2b_3) <- "Closer to family/community members"
var_label(nigeria_data$security_E2b_4) <- "Closer to basic services"
var_label(nigeria_data$security_E2b_5) <- "Lower risk of disasters"
var_label(nigeria_data$security_E2b_6) <- "Lower risk of physical attack"
var_label(nigeria_data$security_E2b_7) <- "Lower risk of discrimination/harassment"
var_label(nigeria_data$security_E2b_8) <- "Lower crime rates"
var_label(nigeria_data$security_E2b_99) <- "Prefer not to answer"
var_label(nigeria_data$security_E3) <- "How would you describe the relationship between people who arrived in the area after having had to leave their home and the people who have lived in this area for a long time (host community)?"
var_label(nigeria_data$security_E4_1) <- "Yes, Competition over agricultural land"
var_label(nigeria_data$security_E4_2) <- "Yes, Competition over grazing land"
var_label(nigeria_data$security_E4_3) <- "Yes, Competition over water"
var_label(nigeria_data$security_E4_4) <- "Yes, Competition over firewood"
var_label(nigeria_data$security_E4_5) <- "Yes, Theft of assets"
var_label(nigeria_data$security_E4_6) <- "Yes, Competition over assistance from UN, NGOs, Government, etc"
var_label(nigeria_data$security_E4_7) <- "Yes, Competition over health facilities"
var_label(nigeria_data$security_E4_8) <- "Yes, Competition over educational facilities"
var_label(nigeria_data$security_E4_9) <- "Yes, Competition over shelter"
var_label(nigeria_data$security_E4_10) <- "Yes, Competition over fishing areas"
var_label(nigeria_data$security_E4_11) <- "No"
var_label(nigeria_data$security_E4_99) <- "Prefer not to answer"
var_label(nigeria_data$security_E7) <- "Please describe how the disputes or tensions were resolved:"
var_label(nigeria_data$security_E2a_0) <- "Have you or a household member been exposed to any form of violence or insecurity over the past 12 months?"
var_label(nigeria_data$security_E2a.1) <- "Have these reasons affected your household’s ability to produce and/or access food?"
var_label(nigeria_data$security_E2a_1_SPECIFY) <- "Please specify how these reasons have affected your household's ability to produce and/or access food:"
var_label(nigeria_data$security_E15) <- "Compared to before people who had to leave their home for reasons out of their control arrived in the area, do you feel now:"
var_label(nigeria_data$security_E16a_1) <- "Poorer housing conditions"
var_label(nigeria_data$security_E16a_2) <- "Less presence of security personnel"
var_label(nigeria_data$security_E16a_3) <- "Further away from family/community members"
var_label(nigeria_data$security_E16a_4) <- "Further away from basic services"
var_label(nigeria_data$security_E16a_5) <- "Higher risk of disasters"
var_label(nigeria_data$security_E16a_6) <- "Higher risk of physical attack"
var_label(nigeria_data$security_E16a_7) <- "Higher risk of discrimination/harassment"
var_label(nigeria_data$security_E16a_8) <- "Higher crime rates"
var_label(nigeria_data$security_E16a_99) <- "Prefer not to answer"
var_label(nigeria_data$security_E16b_1) <- "Better housing conditions"
var_label(nigeria_data$security_E16b_2) <- "Greater presence of security personnel"
var_label(nigeria_data$security_E16b_3) <- "Closer to family/community members"
var_label(nigeria_data$security_E16b_4) <- "Closer to basic services"
var_label(nigeria_data$security_E16b_5) <- "Lower risk of disasters"
var_label(nigeria_data$security_E16b_6) <- "Lower risk of physical attack"
var_label(nigeria_data$security_E16b_7) <- "Lower risk of discrimination/harassment"
var_label(nigeria_data$security_E16b_8) <- "Lower crime rates"
var_label(nigeria_data$security_E16b_99) <- "Prefer not to answer"
var_label(nigeria_data$security_E3_H) <- "How would you describe the relationship between people who arrived in the area after having had to leave their home and the people who have lived in this area for a long time (host community)?"
var_label(nigeria_data$security_E4_H_1) <- "Yes, Competition over agricultural land"
var_label(nigeria_data$security_E4_H_2) <- "Yes, Competition over grazing land"
var_label(nigeria_data$security_E4_H_3) <- "Yes, Competition over water"
var_label(nigeria_data$security_E4_H_4) <- "Yes, Competition over firewood"
var_label(nigeria_data$security_E4_H_5) <- "Yes, Theft of assets"
var_label(nigeria_data$security_E4_H_6) <- "Yes, Competition over assistance from UN, NGOs, Government, etc"
var_label(nigeria_data$security_E4_H_7) <- "Yes, Competition over health facilities"
var_label(nigeria_data$security_E4_H_8) <- "Yes, Competition over educational facilities"
var_label(nigeria_data$security_E4_H_9) <- "Yes, Competition over shelter"
var_label(nigeria_data$security_E4_H_10) <- "Yes, Competition over fishing areas"
var_label(nigeria_data$security_E4_H_11) <- "No"
var_label(nigeria_data$security_E4_H_99) <- "Prefer not to answer"
var_label(nigeria_data$security_E7_H) <- "Please describe how the disputes or tensions were resolved:"
var_label(nigeria_data$security_E16a_0) <- "Have you or a household member been exposed to any form of violence or insecurity over the past 12 months?"
var_label(nigeria_data$security_E16a.1) <- "Have these reasons affected your household’s ability to produce and/or access food?"
var_label(nigeria_data$security_E16a_1_SPECIFY) <- "Please specify how these reasons have affected your household's ability to produce and/or access food:"
var_label(nigeria_data$education_F1a) <- "Earlier you indicated that you have ......... children who are aged between 6 and 14. Is this correct?"
var_label(nigeria_data$education_F1b) <- "How many children do you have aged between 6 and 14?"
var_label(nigeria_data$education_idp_MRK_IDP_CHILDREN) <- "MARKER FOR CHILDREN BETWEEN 6 AND 14 FOR IDP RESPONDENTS"
var_label(nigeria_data$education_idp_F1c_1) <- "Child 1 :"
var_label(nigeria_data$education_idp_F1c_2) <- "Child 2 :"
var_label(nigeria_data$education_idp_F1c_3) <- "Child 3 :"
var_label(nigeria_data$education_idp_F1c_4) <- "Child 4 :"
var_label(nigeria_data$education_idp_F1c_5) <- "Child 5 :"
var_label(nigeria_data$education_idp_F3_1) <- "Boy : How old is your child?"
var_label(nigeria_data$education_idp_F3_2) <- "Girl : How old is your child?"
var_label(nigeria_data$education_idp_F4_1) <- "Boy : Did your child go to school before you left your home?"
var_label(nigeria_data$education_idp_F4_2) <- "Girl : Did your child go to school before you left your home?"
var_label(nigeria_data$education_idp_F5_1) <- "Boy : What type of school did your child attend?"
var_label(nigeria_data$education_idp_F5_2) <- "Girl : What type of school did your child attend?"
var_label(nigeria_data$education_idp_F6_1_1) <- "Child is married and has a family"
var_label(nigeria_data$education_idp_F6_1_2) <- "Child needed to work"
var_label(nigeria_data$education_idp_F6_1_3) <- "Child needed to help at home"
var_label(nigeria_data$education_idp_F6_1_4) <- "Child was not of a school age"
var_label(nigeria_data$education_idp_F6_1_5) <- "School was too far from home"
var_label(nigeria_data$education_idp_F6_1_6) <- "Costs connected with going to school are too high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_idp_F6_1_7) <- "Child is sick"
var_label(nigeria_data$education_idp_F6_1_8) <- "Child refused to go to school"
var_label(nigeria_data$education_idp_F6_1_9) <- "Finished desired level of schooling"
var_label(nigeria_data$education_idp_F6_1_10) <- "Language barriers"
var_label(nigeria_data$education_idp_F6_1_11) <- "Discrimination"
var_label(nigeria_data$education_idp_F6_1_12) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_idp_F6_1_13) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_idp_F6_1_14) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_idp_F6_1_98) <- "Other"
var_label(nigeria_data$education_idp_F6_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F6_2_1) <- "Child is married and has a family"
var_label(nigeria_data$education_idp_F6_2_2) <- "Child needed to work"
var_label(nigeria_data$education_idp_F6_2_3) <- "Child needed to help at home"
var_label(nigeria_data$education_idp_F6_2_4) <- "Child was not of a school age"
var_label(nigeria_data$education_idp_F6_2_5) <- "School was too far from home"
var_label(nigeria_data$education_idp_F6_2_6) <- "Costs connected with going to school are too high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_idp_F6_2_7) <- "Child is sick"
var_label(nigeria_data$education_idp_F6_2_8) <- "Child refused to go to school"
var_label(nigeria_data$education_idp_F6_2_9) <- "Finished desired level of schooling"
var_label(nigeria_data$education_idp_F6_2_10) <- "Language barriers"
var_label(nigeria_data$education_idp_F6_2_11) <- "Discrimination"
var_label(nigeria_data$education_idp_F6_2_12) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_idp_F6_2_13) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_idp_F6_2_14) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_idp_F6_2_98) <- "Other"
var_label(nigeria_data$education_idp_F6_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F7_1) <- "Boy : Did you have to pay any expenses/costs for your child’s education throughout the year?"
var_label(nigeria_data$education_idp_F7_2) <- "Girl : Did you have to pay any expenses/costs for your child’s education throughout the year?"
var_label(nigeria_data$education_idp_F8_1_1) <- "Boy : On average, per: Week"
var_label(nigeria_data$education_idp_F8_1_2) <- "Girl : On average, per: Week"
var_label(nigeria_data$education_idp_F8_2_1) <- "Boy : On average, per: Month"
var_label(nigeria_data$education_idp_F8_2_2) <- "Girl : On average, per: Month"
var_label(nigeria_data$education_idp_F8_3_1) <- "Boy : On average, per: Year"
var_label(nigeria_data$education_idp_F8_3_2) <- "Girl : On average, per: Year"
var_label(nigeria_data$education_idp_F8a_1_1) <- "Tuition costs"
var_label(nigeria_data$education_idp_F8a_1_2) <- "Uniform"
var_label(nigeria_data$education_idp_F8a_1_3) <- "Additional classes"
var_label(nigeria_data$education_idp_F8a_1_4) <- "School materials"
var_label(nigeria_data$education_idp_F8a_1_5) <- "Transportation"
var_label(nigeria_data$education_idp_F8a_1_6) <- "Meals"
var_label(nigeria_data$education_idp_F8a_1_98) <- "Other"
var_label(nigeria_data$education_idp_F8a_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F8a_2_1) <- "Tuition costs"
var_label(nigeria_data$education_idp_F8a_2_2) <- "Uniform"
var_label(nigeria_data$education_idp_F8a_2_3) <- "Additional classes"
var_label(nigeria_data$education_idp_F8a_2_4) <- "School materials"
var_label(nigeria_data$education_idp_F8a_2_5) <- "Transportation"
var_label(nigeria_data$education_idp_F8a_2_6) <- "Meals"
var_label(nigeria_data$education_idp_F8a_2_98) <- "Other"
var_label(nigeria_data$education_idp_F8a_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F8a_1_98Others) <- "Boy : Other Specify"
var_label(nigeria_data$education_idp_F8a_2_98Others) <- "Girl : Other Specify"
var_label(nigeria_data$education_idp_F81_1) <- "Boy : Did your child have any breaks in their schooling due to displacement?"
var_label(nigeria_data$education_idp_F81_2) <- "Girl : Did your child have any breaks in their schooling due to displacement?"
var_label(nigeria_data$education_idp_F81a_1) <- "Boy : For how long did your child have a break due to displacement?"
var_label(nigeria_data$education_idp_F81a_2) <- "Girl : For how long did your child have a break due to displacement?"
var_label(nigeria_data$education_idp_F9_1) <- "Boy : Does your child go to school now?"
var_label(nigeria_data$education_idp_F9_2) <- "Girl : Does your child go to school now?"
var_label(nigeria_data$education_idp_F10_1_1) <- "Child is married and has a family"
var_label(nigeria_data$education_idp_F10_1_2) <- "Child needs to work"
var_label(nigeria_data$education_idp_F10_1_3) <- "Child needs to help at home"
var_label(nigeria_data$education_idp_F10_1_4) <- "School is too far from home"
var_label(nigeria_data$education_idp_F10_1_5) <- "Costs connected with going to school are too high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_idp_F10_1_6) <- "Child is sick or disabled"
var_label(nigeria_data$education_idp_F10_1_7) <- "Child refused to go to school"
var_label(nigeria_data$education_idp_F10_1_8) <- "Finished desired level of schooling"
var_label(nigeria_data$education_idp_F10_1_9) <- "Language barriers"
var_label(nigeria_data$education_idp_F10_1_10) <- "Discrimination"
var_label(nigeria_data$education_idp_F10_1_11) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_idp_F10_1_12) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_idp_F10_1_13) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_idp_F10_1_98) <- "Other"
var_label(nigeria_data$education_idp_F10_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F10_2_1) <- "Child is married and has a family"
var_label(nigeria_data$education_idp_F10_2_2) <- "Child needs to work"
var_label(nigeria_data$education_idp_F10_2_3) <- "Child needs to help at home"
var_label(nigeria_data$education_idp_F10_2_4) <- "School is too far from home"
var_label(nigeria_data$education_idp_F10_2_5) <- "Costs connected with going to school are too high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_idp_F10_2_6) <- "Child is sick or disabled"
var_label(nigeria_data$education_idp_F10_2_7) <- "Child refused to go to school"
var_label(nigeria_data$education_idp_F10_2_8) <- "Finished desired level of schooling"
var_label(nigeria_data$education_idp_F10_2_9) <- "Language barriers"
var_label(nigeria_data$education_idp_F10_2_10) <- "Discrimination"
var_label(nigeria_data$education_idp_F10_2_11) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_idp_F10_2_12) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_idp_F10_2_13) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_idp_F10_2_98) <- "Other"
var_label(nigeria_data$education_idp_F10_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F11_1) <- "Boy : Do you have to pay any expenses/costs for your child’s education to go to school throughout the year?"
var_label(nigeria_data$education_idp_F11_2) <- "Girl : Do you have to pay any expenses/costs for your child’s education to go to school throughout the year?"
var_label(nigeria_data$education_idp_F12_1) <- "Boy : On average, per:"
var_label(nigeria_data$education_idp_F12_2) <- "Girl : On average, per:"
var_label(nigeria_data$education_idp_F12_1_1) <- "Boy : On average, per: Week"
var_label(nigeria_data$education_idp_F12_1_2) <- "Girl : On average, per: Week"
var_label(nigeria_data$education_idp_F12_2_1) <- "Boy : On average, per: Month"
var_label(nigeria_data$education_idp_F12_2_2) <- "Girl : On average, per: Month"
var_label(nigeria_data$education_idp_F12_3_1) <- "Boy : On average, per: Year"
var_label(nigeria_data$education_idp_F12_3_2) <- "Girl : On average, per: Year"
var_label(nigeria_data$education_idp_F12a_1_1) <- "School fees"
var_label(nigeria_data$education_idp_F12a_1_2) <- "Uniform"
var_label(nigeria_data$education_idp_F12a_1_3) <- "Additional classes"
var_label(nigeria_data$education_idp_F12a_1_4) <- "School materials"
var_label(nigeria_data$education_idp_F12a_1_5) <- "Transportation"
var_label(nigeria_data$education_idp_F12a_1_6) <- "Meals"
var_label(nigeria_data$education_idp_F12a_1_98) <- "Other (please specify)"
var_label(nigeria_data$education_idp_F12a_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F12a_2_1) <- "School fees"
var_label(nigeria_data$education_idp_F12a_2_2) <- "Uniform"
var_label(nigeria_data$education_idp_F12a_2_3) <- "Additional classes"
var_label(nigeria_data$education_idp_F12a_2_4) <- "School materials"
var_label(nigeria_data$education_idp_F12a_2_5) <- "Transportation"
var_label(nigeria_data$education_idp_F12a_2_6) <- "Meals"
var_label(nigeria_data$education_idp_F12a_2_98) <- "Other (please specify)"
var_label(nigeria_data$education_idp_F12a_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F12a_1_98Others) <- "Boy : Other Specify"
var_label(nigeria_data$education_idp_F12a_2_98Others) <- "Girl : Other Specify"
var_label(nigeria_data$education_idp_F12b_1_1) <- "Yes, from the government"
var_label(nigeria_data$education_idp_F12b_1_2) <- "Yes, from UN agencies"
var_label(nigeria_data$education_idp_F12b_1_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$education_idp_F12b_1_4) <- "Yes, from other source"
var_label(nigeria_data$education_idp_F12b_1_98) <- "No support"
var_label(nigeria_data$education_idp_F12b_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F12b_2_1) <- "Yes, from the government"
var_label(nigeria_data$education_idp_F12b_2_2) <- "Yes, from UN agencies"
var_label(nigeria_data$education_idp_F12b_2_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$education_idp_F12b_2_4) <- "Yes, from other source"
var_label(nigeria_data$education_idp_F12b_2_98) <- "No support"
var_label(nigeria_data$education_idp_F12b_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F13_1) <- "Boy : Comparing your child's education now with before you had to leave your home, are you:"
var_label(nigeria_data$education_idp_F13_2) <- "Girl : Comparing your child's education now with before you had to leave your home, are you:"
var_label(nigeria_data$education_idp_F14a_1_1) <- "Overcrowding in the classroom"
var_label(nigeria_data$education_idp_F14a_1_2) <- "Lower quality education"
var_label(nigeria_data$education_idp_F14a_1_3) <- "Child fears or suffers from marginalisation/discrimination in school"
var_label(nigeria_data$education_idp_F14a_1_4) <- "Classes are not in my child's mother tongue"
var_label(nigeria_data$education_idp_F14a_1_5) <- "School is further away than in my home area"
var_label(nigeria_data$education_idp_F14a_1_6) <- "School is not accessible by public/affordable method of transport"
var_label(nigeria_data$education_idp_F14a_1_7) <- "Education is too expensive"
var_label(nigeria_data$education_idp_F14a_1_8) <- "Schools is short-staffed"
var_label(nigeria_data$education_idp_F14a_1_9) <- "School does not provide meals to students"
var_label(nigeria_data$education_idp_F14a_1_10) <- "Other"
var_label(nigeria_data$education_idp_F14a_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F14a_2_1) <- "Overcrowding in the classroom"
var_label(nigeria_data$education_idp_F14a_2_2) <- "Lower quality education"
var_label(nigeria_data$education_idp_F14a_2_3) <- "Child fears or suffers from marginalisation/discrimination in school"
var_label(nigeria_data$education_idp_F14a_2_4) <- "Classes are not in my child's mother tongue"
var_label(nigeria_data$education_idp_F14a_2_5) <- "School is further away than in my home area"
var_label(nigeria_data$education_idp_F14a_2_6) <- "School is not accessible by public/affordable method of transport"
var_label(nigeria_data$education_idp_F14a_2_7) <- "Education is too expensive"
var_label(nigeria_data$education_idp_F14a_2_8) <- "Schools is short-staffed"
var_label(nigeria_data$education_idp_F14a_2_9) <- "School does not provide meals to students"
var_label(nigeria_data$education_idp_F14a_2_10) <- "Other"
var_label(nigeria_data$education_idp_F14a_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F14b_1_1) <- "Classes are less crowded"
var_label(nigeria_data$education_idp_F14b_1_2) <- "Higher quality education than in home area"
var_label(nigeria_data$education_idp_F14b_1_3) <- "Child no longer fears or suffers from discrimination/marginalisation"
var_label(nigeria_data$education_idp_F14b_1_4) <- "Classes are in my child's mother tongue"
var_label(nigeria_data$education_idp_F14b_1_5) <- "School is closer than in my home area"
var_label(nigeria_data$education_idp_F14b_1_6) <- "School is accessible by public/affordable method of transport"
var_label(nigeria_data$education_idp_F14b_1_7) <- "Education is less expensive"
var_label(nigeria_data$education_idp_F14b_1_8) <- "School is not short-staffed"
var_label(nigeria_data$education_idp_F14b_1_9) <- "School provides meals to students"
var_label(nigeria_data$education_idp_F14b_1_10) <- "Other"
var_label(nigeria_data$education_idp_F14b_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F14b_2_1) <- "Classes are less crowded"
var_label(nigeria_data$education_idp_F14b_2_2) <- "Higher quality education than in home area"
var_label(nigeria_data$education_idp_F14b_2_3) <- "Child no longer fears or suffers from discrimination/marginalisation"
var_label(nigeria_data$education_idp_F14b_2_4) <- "Classes are in my child's mother tongue"
var_label(nigeria_data$education_idp_F14b_2_5) <- "School is closer than in my home area"
var_label(nigeria_data$education_idp_F14b_2_6) <- "School is accessible by public/affordable method of transport"
var_label(nigeria_data$education_idp_F14b_2_7) <- "Education is less expensive"
var_label(nigeria_data$education_idp_F14b_2_8) <- "School is not short-staffed"
var_label(nigeria_data$education_idp_F14b_2_9) <- "School provides meals to students"
var_label(nigeria_data$education_idp_F14b_2_10) <- "Other"
var_label(nigeria_data$education_idp_F14b_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_idp_F25a) <- "Earlier you indicated that you have ....... children who are aged between 6 and 14. Is this correct?"
var_label(nigeria_data$education_idp_F25b) <- "How many children do you have aged between 6 and 14?"
var_label(nigeria_data$education_host_MRK_HOST_CHILDREN) <- "MARKER FOR CHILDREN BETWEEN 6 AND 14 FOR HOST RESPONDENTS"
var_label(nigeria_data$education_host_F25c_1) <- "Child 1 :"
var_label(nigeria_data$education_host_F25c_2) <- "Child 2 :"
var_label(nigeria_data$education_host_F25c_3) <- "Child 3 :"
var_label(nigeria_data$education_host_F25c_4) <- "Child 4 :"
var_label(nigeria_data$education_host_F25c_5) <- "Child 5 :"
var_label(nigeria_data$education_host_F26_1) <- "Boy : How old is your child?"
var_label(nigeria_data$education_host_F26_2) <- "Girl : How old is your child?"
var_label(nigeria_data$education_host_F15_1) <- "Boy : Did your child go to school before people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F15_2) <- "Girl : Did your child go to school before people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F15_2_1) <- "Boy : What type of school did your child attend?"
var_label(nigeria_data$education_host_F15_2_2) <- "Girl : What type of school did your child attend?"
var_label(nigeria_data$education_host_F16_1_1) <- "Child is married and has a family"
var_label(nigeria_data$education_host_F16_1_2) <- "Child needed to work"
var_label(nigeria_data$education_host_F16_1_3) <- "Child needed to help at home"
var_label(nigeria_data$education_host_F16_1_4) <- "Child was not of school age"
var_label(nigeria_data$education_host_F16_1_5) <- "School was too far from home"
var_label(nigeria_data$education_host_F16_1_6) <- "Costs connected with going to school  are too  high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_host_F16_1_7) <- "Child is sick or disabled"
var_label(nigeria_data$education_host_F16_1_8) <- "Child refused to go to school"
var_label(nigeria_data$education_host_F16_1_9) <- "Finished school"
var_label(nigeria_data$education_host_F16_1_10) <- "Language barriers"
var_label(nigeria_data$education_host_F16_1_11) <- "Discrimination"
var_label(nigeria_data$education_host_F16_1_12) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_host_F16_1_13) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_host_F16_1_14) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_host_F16_1_98) <- "Other"
var_label(nigeria_data$education_host_F16_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F16_2_1) <- "Child is married and has a family"
var_label(nigeria_data$education_host_F16_2_2) <- "Child needed to work"
var_label(nigeria_data$education_host_F16_2_3) <- "Child needed to help at home"
var_label(nigeria_data$education_host_F16_2_4) <- "Child was not of school age"
var_label(nigeria_data$education_host_F16_2_5) <- "School was too far from home"
var_label(nigeria_data$education_host_F16_2_6) <- "Costs connected with going to school  are too  high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_host_F16_2_7) <- "Child is sick or disabled"
var_label(nigeria_data$education_host_F16_2_8) <- "Child refused to go to school"
var_label(nigeria_data$education_host_F16_2_9) <- "Finished school"
var_label(nigeria_data$education_host_F16_2_10) <- "Language barriers"
var_label(nigeria_data$education_host_F16_2_11) <- "Discrimination"
var_label(nigeria_data$education_host_F16_2_12) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_host_F16_2_13) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_host_F16_2_14) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_host_F16_2_98) <- "Other"
var_label(nigeria_data$education_host_F16_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F17_1) <- "Boy : Did you have to pay any expenses/costs for your child's education before people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F17_2) <- "Girl : Did you have to pay any expenses/costs for your child's education before people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F18_1_1) <- "Boy : On average, per:"
var_label(nigeria_data$education_host_F18_1_2) <- "Girl : On average, per:"
var_label(nigeria_data$education_host_F18_2_1) <- "Boy : On average, per:"
var_label(nigeria_data$education_host_F18_2_2) <- "Girl : On average, per:"
var_label(nigeria_data$education_host_F18_3_1) <- "Boy : On average, per:"
var_label(nigeria_data$education_host_F18_3_2) <- "Girl : On average, per:"
var_label(nigeria_data$education_host_F18a_1_1) <- "School fees"
var_label(nigeria_data$education_host_F18a_1_2) <- "Uniform"
var_label(nigeria_data$education_host_F18a_1_3) <- "Additional classes"
var_label(nigeria_data$education_host_F18a_1_4) <- "School materials"
var_label(nigeria_data$education_host_F18a_1_5) <- "Transportation"
var_label(nigeria_data$education_host_F18a_1_6) <- "Meals"
var_label(nigeria_data$education_host_F18a_1_98) <- "Other (please specify)"
var_label(nigeria_data$education_host_F18a_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F18a_2_1) <- "School fees"
var_label(nigeria_data$education_host_F18a_2_2) <- "Uniform"
var_label(nigeria_data$education_host_F18a_2_3) <- "Additional classes"
var_label(nigeria_data$education_host_F18a_2_4) <- "School materials"
var_label(nigeria_data$education_host_F18a_2_5) <- "Transportation"
var_label(nigeria_data$education_host_F18a_2_6) <- "Meals"
var_label(nigeria_data$education_host_F18a_2_98) <- "Other (please specify)"
var_label(nigeria_data$education_host_F18a_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F18a_1_98Other) <- "Boy : Other Specify"
var_label(nigeria_data$education_host_F18a_2_98Other) <- "Girl : Other Specify"
var_label(nigeria_data$education_host_F181_1) <- "Boy : Did your child have any breaks in their schooling when people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F181_2) <- "Girl : Did your child have any breaks in their schooling when people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F181a_1) <- "Boy : For how long did your child have a break when people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F181a_2) <- "Girl : For how long did your child have a break when people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$education_host_F19_1) <- "Boy : Does your child go to school now?"
var_label(nigeria_data$education_host_F19_2) <- "Girl : Does your child go to school now?"
var_label(nigeria_data$education_host_F20_1_1) <- "Child is married and has a family"
var_label(nigeria_data$education_host_F20_1_2) <- "Child needs to work"
var_label(nigeria_data$education_host_F20_1_3) <- "Child needs to help at home"
var_label(nigeria_data$education_host_F20_1_4) <- "School is too far from home"
var_label(nigeria_data$education_host_F20_1_5) <- "Costs connected with going to school are too high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_host_F20_1_6) <- "Child is sick or disabled"
var_label(nigeria_data$education_host_F20_1_7) <- "Child refused to go to school"
var_label(nigeria_data$education_host_F20_1_8) <- "Finished school"
var_label(nigeria_data$education_host_F20_1_9) <- "Language barriers"
var_label(nigeria_data$education_host_F20_1_10) <- "Discrimination"
var_label(nigeria_data$education_host_F20_1_11) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_host_F20_1_12) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_host_F20_1_13) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_host_F20_1_98) <- "Other"
var_label(nigeria_data$education_host_F20_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F20_2_1) <- "Child is married and has a family"
var_label(nigeria_data$education_host_F20_2_2) <- "Child needs to work"
var_label(nigeria_data$education_host_F20_2_3) <- "Child needs to help at home"
var_label(nigeria_data$education_host_F20_2_4) <- "School is too far from home"
var_label(nigeria_data$education_host_F20_2_5) <- "Costs connected with going to school are too high (uniforms, meals, travel, etc)"
var_label(nigeria_data$education_host_F20_2_6) <- "Child is sick or disabled"
var_label(nigeria_data$education_host_F20_2_7) <- "Child refused to go to school"
var_label(nigeria_data$education_host_F20_2_8) <- "Finished school"
var_label(nigeria_data$education_host_F20_2_9) <- "Language barriers"
var_label(nigeria_data$education_host_F20_2_10) <- "Discrimination"
var_label(nigeria_data$education_host_F20_2_11) <- "Lack of places in educational institutions"
var_label(nigeria_data$education_host_F20_2_12) <- "School is inaccessible due to disability"
var_label(nigeria_data$education_host_F20_2_13) <- "School infrastructure is inaccessible (e.g. badly damaged after a conflict, or a flood)."
var_label(nigeria_data$education_host_F20_2_98) <- "Other"
var_label(nigeria_data$education_host_F20_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F27_1) <- "Boy : Do you have any expenses/costs for your child’s education currently?"
var_label(nigeria_data$education_host_F27_2) <- "Girl : Do you have any expenses/costs for your child’s education currently?"
var_label(nigeria_data$education_host_F21_1_1) <- "Boy : On average, per:"
var_label(nigeria_data$education_host_F21_1_2) <- "Girl : On average, per:"
var_label(nigeria_data$education_host_F21_2_1) <- "Boy : On average, per:"
var_label(nigeria_data$education_host_F21_2_2) <- "Girl : On average, per:"
var_label(nigeria_data$education_host_F21_3_1) <- "Boy : On average, per:"
var_label(nigeria_data$education_host_F21_3_2) <- "Girl : On average, per:"
var_label(nigeria_data$education_host_F21a_1_1) <- "Tuition costs"
var_label(nigeria_data$education_host_F21a_1_2) <- "Uniform"
var_label(nigeria_data$education_host_F21a_1_3) <- "Additional classes"
var_label(nigeria_data$education_host_F21a_1_4) <- "School materials"
var_label(nigeria_data$education_host_F21a_1_5) <- "Transportation"
var_label(nigeria_data$education_host_F21a_1_6) <- "Meals"
var_label(nigeria_data$education_host_F21a_1_98) <- "Other (please specify)"
var_label(nigeria_data$education_host_F21a_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F21a_2_1) <- "Tuition costs"
var_label(nigeria_data$education_host_F21a_2_2) <- "Uniform"
var_label(nigeria_data$education_host_F21a_2_3) <- "Additional classes"
var_label(nigeria_data$education_host_F21a_2_4) <- "School materials"
var_label(nigeria_data$education_host_F21a_2_5) <- "Transportation"
var_label(nigeria_data$education_host_F21a_2_6) <- "Meals"
var_label(nigeria_data$education_host_F21a_2_98) <- "Other (please specify)"
var_label(nigeria_data$education_host_F21a_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F21a_1_98Others) <- "Boy : Other Specify"
var_label(nigeria_data$education_host_F21a_2_98Others) <- "Girl : Other Specify"
var_label(nigeria_data$education_host_F21b_1_1) <- "Yes, from the government"
var_label(nigeria_data$education_host_F21b_1_2) <- "Yes, from UN agencies"
var_label(nigeria_data$education_host_F21b_1_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$education_host_F21b_1_4) <- "Yes, from other source"
var_label(nigeria_data$education_host_F21b_1_98) <- "No support"
var_label(nigeria_data$education_host_F21b_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F21b_2_1) <- "Yes, from the government"
var_label(nigeria_data$education_host_F21b_2_2) <- "Yes, from UN agencies"
var_label(nigeria_data$education_host_F21b_2_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$education_host_F21b_2_4) <- "Yes, from other source"
var_label(nigeria_data$education_host_F21b_2_98) <- "No support"
var_label(nigeria_data$education_host_F21b_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F22_1) <- "Boy : Comparing your child's education now with before people who had to leave their home for reasons out of their control arrived in this area, are you:"
var_label(nigeria_data$education_host_F22_2) <- "Girl : Comparing your child's education now with before people who had to leave their home for reasons out of their control arrived in this area, are you:"
var_label(nigeria_data$education_host_F23a_1_1) <- "Overcrowding in the classroom"
var_label(nigeria_data$education_host_F23a_1_2) <- "Lower quality education"
var_label(nigeria_data$education_host_F23a_1_3) <- "Child fears or suffers from marginalisation/discrimination in school"
var_label(nigeria_data$education_host_F23a_1_4) <- "Classes are not in my child's mother tongue"
var_label(nigeria_data$education_host_F23a_1_5) <- "School is further away than in my home area"
var_label(nigeria_data$education_host_F23a_1_6) <- "School is not accessible by public/affordable method of transport"
var_label(nigeria_data$education_host_F23a_1_7) <- "Education is too expensive"
var_label(nigeria_data$education_host_F23a_1_8) <- "Schools is short-staffed"
var_label(nigeria_data$education_host_F23a_1_9) <- "School does not provide meals to students"
var_label(nigeria_data$education_host_F23a_1_10) <- "Other"
var_label(nigeria_data$education_host_F23a_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F23a_2_1) <- "Overcrowding in the classroom"
var_label(nigeria_data$education_host_F23a_2_2) <- "Lower quality education"
var_label(nigeria_data$education_host_F23a_2_3) <- "Child fears or suffers from marginalisation/discrimination in school"
var_label(nigeria_data$education_host_F23a_2_4) <- "Classes are not in my child's mother tongue"
var_label(nigeria_data$education_host_F23a_2_5) <- "School is further away than in my home area"
var_label(nigeria_data$education_host_F23a_2_6) <- "School is not accessible by public/affordable method of transport"
var_label(nigeria_data$education_host_F23a_2_7) <- "Education is too expensive"
var_label(nigeria_data$education_host_F23a_2_8) <- "Schools is short-staffed"
var_label(nigeria_data$education_host_F23a_2_9) <- "School does not provide meals to students"
var_label(nigeria_data$education_host_F23a_2_10) <- "Other"
var_label(nigeria_data$education_host_F23a_2_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F23b_1_1) <- "Classes are less crowded"
var_label(nigeria_data$education_host_F23b_1_2) <- "Higher quality education than in home area"
var_label(nigeria_data$education_host_F23b_1_3) <- "Child no longer fears or suffers from discrimination/marginalisation"
var_label(nigeria_data$education_host_F23b_1_4) <- "Classes are in my child’s mother tongue"
var_label(nigeria_data$education_host_F23b_1_5) <- "School is closer than in my home area"
var_label(nigeria_data$education_host_F23b_1_6) <- "School is accessible by public/affordable method of transport"
var_label(nigeria_data$education_host_F23b_1_7) <- "Education is less expensive"
var_label(nigeria_data$education_host_F23b_1_8) <- "School is not short-staffed"
var_label(nigeria_data$education_host_F23b_1_9) <- "School provides meals to students"
var_label(nigeria_data$education_host_F23b_1_10) <- "Other"
var_label(nigeria_data$education_host_F23b_1_99) <- "Prefer not to answer"
var_label(nigeria_data$education_host_F23b_2_1) <- "Classes are less crowded"
var_label(nigeria_data$education_host_F23b_2_2) <- "Higher quality education than in home area"
var_label(nigeria_data$education_host_F23b_2_3) <- "Child no longer fears or suffers from discrimination/marginalisation"
var_label(nigeria_data$education_host_F23b_2_4) <- "Classes are in my child’s mother tongue"
var_label(nigeria_data$education_host_F23b_2_5) <- "School is closer than in my home area"
var_label(nigeria_data$education_host_F23b_2_6) <- "School is accessible by public/affordable method of transport"
var_label(nigeria_data$education_host_F23b_2_7) <- "Education is less expensive"
var_label(nigeria_data$education_host_F23b_2_8) <- "School is not short-staffed"
var_label(nigeria_data$education_host_F23b_2_9) <- "School provides meals to students"
var_label(nigeria_data$education_host_F23b_2_10) <- "Other"
var_label(nigeria_data$education_host_F23b_2_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_G01) <- "How many household members earned money from work before you left your home? Please include yourself"
var_label(nigeria_data$livelihhods_G02) <- "Can you estimate how much money was earned in the household each month before you left your home?"
var_label(nigeria_data$livelihhods_G01_1) <- "Did you, personally, earn any money before you left your home?"
var_label(nigeria_data$livelihhods_G01_bis) <- "What were your main sources of that income?"
var_label(nigeria_data$livelihhods_G02_1) <- "Can you estimate how much money you, personally, earned each month before you left your home?"
var_label(nigeria_data$livelihhods_G3_1) <- "'Me, personally' :"
var_label(nigeria_data$livelihhods_G3_2) <- "Another member of my household :"
var_label(nigeria_data$livelihhods_G4) <- "How many months did you not earn any money or remain unemployed for?"
var_label(nigeria_data$livelihhods_G4_1) <- "How many months did your household member not earn any money or remain unemployed for?"
var_label(nigeria_data$livelihhods_G40) <- "Did your household  receive financial support from family or friends before you left your home?"
var_label(nigeria_data$livelihhods_G7_1) <- "Yes, from the government"
var_label(nigeria_data$livelihhods_G7_2) <- "Yes, from UN agencies"
var_label(nigeria_data$livelihhods_G7_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$livelihhods_G7_4) <- "Yes, from other source"
var_label(nigeria_data$livelihhods_G7_98) <- "No support"
var_label(nigeria_data$livelihhods_G7_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_G1) <- "How many household members earn money from work, now? Please include yourself"
var_label(nigeria_data$livelihhods_G2) <- "Can you estimate how much money is earned in the household each month, now?"
var_label(nigeria_data$livelihhods_G1_1) <- "Do you, personally, earn any money from work, now?"
var_label(nigeria_data$livelihhods_G1_bis) <- "What are your main sources of that income?"
var_label(nigeria_data$livelihhods_G2_1) <- "Can you estimate how much money you, personally, earn each month, now?"
var_label(nigeria_data$livelihhods_G11) <- "Does your household currently receive financial support from family or friends?"
var_label(nigeria_data$livelihhods_G13_1) <- "Yes, from the government"
var_label(nigeria_data$livelihhods_G13_2) <- "Yes, from UN agencies"
var_label(nigeria_data$livelihhods_G13_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$livelihhods_G13_4) <- "Yes, from other source"
var_label(nigeria_data$livelihhods_G13_98) <- "No support"
var_label(nigeria_data$livelihhods_G13_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_G15) <- "Does your household receive specific financial support because you had to leave your previous home?"
var_label(nigeria_data$livelihhods_G17) <- "Do you feel like your household’s financial resources were enough to fulfil all your needs and wants before your displacement?"
var_label(nigeria_data$livelihhods_G18) <- "Do you feel like your household’s financial resources are enough to fulfil all your needs and wants now?"
var_label(nigeria_data$livelihhods_G38) <- "Did your household have access to land for agriculture before being displaced?"
var_label(nigeria_data$livelihhods_G39) <- "Does your household currently have access to land for agriculture?"
var_label(nigeria_data$livelihhods_G40_1) <- "Are you afraid of being dispossessed of your land by your landlord?"
var_label(nigeria_data$livelihhods_G42) <- "Do you find the rent cost manageable?"
var_label(nigeria_data$livelihhods_G43) <- "Are there any limitations set by the landowner on how you can use the land?"
var_label(nigeria_data$livelihhods_G45) <- "Is the land you currently use suitable for agriculture?"
var_label(nigeria_data$livelihhods_G46_1) <- "Yes"
var_label(nigeria_data$livelihhods_G46_2) <- "No, due to lack of inputs (seeds/tools)"
var_label(nigeria_data$livelihhods_G46_3) <- "No, due to environmental challenges (drought/floods)"
var_label(nigeria_data$livelihhods_G46_4) <- "No, due to safety concerns (armed groups/bandits)"
var_label(nigeria_data$livelihhods_G46_5) <- "No, due to insufficient financial resources"
var_label(nigeria_data$livelihhods_G46_6) <- "Other"
var_label(nigeria_data$livelihhods_G46_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_AG10_1) <- "What are the primary crops your household is cultivating?"
var_label(nigeria_data$livelihhods_AG10_2) <- "What are the primary crops your household is cultivating?"
var_label(nigeria_data$livelihhods_AG10_3) <- "What are the primary crops your household is cultivating?"
var_label(nigeria_data$livelihhods_AG11) <- "Is the harvest sufficient to meet your household’s food and income needs?"
var_label(nigeria_data$livelihhods_AG12_1) <- "Lack of adequate inputs (seeds/tools)"
var_label(nigeria_data$livelihhods_AG12_2) <- "Limited skills in agricultural production"
var_label(nigeria_data$livelihhods_AG12_3) <- "Limited financial resources"
var_label(nigeria_data$livelihhods_AG12_4) <- "Soil infertility"
var_label(nigeria_data$livelihhods_AG12_5) <- "Drought or flood impact"
var_label(nigeria_data$livelihhods_AG12_6) <- "Pest issues"
var_label(nigeria_data$livelihhods_AG12_7) <- "Other"
var_label(nigeria_data$livelihhods_AG12_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_AG13) <- "Does your household currently own livestock?"
var_label(nigeria_data$livelihhods_AG14_1) <- "What types of livestock do you own?"
var_label(nigeria_data$livelihhods_AG14_2) <- "What types of livestock do you own?"
var_label(nigeria_data$livelihhods_AG14_3) <- "What types of livestock do you own?"
var_label(nigeria_data$livelihhods_AG15_1) <- "Lack of grazing land"
var_label(nigeria_data$livelihhods_AG15_2) <- "Disease"
var_label(nigeria_data$livelihhods_AG15_3) <- "Lack of fodder"
var_label(nigeria_data$livelihhods_AG15_4) <- "Limited space"
var_label(nigeria_data$livelihhods_AG15_5) <- "Environmental hazards (drought/flood)"
var_label(nigeria_data$livelihhods_AG15_6) <- "Conflict"
var_label(nigeria_data$livelihhods_AG15_7) <- "Insecurity"
var_label(nigeria_data$livelihhods_AG15_8) <- "Too expensive"
var_label(nigeria_data$livelihhods_AG15_9) <- "Other"
var_label(nigeria_data$livelihhods_AG15_10) <- "None"
var_label(nigeria_data$livelihhods_AG15_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_AG16_1) <- "In your opinion, which agricultural activities have the most potential to help you earn income here?"
var_label(nigeria_data$livelihhods_AG16_2) <- "In your opinion, which agricultural activities have the most potential to help you earn income here?"
var_label(nigeria_data$livelihhods_AG16_3) <- "In your opinion, which agricultural activities have the most potential to help you earn income here?"
var_label(nigeria_data$livelihhods_G19) <- "How many household members earned money from work before people who had to leave their home for reasons out of their control arrived in .........?"
var_label(nigeria_data$livelihhods_G20) <- "Can you estimate how much your household earned each month before people who had to leave their home for reasons out of their control arrived in .........?"
var_label(nigeria_data$livelihhods_G19_1) <- "Did you, personally, earn money from work before people who had to leave their home for reasons out of their control arrived in .........?"
var_label(nigeria_data$livelihhods_G19_bis) <- "What were your main sources of that income?"
var_label(nigeria_data$livelihhods_G20_4) <- "Can you estimate how much money you, personally, earned each month, before people who had to leave their home for reasons out of their control arrived in ........."
var_label(nigeria_data$livelihhods_G20_1_1) <- "'Me, personally' :"
var_label(nigeria_data$livelihhods_G20_1_2) <- "Another member of my household :"
var_label(nigeria_data$livelihhods_G20_2) <- "How many months did you not earn any money or remain unemployed for?"
var_label(nigeria_data$livelihhods_G20_3) <- "How many months did your household member not earn any money or remain unemployed for?"
var_label(nigeria_data$livelihhods_G21) <- "Did your  household receive financial support from family or friends before people who had to leave their home for reasons out of their control arrived in .......?"
var_label(nigeria_data$livelihhods_G23_1) <- "Yes, from the government"
var_label(nigeria_data$livelihhods_G23_2) <- "Yes, from UN agencies"
var_label(nigeria_data$livelihhods_G23_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$livelihhods_G23_4) <- "Yes, from other source"
var_label(nigeria_data$livelihhods_G23_98) <- "No support"
var_label(nigeria_data$livelihhods_G23_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_G25) <- "How many household members earn money from work now?"
var_label(nigeria_data$livelihhods_G26) <- "Can you estimate how much your household earn each month?"
var_label(nigeria_data$livelihhods_G25_1) <- "Do you, personally, earn any money from work, now?"
var_label(nigeria_data$livelihhods_G25_bis) <- "What are your main sources of that income?"
var_label(nigeria_data$livelihhods_G26_1) <- "Can you estimate how much money you, personally, earn each month, now?"
var_label(nigeria_data$livelihhods_G27) <- "Does your household currently receive financial support from family or friends?"
var_label(nigeria_data$livelihhods_G29_1) <- "Yes, from the government"
var_label(nigeria_data$livelihhods_G29_2) <- "Yes, from UN agencies"
var_label(nigeria_data$livelihhods_G29_3) <- "Yes, from international or local NGOs"
var_label(nigeria_data$livelihhods_G29_4) <- "Yes, from other source"
var_label(nigeria_data$livelihhods_G29_98) <- "No support"
var_label(nigeria_data$livelihhods_G29_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_G31) <- "Does your  household currently receive specific financial support because of people who had to leave their home for reasons out of their control arrived in ......?"
var_label(nigeria_data$livelihhods_G33) <- "Do you feel like your household’s financial resources were enough to fulfil all your needs and wants before people who had to leave their home for reasons out of their control arrived in ........?"
var_label(nigeria_data$livelihhods_G34) <- "Do you feel like your household’s financial resources are enough to fulfil all your needs and wants now?"
var_label(nigeria_data$livelihhods_G38_H) <- "Did your household have access to land for agriculture before people who had to leave their home for reasons out of their control arrived in .........?"
var_label(nigeria_data$livelihhods_G39_H) <- "Does your household currently have access to land for agriculture?"
var_label(nigeria_data$livelihhods_G40_H) <- "Are  you afraid of being dispossessed of your land by your landlord?"
var_label(nigeria_data$livelihhods_G42_H) <- "Do you find the rent cost manageable?"
var_label(nigeria_data$livelihhods_G43_H) <- "Are there any limitations set by the landowner on how you can use the land?"
var_label(nigeria_data$livelihhods_G45_H) <- "Is the land you currently use suitable for agriculture?"
var_label(nigeria_data$livelihhods_G46_H_1) <- "Yes"
var_label(nigeria_data$livelihhods_G46_H_2) <- "No, due to lack of inputs (seeds/tools)"
var_label(nigeria_data$livelihhods_G46_H_3) <- "No, due to environmental challenges (drought/floods)"
var_label(nigeria_data$livelihhods_G46_H_4) <- "No, due to safety concerns (armed groups/bandits)"
var_label(nigeria_data$livelihhods_G46_H_5) <- "No, due to insufficient financial resources"
var_label(nigeria_data$livelihhods_G46_H_6) <- "Other"
var_label(nigeria_data$livelihhods_G46_H_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_AG10_H_1) <- "What are the primary crops your household is cultivating?"
var_label(nigeria_data$livelihhods_AG10_H_2) <- "What are the primary crops your household is cultivating?"
var_label(nigeria_data$livelihhods_AG10_H_3) <- "What are the primary crops your household is cultivating?"
var_label(nigeria_data$livelihhods_AG11_H) <- "Is the harvest sufficient to meet your household’s food and income needs?"
var_label(nigeria_data$livelihhods_AG12_H_1) <- "Lack of adequate inputs (seeds/tools)"
var_label(nigeria_data$livelihhods_AG12_H_2) <- "Limited skills in agricultural production"
var_label(nigeria_data$livelihhods_AG12_H_3) <- "Limited financial resources"
var_label(nigeria_data$livelihhods_AG12_H_4) <- "Soil infertility"
var_label(nigeria_data$livelihhods_AG12_H_5) <- "Drought or flood impact"
var_label(nigeria_data$livelihhods_AG12_H_6) <- "Pest issues"
var_label(nigeria_data$livelihhods_AG12_H_7) <- "Other"
var_label(nigeria_data$livelihhods_AG12_H_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_AG13_H) <- "Does your household currently own livestock?"
var_label(nigeria_data$livelihhods_AG14_H_1) <- "What types of livestock do you own?"
var_label(nigeria_data$livelihhods_AG14_H_2) <- "What types of livestock do you own?"
var_label(nigeria_data$livelihhods_AG14_H_3) <- "What types of livestock do you own?"
var_label(nigeria_data$livelihhods_AG15_H_1) <- "Lack of grazing land"
var_label(nigeria_data$livelihhods_AG15_H_2) <- "Disease"
var_label(nigeria_data$livelihhods_AG15_H_3) <- "Lack of fodder"
var_label(nigeria_data$livelihhods_AG15_H_4) <- "Limited space"
var_label(nigeria_data$livelihhods_AG15_H_5) <- "Environmental hazards (drought/flood)"
var_label(nigeria_data$livelihhods_AG15_H_6) <- "Conflict"
var_label(nigeria_data$livelihhods_AG15_H_7) <- "Insecurity"
var_label(nigeria_data$livelihhods_AG15_H_8) <- "Too expensive"
var_label(nigeria_data$livelihhods_AG15_H_9) <- "Other"
var_label(nigeria_data$livelihhods_AG15_H_10) <- "None"
var_label(nigeria_data$livelihhods_AG15_H_99) <- "Prefer not to answer"
var_label(nigeria_data$livelihhods_AG16_H_1) <- "In your opinion, which agricultural activities have the most potential to help you earn income here?"
var_label(nigeria_data$livelihhods_AG16_H_2) <- "In your opinion, which agricultural activities have the most potential to help you earn income here?"
var_label(nigeria_data$livelihhods_AG16_H_3) <- "In your opinion, which agricultural activities have the most potential to help you earn income here?"
var_label(nigeria_data$health_H1) <- "Compared to before you left your home, do you feel that your own physical health:"
var_label(nigeria_data$health_H2a_1) <- "Less access to clean water"
var_label(nigeria_data$health_H2a_2) <- "Less access to food"
var_label(nigeria_data$health_H2a_3) <- "Less access to health services"
var_label(nigeria_data$health_H2a_4) <- "Less access to hygiene facilities"
var_label(nigeria_data$health_H2a_5) <- "Poorer housing conditions"
var_label(nigeria_data$health_H2a_6) <- "Higher frequency of diseases"
var_label(nigeria_data$health_H2a_99) <- "Prefer not to answer"
var_label(nigeria_data$health_H2b_1) <- "Better access to clean water"
var_label(nigeria_data$health_H2b_2) <- "Better access to food"
var_label(nigeria_data$health_H2b_3) <- "Better access to health services"
var_label(nigeria_data$health_H2b_4) <- "Better access to hygiene facilities"
var_label(nigeria_data$health_H2b_5) <- "Better housing conditions"
var_label(nigeria_data$health_H2b_6) <- "Lower frequency of diseases"
var_label(nigeria_data$health_H2b_99) <- "Prefer not to answer"
var_label(nigeria_data$health_H222) <- "In the past 30 days, was there ever no food to eat of any kind in your house because of lack of resources to get food?"
var_label(nigeria_data$health_H223) <- "How often did this happen?"
var_label(nigeria_data$health_H224) <- "In the past 30 days, did you or any household member ever go to sleep at night hungry because there was not enough food?"
var_label(nigeria_data$health_H225) <- "How often did this happen?"
var_label(nigeria_data$health_H226) <- "In the past 30 days, did you or any household member ever go a whole day and night without eating anything at all because there was not enough food?"
var_label(nigeria_data$health_H227) <- "How often did this happen?"
var_label(nigeria_data$health_H228) <- "Are you currently receiving any of the following assistance from NGOs, UN, or other partners?"
var_label(nigeria_data$health_H3) <- "Compared to before you left your home, would you say that you, and other household members, have:"
var_label(nigeria_data$health_H4a_1) <- "Cost of healthcare"
var_label(nigeria_data$health_H4a_2) <- "Cost of transport to access healthcare"
var_label(nigeria_data$health_H4a_3) <- "Distance to healthcare facilities"
var_label(nigeria_data$health_H4a_4) <- "The facilities are not physically accessible"
var_label(nigeria_data$health_H4a_5) <- "Language barriers"
var_label(nigeria_data$health_H4a_6) <- "Lack of information"
var_label(nigeria_data$health_H4a_7) <- "Discrimination in healthcare provisions"
var_label(nigeria_data$health_H4a_8) <- "Health has worsened beyond the managing capacity of the local healthcare services"
var_label(nigeria_data$health_H4a_9) <- "Loss of necessary medical documentation"
var_label(nigeria_data$health_H4a_99) <- "Prefer not to answer"
var_label(nigeria_data$health_H4ba_1) <- "Cost of healthcare"
var_label(nigeria_data$health_H4ba_2) <- "Cost of transport to access healthcare"
var_label(nigeria_data$health_H4ba_3) <- "Distance to healthcare facilities"
var_label(nigeria_data$health_H4ba_4) <- "The facilities are physically accessible"
var_label(nigeria_data$health_H4ba_5) <- "Language barriers addressed"
var_label(nigeria_data$health_H4ba_6) <- "Lack of information"
var_label(nigeria_data$health_H4ba_7) <- "Less discrimination in healthcare provisions"
var_label(nigeria_data$health_H4ba_8) <- "The local healthcare services are well equipped to respond to my needs"
var_label(nigeria_data$health_H4ba_99) <- "Prefer not to answer"
var_label(nigeria_data$health_H5) <- "Before you left your home, did you and other household members have free access to a healthcare professional?"
var_label(nigeria_data$health_H8) <- "How much did it cost to see a healthcare professional before you left your home?"
var_label(nigeria_data$health_H6) <- "Do you currently have free access to a healthcare professional?"
var_label(nigeria_data$health_H9) <- "How much does it cost to see a healthcare professional now?"
var_label(nigeria_data$health_H7) <- "Compared to before you left your home, would you say that you are spending:"
var_label(nigeria_data$health_H10) <- "Compared to before you left your home, would you say that now you feel worried, nervous, angry or sad"
var_label(nigeria_data$health_H11_1) <- "Compared to before people who had to leave their home for reasons out of their control arrived in .........., do you feel that your own physical health:"
var_label(nigeria_data$health_H12a_1) <- "Less access to clean water"
var_label(nigeria_data$health_H12a_2) <- "Less access to food"
var_label(nigeria_data$health_H12a_3) <- "Less access to health services"
var_label(nigeria_data$health_H12a_4) <- "Less access to hygiene facilities"
var_label(nigeria_data$health_H12a_5) <- "Poorer housing conditions"
var_label(nigeria_data$health_H12a_6) <- "Higher frequency of diseases"
var_label(nigeria_data$health_H12a_99) <- "Prefer not to answer"
var_label(nigeria_data$health_H12b_1) <- "Better access to clean water"
var_label(nigeria_data$health_H12b_2) <- "Better access to food"
var_label(nigeria_data$health_H12b_3) <- "Better access to health services"
var_label(nigeria_data$health_H12b_4) <- "Better access to hygiene facilities"
var_label(nigeria_data$health_H12b_5) <- "Better housing conditions"
var_label(nigeria_data$health_H12b_6) <- "Lower frequency of diseases"
var_label(nigeria_data$health_H12b_99) <- "Prefer not to answer"
var_label(nigeria_data$health_H222_H) <- "In the past 30 days, was there ever no food to eat of any kind in your house because of lack of resources to get food?"
var_label(nigeria_data$health_H223_H) <- "How often did this happen?"
var_label(nigeria_data$health_H224_H) <- "In the past 30 days, did you or any household member ever go to sleep at night hungry because there was not enough food?"
var_label(nigeria_data$health_H225_H) <- "How often did this happen?"
var_label(nigeria_data$health_H226_H) <- "In the past 30 days, did you or any household member ever go a whole day and night without eating anything at all because there was not enough food?"
var_label(nigeria_data$health_H227_H) <- "How often did this happen?"
var_label(nigeria_data$health_H228_H) <- "Are you currently receiving any of the following assistance from NGOs, UN, or other partners?"
var_label(nigeria_data$health_H13) <- "Compared to before people who had to leave their home for reasons out of their control arrived in the area, would you say that you and other household members have:"
var_label(nigeria_data$health_H14a_1) <- "Cost of healthcare"
var_label(nigeria_data$health_H14a_2) <- "Cost of transport to access healthcare"
var_label(nigeria_data$health_H14a_3) <- "Distance to healthcare facilities"
var_label(nigeria_data$health_H14a_4) <- "The facilities are not physically accessible"
var_label(nigeria_data$health_H14a_5) <- "Language barriers"
var_label(nigeria_data$health_H14a_6) <- "Lack of information"
var_label(nigeria_data$health_H14a_7) <- "Discrimination in healthcare provisions"
var_label(nigeria_data$health_H14a_8) <- "Health has worsened beyond the managing capacity of the local healthcare services"
var_label(nigeria_data$health_H14a_9) <- "Loss of necessary medical documentation"
var_label(nigeria_data$health_H14a_99) <- "Prefer not to answer"
var_label(nigeria_data$health_H14b_1) <- "Cost of healthcare"
var_label(nigeria_data$health_H14b_2) <- "Cost of transport to access healthcare"
var_label(nigeria_data$health_H14b_3) <- "Distance to healthcare facilities"
var_label(nigeria_data$health_H14b_4) <- "The facilities are physically accessible"
var_label(nigeria_data$health_H14b_5) <- "Language barriers addressed"
var_label(nigeria_data$health_H14b_6) <- "Lack of information"
var_label(nigeria_data$health_H14b_7) <- "Less discrimination in healthcare provisions"
var_label(nigeria_data$health_H14b_8) <- "The local healthcare services are well equipped to respond to my needs"
var_label(nigeria_data$health_H14b_9) <- "Prefer not to answer"
var_label(nigeria_data$health_H17) <- "Before people who had to leave their home for reasons out of their control arrived in your area, did you have free access to a healthcare professional?"
var_label(nigeria_data$health_H18) <- "Do you currently have free access to a healthcare professional?"
var_label(nigeria_data$health_H19) <- "Compared to before people who had to leave their home for reasons out of their control arrived in the area, would you say that you are spending:"
var_label(nigeria_data$health_H20) <- "Can you estimate how much it costed to see a healthcare professional before people who had to leave their home for reasons out of their control arrived in the area?"
var_label(nigeria_data$health_H21) <- "Can you estimate how much it costs to see a healthcare professional now?"
var_label(nigeria_data$health_H22) <- "Compared to before people who had to leave their home for reasons out of their control arrived in ........., would you say that you feel worried, nervous, angry or sad:"
var_label(nigeria_data$closing_I0_1) <- "If you agree, please may I ask some follow-up questions specific to these difficulties? If you agree, this would be just five additional question"
var_label(nigeria_data$closing_I0_2_1) <- "Yes, I faced challenges with transporting to another area"
var_label(nigeria_data$closing_I0_2_2) <- "Yes, I faced challenges with finding a place to stay after I left my home"
var_label(nigeria_data$closing_I0_2_3) <- "Yes, I faced challenges accessing assistance available to other IDPs"
var_label(nigeria_data$closing_I0_2_4) <- "Yes, I had to leave behind my assistive devices when I left my home"
var_label(nigeria_data$closing_I0_2_98) <- "No, I did not face any challenges"
var_label(nigeria_data$closing_I0_2_99) <- "Prefer not to answer"
var_label(nigeria_data$closing_I0_5_1) <- "Accessing healthcare :"
var_label(nigeria_data$closing_I0_5_2) <- "Accessing work/income :"
var_label(nigeria_data$closing_I0_5_3) <- "Participating in community life/activities :"
var_label(nigeria_data$closing_I0_5_4) <- "Accessing enough food :"
var_label(nigeria_data$closing_I0_5_5) <- "Accessing clean water :"
var_label(nigeria_data$closing_I0_5_6) <- "Accessing toilets/hygiene facilities :"
var_label(nigeria_data$closing_I0_6_1) <- "Distance"
var_label(nigeria_data$closing_I0_6_2) <- "Discrimination"
var_label(nigeria_data$closing_I0_6_3) <- "Fear of violence and/or harassment"
var_label(nigeria_data$closing_I0_6_4) <- "Difficulties communicating and/or accessing information"
var_label(nigeria_data$closing_I0_6_5) <- "Lack of economic resources"
var_label(nigeria_data$closing_I0_6_6) <- "Services are not accessible/do not respond to your needs"
var_label(nigeria_data$closing_I0_6_99) <- "Prefer not to answer"
var_label(nigeria_data$closing_I0_7_1) <- "Yes, Financial support/money"
var_label(nigeria_data$closing_I0_7_2) <- "Yes, Specialized healthcare"
var_label(nigeria_data$closing_I0_7_3) <- "Yes, Equipment to help with your difficulties (walking cane, hearing aid, wheelchair…)"
var_label(nigeria_data$closing_I0_7_4) <- "Yes, Other"
var_label(nigeria_data$closing_I0_7_5) <- "No"
var_label(nigeria_data$closing_I0_7_99) <- "Prefer not to answer"
var_label(nigeria_data$closing_I0_8) <- "Have you at any stage been consulted about how assistance could be adapted to meet your needs in displacement?"
var_label(nigeria_data$closing_I1) <- "This is now the end of this interview. The information you agreed to share with us will be very useful. Would you like to add any other information on the impacts that internal displacement has had on you, your family, your friends or your community?"


write.xlsx(data, "D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/data/nigeria_data_names.xlsx")
save(nigeria_data, file = "D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/data/nigeria_data.RData")

 
