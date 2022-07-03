#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for health procedures data
#
#
#######################################################################################################

# 0. Set-up
# =================================================================

rm(list=ls())

# packages
packages<-c('readr',
            'tidyverse',
            'dplyr',
            'RCurl',
            'tidyr',
            'scales',
            'RColorBrewer',
            'ggplot2',
            'xlsx',
            'stringdist',
            'textclean',
            'readstata13',
            'lfe',
            'fastDummies',
            'purrr',
            'boot',
            'broom',
            'modelsummary',
            'ggsci')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)

# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))

# 2. Define outcomes output name and output functions
# =================================================================

var_map <- rbind(cbind('sia_ncnes_amb_mun_pcapita','N. of Health Facilities with Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_amb_lc_mun_pcapita','N. of Health Facilities with Low & Mid Complexity Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_amb_hc_mun_pcapita','N. of Health Facilities with High Complexity Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_low_skill_mun_pcapita','N. of Health Facilities with Ambulatory Service by Low Skilled Workers (per capita*1000)'),
                 cbind('sia_ncnes_med_skill_mun_pcapita','N. of Health Facilities with Ambulatory Service by Mid Skilled Workers (per capita*1000)'),
                 cbind('sia_ncnes_enf_mun_pcapita','N. of Health Facilities with Ambulatory Service by Nurses (per capita*1000)'),
                 cbind('sia_ncnes_enfobs_mun_pcapita','N. of Health Facilities with Ambulatory Service by Obstetrical Nurses (per capita*1000)'),
                 cbind('sia_ncnes_medcom_pcapita','N. of Health Facilities with Ambulatory Service and Community Doctors (per capita*1000)'),
                 cbind('sia_ncnes_ginobs_mun_pcapita','N. of Health Facilities with Obstetrical/Gyneco. Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_pediat_mun_pcapita','N. of Health Facilities with Pediatric Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_medpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Doctors (per capita*1000)'),
                 cbind('sia_ncnes_enfpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nurses (per capita*1000)'),
                 cbind('sia_ncnes_outpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nursing Assistants (per capita*1000)'),
                 cbind('sia_ncnes_psf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Teams (per capita*1000)'),
                 cbind('sia_ncnes_acs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Teams (per capita*1000)'),
                 cbind('sia_ncnes_enfacs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Nurses (per capita*1000)'),
                 cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
                 cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_low_skill_mun_pcapita','N. Outpatient Procedures by Low Skilled Workers (per capita)'),
                 cbind('sia_nprod_med_skill_mun_pcapita','N. Outpatient procedures by Mid Skilled Workers (per capita)')
                 
)

                 
                 
                 
                 
                 

                 # cbind('sia_nprod_amb_lc_pcapita','Low & Mid Complexity Outpatient Procedures (per capita)'),
                 # cbind('sia_nprod_amb_lc_pub_pcapita','Public Low & Mid Complexity Outpatient Procedures (per capita)'),
                 # cbind('sia_nprod_amb_lc_pvt_pcapita','Private Low & Mid Complexity Outpatient Procedures (per capita)'),
                 # 
                 # cbind('sia_nprod_amb_hc_pcapita','High Complexity Outpatient Procedures (per capita)'),
                 # cbind('sia_nprod_amb_hc_pub_pcapita','Public High Complexity Outpatient Procedures (per capita)'),
                 # cbind('sia_nprod_amb_hc_pvt_pcapita','Private High Complexity Outpatient Procedures (per capita)'),
                 # 
                 # cbind('sia_nprod_low_skill_pcapita','Outpatient Procedures by Low Skilled Workers (per capita)'),
                 # cbind('sia_nprod_low_skill_pub_pcapita','Public Outpatient Procedures by Low Skilled Workers (per capita)'),
                 # cbind('sia_nprod_low_skill_pvt_pcapita','Private Outpatient Procedures by Low Skilled Workers (per capita)'),
                 # 
                 # cbind('sia_nprod_med_skill_pcapita','Outpatient Procedures by Mid Skilled Workers (per capita)'),
                 # cbind('sia_nprod_med_skill_pub_pcapita','Public Outpatient procedures by Mid Skilled Workers (per capita)'),
                 # cbind('sia_nprod_med_skill_pvt_pcapita','Private Outpatient procedures by Mid Skilled Workers (per capita)'),
                 # 
                 # cbind('sia_nprod_high_skill_pcapita','Outpatient Procedures by High Skilled Workers (per capita)'),
                 # cbind('sia_nprod_high_skill_mun_pcapita','Municipal Outpatient Procedures by High Skilled Workers (per capita)'),
                 # cbind('sia_nprod_high_skill_pub_pcapita','Public Outpatient Procedures by High Skilled Workers (per capita)'),
                 # cbind('sia_nprod_high_skill_pvt_pcapita','Private Outpatient Procedures by High Skilled Workers (per capita)'),
                 # 
                 # cbind('sia_ncnes_amb_pcapita','N. of Health Facilities with Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_amb_pub_pcapita','N. of Public Health Facilities with Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_amb_pvt_pcapita','N. of Private Health Facilities with Ambulatory Service (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_amb_lc_pcapita','N. of Health Facilities with Low & Mid Complexity Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_amb_lc_pub_pcapita','N. of Public Health Facilities with Low & Mid Complexity Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_amb_lc_pvt_pcapita','N. of Private Health Facilities with Low & Mid Complexity Ambulatory Service (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_amb_hc_pcapita','N. of Health Facilities with High Complexity Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_amb_hc_pub_pcapita','N. of Public Health Facilities with High Complexity Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_amb_hc_pvt_pcapita','N. of Private Health Facilities with High Complexity Ambulatory Service (per capita*1000)'),
                 # 
                 # 
                 # cbind('sia_ncnes_low_skill_pcapita','N. of Health Facilities with Ambulatory Service by Low Skilled Workers (per capita*1000)'),
                 # cbind('sia_ncnes_low_skill_pub_pcapita','N. of Public Health Facilities with Ambulatory Service by Low Skilled Workers (per capita*1000)'),
                 # cbind('sia_ncnes_low_skill_pvt_pcapita','N. of Private Health Facilities with Ambulatory Service by Low Skilled Workers (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_med_skill_pcapita','N. of Health Facilities with Ambulatory Service by Mid Skilled Workers (per capita*1000)'),
                 # cbind('sia_ncnes_med_skill_pub_pcapita','N. of Public Health Facilities with Ambulatory Service by Mid Skilled Workers (per capita*1000)'),
                 # cbind('sia_ncnes_med_skill_pvt_pcapita','N. of Private Health Facilities with Ambulatory Service by Mid Skilled Workers (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_high_skill_pcapita','N. of Health Facilities with Ambulatory Service by High Skilled Workers (per capita*1000)'),
                 # cbind('sia_ncnes_high_skill_mun_pcapita','N. of Municipal Health Facilities with Ambulatory Service by High Skilled Workers (per capita*1000)'),
                 # cbind('sia_ncnes_high_skill_pub_pcapita','N. of Public Health Facilities with Ambulatory Service by High Skilled Workers (per capita*1000)'),
                 # cbind('sia_ncnes_high_skill_pvt_pcapita','N. of Private Health Facilities with Ambulatory Service by High Skilled Workers (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_enf_pcapita','N. of Health Facilities with Ambulatory Service by Nurses (per capita*1000)'),
                 # cbind('sia_ncnes_enf_pub_pcapita','N. of Public Health Facilities with Ambulatory Service and with Nurses (per capita*1000)'),
                 # cbind('sia_ncnes_enf_pvt_pcapita','N. of Private Health Facilities with Ambulatory Service and with Nurses (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_enfobs_pcapita','N. of Health Facilities with Ambulatory Service by Obstetrical Nurses (per capita*1000)'),
                 # cbind('sia_ncnes_enfobs_pub_pcapita','N. of Public Health Facilities with Ambulatory Service by Obstetrical Nurses (per capita*1000)'),
                 # cbind('sia_ncnes_enfobs_pvt_pcapita','N. of Private Health Facilities with Ambulatory Service by Obstetrical Nurses (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_ginobs_pcapita','N. of Health Facilities with Obstetrical/Gyneco. Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_ginobs_pub_pcapita','N. of Public Health Facilities with Obstetrical/Gyneco. Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_ginobs_pvt_pcapita','N. of Private Health Facilities with Obstetrical/Gyneco. Ambulatory Service (per capita*1000)'),
                 # 
                 # cbind('sia_ncnes_pediat_pcapita','N. of Health Facilities with Pediatric Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_pediat_pub_pcapita','N. of Public Health Facilities with Pediatric Ambulatory Service (per capita*1000)'),
                 # cbind('sia_ncnes_pediat_pvt_pcapita','N. of Private Health Facilities with Pediatric Ambulatory Service (per capita*1000)'),
                 # 
                 # 
                 # 
                 # 
                 # 
                 # )



# 3. Run and ouput
# =================================================================
df <- df %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano<=2000,0,iv),
         iv_firstterm = ifelse(ano<=2000,0,iv_firstterm),
         firstterm = ifelse(ano<=2000,0,firstterm)) 



for (i in seq(1,22,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  
  regress_output(var,var_name,3,1998,"peso_eq")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all

  }
    

  
}



# exporting results
# ---------------------

write.xlsx2(df_table_all %>% as.data.frame(), file = paste0(dir,main_folder,output_file),sheetName = "sia",row.names = F,append = T)



