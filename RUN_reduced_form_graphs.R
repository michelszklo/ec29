#######################################################################################################
# Author: Michel Szklo
# April 2022
# 
# This scripts runs reduced form graphs for all outcomes
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

df <- df %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano==2000,0,iv)) 
df_below <- df_below %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano==2000,0,iv)) 
df_above <- df_above %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano==2000,0,iv)) 



# 2. Spending
# =================================================================


var_map <- rbind(cbind('finbra_desp_o_pcapita','Total Spending per capita (asinh)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (asinh)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (asinh)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (asinh)'),
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (asinh)'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita (asinh)'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (asinh)'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (asinh)'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita (asinh)'),
                 cbind('finbra_desp_outros_area_pcapita','Other Areas Spending per capita (asinh)'),
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (asinh)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (asinh)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (asinh)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (asinh)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (asinh)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (asinh)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (asinh)'),
                 cbind('finbra_reccorr_pcapita','Total Revenue per capita (asinh)'),
                 cbind('finbra_rectribut_pcapita','Tax Revenue per capita (asinh)'),
                 cbind('finbra_rectransf_pcapita','Transfers Revenue per capita (asinh)'),
                 cbind('finbra_rec_outros_pcapita','Other Revenues per capita (asinh)'),
                 
                 cbind('finbra_desp_pessoal_share','Human Resources Spending (% Total Spending)'),
                 cbind('finbra_desp_investimento_share','Investment Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_nature_share','Other Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_saude_san_share','Health and Sanitation Spending (% Total Spending)'),
                 cbind('finbra_desp_transporte_share','Transport Spending (% Total Spending)'),
                 cbind('finbra_desp_educ_cultura_share','Education and Culture Spending (% Total Spending)'),
                 cbind('finbra_desp_hab_urb_share','Housing and Urban Spending (% Total Spending)'),
                 cbind('finbra_desp_assist_prev_share','Social Assistance Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_area_share','Other Areas Spending (% Total Spending)'),
                 cbind('finbra_rectribut_share','Tax Revenue (% Total Revenue)'),
                 cbind('finbra_rectransf_share','Transfers Revenue (% Total Revenue)'),
                 cbind('finbra_rec_outros_share','Other Revenues (% Total Revenue)'),
                 cbind('siops_desprecpropriosaude_share','Health Spending - Own Resources (% Health Spending)'),
                 cbind('siops_despexrecproprio_share','Health Spending - Transfers (% Health Spending)'),
                 cbind('siops_desppessoal_share','Health Spending - Human Resources (% Health Spending)'),
                 cbind('siops_despinvest_share','Health Spending - Investiment (% Health Spending)'),
                 cbind('siops_despservicoster_share','Health Spending - 3rd parties services (% Health Spending)'),
                 cbind('siops_despoutros_share','Health Spending - other expenditures (% Health Spending)'))


if (instrument=="ec29_baseline"){
  for (i in seq(1,30,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly(var,var_name,df,1,1998,-22,12,2,"full",below = below) # ec29baseline
    # reduced_yearly(var,var_name,df_below,1,1998,-30,25,5,"below",below = below) # ec29baseline
  }
}

if (instrument=="dist_ec29_baseline"){
  for (i in seq(1,21,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly(var,var_name,df,2,1998,-4,10,1,"full",below = below) # ec29baseline
    # reduced_yearly(var,var_name,df_below,1,1998,-30,25,5,"below",below = below) # ec29baseline
  }
  for (i in seq(22,39,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly(var,var_name,df,3,1998,-1.6,1.6,0.2,"full",below = below) # ec29baseline
    # reduced_yearly(var,var_name,df_below,1,1998,-30,25,5,"below",below = below) # ec29baseline
  }
}


if (instrument=="dist_spending_pc_baseline"){
  for (i in seq(1,30,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly(var,var_name,df,1,1998,-0.01,0.04,0.01,"full",below = below) # ec29baseline
    # reduced_yearly(var,var_name,df_below,1,1998,-30,25,5,"below",below = below) # ec29baseline
  }
}



if (instrument=="ec29_baseline_below"){
  for (i in seq(1,30,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly(var,var_name,df,1,1998,-40,20,5,"full",below = below) # ec29baseline
    # reduced_yearly(var,var_name,df_below,1,1998,-30,25,5,"below",below = below) # ec29baseline
  }
}


if (instrument=="dist_ec29_baseline_below"){
  for (i in seq(1,30,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly(var,var_name,df,1,1998,-20,40,5,"full",below = below) # ec29baseline
    # reduced_yearly(var,var_name,df_below,1,1998,-30,25,5,"below",below = below) # ec29baseline
  }
}



if (instrument=="dist_spending_pc_baseline_below"){
  for (i in seq(1,30,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly(var,var_name,df,1,1998,-0.04,0.06,0.01,"full",below = below) # ec29baseline
    # reduced_yearly(var,var_name,df_below,1,1998,-30,25,5,"below",below = below) # ec29baseline
  }
}



# 3. Infra
# =================================================================

var_map <- rbind(cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
                 cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
                 cbind('siab_regist_pers_pcapita','N. of People Register in the Primary Care System (per capita)'),
                 cbind('siab_regist_pers_pacs_pcapita','N. of People Register in the CH Program (per capita)'),
                 cbind('siab_regist_pers_psf_pcapita','N. of People Register in the FH Program (per capita)'),
                 cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
                 cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
                 cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
                 cbind('siab_visit_cha_pcapita','N. of Household Visits (per capita)'),
                 cbind('siab_visit_cha_pacs_pcapita','N. of Household Visits by Community Health Agents (per capita)'),
                 cbind('siab_visit_cha_psf_pcapita','N. of Household Visits by Family Health Agents (per capita)'),
                 cbind('siab_cons_especif_pcapita','N. of Appointments (per capita)'),
                 cbind('siab_cons_especif_pacs_pcapita','N. of Appointments from Community Health Program (per capita)'),
                 cbind('siab_cons_especif_pcapita','N. of Appointments from Family Health Program (per capita)')
                 )

if (instrument=="ec29_baseline"){
  for (i in c(1,2,4)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-15,35,5,sample = "full",below = below) # ec29baseline
    
  }
  
  for (i in c(3)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-1,1,0.2,sample = "full",below = below) # ec29baseline
    
  }
  
}

if (instrument=="dist_ec29_baseline"){
  for (i in seq(1,14,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-2,2.75,0.5,sample = "full",below = below) # ec29baseline
    
  }
  
  
  
}


if (instrument=="dist_spending_pc_baseline"){
  for (i in c(1,2,4)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.05,0.02,0.01,sample = "full",below = below) # ec29baseline
    
  }
  
  for (i in c(3)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-0.05,0.02,0.01,sample = "full",below = below) # ec29baseline
    
  }
  
}




if (instrument=="ec29_baseline_below"){
  for (i in c(1,2,4)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-40,55,5,sample = "full",below = below) # ec29baseline
    
  }
  
  for (i in c(3)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-0.6,0.6,0.2,sample = "full",below = below) # ec29baseline
    
  }
  
}

if (instrument=="dist_ec29_baseline_below"){
  for (i in c(1,2,4)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-55,40,5,sample = "full",below = below) # ec29baseline
    
  }
  
  for (i in c(3)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-0.6,0.6,0.2,sample = "full",below = below) # ec29baseline
    
  }
  
}



if (instrument=="dist_spending_pc_baseline_below"){
  for (i in c(1,2,4)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.1,0.04,0.01,sample = "full",below = below) # ec29baseline
    
  }
  
  for (i in c(3)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-0.02,0.02,0.005,sample = "full",below = below) # ec29baseline
    
  }
  
}



# 4. Sia
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


if (instrument=="ec29_baseline"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-20,15,5,sample = "full",below = below) # ec29baseline
    
  }
  
}


if (instrument=="dist_ec29_baseline"){
  for (i in seq(17,22,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-12.5,25,2.5,sample = "full",below = below) # ec29baseline
    
  }
  
  for (i in seq(1,16,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-.6,.4,0.2,sample = "full",below = below) # ec29baseline
    
  }
  
}

if (instrument=="dist_spending_pc_baseline"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.02,0.03,0.01,sample = "full",below = below) # ec29baseline
    
  }
  
}



if (instrument=="ec29_baseline_below"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-45,25,5,sample = "full",below = below) # ec29baseline
    
  }
  
}

if (instrument=="dist_ec29_baseline_below"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-25,45,5,sample = "full",below = below) # ec29baseline
    
  }
  
}


if (instrument=="dist_spending_pc_baseline_below"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.04,0.04,0.01,sample = "full",below = below) # ec29baseline
    
  }
  
}

# 5. HR
# =================================================================


# 
# var_map <- rbind(cbind('hr_all_pcapita','Total Health workers per 1000 population (log)'),
#                  cbind('hr_superior_pcapita','Doctors per 1000 population (log)'),
#                  cbind('hr_technician_pcapita','Health Technicians per 1000 population (log)'),
#                  cbind('hr_elementary_pcapita','Elementary Health workers per 1000 population (log)'),
#                  cbind('hr_admin_pcapita','Administrative workers per 1000 population (log)'))
# 
# 
# if (instrument=="ec29_baseline"){
#   
#   for (i in seq(1,5,1)){
#     var <- var_map[i,1]
#     var_name <- var_map[i,2]
#     print(var_name)
#     
#     reduced_yearly(var,var_name,df,1,1998,-8,8,2, sample = "full",below = "below") # ec29baseline
#     
#     
#     
#   }
#   
# }
# 
# 
# 
# 
# if (instrument=="dist_ec29_baseline"){
#   
#   for (i in seq(1,5,1)){
#     var <- var_map[i,1]
#     var_name <- var_map[i,2]
#     print(var_name)
#     
#     reduced_yearly(var,var_name,df,1,1998,-8,8,2, sample = "full",below = "below") # ec29baseline
#     
#     
#     
#   }
#   
# }
# 
# 
# if (instrument=="ec29_baseline_below"){
#   
#   for (i in seq(1,5,1)){
#     var <- var_map[i,1]
#     var_name <- var_map[i,2]
#     print(var_name)
#     
#     reduced_yearly(var,var_name,df,1,1998,-10,10,2, sample = "full",below = "below") # ec29baseline
#     
#     
#     
#   }
#   
# }

# 6. IMR
# =================================================================

var_map <-  rbind(cbind('tx_mi','Infant Mortality Rate'),
                  cbind('tx_mi_icsap','Infant Mortality Rate - APC'),
                  cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC'),
                  cbind('tx_mi_infec','Infant Mortality Rate - Infectious'),
                  cbind('tx_mi_resp','Infant Mortality Rate - Respiratory'),
                  cbind('tx_mi_perinat','Infant Mortality Rate - Perinatal'),
                  cbind('tx_mi_cong','Infant Mortality Rate - Congenital'),
                  cbind('tx_mi_ext','Infant Mortality Rate - External'),
                  cbind('tx_mi_nut','Infant Mortality Rate - Nutritional'),
                  cbind('tx_mi_out','Infant Mortality Rate - Other'),
                  cbind('tx_mi_illdef','Infant Mortality Rate - Ill-Defined'),
                  cbind('tx_mi_fet','Infant Mortality Rate - Fetal'),
                  cbind('tx_mi_24h','Infant Mortality Rate - Within 24h'),
                  cbind('tx_mi_27d','Infant Mortality Rate - 1 to 27 days'),
                  cbind('tx_mi_ano','Infant Mortality Rate - 27 days to 1 year'))

if (instrument=="ec29_baseline"){
  for (i in seq(1,15,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-15,20,5,sample = "full",below = below) # ec29baseline
    
    
  }
}


if (instrument=="dist_ec29_baseline"){
  for (i in seq(1,15,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-40,15,5,sample = "full",below = below) # ec29baseline
    
    
  }
}


if (instrument=="dist_spending_pc_baseline"){
  for (i in seq(1,15,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.04,0.03,0.01,sample = "full",below = below) # ec29baseline
    
    
  }
}


if (instrument=="ec29_baseline_below"){
  for (i in seq(1,15,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-30,40,5,sample = "full",below = below) # ec29baseline
    
    
  }
}


if (instrument=="dist_ec29_baseline_below"){
  for (i in seq(1,15,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-40,30,5,sample = "full",below = below) # ec29baseline
    
    
  }
}


if (instrument=="dist_spending_pc_baseline_below"){
  for (i in seq(1,15,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.08,0.06,0.01,sample = "full",below = below) # ec29baseline
    
    
  }
}



# 7. AMR
# =================================================================


var_map <-  rbind(cbind('tx_ma','Adult Mortality Rate'),
                  cbind('tx_ma_circ','Adult Mortality Rate - Circulatory'),
                  cbind('tx_ma_neop','Adult Mortality Rate - Neoplasm'),
                  cbind('tx_ma_resp','Adult Mortality Rate - Respiratory'),
                  cbind('tx_ma_endoc','Adult Mortality Rate - Endocrine'),
                  cbind('tx_ma_ext','Adult Mortality Rate - External'),
                  cbind('tx_ma_nut','Adult Mortality Rate - Nutritional'),
                  cbind('tx_ma_illdef','Adult Mortality Rate - Ill-Defined'),
                  cbind('tx_ma_out','Adult Mortality Rate - Other'),
                  cbind('tx_ma_diab','Adult Mortality Rate - Diabetes'),
                  cbind('tx_ma_hyper','Adult Mortality Rate - Hypertension'),
                  cbind('tx_ma_icsap','Adult Mortality Rate - APC'),
                  cbind('tx_ma_nicsap','Adult Mortality Rate - non-APC'))

if (instrument=="ec29_baseline"){
  for (i in seq(1,13,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-4,8,1,sample = "full",below = below)  # ec29baseline
    
    
  }
}


if (instrument=="dist_ec29_baseline"){
  for (i in seq(1,13,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-20,10,5,sample = "full",below = below)  # ec29baseline
    
    
  }
}


if (instrument=="dist_spending_pc_baseline"){
  for (i in seq(1,13,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.01,0.01,0.002,sample = "full",below = below)  # ec29baseline
    
    
  }
}


if (instrument=="ec29_baseline_below"){
  for (i in seq(1,13,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-10,14,2,sample = "full",below = below)  # ec29baseline
    
    
  }
}


if (instrument=="dist_ec29_baseline_below"){
  for (i in seq(1,13,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-14,10,2,sample = "full",below = below)  # ec29baseline
    
    
  }
}


if (instrument=="dist_spending_pc_baseline_below"){
  for (i in seq(1,13,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.02,0.01,0.002,sample = "full",below = below)  # ec29baseline
    
    
  }
}



# 8. IMR Lagged
# =================================================================

# var_map <-  rbind(cbind('tx_mi_l1','Infant Mortality Rate (log) - 1y lag'),
#                   cbind('tx_mi_l2','Infant Mortality Rate (log) - 2y lag'),
#                   cbind('tx_mi_l3','Infant Mortality Rate (log) - 3y lag'),
#                   cbind('tx_mi_l4','Infant Mortality Rate (log) - 4y lag'),
#                   cbind('tx_mi_l5','Infant Mortality Rate (log) - 5y lag'),
#                   cbind('tx_mi_icsap_l1','Infant Mortality Rate - APC (log) - 1y lag'),
#                   cbind('tx_mi_icsap_l2','Infant Mortality Rate - APC (log) - 2y lag'),
#                   cbind('tx_mi_icsap_l3','Infant Mortality Rate - APC (log) - 3y lag'),
#                   cbind('tx_mi_icsap_l4','Infant Mortality Rate - APC (log) - 4y lag'),
#                   cbind('tx_mi_icsap_l5','Infant Mortality Rate - APC (log) - 5y lag'),
#                   cbind('tx_mi_nicsap_l1','Infant Mortality Rate - non-APC (log) - 1y lag'),
#                   cbind('tx_mi_nicsap_l2','Infant Mortality Rate - non-APC (log) - 2y lag'),
#                   cbind('tx_mi_nicsap_l3','Infant Mortality Rate - non-APC (log) - 3y lag'),
#                   cbind('tx_mi_nicsap_l4','Infant Mortality Rate - non-APC (log) - 4y lag'),
#                   cbind('tx_mi_nicsap_l4','Infant Mortality Rate - non-APC (log) - 5y lag')
# )
# 
# 
# 
# for (i in seq(1,15,1)){
#   var <- var_map[i,1]
#   var_name <- var_map[i,2]
#   print(var_name)
#   
#   reduced_yearly(var,var_name,df,1,2000,-0.02,0.015,0.005) # ec29baseline
#   
#   
# }
# 
# 
# # 9. AMR Lagged
# # =================================================================
# 
# 
# 
# var_map <-  rbind(cbind('tx_ma_l1','Adult Mortality Rate (log) - 1y lag'),
#                   cbind('tx_ma_l2','Adult Mortality Rate (log) - 2y lag'),
#                   cbind('tx_ma_l3','Adult Mortality Rate (log) - 3y lag'),
#                   cbind('tx_ma_l4','Adult Mortality Rate (log) - 4y lag'),
#                   cbind('tx_ma_l5','Adult Mortality Rate (log) - 5y lag'),
#                   cbind('tx_ma_icsap_l1','Adult Mortality Rate - APC (log) - 1y lag'),
#                   cbind('tx_ma_icsap_l2','Adult Mortality Rate - APC (log) - 2y lag'),
#                   cbind('tx_ma_icsap_l3','Adult Mortality Rate - APC (log) - 3y lag'),
#                   cbind('tx_ma_icsap_l4','Adult Mortality Rate - APC (log) - 4y lag'),
#                   cbind('tx_ma_icsap_l5','Adult Mortality Rate - APC (log) - 5y lag'),
#                   cbind('tx_ma_nicsap_l1','Adult Mortality Rate - non-APC (log) - 1y lag'),
#                   cbind('tx_ma_nicsap_l2','Adult Mortality Rate - non-APC (log) - 2y lag'),
#                   cbind('tx_ma_nicsap_l3','Adult Mortality Rate - non-APC (log) - 3y lag'),
#                   cbind('tx_ma_nicsap_l4','Adult Mortality Rate - non-APC (log) - 4y lag'),
#                   cbind('tx_ma_nicsap_l4','Adult Mortality Rate - non-APC (log) - 5y lag')
# )
# 
# 
# 
# for (i in seq(1,15,1)){
#   var <- var_map[i,1]
#   var_name <- var_map[i,2]
#   print(var_name)
#   
#   reduced_yearly(var,var_name,df,1,1998,-0.01,0.015,0.005)
#   
#   
# }



# 10. Birth and Access
# =================================================================

var_map <- rbind(cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_c_sections','Share of C-Section'),
                 cbind('birth_gest_37plus','Gestation Weeks 37+'),
                 cbind('birth_hospital','Birth at Hospital'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_prenat_0','Prenatal Visits None'),
                 cbind('birth_prenat_1_6','Prenatal Visits 1-6'),
                 cbind('birth_prenat_7_plus','Prenatal Visits 7+'))

if (instrument=="ec29_baseline"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-2,2,0.5,sample = "full",below = below) # ec29baseline
    
  }
  
}


if (instrument=="dist_ec29_baseline"){
  for (i in seq(1,9,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,3,1998,-0.75,2,0.25,sample = "full",below = below) # ec29baseline
    
  }
  
}


if (instrument=="dist_spending_pc_baseline"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.002,0.002,0.0005,sample = "full",below = below) # ec29baseline
    
  }
  
}



if (instrument=="ec29_baseline_below"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-2.5,2.5,0.5,sample = "full",below = below) # ec29baseline
    
  }
  
}


if (instrument=="dist_ec29_baseline_below"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-2.5,2.5,0.5,sample = "full",below = below) # ec29baseline
    
  }
  
}

if (instrument=="dist_spending_pc_baseline_below"){
  for (i in seq(1,7,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    
    reduced_yearly(var,var_name,df,1,1998,-0.002,0.002,0.0005,sample = "full",below = below) # ec29baseline
    
  }
  
}



