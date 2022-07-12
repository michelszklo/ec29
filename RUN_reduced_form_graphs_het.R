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


# 1. Load data, folder and heterogeneity set up
# =================================================================
load(paste0(dir,"regs.RData"))


yearly_folder <- "yearly_reduced_form_dist_ec29_ineq/"
df1 <- df_low_inc
df1_name <- "1. Low Inequality"
df2 <- df_high_ineq
df2_name <- "2. High Inequality"



# 2. Spending
# =================================================================


var_map <- rbind(cbind('finbra_reccorr_pcapita','Total Revenue per capita (2010 R$)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (2010 R$)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita (2010 R$)'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (2010 R$)'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (2010 R$)'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_area_pcapita','Other Areas Spending per capita (2010 R$)'),
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'))

# figure 6

for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-7500,2500,1000,"6",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
  
}

# figure 7

for (i in seq(3,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-3000,1000,500,"7",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

# figure 8

for (i in seq(6,11,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-2000,1000,500,"8",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

# figure 9

for (i in c(6,seq(12,14,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-500,800,100,"9",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# figure 10

for (i in seq(15,18,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-150,350,50,"10",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# 3. Coverage and Infra
# =================================================================

var_map <- rbind(cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
                 cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
                 cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
                 cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
                 cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
                 cbind('siab_visit_cha_pcapita','N. of Household Visits (per capita)'),
                 cbind('siab_visit_cha_pacs_pcapita','N. of Household Visits by Community Health Agents (per capita)'),
                 cbind('siab_visit_cha_psf_pcapita','N. of Household Visits by Family Health Agents (per capita)'),
                 cbind('siab_cons_especif_pcapita','N. of Appointments (per capita)'),
                 cbind('siab_cons_especif_pacs_pcapita','N. of Appointments from Community Health Program (per capita)'),
                 cbind('siab_cons_especif_psf_pcapita','N. of Appointments from Family Health Program (per capita)'),
                 
                 cbind('sia_ncnes_amb_mun_pcapita','N. of Health Facilities with Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_acs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Teams (per capita*1000)'),
                 cbind('sia_ncnes_psf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Teams (per capita*1000)'),
                 cbind('sia_ncnes_medcom_pcapita','N. of Health Facilities with Ambulatory Service and Community Doctors (per capita*1000)'),
                 cbind('sia_ncnes_medpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Doctors (per capita*1000)'),
                 cbind('sia_ncnes_enfacs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Nurses (per capita*1000)'),
                 cbind('sia_ncnes_enfpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nurses (per capita*1000)'),
                 cbind('sia_ncnes_outpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nursing Assistants (per capita*1000)'),
                 cbind('leitos_pc',"N. of Hospital Beds (per capita)"),
                 cbind('hospital','Presence of Hospital')
)


# figure 11

for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-0.5,0.5,0.1,"11",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# figure 12
for (i in seq(3,11,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-2,2.5,0.5,"12",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# figure 13
for (i in seq(12,19,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-0.3,0.3,0.1,"13",below = below,weight = "peso_eq",year_cap = 2007) # ec29baseline
}





# 4. Access and Production
# =================================================================

var_map <- rbind(cbind('birth_prenat_0','Prenatal Visits None'),
                 cbind('birth_prenat_1_6','Prenatal Visits 1-6'),
                 cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
                 cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
                 cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_low_skill_mun_pcapita','N. Outpatient Procedures by Low Skilled Workers (per capita)'),
                 cbind('sia_nprod_med_skill_mun_pcapita','N. Outpatient procedures by Mid Skilled Workers (per capita)')
                 
)



# figure 14
for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-0.2,0.2,0.05,"14",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}


# figure 15
for (i in seq(4,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-6,10,2,"15",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

for (i in seq(6,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-6,10,2,"15",below = below,weight = "peso_eq",year_cap = 2007) # ec29baseline
}




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


# figure 16
# for (i in seq(1,3,1)){
#   var <- var_map[i,1]
#   var_name <- var_map[i,2]
#   print(var_name)
#   reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-20,10,5,"16",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
# }

for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-20,10,5,"16",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}


# figure 17
# for (i in seq(12,15,1)){
#   var <- var_map[i,1]
#   var_name <- var_map[i,2]
#   print(var_name)
#   reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-10,10,5,"17",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
# }

for (i in seq(12,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-10,10,5,"17",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}


# figure 18
# for (i in seq(4,11,1)){
#   var <- var_map[i,1]
#   var_name <- var_map[i,2]
#   print(var_name)
#   reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-10,10,5,"18",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
# }

for (i in seq(4,11,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-10,10,5,"18",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}




# 7. Fertility and Birth
# =================================================================

var_map <- rbind(cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
                 cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_premature','Premature Birth'),
                 cbind('birth_sexratio',"Sex Ratio at Birth"))



# Figure 19
for (i in c(1,seq(4,6,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-0.2,0.2,0.05,"19",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}

for (i in seq(2,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het(var,var_name,df1,df1_name,df2,df2_name,3,1998,-1.5,1.5,0.5,"19",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}







