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


yearly_folder <- "eq_weight/"

# 2. Spending
# =================================================================


var_map <- rbind(cbind('finbra_reccorr_pcapita','Total Revenue per capita (2010 R$)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (2010 R$)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                 cbind('finbra_desp_nao_saude_pcapita','All Other Spending per capita (2010 R$)'),
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

# figure B1 (ex 6)

for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-2,3,0.5,"B1",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
  
}

# figure 

# for (i in seq(3,5,1)){
#   var <- var_map[i,1]
#   var_name <- var_map[i,2]
#   print(var_name)
#   reduced_yearly(var,var_name,df,3,1998,-3000,1000,500,"",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
# }

# figure B2 (ex 8 )

for (i in seq(6,12,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-2,3,0.5,"B2",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

# figure 6 (ex 9)
# 
for (i in c(6,seq(13,15,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-4,10,1,"6",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# figure 7 (ex 10)

for (i in seq(16,19,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-4,10,1,"7",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


