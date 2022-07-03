#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public spending
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


# 3. Run and ouput
# =================================================================
df <- df %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano<=2000,0,iv)) 
df_below <- df_below %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano<=2000,0,iv)) 
df_above <- df_above %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano<=2000,0,iv)) 

for (i in seq(1,39,1)){
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

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "spending",row.names = F,append = T)


