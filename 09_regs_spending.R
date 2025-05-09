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

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

DAT <- paste0(dir,"data/processed/")
TAB <- paste0(dir,"results/tables/")
FIG <- paste0(dir,"results/figures/")

# ------------------------------------


# 1. Load data
# =================================================================
load(paste0(DAT,"regs.RData"))


# dropping municipalities with outliers in spending
outliers <- df %>% 
  mutate(s = log(finbra_desp_o_pcapita)) %>% 
  select(s,everything())

ndesv <- 5
x <- mean(outliers$s, na.rm = T)
sd <- sd(outliers$s, na.rm = T)
outliers <- outliers %>% 
  mutate(s1 = x - sd * ndesv,
         s2 = x + sd * ndesv) %>% 
  filter(s<=s1 | s>=s2) %>% 
  select(cod_mun) %>% 
  unique()

outliers <- outliers$cod_mun

df <- df %>% 
  filter(!(cod_mun %in% outliers))



# creating above and below target sample

df_above = df %>% filter(ec29_baseline>=0.15)
df_below = df %>% filter(ec29_baseline<0.15)

df_second <- df_above
df_first <- df_below


df_balance_finbra <- df[complete.cases(df[c('finbra_recorc_pcapita',
                                            'finbra_desp_o_pcapita',
                                            'finbra_desp_saude_san_pcapita',
                                            'finbra_desp_nao_saude_pcapita',
                                            'finbra_despsocial_pcapita',
                                            'finbra_desp_outros_area_pcapita',
                                            'gdp_mun_pcapita',
                                            'pbf_pcapita',
                                            't_tx_mi_baseline',
                                            'dist_ec29_baseline')]) & 
                          df$finbra_recorc_pcapita != 0 & 
                          df$finbra_desp_o_pcapita != 0 & 
                          df$finbra_desp_saude_san_pcapita != 0 & 
                          df$finbra_desp_nao_saude_pcapita != 0 & 
                          df$finbra_despsocial_pcapita != 0 & 
                          df$finbra_desp_outros_area_pcapita != 0, ]
df_balance_siops <- df[complete.cases(df[c('siops_despsaude_pcapita',
                                           'siops_desprecpropriosaude_pcapita',
                                           'siops_despexrecproprio_pcapita',
                                           'siops_desppessoal_pcapita',
                                           'siops_despinvest_pcapita',
                                           'siops_despservicoster_pcapita',
                                           'siops_despoutros_pcapita',
                                           'gdp_mun_pcapita',
                                           'pbf_pcapita',
                                           't_tx_mi_baseline',
                                           'dist_ec29_baseline')]) & 
                         df$siops_despsaude_pcapita != 0 & 
                         df$siops_desprecpropriosaude_pcapita != 0 & 
                         df$siops_despexrecproprio_pcapita != 0 & 
                         df$siops_desppessoal_pcapita != 0 & 
                         df$siops_despinvest_pcapita != 0 & 
                         df$siops_despservicoster_pcapita != 0 & 
                         df$siops_despoutros_pcapita>0, ]



# 2. Define outcomes output name and output functions
# =================================================================


# logs
var_map1 <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (log)'),
                  cbind('finbra_reccorr_pcapita','Current Revenue per capita (log)'),
                  
                  cbind('finbra_desp_o_pcapita','Total Spending per capita (log)'),
                  
                  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
                  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)'),
                  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (log)'),
                  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (log)'),
                  
                  cbind('finbra_impostos_total_pcapita', 'Total Tax Revenue (log)'),
                  cbind('finbra_iptu_pcapita', 'Property Tax Revenue (log)'),
                  cbind('finbra_iss_pcapita', 'Services Tax Revenue (log)'),
                  
                  cbind('finbra_passivo_pcapita','Total Liabilities (log)'),
                  cbind('finbra_passivo_pcapita','Financial Liabilities (log)'),
                  
                  cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
                  cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
                  cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)'),
                  cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (log)'),
                  cbind('siops_despinvest_pcapita','Health Spending per capita - Investment (log)'),
                  cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (log)'),
                  cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (log)'))



# per capita level
var_map2 <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (2010 R$)'),
                  cbind('finbra_reccorr_pcapita','Current Revenue per capita (2010 R$)'),
                  
                  cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                  
                  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (2010 R$)'),
                  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (2010 R$)'),
                  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (2010 R$)'),
                  
                  cbind('finbra_impostos_total_pcapita', 'Total Tax Revenue (2010 R$)'),
                  cbind('finbra_iptu_pcapita', 'Property Tax Revenue (2010 R$)'),
                  cbind('finbra_iss_pcapita', 'Services Tax Revenue (2010 R$)'),
                  
                  cbind('finbra_passivo_pcapita','Total Liabilities (2010 R$)'),
                  cbind('finbra_passivo_pcapita','Financial Liabilities (2010 R$)'),
                  
                  cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                  cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                  cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (2010 R$)'),
                  cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (2010 R$)'),
                  cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                  cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (2010 R$)'),
                  cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (2010 R$)'))


# 3. Run and ouput
# =================================================================

# log
for (i in seq(1,14,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  
  regress_output(var,var_name,1,1998,"peso_pop")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}

# level per capita
for (i in seq(1,14,1)){
  var <- var_map2[i,1]
  var_name <- var_map2[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998,"peso_pop")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}





# 4. exporting results
# =================================================================

output_file <- "regression_tables_raw_28Aug2024.xlsx"

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "fiscal_response",row.names = F,append = T)




# log
df_save <- df
df <- df_balance_finbra
for (i in seq(1,14,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  
  regress_output(var,var_name,1,1998,"peso_pop")
  if(exists("df_table_f")){
    df_table_f <- rbind(df_table_f,table_all)
  } else {
    df_table_f <- table_all
  }
}
write.xlsx2(df_table_f, file = paste0(dir,main_folder,output_file),sheetName = "fiscal_response_finbra",row.names = F,append = T)

df <- df_balance_siops
for (i in seq(1,14,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  
  regress_output(var,var_name,1,1998,"peso_pop")
  if(exists("df_table_s")){
    df_table_s <- rbind(df_table_s,table_all)
  } else {
    df_table_s <- table_all
  }
}
write.xlsx2(df_table_s, file = paste0(dir,main_folder,output_file),sheetName = "fiscal_response_siops",row.names = F,append = T)
df <- df_save

