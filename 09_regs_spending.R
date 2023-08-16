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
                  
                  cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
                  cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
                  cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)'),
                  cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (log)'),
                  cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (log)'),
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

output_file <- "regression_tables_raw.xlsx"

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "fiscal_response",row.names = F,append = T)






