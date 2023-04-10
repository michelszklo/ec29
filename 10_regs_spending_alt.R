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


vars2 <- c("finbra_desp_o_pcapita", "finbra_desp_pessoal_pcapita", "finbra_reccorr_pcapita",
           "finbra_desp_investimento_pcapita", "finbra_desp_outros_nature_pcapita",
           "finbra_desp_saude_san_pcapita", "finbra_desp_nao_saude_pcapita", "finbra_desp_transporte_pcapita",
           "finbra_desp_educ_cultura_pcapita", "finbra_desp_hab_urb_pcapita", "finbra_desp_assist_prev_pcapita",
           "finbra_desp_outros_area_pcapita")

df_teste <- df %>% 
  mutate_at(vars2, ~na_if(., 0)) %>%
  group_by(cod_mun) %>% 
  mutate(check = finbra_desp_o_pcapita/dplyr::lag(finbra_desp_o_pcapita,1)) %>% 
  ungroup() %>%
  select(check,everything())

df_select1 <- df_teste %>% 
  filter(ano<=2002) %>% 
  arrange(check) %>% 
  select(cod_mun) %>%
  unique() %>% 
  slice(1:10)

df_select2 <- df_teste %>% 
  filter(ano<=2002) %>% 
  arrange(desc(check)) %>% 
  select(cod_mun) %>%
  slice(1:10)

df_select <- c(df_select1$cod_mun,df_select2$cod_mun)


df <- df %>% 
  filter(!(cod_mun %in% df_select)) %>%
  filter(cod_mun!=311860) %>% 
  filter(!is.na(dist_ec29_baseline)) %>% 
  mutate_at(vars2, ~na_if(., 0))


df_above = df %>% filter(ec29_baseline>=0.15)
df_below = df %>% filter(ec29_baseline<0.15)

df_second <- df_above
df_first <- df_below



# 2. Define outcomes output name and output functions
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


# 3. Run and ouput
# =================================================================

for (i in seq(1,19,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,1,1998,"peso_pop")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}




# exporting results
# ---------------------

main_folder <- "regs_outputs/finbra_check/"

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "log",row.names = F,append = T)


