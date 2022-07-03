#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public Infant Adult rates
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

var_map <-  rbind(
  cbind('tx_ma3','25-59y Mortality Rate'),
  cbind('tx_ma3_icsap','25-59y Mortality Rate - APC'),
  cbind('tx_ma3_nicsap','25-59y Mortality Rate - non-APC'),
  cbind('tx_ma3_circ','25-59y Mortality Rate - Circulatory'),
  cbind('tx_ma3_neop','25-59y Mortality Rate - Neoplasm'),
  cbind('tx_ma3_resp','25-59y Mortality Rate - Respiratory'),
  cbind('tx_ma3_infec','25-59y Mortality Rate - Infectious'),
  cbind('tx_ma3_ext','25-59y Mortality Rate - External'),
  cbind('tx_ma3_dig','25-59y Mortality Rate - Digestive'),
  cbind('tx_ma3_illdef','25-59y Mortality Rate - Ill-Defined'),
  cbind('tx_ma3_out','25-59y Mortality Rate - Other'), #
  cbind('tx_ma3_diab','25-59y Mortality Rate - Diabetes'),
  cbind('tx_ma3_hyper','25-59y Mortality Rate - Hypertension'),
  
  cbind('tx_ma4','25-39y Mortality Rate'),
  cbind('tx_ma4_icsap','25-39y Mortality Rate - APC'),
  cbind('tx_ma4_nicsap','25-39y Mortality Rate - non-APC'),
  cbind('tx_ma4_circ','25-39y Mortality Rate - Circulatory'),
  cbind('tx_ma4_neop','25-39y Mortality Rate - Neoplasm'),
  cbind('tx_ma4_resp','25-39y Mortality Rate - Respiratory'),
  cbind('tx_ma4_infec','25-39y Mortality Rate - Infectious'),
  cbind('tx_ma4_ext','25-39y Mortality Rate - External'),
  cbind('tx_ma4_dig','25-39y Mortality Rate - Digestive'),
  cbind('tx_ma4_illdef','25-39y Mortality Rate - Ill-Defined'),
  cbind('tx_ma4_out','25-39y Mortality Rate - Other'), #
  cbind('tx_ma4_diab','25-39y Mortality Rate - Diabetes'),
  cbind('tx_ma4_hyper','25-39y Mortality Rate - Hypertension'),
  
  cbind('tx_ma2','40-59y Mortality Rate'),
  cbind('tx_ma2_icsap','40-59y Mortality Rate - APC'),
  cbind('tx_ma2_nicsap','40-59y Mortality Rate - non-APC'),
  cbind('tx_ma2_circ','40-59y Mortality Rate - Circulatory'),
  cbind('tx_ma2_neop','40-59y Mortality Rate - Neoplasm'),
  cbind('tx_ma2_resp','40-59y Mortality Rate - Respiratory'),
  cbind('tx_ma2_infec','40-59y Mortality Rate - Infectious'),
  cbind('tx_ma2_ext','40-59y Mortality Rate - External'),
  cbind('tx_ma2_dig','40-59y Mortality Rate - Digestive'),
  cbind('tx_ma2_illdef','40-59y Mortality Rate - Ill-Defined'),
  cbind('tx_ma2_out','40-59y Mortality Rate - Other'), #
  cbind('tx_ma2_diab','40-59y Mortality Rate - Diabetes'),
  cbind('tx_ma2_hyper','40-59y Mortality Rate - Hypertension')
  
)

# infec dig


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


for (i in seq(1,13,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998,"peso_a")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}

for (i in seq(14,26,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998,"peso_a1")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}

for (i in seq(27,39,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998,"peso_a2")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}



# exporting results
# ---------------------

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "amr",row.names = F,append = T)







