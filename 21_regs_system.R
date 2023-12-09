#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for health system
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

var_map <- rbind(cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                 cbind('cobertura_plano','Private Insurance Coverage'),
                 cbind('tx_sih_in_hosp_total','Hospitalization Inflow rate (pop * 1000)'),
                 cbind('tx_sih_in_hosp_icsap','Hospitalization Inflow rate - APC (pop * 1000)'),
                 cbind('tx_sih_in_hosp_nicsap','Hospitalization Inflow rate - non-APC (pop * 1000)'),
                 cbind('tx_sih_out_hosp_total','Hospitalization Outflow rate (pop * 1000)'),
                 cbind('tx_sih_out_hosp_icsap','Hospitalization Outflow rate - APC (pop * 1000)'),
                 cbind('tx_sih_out_hosp_nicsap','Hospitalization Outflow rate - non-APC (pop * 1000)')
               
                 
                 
                 
)



# 3. Run and ouput
# =================================================================

for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output_imr(var,var_name,3,1998,"peso_pop")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}



# 4. exporting results
# =================================================================

output_file <- "regression_tables_raw.xlsx"


write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "system_illdefTrend",row.names = F,append = T)

