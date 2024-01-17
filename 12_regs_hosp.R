#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for hospitalization
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

var_map <- rbind(cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)')
                 # cbind('tx_sih_adult','Adult Hospitalization Rate (pop 40+y * 1000)'),
                 # cbind('tx_sih_adult_icsap','Adult Hospitalization Rate - APC (pop 40+y * 1000)'),
                 # cbind('tx_sih_adult_nicsap','Adult Hospitalization Rate - non-APC (pop 40+y * 1000)')
                 # cbind('tx_sih_25_44','Adult Hospitalization Rate (pop 25-44y * 1000)'),
                 # cbind('tx_sih_25_44_icsap','Adult Hospitalization Rate - APC (pop 25-44y * 1000)'),
                 # cbind('tx_sih_25_44_nicsap','Adult Hospitalization Rate - non-APC (pop 25-44y * 1000)'),
                 # cbind('tx_sih_45_54','Adult Hospitalization Rate (pop 45-54y * 1000)'),
                 # cbind('tx_sih_45_54_icsap','Adult Hospitalization Rate - APC (pop 45-54y * 1000)'),
                 # cbind('tx_sih_45_54_nicsap','Adult Hospitalization Rate - non-APC (pop 45-54y * 1000)')
                 
)



# 3. Run and ouput
# =================================================================

for (i in seq(1,5,1)){
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


write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "hosp_illdefTrend",row.names = F,append = T)



