#######################################################################################################
# Author: Michel Szklo
# October 2022
# 
# This scripts runs regressions for public infrastructure
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

var_map <- rbind(cbind('tx_sih_in_hosp_total','Hospitalization Inflow rate (pop * 1000)'),
                 cbind('tx_sih_in_hosp_icsap','Hospitalization Inflow rate - APC (pop * 1000)'),
                 cbind('tx_sih_in_hosp_nicsap','Hospitalization Inflow rate - non-APC (pop * 1000)'),
                 cbind('tx_sih_out_hosp_total','Hospitalization Outflow rate (pop * 1000)'),
                 cbind('tx_sih_out_hosp_icsap','Hospitalization Outflow rate - APC (pop * 1000)'),
                 cbind('tx_sih_out_hosp_nicsap','Hospitalization Outflow rate - non-APC (pop * 1000)'),
                 cbind('cobertura_plano','Private Insurance Coverage')
                 

)




# 3. Run and ouput
# =================================================================


for (i in 1:7){
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

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "hosp_flow",row.names = F,append = T)
