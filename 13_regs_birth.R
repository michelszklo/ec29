#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for birth outcomes
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

var_map <- rbind(cbind('birth_prenat_ig','Prenatal Visits - Ignored'),
                 cbind('birth_prenat_0','Prenatal Visits -  None'),
                 cbind('birth_prenat_1_6','Prenatal Visits - 1-6'),
                 cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
                 cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
                 cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_premature','Premature Birth'),
                 cbind('birth_sexratio',"Sex Ratio at Birth"),
                 cbind('birth_c_sections','Share of C-Section'),
                 cbind('birth_hospital','Birth at Hospital'))



# 3. Run and ouput
# =================================================================

for (i in seq(1,12,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
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


write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "birth",row.names = F,append = T)



