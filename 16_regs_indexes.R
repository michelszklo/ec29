#######################################################################################################
# Author: Michel Szklo
# September 2022
# 
# This scripts runs regressions for variables index created based on Anderson (2008)
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
index <- data.frame(read.dta13("C:/Users/Michel/Documents/GitHub/ec29/indexes.dta"))


# merge indexes to main df
all_df <- c("df")

imerge <- function(df){
  df <- df %>% 
    left_join(index, by = c("ano","cod_mun","cod_uf"))
}

for(d in all_df){
  df_merge <- get(d)
  df_merge <- df_merge %>% imerge()
  assign(d,df_merge,envir = .GlobalEnv)
}

# 2. Define outcomes output name and output functions
# =================================================================

var_map <-  rbind(cbind('access_index','Access and Production of Health Services Index','peso_pop'),
                  cbind('access_pc_index','Primary Care Access and Production Index','peso_pop'),
                  cbind('access_npc_index','Non-Primary Care Access and Production Index','peso_pop'),
                  cbind('input_index','Health Inputs Index','peso_pop'),
                  cbind('hr_index','Human Resources Index','peso_pop'),
                  cbind('hospital_index','Hospitals Index','peso_pop'),
                  cbind('birth_index','Birth Outcomes Index','peso_pop'),
                  cbind('imr_index','Infant Mortality Index','peso_pop'),
                  cbind('birth_others_index','Other Birth Outcomes Index','peso_pop')
)


# 3. Run and ouput
# =================================================================

for (i in seq(1,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  
  regress_output(var,var_name,3,1998,w)
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}

i <- 8
var <- var_map[i,1]
var_name <- var_map[i,2]
w <- var_map[i,3]
print(var_name)

regress_output_imr(var,var_name,3,1998,w)


if(exists("df_table_all")){
  df_table_all <- rbind(df_table_all,table_all)
  
} else {
  
  df_table_all <- table_all
  
}



i <- 9
var <- var_map[i,1]
var_name <- var_map[i,2]
w <- var_map[i,3]
print(var_name)

regress_output(var,var_name,3,1998,w)


if(exists("df_table_all")){
  df_table_all <- rbind(df_table_all,table_all)
  
} else {
  
  df_table_all <- table_all
  
}


# 4. exporting results
# =================================================================

output_file <- "regression_tables_raw.xlsx"

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "indexes",row.names = F,append = T)

