#######################################################################################################
# Author: Michel Szklo
# April 2021
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
all_df <- c("df","df_low_ineq","df_high_ineq","df_low_pov","df_high_pov","df_low_hi","df_high_hi","df_first","df_second")

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

var_map <-  rbind(cbind('input_index','Health Inputs Index (Primary Care, HR, Infra)','peso_eq'),
                  cbind('access_index','Access to Health Care Index','peso_eq'),
                  cbind('birth_index','Birth Outcomes Index','peso_b'),
                  cbind('health_index','Health Outputs Index (Hospitalization and Mortality)','peso_b')
          )


# 3. Run and ouput
# =================================================================

for (i in seq(1,3,1)){
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

i <- 4
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


# exporting results
# ---------------------

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "indexes",row.names = F,append = T)




# 4. Regressions Graph
# =================================================================

# inputs
i <- 1
var <- var_map[i,1]
var_name <- var_map[i,2]
w <- var_map[i,3]
print(var_name)
reduced_yearly(var,var_name,df,3,1998,-1,1,0.5,"20",below = below,weight = w,year_cap = 2010)

# access
i <- 2
var <- var_map[i,1]
var_name <- var_map[i,2]
w <- var_map[i,3]
print(var_name)
reduced_yearly(var,var_name,df,3,1998,-1,1,0.5,"20",below = below,weight = w,year_cap = 2010)

# birth
i <- 3
var <- var_map[i,1]
var_name <- var_map[i,2]
w <- var_map[i,3]
print(var_name)
reduced_yearly(var,var_name,df,3,1998,-1,1,0.5,"20",below = below,weight = w,year_cap = 2010)

# Outputs
i <- 4
var <- var_map[i,1]
var_name <- var_map[i,2]
w <- var_map[i,3]
print(var_name)
reduced_yearly_imr(var,var_name,df,3,1998,-1,1,0.5,"20",below = below,weight = w,year_cap = 2010)





for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-200,400,50,"15",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}

for (i in seq(4,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-50,50,10,"15",below = below,weight = "peso_m",year_cap = 2010) # ec29baseline
}






