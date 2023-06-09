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

df_low_ineq <- df_below
df_high_ineq <- df_above

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

var_map <-  rbind(cbind('access_index','Access and Production of Health Services Index','peso_eq'),
                  cbind('access_pc_index','Primary Care Access and Production Index','peso_eq'),
                  cbind('access_npc_index','Non-Primary Care Access and Production Index','peso_eq'),
                  cbind('input_index','Health Inputs Index','peso_eq'),
                  cbind('hr_index','Human Resources Index','peso_eq'),
                  cbind('hospital_index','Hospitals Index','peso_eq'),
                  cbind('birth_index','Birth Outcomes Index','peso_b'),
                  cbind('imr_index','Infant Mortality Index','peso_b'),
                  cbind('birth_others_index','Other Birth Outcomes Index','peso_b')
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


# exporting results
# ---------------------

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "indexes",row.names = F,append = T)




# 4. Regressions Graph
# =================================================================

for (i in seq(1,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.0,1.75,0.25,"20",below = below,weight = w,year_cap = 2010) # ec29baseline
}

for (i in seq(8,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1.0,1.75,0.25,"20",below = below,weight = w,year_cap = 2010) # ec29baseline
}

for (i in 9){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.0,1.75,0.25,"20",below = below,weight = w,year_cap = 2010) # ec29baseline
}



# 4. Regressions Graph - HETEROGENEITY
# =================================================================


df1 <- df_low_ineq
df1_name <- "1. Above"
df2 <- df_high_ineq
df2_name <- "2. Below"

gf_name <- "_abovebelow"

for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df, df1_name, df2_name,3,1998,-2,2,0.5,paste0("20",gf_name),below = below,weight = w,year_cap = 2010) 
}

for (i in seq(4,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df %>% mutate(above_pre_99_dist_ec29_baseline=0, below_pre_99_dist_ec29_baseline),df1_name, df2_name,3,1998,-2,2,0.5,paste0("20",gf_name),below = below,weight = w,year_cap = 2010) 
}

for (i in seq(7,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df, df1_name, df2_name,3,1998,-2,2,0.5,paste0("20",gf_name),below = below,weight = w,year_cap = 2010) 
}



for (i in seq(8,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df, df1_name, df2_name,3,1998,-2,2,0.5,paste0("20",gf_name),below = below,weight = w,year_cap = 2010) 
}


for (i in 9){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  w <- var_map[i,3]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df, df1_name, df2_name,3,1998,-2,2,0.5,paste0("20",gf_name),below = below,weight = w,year_cap = 2010) 
}


df %>% filter(dist_ec29_baseline<0 & dist_ec29_baseline>-0.025) %>% filter(ano==2000)

