#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public Elderly Mortality rates
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

var_map <-  rbind(cbind('tx_me','Elderly Mortality Rate'), 
                  cbind('tx_me_icsap','Elderly Mortality Rate - APC'), 
                  cbind('tx_me_nicsap','Elderly Mortality Rate - non-APC'), 
                  cbind('tx_me_circ','Elderly Mortality Rate - Circulatory'), 
                  cbind('tx_me_neop','Elderly Mortality Rate - Neoplasm'), 
                  cbind('tx_me_resp','Elderly Mortality Rate - Respiratory'), 
                  cbind('tx_me_endoc','Elderly Mortality Rate - Endocrine'), 
                  cbind('tx_me_dig','Elderly Mortality Rate - Digestive'), 
                  cbind('tx_me_illdef','Elderly Mortality Rate - Ill-Defined'),
                  cbind('tx_me_out','Elderly Mortality Rate - Other'), 
                  cbind('tx_me_diab','Elderly Mortality Rate - Diabetes'),
                  cbind('tx_me_hyper','Elderly Mortality Rate - Hypertension') 
)



table_formating <- function(df,s){
  df <- df %>% 
    filter(spec==s) %>%
    select(-spec) %>% 
    mutate(term=var_name) %>% 
    mutate(sig = ifelse(p.value<=0.01,"***",""),
           sig = ifelse(p.value<=0.05 & p.value>0.01,"**",sig),
           sig = ifelse(p.value<=0.1 & p.value>0.05,"*",sig)) %>% 
    mutate(std.error = paste0("(",round(std.error,digits = 3),")"),
           estimate = paste0(round(estimate,digits = 3),sig))
  
  df <- bind_rows(df %>%
                    select(term,estimate) %>%
                    rename(`2SLS` = estimate),
                  df %>% 
                    select(term,std.error) %>% 
                    rename(`2SLS` = std.error))
}  # formats regression outputs into article format

graph_formatting <- function(df){
  df <- df %>% 
    mutate(lb = estimate - 1.96*std.error,
           ub = estimate + 1.96*std.error,
           term = var_name) %>% 
    select(term,estimate,lb,ub,spec)
} # formats regression outputs into dataframe ready for graph

regress_output <- function(var,var_name,transform,year_filter){
  
  
  # REDUCED FORM REGRESSION
  # ----------------------------------------
  
  for (data in c("df")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced(var,var_name,d,obj,transform,year_filter) # function for OLS regression
    
    
    print(paste0("Reduced form regs for sample ",data))
  }
  
  # Reduced form final tables
  
  obs_all_1 <- reg_df %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_all_2 <- reg_df %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_all_3 <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("RF" = "2SLS") %>% mutate(spec=1) %>% mutate(obs = obs_all_1)
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("RF" = "2SLS") %>% mutate(spec=2) %>% mutate(obs = obs_all_2)
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("RF" = "2SLS") %>% mutate(spec=3) %>% mutate(obs = obs_all_3)
  
  
  
  table_rf <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3)) 
  
  
  # REDUCED FORM REGRESSION - interaction with electoral term
  # ----------------------------------------
  
  for (data in c("df")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced_elect(var,var_name,d,obj,transform,year_filter) # function for OLS regression
    
    
    print(paste0("Reduced form + electoral term interaction regs for sample ",data))
  }
  
  # Reduced form final tables
  obs_all_1 <- reg_df %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_all_2 <- reg_df %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_all_3 <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  
  
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("RF_elect" = "2SLS") %>% mutate(spec=1) %>% mutate(obs_elect = obs_all_1)
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("RF_elect" = "2SLS") %>% mutate(spec=2) %>% mutate(obs_elect = obs_all_2)
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("RF_elect" = "2SLS") %>% mutate(spec=3) %>% mutate(obs_elect = obs_all_3)
  
  
  
  table_rf_elect <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3)) 
  
  
  # REDUCED FORM REGRESSION - interaction fiscal governance index
  # ----------------------------------------
  
  for (data in c("df")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced_gov(var,var_name,d,obj,transform,year_filter) # function for OLS regression
    
    
    print(paste0("Reduced form + fiscal governance interaction regs for sample ",data))
  }
  
  # Reduced form final tables
  obs_all_1 <- reg_df %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_all_2 <- reg_df %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_all_3 <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  
  
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("RF_gov" = "2SLS") %>% mutate(spec=1) %>% mutate(obs_gov = obs_all_1)
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("RF_gov" = "2SLS") %>% mutate(spec=2) %>% mutate(obs_gov = obs_all_2)
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("RF_gov" = "2SLS") %>% mutate(spec=3) %>% mutate(obs_gov = obs_all_3)
  
  
  
  
  
  table_rf_gov <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3)) 
  
  
  
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  table_all <- cbind.data.frame(table_rf %>% select(term,RF,obs),
                                table_rf_elect %>% select(RF_elect,obs_elect),
                                table_rf_gov %>% select(RF_gov,obs_gov),
                                table_rf %>% select(`spec`))
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects



# 3. Run and ouput
# =================================================================
df <- df %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano<=2000,0,iv),
         iv_elect=ifelse(ano<=2000,0,iv_elect),
         iv_gov=ifelse(ano<=2000,0,iv_gov)) 



for (i in seq(1,12,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998)
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
 
  } else {
    
    df_table_all <- table_all

  }
  
}


# exporting results
# ---------------------

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "emr",row.names = F,append = T)





