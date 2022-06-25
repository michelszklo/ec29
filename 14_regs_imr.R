#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public Infant Mortality rates
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

var_map <-  rbind(cbind('tx_mi','Infant Mortality Rate'),
                  cbind('tx_mi_icsap','Infant Mortality Rate - APC'),
                  cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC'),
                  cbind('tx_mi_infec','Infant Mortality Rate - Infectious'),
                  cbind('tx_mi_resp','Infant Mortality Rate - Respiratory'),
                  cbind('tx_mi_perinat','Infant Mortality Rate - Perinatal'),
                  cbind('tx_mi_cong','Infant Mortality Rate - Congenital'),
                  cbind('tx_mi_ext','Infant Mortality Rate - External'),
                  cbind('tx_mi_nut','Infant Mortality Rate - Nutritional'),
                  cbind('tx_mi_out','Infant Mortality Rate - Other'),
                  cbind('tx_mi_illdef','Infant Mortality Rate - Ill-Defined'),
                  cbind('tx_mi_fet','Infant Mortality Rate - Fetal'),
                  cbind('tx_mi_24h','Infant Mortality Rate - Within 24h'),
                  cbind('tx_mi_27d','Infant Mortality Rate - 1 to 27 days'),
                  cbind('tx_mi_ano','Infant Mortality Rate - 27 days to 1 year'),
                  cbind('tx_mm',"Maternal Mortality Rate"))



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
                    select(term,estimate,coeff,nobs) %>% filter(coeff=="iv") %>% mutate(item="b"),
                  df %>% 
                    select(term,std.error,coeff,nobs) %>% filter(coeff=="iv") %>% rename(estimate = std.error)%>% mutate(item="se"),
                  df %>%
                    select(term,estimate,coeff,nobs) %>% filter(coeff=="iv_firstterm") %>% mutate(item="b"),
                  df %>% 
                    select(term,std.error,coeff,nobs) %>% filter(coeff=="iv_firstterm") %>% rename(estimate = std.error) %>% mutate(item="se"),
                  df %>%
                    select(term,estimate,coeff,nobs) %>% filter(coeff=="t_firstterm") %>% mutate(item="b"),
                  df %>% 
                    select(term,std.error,coeff,nobs) %>% filter(coeff=="t_firstterm") %>% rename(estimate = std.error) %>% mutate(item="se"),
                  
  ) %>% 
    pivot_wider(id_cols = c("item","term","nobs"),
                names_from = "coeff",
                values_from = "estimate") %>% 
    select(item,term,iv,iv_firstterm,t_firstterm,everything())
}  # formats regression outputs into article format

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
  table_all_1 <- reg_df  %>% table_formating(1) %>% mutate(spec = 1)
  table_all_2 <- reg_df  %>% table_formating(2) %>% mutate(spec = 2)
  table_all_3 <- reg_df  %>% table_formating(3) %>% mutate(spec = 3)
  
  
  table_all <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3)) %>% 
    select(-item)
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects


# 3. Run and ouput
# =================================================================
df <- df %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano<=2000,0,iv),
         iv_firstterm = ifelse(ano<=2000,0,iv_firstterm),
         firstterm = ifelse(ano<=2000,0,firstterm)) 




for (i in seq(1,16,1)){
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

write.xlsx2(df_table_all %>% as.data.frame(), file = paste0(dir,main_folder,output_file),sheetName = "imr",row.names = F,append = T)




