#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public spending
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

var_map <- rbind(cbind('finbra_desp_o_pcapita','Total Spending per capita'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita'),
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita'),
                 cbind('finbra_desp_outros_area_pcapita','Other Areas Spending per capita'),
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures'),
                 cbind('finbra_reccorr_pcapita','Total Revenue per capita'),
                 cbind('finbra_rectribut_pcapita','Tax Revenue per capita'),
                 cbind('finbra_rectransf_pcapita','Transfers Revenue per capita'),
                 cbind('finbra_rec_outros_pcapita','Other Revenues per capita'),
                 
                 cbind('finbra_desp_pessoal_share','Human Resources Spending (% Total Spending)'),
                 cbind('finbra_desp_investimento_share','Investment Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_nature_share','Other Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_saude_san_share','Health and Sanitation Spending (% Total Spending)'),
                 cbind('finbra_desp_transporte_share','Transport Spending (% Total Spending)'),
                 cbind('finbra_desp_educ_cultura_share','Education and Culture Spending (% Total Spending)'),
                 cbind('finbra_desp_hab_urb_share','Housing and Urban Spending (% Total Spending)'),
                 cbind('finbra_desp_assist_prev_share','Social Assistance Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_area_share','Other Areas Spending (% Total Spending)'),
                 cbind('finbra_rectribut_share','Tax Revenue (% Total Revenue)'),
                 cbind('finbra_rectransf_share','Transfers Revenue (% Total Revenue)'),
                 cbind('finbra_rec_outros_share','Other Revenues (% Total Revenue)'),
                 cbind('siops_desprecpropriosaude_share','Health Spending - Own Resources (% Health Spending)'),
                 cbind('siops_despexrecproprio_share','Health Spending - Transfers (% Health Spending)'),
                 cbind('siops_desppessoal_share','Health Spending - Human Resources (% Health Spending)'),
                 cbind('siops_despinvest_share','Health Spending - Investiment (% Health Spending)'),
                 cbind('siops_despservicoster_share','Health Spending - 3rd parties services (% Health Spending)'),
                 cbind('siops_despoutros_share','Health Spending - other expenditures (% Health Spending)'))



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


for (i in seq(1,39,1)){
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

write.xlsx2(df_table_all %>% as.data.frame(), file = paste0(dir,main_folder,output_file),sheetName = "spending",row.names = F,append = T)






