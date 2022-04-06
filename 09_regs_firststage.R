#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs IV 1st stage regressions
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



# 2. Functions
# =================================================================

# log transformation of treatment variable and select main variables
transform_select <- function(df,treat){
  
  # log of treatment variable
  ln_treat <- paste0("ln_",treat)
  df[ln_treat] <- sapply(df[treat], function(x) ifelse(x==0,x+0.000001,x))
  df <- df %>% 
    mutate_at(ln_treat,log)
  
  # selecting main variables
  df <- df %>% 
    select(ano, cod_mun, mun_name, cod_uf, uf_y_fe, all_of(treat),all_of(ln_treat),post_ec29_baseline,post_dist_spending_pc_baseline,post_dist_spending_baseline,all_of(controls),pop)
  
  # balanced panel
  # df <- df[complete.cases(df),]
  df <- df[complete.cases(df[,ln_treat]),]

} 



# formats output table for IV regression
table_formatting <- function(df){
  df <- df %>% 
    mutate(sig = ifelse(p.value<=0.01,"***",""),
           sig = ifelse(p.value<=0.05 & p.value>0.01,"**",sig),
           sig = ifelse(p.value<=0.1 & p.value>0.05,"*",sig)) %>% 
    mutate(std.error = paste0("(",round(std.error,digits = 3),")"),
           estimate = paste0(round(estimate,digits = 3),sig)) %>% 
    mutate(std.error = ifelse(nchar(std.error)==3,"(0.000)",std.error))
  
  
  df1 <- df %>% filter(spec==1)
  df1 <-  bind_rows(df1 %>%
                      select(term,estimate),
                    df1 %>% 
                      select(term,std.error) %>% 
                      rename(estimate=std.error),
                    df1 %>% 
                      select(term,f_statistic) %>%
                      mutate(f_statistic = as.character(f_statistic)) %>% 
                      rename(estimate=f_statistic),
                    df1 %>% 
                      select(term,nobs) %>% 
                      mutate(nobs = as.character(nobs)) %>% 
                      rename(estimate=nobs)) %>% 
    mutate(spec = 1)
  df2 <- df %>% filter(spec==2)
  df2 <-  bind_rows(df2 %>%
                      select(term,estimate),
                    df2 %>% 
                      select(term,std.error) %>% 
                      rename(estimate=std.error),
                    df2 %>% 
                      select(term,f_statistic)%>%
                      mutate(f_statistic = as.character(f_statistic)) %>%
                      rename(estimate=f_statistic),
                    df2 %>% 
                      select(term,nobs) %>% 
                      mutate(nobs = as.character(nobs)) %>%
                      rename(estimate=nobs)) %>% 
    mutate(spec = 2)
  
  
  df3 <- df %>% filter(spec==3)
  df3 <-  bind_rows(df3 %>%
                      select(term,estimate),
                    df3 %>% 
                      select(term,std.error) %>% 
                      rename(estimate=std.error),
                    df3 %>% 
                      select(term,f_statistic)%>%
                      mutate(f_statistic = as.character(f_statistic)) %>%
                      rename(estimate=f_statistic),
                    df3 %>% 
                      select(term,nobs) %>% 
                      mutate(nobs = as.character(nobs)) %>%
                      rename(estimate=nobs)) %>% 
    mutate(spec = 3)
  
  df <- bind_rows(df1,df2,df3)
  
}

# 3. Running regs
# =================================================================
df <- df %>% transform_select("finbra_desp_saude_san_pcapita")
df_below <- df_below %>% transform_select("finbra_desp_saude_san_pcapita")
df_above <- df_above %>% transform_select("finbra_desp_saude_san_pcapita")

iv_first(df,"finbra_desp_saude_san_pcapita",1998,"table_all")
iv_first(df_below,"finbra_desp_saude_san_pcapita",1998,"table_below")
iv_first(df_above,"finbra_desp_saude_san_pcapita",1998,"table_above")


# df <- df %>% transform_select("siops_desptotalsaude")
# df_below <- df_below %>% transform_select("siops_desptotalsaude")
# df_above <- df_above %>% transform_select("siops_desptotalsaude")
# 
# iv_first(df,"siops_desptotalsaude",2000,"table_all")
# iv_first(df_below,"siops_desptotalsaude",2000,"table_below")
# iv_first(df_above,"siops_desptotalsaude",2000,"table_above")




# 4. tables
# =================================================================

table_all <- table_all %>% table_formatting() %>% mutate(sample = "all")
table_below <- table_below %>% table_formatting() %>% mutate(sample = "below")
table_above <- table_above %>% table_formatting() %>% mutate(sample = "above")

tables <- bind_rows(table_all,table_below,table_above)



# 5. exporting
# =================================================================


write.xlsx2(tables, file = paste0(dir,main_folder,output_file) ,sheetName = "first_stage",row.names = F,append = T)




