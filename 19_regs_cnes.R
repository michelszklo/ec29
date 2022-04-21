#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts inputs CNES data and runs regressions for birth outcomes
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


# 2. selecting variables
# =================================================================

vars <- c("ano","cod_mun","uf","pop","finbra_desp_saude_san_pcapita",
          "iv","ec29_baseline","dist_ec29_baseline","dist_spending_pc_baseline",
          "ec29_baseline_below","dist_ec29_baseline_below","dist_spending_pc_baseline_below",
          grep("ano_2005",names(df),value = T))

df <- df %>% 
  select(all_of(vars))

df_above <- df_above %>% 
  select(all_of(vars))
  

df_below <- df_below %>% 
  select(all_of(vars))



# 3. loading and merging CNES data
# =================================================================

CNES <- read.csv(paste0(dir,"data/CNES/CNES_BR.csv")) %>% rename(ano = year)
cnes_vars <- grep("cnes",names(CNES),value = T)
cnes_vars_pcapita_sinh <- sapply(cnes_vars, function(x) paste0(x,"_pcapita"), simplify = "array", USE.NAMES = F)
CNES[cnes_vars_pcapita_sinh] <- CNES[cnes_vars]


avg_2005_2010 <- function(df){
  df <- df %>% 
    left_join(CNES, by = c("cod_mun","ano")) %>% 
    filter(ano>=2005 & ano <=2010) %>% 
    
    
    mutate_at(cnes_vars_pcapita_sinh,`/`,quote(pop)) %>% 
    mutate_at(cnes_vars_pcapita_sinh,`*`,1000000) %>% 
    group_by(cod_mun) %>% 
    mutate_at(cnes_vars_pcapita_sinh, function(x) mean(x,na.rm = T)) %>% 
    ungroup() %>% 
    filter(ano==2005) %>% 
    mutate_at(cnes_vars_pcapita_sinh, function(x) asinh(x))
}


df <- df %>% avg_2005_2010()

df_above <- df_above %>% avg_2005_2010()


df_below <- df_below %>% avg_2005_2010()



# 4. 1st stage Regressions
# =================================================================

# first stage specification
controls_cs <- grep("2005",controls,value = T)

spec1_cs_1st <- paste(" ~ ","iv"," | 0 | 0 | cod_mun")
spec2_cs_1st <- paste(" ~ ","iv"," | uf | 0 | cod_mun")
spec3_cs_1st <- paste(" ~ ","iv"," + ", paste(controls_cs, collapse = " + ")," | uf | 0 | cod_mun")

# formats output table for 1st stage regression
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

# regression function
iv_first_cs <- function(df,treat,year_filter,obj_name){
  
  df_reg <- df %>% filter(ano>=year_filter)
  df_reg["iv"] <- df[instrument]
  
  ln_treat <- paste0("ln_",treat)
  df_reg[ln_treat] <- sapply(df_reg[treat], function(x) ifelse(x==0,x+0.000001,x))
  df_reg <- df_reg %>% 
    mutate_at(ln_treat,log)
  
  for (spec in c(1,2,3)){
    
    spec_first <-get(paste0("spec",spec,"_cs_1st"))
    
    regformula <- as.formula(paste(ln_treat,spec_first))
    
    fit <- felm(regformula, data = df_reg, weights = df_reg$pop,exactDOF = T)
    
    if(spec==1){
      out <- cbind(fit %>% broom::tidy() %>% slice(2), fit %>% broom::glance() %>% select(statistic,nobs) %>% rename(f_statistic = statistic))
      
    }else{
      out <- cbind(fit %>% broom::tidy() %>% slice(1), fit %>% broom::glance() %>% select(statistic,nobs) %>% rename(f_statistic = statistic))
      
    }
    out <- out %>%
      mutate(spec=spec)
    
    if(spec==1){
      table <- out
    } else{
      table <- rbind(table,out)
    }
    
  }
  
  assign(obj_name,table,envir = .GlobalEnv)
  
}


# running regs
iv_first_cs(df,"finbra_desp_saude_san_pcapita",1998,"table_all")
iv_first_cs(df_above,"finbra_desp_saude_san_pcapita",1998,"table_above")
iv_first_cs(df_below,"finbra_desp_saude_san_pcapita",1998,"table_below")




# 5. First Stage Tables
# =================================================================


table_all <- table_all %>% table_formatting() %>% mutate(sample = "all")
table_below <- table_below %>% table_formatting() %>% mutate(sample = "below")

if(below!=1){
  table_above <- table_above %>% table_formatting() %>% mutate(sample = "above")
}

if(below==1){
  tables <- bind_rows(table_all,table_below)
}else{
  tables <- bind_rows(table_all,table_below,table_above)
}


# 6. First Stage export
# =================================================================

if(below==1){
  write.xlsx2(tables, file = paste0(dir,main_folder,output_file) ,sheetName = "first_stage_cs_b",row.names = F,append = T)
  
} else{
  write.xlsx2(tables, file = paste0(dir,main_folder,output_file) ,sheetName = "first_stage_cs",row.names = F,append = T)
}


# 7. IV Regressions
# =================================================================

# OLS specifications
# ------------------------------------------------

spec1_cs <- paste(" | 0 | 0 | cod_mun")
spec2_cs <- paste(" | uf | 0 | cod_mun")
spec3_cs <- paste(" + ", paste(controls_cs, collapse = " + ")," | uf | 0 | cod_mun")

ols_cs <- function(outcome,treat,df,regression_output,transform,year_filter){
  
  df_reg <- df
  ln_outcome <- outcome
  
  # log of treatment variable
  ln_treat <- paste0("ln_",treat)
  df_reg[ln_treat] <- sapply(df_reg[treat], function(x) ifelse(x==0,x+0.000001,x))
  df_reg <- df_reg %>% 
    mutate_at(ln_treat,log)
  
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_treat]),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3)){
    
    spec_ols<- get(paste0("spec",spec,"_cs"))
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome," ~ ",ln_treat,spec_ols))
    fit <- felm(regformula, data = df_reg, weights = df_reg$pop,exactDOF = T)
    
    if(spec==1){
      out <- cbind(fit %>% broom::tidy() %>% slice(2),fit %>% broom::glance() %>% select(nobs))
      
    }else{
      out <- cbind(fit %>% broom::tidy() %>% slice(1),fit %>% broom::glance() %>% select(nobs))
    }
    
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(term = ln_outcome)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
}


# Reduced form specifications
# ------------------------------------------------
# uses same spec as first stage regressions

reduced_cs <- function(outcome,var_name,df,regression_output,transform,year_filter){
  
  df_reg <- df
  ln_outcome <- outcome
  
  # outcome variable transformation
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_cs_1st"))
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg, weights = df_reg$pop,exactDOF = T)
    
    if(spec==1){
      out <- cbind(fit %>% broom::tidy() %>% slice(2),fit %>% broom::glance() %>% select(nobs))
      
    } else {
      out <- cbind(fit %>% broom::tidy() %>% slice(1),fit %>% broom::glance() %>% select(nobs))
      
    }
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(term = ln_outcome)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
}



# IV specifications
# ------------------------------------------------

spec1_iv_cs <- paste(" ~ 0","| 0 | (")
spec2_iv_cs <- paste(" ~ 0","| uf | (")
spec3_iv_cs <- paste(" ~ ", " + ",paste(controls_cs, collapse = " + ")," | uf | (")


iv_cs <- function(outcome,treat,df,regression_output,transform,year_filter){
  
  
  
  df_reg <- df
  ln_outcome <- outcome
  
  # log of treatment variable
  ln_treat <- paste0("ln_",treat)
  df_reg[ln_treat] <- sapply(df_reg[treat], function(x) ifelse(x==0,x+0.000001,x))
  df_reg <- df_reg %>% 
    mutate_at(ln_treat,log)
  
  
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_treat]),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3)){
    
    # regression specs
    spec_reg <-get(paste0("spec",spec,"_iv_cs"))
    regformula <- as.formula(paste(ln_outcome,spec_reg,ln_treat,spec_instrument))
    
    # regression model
    fit <- felm(regformula, data = df_reg, weights = df_reg$pop ,exactDOF = T)
    
    # output

    out <- cbind(fit %>% broom::tidy() %>% slice_tail(),fit %>% broom::glance() %>% select(nobs))
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(term = ln_outcome)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
  
  
}



# Variables and output functions
# ------------------------------------------------

var_map <- rbind(cbind('cnes_st_all_mun_pcapita','Municipal Health Facilities (per capita * 1mi)'),
                 cbind('cnes_st_posto_mun_pcapita','Municipal Health Center (per capita * 1mi)'),
                 cbind('cnes_st_consultorio_mun_pcapita','Municipal Consulting Rooms (per capita * 1mi)'),
                 cbind('cnes_st_clinicaesp_mun_pcapita','Municipal Specialized Health Clinics (per capita * 1mi)'),
                 cbind('cnes_st_diagnos_mun_pcapita','Municipal Diagnostic Center (per capita * 1mi)'),
                 cbind('cnes_st_all_private_pcapita','Private Health Facilities (per capita * 1mi)'),
                 cbind('cnes_st_posto_private_pcapita','Private Health Center (per capita * 1mi)'),
                 cbind('cnes_st_consultorio_private_pcapita','Private Consulting Rooms (per capita * 1mi)'),
                 cbind('cnes_st_clinicaesp_private_pcapita','Private Specialized Health Clinics (per capita * 1mi)'),
                 cbind('cnes_st_diagnos_private_pcapita','Private Diagnostic Center (per capita * 1mi)'),
                 cbind('cnes_st_all_public_pcapita','Public Health Facilities (per capita * 1mi)'),
                 cbind('cnes_st_posto_public_pcapita','Public Health Center (per capita * 1mi)'),
                 cbind('cnes_st_consultorio_public_pcapita','Public Consulting Rooms (per capita * 1mi)'),
                 cbind('cnes_st_clinicaesp_public_pcapita','Public Specialized Health Clinics (per capita * 1mi)'),
                 cbind('cnes_st_diagnos_public_pcapita','Public Diagnostic Center (per capita * 1mi)'),
                 cbind('cnes_lt_private_sus_funded_pcapita','Private Hospital Beds funded by SUS (per capita * 1mi)'),
                 cbind('cnes_lt_mun_pcapita','Municipal Hospital Beds (per capita * 1mi)'),
                 cbind('cnes_lt_public_pcapita','Public Hospital Beds (per capita * 1mi)'),
                 cbind('cnes_eq_mun_pcapita','Municipal Health Equipments (per capita * 1mi)'),
                 cbind('cnes_eq_private_pcapita','Private Health Equipments (per capita * 1mi)'),
                 cbind('cnes_eq_public_pcapita','Public Health Equipments (per capita * 1mi)')
                 )

# write regression and regression output all and below

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

regress_output <- function(var,var_name,transform,year_filter){
  
  # IV REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_above","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    iv_cs(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for IV regression and bootstrap estimating of SE
    
    print(paste0("IV regs for sample ",data))
  } 
  
  # 2sls final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("2SLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("2SLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("2SLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("2SLS_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("2SLS_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("2SLS_below" = "2SLS") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("2SLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("2SLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("2SLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_2sls <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                          bind_rows(table_below_1,table_below_2,table_below_3),
                          bind_rows(table_above_1,table_above_2,table_above_3)) 
  

  
  # OLS REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_above","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    ols_cs(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for OLS regression
    
    print(paste0("OLS regs for sample ",data))
  }
  
  # OLS final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("OLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("OLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("OLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("OLS_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("OLS_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("OLS_below" = "2SLS") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("OLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("OLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("OLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_ols <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                         bind_rows(table_below_1,table_below_2,table_below_3),
                         bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  
  # REDUCED FORM REGRESSION
  # ----------------------------------------
  
  for (data in c("df","df_above","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced_cs(var,var_name,d,obj,transform,year_filter) # function for OLS regression
    
    
    print(paste0("Reduced form regs for sample ",data))
  }
  
  # Reduced form final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("RF_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("RF_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("RF_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("RF_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("RF_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("RF_below" = "2SLS") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("RF_above" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("RF_above" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("RF_above" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_rf <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                        bind_rows(table_below_1,table_below_2,table_below_3),
                        bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  table_all <- cbind.data.frame(table_ols %>% select(term,OLS_full),
                                table_2sls %>% select(`2SLS_full`),
                                table_rf %>% select(`RF_full`),
                                table_ols %>% select(OLS_below),
                                table_2sls %>% select(`2SLS_below`),
                                table_rf %>% select(`RF_below`),
                                table_ols %>% select(OLS_above),
                                table_2sls %>% select(`2SLS_above`),
                                table_rf %>% select(`RF_above`),
                                table_rf %>% select(`spec`))
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 

}  # runs regressions and output objects

regress_output_below <- function(var,var_name,transform,year_filter){
  
  # IV REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    iv_cs(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for IV regression and bootstrap estimating of SE
    
    print(paste0("IV regs for sample ",data))
  } 
  
  # 2sls final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("2SLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("2SLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("2SLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("2SLS_below" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("2SLS_below" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("2SLS_below" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  table_2sls <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                          bind_rows(table_below_1,table_below_2,table_below_3)) 
  

  # OLS REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    ols_cs(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for OLS regression
    
    print(paste0("OLS regs for sample ",data))
  }
  
  # OLS final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("OLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("OLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("OLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("OLS_below" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("OLS_below" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("OLS_below" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  table_ols <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                         bind_rows(table_below_1,table_below_2,table_below_3)) 
  
  
  # REDUCED FORM REGRESSION
  # ----------------------------------------
  
  for (data in c("df","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced_cs(var,var_name,d,obj,transform,year_filter) # function for OLS regression
    
    
    print(paste0("Reduced form regs for sample ",data))
  }
  
  # Reduced form final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("RF_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("RF_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("RF_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("RF_below" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("RF_below" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("RF_below" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  table_rf <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                        bind_rows(table_below_1,table_below_2,table_below_3)) 
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  table_all <- cbind.data.frame(table_ols %>% select(term,OLS_full),
                                table_2sls %>% select(`2SLS_full`),
                                table_rf %>% select(`RF_full`),
                                table_ols %>% select(OLS_below),
                                table_2sls %>% select(`2SLS_below`),
                                table_rf %>% select(`RF_below`),
                                table_rf %>% select(`spec`))
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 

}  # runs regressions and output objects



# 8. Run and output
# =================================================================

for (i in seq(1,21,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  
  if(below==1){
    
    regress_output_below(var,var_name,1,1998)
    
    
    if(exists("df_table_all")){
      df_table_all <- rbind(df_table_all,table_all)

    } else {
      
      df_table_all <- table_all

    }
    
  }else{
    regress_output(var,var_name,1,1998)
    
    
    if(exists("df_table_all")){
      df_table_all <- rbind(df_table_all,table_all)
      
    } else {
      
      df_table_all <- table_all
    }
    
  }
  
  
  
}


# 9. Saving
# =================================================================

if(below==1){
  write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file) ,sheetName = "cnes_cs_b",row.names = F,append = T)
  
} else{
  write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file) ,sheetName = "cnes_cs",row.names = F,append = T)
}

