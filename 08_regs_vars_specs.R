#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts generates variables for the regression and set the regression specifications
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
            'modelsummary')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)

path <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"


# 1. Load data frame
# =================================================================
raw <- readRDS(paste0(path,"data/CONSOL_DATA.RDS"))


df <- raw %>% 
  # adding data variable for calculating lag of variables
  mutate(ano = as.character(ano)) %>% 
  mutate(data = as.Date(ano, format=c('%Y'))) %>%
  mutate(ano = as.numeric(ano)) %>% 
  # cleaning id variables
  select(-c(nome_mun,UF,cod_uf.y,cod_uf.x,estado)) %>% 
  mutate(cod_uf = as.numeric(substr(as.character(cod_mun),1,2))) %>% 
  select(ano, cod_mun, mun_name, cod_uf,uf, region, pop, everything()) %>% 
  # replace 0 spending values into NAs
  mutate(finbra_desp_saude_san_pcapita = ifelse(finbra_desp_saude_san_pcapita==0,NA,finbra_desp_saude_san_pcapita),
         siops_rimpostosetransfconst = ifelse(siops_rimpostosetransfconst==0,NA,siops_rimpostosetransfconst))
 



# 2. Regression variables
# =================================================================


df <- df %>% 
  # state year fixed effects
  mutate(uf_y_fe = as.factor(paste0(ano,"_",uf))) %>% 
  # distance to the EC29 target spending in the baseline
  mutate(dist_ec29 = (siops_pct_recproprios_ec29 - 0.15)) %>% 
  mutate(dist_ec29_baseline = ifelse(ano==2000,dist_ec29,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_ec29_baseline = mean(dist_ec29_baseline, na.rm = T)) %>%
  ungroup() %>% 
  # distance to the EC29 target as per capita spending
  mutate(dist_spending_pc = -((siops_pct_recproprios_ec29 - 0.15)*(siops_rimpostosetransfconst/pop))) %>% 
  mutate(dist_spending_pc_baseline = ifelse(ano==2000,dist_spending_pc,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_spending_pc_baseline = mean(dist_spending_pc_baseline, na.rm = T)) %>% 
  ungroup() %>% 
  # baseline year
  mutate(baseline = 2000) %>% 
  # post dummy
  mutate(post = ifelse(ano>baseline, 1, 0)) %>% 
  # yearly pre and post dummies
  mutate(pre_98 = ifelse(ano==1998,1,0)) %>% 
  mutate(pre_99 = ifelse(ano==1999,1,0)) %>% 
  mutate(post_00 = 0) %>% 
  mutate(post_01 = ifelse(ano==2001,1,0)) %>% 
  mutate(post_02 = ifelse(ano==2002,1,0)) %>% 
  mutate(post_03 = ifelse(ano==2003,1,0)) %>% 
  mutate(post_04 = ifelse(ano==2004,1,0)) %>% 
  mutate(post_05 = ifelse(ano==2005,1,0)) %>% 
  mutate(post_06 = ifelse(ano==2006,1,0)) %>% 
  mutate(post_07 = ifelse(ano==2007,1,0)) %>% 
  mutate(post_08 = ifelse(ano==2008,1,0)) %>% 
  mutate(post_09 = ifelse(ano==2009,1,0)) %>% 
  mutate(post_10 = ifelse(ano==2010,1,0)) 

  


# baseline controls
controlvars <- c("espvida", "e_anosestudo", "t_analf18m","pmpob","rdpc","gini",
                 "sewage_gen_network","garbage_coll_service","water_gen_network", "elect_access", "urb")
controlsvar_baseline <- sapply(controlvars, function(x) paste0(x,"_baseline"), simplify = "array", USE.NAMES = F)
df[controlsvar_baseline] <- df[controlvars]


df <- df %>%
  mutate_at(controlsvar_baseline, .funs = list(~ ifelse(ano==2000,.,NA))) %>%
  unnest(all_of(controlsvar_baseline)) %>% 
  group_by(cod_mun) %>% 
  mutate_at(controlsvar_baseline, function(x) mean(x,na.rm = T)) %>% 
  ungroup()

df <- df %>% 
  # creating year dummies
  dummy_cols(select_columns = "ano", ignore_na = TRUE)

# interacting year dummies with controls
yeardummies <- grep("^ano_",names(df),value = T)

for(i in seq.int(1,length(controlsvar_baseline))){
  var <- as.symbol(controlsvar_baseline[i])
  interact_vars <- sapply(yeardummies, function(x) paste0(x,"_",var), simplify = "array", USE.NAMES = F)
  df[interact_vars] <- df[yeardummies]
  df <- df %>% 
    mutate_at(interact_vars,`*`,quote(df[var])) %>% 
    mutate_at(interact_vars, function(x) unlist(x))
}


# 3. Setting regression samples
# =================================================================

# sample 1: municipalities below target (positive distance to the target)
# ------------------------------------------------------------------------
df_below <- df %>%
  filter(dist_ec29_baseline<0) #  %>%
  # mutate(ln_dist_spending_pc_baseline = log(dist_spending_pc_baseline))

# interacting dummies with treatment (dist_ec29_baseline)

dummies <- c(grep("^pre",names(df_below),value = T),grep("^post_",names(df), value = T))
# yeartreat_dummies <- sapply(dummies, function(x) paste0(x,"_ln_dist_spending_pc_baseline"), simplify = "array", USE.NAMES = F)
yeartreat_dummies <- sapply(dummies, function(x) paste0(x,"_dist_spending_pc_baseline"), simplify = "array", USE.NAMES = F)
df_below[yeartreat_dummies] <- df_below[dummies]

df_below <- df_below %>%
  # mutate_at(yeartreat_dummies, `*`,quote(ln_dist_spending_pc_baseline)) %>%
  # unnest(all_of(yeartreat_dummies)) %>%
  # mutate(post_ln_dist_spending_pc_baseline = post * ln_dist_spending_pc_baseline)
  mutate_at(yeartreat_dummies, `*`,quote(dist_spending_pc_baseline)) %>%
  unnest(all_of(yeartreat_dummies)) %>%
  mutate(post_dist_spending_pc_baseline = post * dist_spending_pc_baseline)

# sample 2: municipalities above target
# ------------------------------------------------------------------------
df_above <- df %>%
  filter(dist_ec29_baseline>0) # %>% 
  # mutate(dist_spending_pc_baseline = - dist_spending_pc_baseline) %>% 
  # mutate(ln_dist_spending_pc_baseline = log(dist_spending_pc_baseline))

# interacting dummies with treatment (dist_ec29_baseline)

df_above[yeartreat_dummies] <- df_above[dummies]

df_above <- df_above %>%
  # mutate_at(yeartreat_dummies, `*`,quote(ln_dist_spending_pc_baseline)) %>%
  # unnest(all_of(yeartreat_dummies)) %>%
  # mutate(post_ln_dist_spending_pc_baseline = post * ln_dist_spending_pc_baseline)
  mutate_at(yeartreat_dummies, `*`,quote(dist_spending_pc_baseline)) %>%
  unnest(all_of(yeartreat_dummies)) %>%
  mutate(post_dist_spending_pc_baseline = post * dist_spending_pc_baseline)


# Full sample
# ------------------------------------------------------------------------
df <- df %>% 
  mutate(post_dist_spending_pc_baseline = post * dist_spending_pc_baseline)




# 4. regression specifications
# =================================================================

controls <- c(grep("^ano_1998_", names(df), value = T),
              grep("^ano_1999_", names(df), value = T),
              grep("^ano_2000_", names(df), value = T),
              grep("^ano_2001_", names(df), value = T),
              grep("^ano_2002_", names(df), value = T),
              grep("^ano_2003_", names(df), value = T),
              grep("^ano_2004_", names(df), value = T),
              grep("^ano_2005_", names(df), value = T),
              grep("^ano_2006_", names(df), value = T),
              grep("^ano_2007_", names(df), value = T),
              grep("^ano_2008_", names(df), value = T),
              grep("^ano_2009_", names(df), value = T),
              grep("^ano_2010_", names(df), value = T))


# spec1 <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
# spec2 <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec3 <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# spec1_post <- paste(" ~ ","post_ln_dist_spending_pc_baseline"," | cod_mun + ano | 0 | cod_mun")
# spec2_post <- paste(" ~ ","post_ln_dist_spending_pc_baseline"," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec3_post <- paste(" ~ ","post_ln_dist_spending_pc_baseline"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# first stage specifications
# ------------------------------------------------
spec1_post <- paste(" ~ ","post_dist_spending_pc_baseline"," | cod_mun + ano | 0 | cod_mun")
spec2_post <- paste(" ~ ","post_dist_spending_pc_baseline"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post <- paste(" ~ ","post_dist_spending_pc_baseline"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# second stage specifications
# ------------------------------------------------

spec1 <- paste(" | cod_mun + ano | 0 | cod_mun")
spec2 <- paste(" | cod_mun + uf_y_fe | 0 | cod_mun")
spec3 <- paste(" + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# 5. 2 stages least squares function with bootstraps to estimate second stage SE
# =================================================================

iv <- function(outcome,treat,df,boots,regression_output,transform,year_filter){
  
  
  
  df_reg <- df
  
  
  # log of treatment variable
  ln_treat <- paste0("ln_",treat)
  df_reg[ln_treat] <- sapply(df_reg[treat], function(x) ifelse(x==0,x+0.000001,x))
  df_reg <- df_reg %>% 
    mutate_at(ln_treat,log)
  
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,x+0.000001,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
   
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(ln_treat),post_dist_spending_pc_baseline,all_of(controls)) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_treat]),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  
  
  # municipalities list
  munlist <- df_reg %>% select(cod_mun) %>% unique()
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3)){
    
    # regression specs
    spec_first <-get( paste0("spec",spec,"_post"))
    spec_second <- get(paste0("spec",spec))
    
    # first stage regression
    # ------------------------------
    regformula1 <- as.formula(paste(ln_treat,spec_first))
    fitted_var <- paste0("fitted_",ln_treat)
    
    fit <- felm(regformula1, data = df_reg, exactDOF = T)
    # extracting matrix of fitted values
    fitted <- fit$fitted.values %>% as.data.frame()
    names(fitted) <- fitted_var
    # merging with regression dataframe
    df_reg_fit <- bind_cols(df_reg,fitted)
    
    
    # second stage regression
    # ------------------------------
    
    regformula2 <- as.formula(paste(ln_outcome," ~ ",fitted_var,spec_second))
    fit2 <- felm(regformula2, data = df_reg_fit, exactDOF = T)
    
    out <- cbind(fit2 %>% broom::tidy() %>% slice(1),fit2 %>% broom::glance() %>% select(nobs))
    
    # BOOTSTRAP
    bs_coeffs <- vector(mode = "numeric", length = boots)
    
    for (i in 1:boots){
      
      
      sample.mun <- munlist[sample(nrow(munlist),replace = T),] # drawing sample of municipalities
      sample.data <- df_reg %>% right_join(sample.mun, by = "cod_mun")
      
      
      
      # first stage regression
      # ------------------------------
      regformula1 <- as.formula(paste(ln_treat,spec_first))
      fitted_var <- paste0("fitted_",ln_treat)
      
      fit <- felm(regformula1, data = sample.data, exactDOF = T)
      # extracting matrix of fitted values
      fitted <- fit$fitted.values %>% as.data.frame()
      names(fitted) <- fitted_var
      # merging with regression dataframe
      sample.data <- bind_cols(sample.data,fitted)
      
      
      # second stage regression
      # ------------------------------
      
      regformula2 <- as.formula(paste(ln_outcome," ~ ",fitted_var,spec_second))
      fit2 <- felm(regformula2, data = sample.data, exactDOF = T)
      
      # collecting coefficients
      beta <- fit2$coefficients[1] %>% as.numeric()
      
      bs_coeffs[i] <- beta
      
      # if (i==1){
      #   bs_coeffs <- beta
      # }
      # else{
      #   bs_coeffs <- rbind(bs_coeffs,beta)
      # }
      
      
      print(paste("Bootstrap ",i," spec ",spec))
      
    }
    
    
    
    bs_std.error <- sd(bs_coeffs)
    bs_mean <- mean(bs_coeffs)
    
    out <- cbind(out,bs_mean,bs_std.error,spec)
    
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


# 6. OLS
# =================================================================


ols <- function(outcome,treat,df,regression_output,transform,year_filter){
  
  
  
  df_reg <- df
  
  
  # log of treatment variable
  ln_treat <- paste0("ln_",treat)
  df_reg[ln_treat] <- sapply(df_reg[treat], function(x) ifelse(x==0,x+0.000001,x))
  df_reg <- df_reg %>% 
    mutate_at(ln_treat,log)
  
  
  
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,x+0.000001,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(ln_treat),post_dist_spending_pc_baseline,all_of(controls)) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_treat]),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3)){
    
  spec_ols<- get(paste0("spec",spec))
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome," ~ ",ln_treat,spec_ols))
    fit <- felm(regformula, data = df_reg, exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1),fit %>% broom::glance() %>% select(nobs))
    
    
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


# 5. Saving
# =================================================================
rm(raw)

save.image("regs.RData")














