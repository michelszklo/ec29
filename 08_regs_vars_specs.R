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


# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


# Regressions' outputs main folder
# ------------------------------------
main_folder <- "regs_outputs/"

# 2SLS specification robustness figures folder
# ------------------------------------
robust_folder <- "post_robust/"

# Reduced form yearly estimates figures folder
# ------------------------------------
yearly_folder <- "yearly_reduced/"


# don't forget to add "/" in the end of the folder name


# Regression output excel file
# ------------------------------------
output_file <- "results.xlsx"



# 1. Load data frame
# =================================================================
raw <- readRDS(paste0(dir,"data/CONSOL_DATA.RDS"))


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
         siops_rimpostosetransfconst = ifelse(siops_rimpostosetransfconst==0,NA,siops_rimpostosetransfconst)) %>% 
  mutate(siops_despinvest = siops_despinvest_pcapita * pop,
         siops_desppessoal = siops_desppessoal_pcapita * pop,
         siops_despservicoster = siops_despservicoster_pcapita * pop,
         siops_despoutros = siops_despoutros_pcapita * pop)
 



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
  mutate(ec29_baseline = dist_ec29_baseline + 0.15) %>% 
  ungroup() %>% 
  # distance to the EC29 target as per capita spending
  mutate(dist_spending_pc = -((siops_pct_recproprios_ec29 - 0.15)*(siops_rimpostosetransfconst/pop))) %>% 
  mutate(dist_spending_pc_baseline = ifelse(ano==2000,dist_spending_pc,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_spending_pc_baseline = mean(dist_spending_pc_baseline, na.rm = T)) %>% 
  ungroup() %>% 
  # distance to the EC29 target as total spending
  mutate(dist_spending = -((siops_pct_recproprios_ec29 - 0.15)*(siops_rimpostosetransfconst))) %>% 
  mutate(dist_spending_baseline = ifelse(ano==2000,dist_spending,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_spending_baseline = mean(dist_spending_baseline, na.rm = T)) %>% 
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
  mutate(post_dist_spending_pc_baseline = post * dist_spending_pc_baseline,
         post_dist_spending_baseline = post * dist_spending_baseline,
         post_ec29_baseline = post * ec29_baseline)

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
  mutate(post_dist_spending_pc_baseline = post * dist_spending_pc_baseline,
         post_dist_spending_baseline = post * dist_spending_baseline,
         post_ec29_baseline = post * ec29_baseline)


# Full sample
# ------------------------------------------------------------------------

df[yeartreat_dummies] <- df[dummies]
df <- df %>% 
  mutate(post_dist_spending_pc_baseline = post * dist_spending_pc_baseline,
         post_dist_spending_baseline = post * dist_spending_baseline,
         post_ec29_baseline = post * ec29_baseline) %>% 
  mutate_at(yeartreat_dummies, `*`,quote(dist_spending_pc_baseline)) 




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
              grep("^ano_2010_", names(df), value = T),
              "siops_despsaude_pcapita_neighbor","lrf")


spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# spec1_post <- paste(" ~ ","post_ln_dist_spending_pc_baseline"," | cod_mun + ano | 0 | cod_mun")
# spec2_post <- paste(" ~ ","post_ln_dist_spending_pc_baseline"," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec3_post <- paste(" ~ ","post_ln_dist_spending_pc_baseline"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# first stage specifications
# ------------------------------------------------

# spending in per capita figures
spec1_post <- paste(" ~ ","post_dist_spending_pc_baseline"," | cod_mun + ano | 0 | cod_mun")
spec2_post <- paste(" ~ ","post_dist_spending_pc_baseline"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post <- paste(" ~ ","post_dist_spending_pc_baseline"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# spending in total figures
# spec1_post <- paste(" ~ ","post_dist_spending_baseline"," | cod_mun + ano | 0 | cod_mun")
# spec2_post <- paste(" ~ ","post_dist_spending_baseline"," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec3_post <- paste(" ~ ","post_dist_spending_baseline"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# Share of own resources spent in health
# spec1_post <- paste(" ~ ","post_ec29_baseline"," | cod_mun + ano | 0 | cod_mun")
# spec2_post <- paste(" ~ ","post_ec29_baseline"," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec3_post <- paste(" ~ ","post_ec29_baseline"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(ln_treat),post_ec29_baseline,post_dist_spending_pc_baseline,post_dist_spending_baseline,all_of(controls)) %>% 
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(ln_treat),post_ec29_baseline,post_dist_spending_pc_baseline,post_dist_spending_baseline,all_of(controls)) %>% 
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


# 7. IV regression first stage
# =================================================================
# IV regression first stage
iv_first <- function(df,treat,year_filter,obj_name){
  
  df_reg <- df %>% filter(ano>=year_filter)
  ln_treat <- paste0("ln_",treat)
  
  for (spec in c(1,2,3)){
    
    spec_first <-get( paste0("spec",spec,"_post"))
    
    regformula1 <- as.formula(paste(ln_treat,spec_first))
    
    fit <- felm(regformula1, data = df_reg, exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1), fit %>% broom::glance() %>% select(statistic,nobs) %>% rename(f_statistic = statistic))
    out <- out %>% mutate(spec=spec)
    
    if(spec==1){
      table <- out
    } else{
      table <- rbind(table,out)
    }
    
    assign(obj_name,table,envir = .GlobalEnv)
    
  }
  
}


# 8. Reduced Form
# =================================================================

reduced <- function(outcome,var_name,df,regression_output,transform,year_filter){
  
  df_reg <- df
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),post_ec29_baseline,post_dist_spending_pc_baseline,post_dist_spending_baseline,all_of(controls)) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post"))
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
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

reduced_yearly <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys){
  
  df_reg <- df
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(yeartreat_dummies),post_ec29_baseline,post_dist_spending_pc_baseline,post_dist_spending_baseline,all_of(controls)) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  spec_reduced<- get(paste0("spec",3,"_post_y"))
  
  # second stage regression
  # ------------------------------
  
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(3:13) %>%
    select(term,estimate,std.error) %>% 
    mutate(estimate = ifelse(term=="post_00_dist_spending_pc_baseline",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           year = seq.int(year_filter,2010))
  
  graph <- table %>%
    ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub))+
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_pointrange(size = 0.5, alpha = 0.8) +
    scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
    scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    theme_light() +
    labs(y = var_name) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size=10),
          legend.position="bottom")
  
  ggsave(paste0(dir,main_folder,yearly_folder,ln_outcome,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,ln_outcome,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
}



# 10. Variables labels dictionary
# =================================================================

dict <- rbind(cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita'),
              cbind('finbra_desp_transporte_pcapita','Trasnport Spending per capita - Total'),
              cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita'),
              cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita'),
              cbind('finbra_desp_assist_prev_pcapita','Social Security Spending per capita'),
              cbind('siops_despsaude_pcapita','Health Spending per capita - Total'),
              cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources'),
              cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers'),
              cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources'),
              cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment'),
              cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services'),
              cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures'),
              cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
              cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
              cbind('hospital','Presence of Municipal Hospital'),
              cbind('unity_mun_pcapita','Municipal Outpatient Facilities per 1000 population'),
              cbind('leitos_pc','Hospital Beds per capita'),
              cbind('sia_pcapita','Outpatient procedures per capita'),
              cbind('sia_ab_nsuperior_pcapita','PC outpatient proced college degree personal per capita'),
              cbind('sia_ab_enfermagem_pcapita','PC outpatient proced non college degree personal per capita'),
              cbind('sia_visita_superior_pcapita','Household visits by college degree personal per capita'),
              cbind('sia_visita_medio_pcapita','Household visits by non college degree personal per capita'),
              cbind('sia_ativ_grupo_pcapita','Educational activities in group per capita'),
              cbind('hr_all_pcapita','Total Health workers per 1000 population'),
              cbind('hr_superior_pcapita','Doctors per 1000 population'),
              cbind('hr_technician_pcapita','Health Technicians per 1000 population'),
              cbind('hr_elementary_pcapita','Elementary Health workers per 1000 population'),
              cbind('hr_admin_pcapita','Administrative workers per 1000 population'),
              cbind('tx_mi','Infant Mortality Rate'),
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
              cbind('tx_ma','Adult Mortality Rate'),
              cbind('tx_ma_circ','Adult Mortality Rate - Circulatory'),
              cbind('tx_ma_neop','Adult Mortality Rate - Neoplasm'),
              cbind('tx_ma_resp','Adult Mortality Rate - Respiratory'),
              cbind('tx_ma_endoc','Adult Mortality Rate - Endocrine'),
              cbind('tx_ma_ext','Adult Mortality Rate - External'),
              cbind('tx_ma_nut','Adult Mortality Rate - Nutritional'),
              cbind('tx_ma_illdef','Adult Mortality Rate - Ill-Defined'),
              cbind('tx_ma_out','Adult Mortality Rate - Other'),
              cbind('tx_ma_diab','Adult Mortality Rate - Diabetes'),
              cbind('tx_ma_hyper','Adult Mortality Rate - Hypertension'),
              cbind('tx_ma_icsap','Adult Mortality Rate - APC'),
              cbind('tx_ma_nicsap','Adult Mortality Rate - non-APC'),
              cbind('tx_mi_l1','Infant Mortality Rate - 1y lag'),
              cbind('tx_mi_l2','Infant Mortality Rate - 2y lag'),
              cbind('tx_mi_l3','Infant Mortality Rate - 3y lag'),
              cbind('tx_mi_l4','Infant Mortality Rate - 4y lag'),
              cbind('tx_mi_l5','Infant Mortality Rate - 5y lag'),
              cbind('tx_mi_icsap_l1','Infant Mortality Rate - APC - 1y lag'),
              cbind('tx_mi_icsap_l2','Infant Mortality Rate - APC - 2y lag'),
              cbind('tx_mi_icsap_l3','Infant Mortality Rate - APC - 3y lag'),
              cbind('tx_mi_icsap_l4','Infant Mortality Rate - APC - 4y lag'),
              cbind('tx_mi_icsap_l5','Infant Mortality Rate - APC - 5y lag'),
              cbind('tx_mi_nicsap_l1','Infant Mortality Rate - non-APC - 1y lag'),
              cbind('tx_mi_nicsap_l2','Infant Mortality Rate - non-APC - 2y lag'),
              cbind('tx_mi_nicsap_l3','Infant Mortality Rate - non-APC - 3y lag'),
              cbind('tx_mi_nicsap_l4','Infant Mortality Rate - non-APC - 4y lag'),
              cbind('tx_mi_nicsap_l4','Infant Mortality Rate - non-APC - 5y lag'),
              cbind('tx_ma_l1','Adult Mortality Rate - 1y lag'),
              cbind('tx_ma_l2','Adult Mortality Rate - 2y lag'),
              cbind('tx_ma_l3','Adult Mortality Rate - 3y lag'),
              cbind('tx_ma_l4','Adult Mortality Rate - 4y lag'),
              cbind('tx_ma_l5','Adult Mortality Rate - 5y lag'),
              cbind('tx_ma_icsap_l1','Adult Mortality Rate - APC - 1y lag'),
              cbind('tx_ma_icsap_l2','Adult Mortality Rate - APC - 2y lag'),
              cbind('tx_ma_icsap_l3','Adult Mortality Rate - APC - 3y lag'),
              cbind('tx_ma_icsap_l4','Adult Mortality Rate - APC - 4y lag'),
              cbind('tx_ma_icsap_l5','Adult Mortality Rate - APC - 5y lag'),
              cbind('tx_ma_nicsap_l1','Adult Mortality Rate - non-APC - 1y lag'),
              cbind('tx_ma_nicsap_l2','Adult Mortality Rate - non-APC - 2y lag'),
              cbind('tx_ma_nicsap_l3','Adult Mortality Rate - non-APC - 3y lag'),
              cbind('tx_ma_nicsap_l4','Adult Mortality Rate - non-APC - 4y lag'),
              cbind('tx_ma_nicsap_l4','Adult Mortality Rate - non-APC - 5y lag'))


# 9. Saving
# =================================================================
rm(raw)

save.image(paste0(dir,"regs.RData"))














