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
            'modelsummary',
            'ggarchery')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)


# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"


# loading Folder, files and instrument setup

load(paste0(dir,"output_setup.RData"))

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


# Regressions' outputs main folder
# ------------------------------------
# main_folder <- "regs_outputs/"

# 2SLS specification robustness figures folder
# ------------------------------------
# robust_folder <- "post_robust_finbra_popw_review/"

# Reduced form yearly estimates figures folder
# ------------------------------------
# yearly_folder <- "yearly_reduced_finbra_popw_review/"


# Instrumental variable (uncoment the selected one)
# ------------------------------------

# instrument <- "ec29_baseline"
# instrument <- "ec29_baseline_below"
# instrument <- "dist_ec29_baseline"
# instrument <- "dist_ec29_baseline_below"
# instrument <- "dist_spending_pc_baseline"
# instrument <- "dist_spending_pc_baseline_below"

# is the instrument restricted to the sample below the target?
# below <- 0

# Regression output excel file
# ------------------------------------
# output_file <- "results_1st_dist_spend_b.xlsx"


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
  mutate(dist_ec29 = -(siops_pct_recproprios_ec29 - 0.15)) %>% 
  mutate(dist_ec29_baseline = ifelse(ano==2000,dist_ec29,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_ec29_baseline = mean(dist_ec29_baseline, na.rm = T)) %>%
  mutate(ec29_baseline = -dist_ec29_baseline + 0.15) %>% 
  mutate(dist_ec29_baseline_below = ifelse(dist_ec29_baseline>0,dist_ec29_baseline,0),
         ec29_baseline_below = ifelse(ec29_baseline<0.15,ec29_baseline,0.15)) %>% 
  ungroup() %>% 
  # distance to the EC29 target as per capita spending
  mutate(dist_spending_pc = -((siops_pct_recproprios_ec29 - 0.15)*(siops_rimpostosetransfconst/pop))) %>% 
  mutate(dist_spending_pc_baseline = ifelse(ano==2000,dist_spending_pc,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_spending_pc_baseline = mean(dist_spending_pc_baseline, na.rm = T)) %>% 
  mutate(dist_spending_pc_baseline_below = ifelse(dist_spending_pc_baseline>0,dist_spending_pc_baseline,0)) %>% 
  ungroup() %>% 
  # distance to the EC29 target as total spending
  mutate(dist_spending = (-((siops_pct_recproprios_ec29 - 0.15)*(siops_rimpostosetransfconst)))/1000000) %>% 
  mutate(dist_spending_baseline = ifelse(ano==2000,dist_spending,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_spending_baseline = mean(dist_spending_baseline, na.rm = T)) %>% 
  mutate(dist_spending_baseline_below = ifelse(dist_spending_baseline>0,dist_spending_baseline,0)) %>% 
  ungroup() %>% 
  # percentual distance to the EC29 target
  mutate(pcent_dist_ec29 = dist_ec29 / 0.15,
         pcent_dist_ec29_baseline = dist_ec29_baseline / 0.15,
         pcent_dist_ec29_baseline_below = ifelse(pcent_dist_ec29_baseline<0,0,pcent_dist_ec29_baseline)) %>% 
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
  mutate(post_10 = ifelse(ano==2010,1,0)) %>% 
  # weights
  mutate(peso_eq = 1) %>% 
  group_by(cod_mun) %>% 
  mutate(peso_b = mean(birth_nasc_vivos, na.rm = T),
         peso_a = mean(pop_25_59, na.rm = T),
         peso_a1 = mean(pop_25_39, na.rm = T),
         peso_a2 = mean(pop_40_59, na.rm = T),
         peso_r = mean(finbra_reccorr_pcapita,na.rm = T),
         peso_m = mean(pop_fem_10_49,na.rm = T)) %>% 
  ungroup() %>% 
  # sample split variable (by LRF)
  group_by(cod_mun) %>% 
  mutate(lrf_baseline = ifelse(ano==2000,lrf,NA)) %>% 
  mutate(lrf_baseline  = mean(lrf_baseline,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(lrf_baseline_median = median(lrf_baseline,na.rm = T)) %>% 
  mutate(lrf_baseline_above = ifelse(lrf_baseline>lrf_baseline_median,1,0)) %>%
  # infant mortality rate at the baseline
  mutate(tx_mi_baseline = ifelse(ano==2000,
                                 (dplyr::lag(tx_mi_illdef,2) + dplyr::lag(tx_mi_illdef,1) + tx_mi_illdef)/3,
                                 NA)) %>%
  # mutate(tx_mi_baseline = ifelse(ano==2000,tx_mi_illdef,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(tx_mi_baseline = mean(tx_mi_baseline, na.rm = T)) %>% 
  ungroup() %>% 
  # change in spending 2000-2005 in the baseline
  mutate(change_05_siops_despsaude_pcapita_baseline = ifelse(ano==2000,change_05_siops_despsaude_pcapita,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(change_05_siops_despsaude_pcapita_baseline = mean(change_05_siops_despsaude_pcapita_baseline,na.rm = T)) %>% 
  ungroup() 



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

# generating high (low) income sample, high (low) inequality sample, high (low) below poverty line population
df <- df %>% 
  mutate(rdpc_baseline_median = median(rdpc_baseline,na.rm = T),
         gini_baseline_median = median(gini_baseline, na.rm = T),
         pmpob_baseline_median = median(pmpob_baseline, na.rm = T)) %>% 
  mutate(rdpc_baseline_above = ifelse(rdpc_baseline>rdpc_baseline_median,1,0),
         gini_baseline_above = ifelse(gini_baseline>=gini_baseline_median,1,0),
         pmpob_baseline_above = ifelse(pmpob_baseline>pmpob_baseline_median,1,0)) %>% 
  mutate(health_income_baseline = change_05_siops_despsaude_pcapita_baseline/rdpc_baseline) %>% 
  mutate(health_income_baseline_median = median(health_income_baseline,na.rm = T)) %>% 
  mutate(health_income_baseline_above = ifelse(health_income_baseline>health_income_baseline_median,1,0))


# generating time variable
df <- df %>% 
  group_by(cod_mun) %>% 
  mutate(t = seq(-2,15,1)) %>% 
  ungroup()


# creating t * baseline controls
interact_vars <- sapply(controlsvar_baseline, function(x) paste0("t_",x), simplify = "array", USE.NAMES = F)
df[interact_vars] <- df[controlsvar_baseline]
df <- df %>% 
  mutate_at(interact_vars,`*`,quote(t))


# creating t * baseline IMR
df <- df %>% 
  mutate(t_tx_mi_baseline = tx_mi_baseline * t)

# creating year dummies
df <- df %>% 
  dummy_cols(select_columns = "ano", ignore_na = TRUE)

yeardummies <- grep("^ano_",names(df),value = T)
dummies <- c(grep("^pre",names(df),value = T),grep("^post_",names(df), value = T))
yeartreat_dummies <- sapply(dummies, function(x) paste0(x,"_",instrument), simplify = "array", USE.NAMES = F)

# 3. Setting regression samples
# =================================================================

# Full sample
# ------------------------------------------------------------------------

df[yeartreat_dummies] <- df[dummies]
df["iv"] <- df[instrument]

df <- df %>% 
  mutate_at(yeartreat_dummies, `*`,quote(iv)) %>% 
  unnest(all_of(yeartreat_dummies))

# filters to 2010
df <- df %>%
  filter(ano<=2010)

# transform treatment into treatment*post
df <- df %>%
  mutate(iv=ifelse(ano<=2000,0,iv)) 


# Municipalities with mayors at first and second term
# -----------------------------------------------------------------

df_first <- df %>% 
  filter(second_term==0)

df_second <- df %>%
  filter(second_term==1)


# Municipalities below and above median LRF rule
# -----------------------------------------------------------------

df_above <- df %>% 
  filter(lrf_baseline_above==1)

df_below <- df %>% 
  filter(lrf_baseline_above==0)


# Municipalities below and above median Gini inequality
# -----------------------------------------------------------------
df_high_ineq <- df %>% 
  filter(gini_baseline_above==1)

df_low_ineq <- df %>% 
  filter(gini_baseline_above==0)

# Municipalities below and above median Income
# -----------------------------------------------------------------
df_high_inc <- df %>% 
  filter(rdpc_baseline_above==1)

df_low_inc <- df %>% 
  filter(rdpc_baseline_above==0)


# Municipalities below and above median population below poverty line
# -----------------------------------------------------------------
df_high_pov <- df %>% 
  filter(pmpob_baseline_above==1)

df_low_pov <- df %>% 
  filter(pmpob_baseline_above==0)


# Municipalities below and above median population below poverty line
# -----------------------------------------------------------------
df_high_hi <- df %>% 
  filter(health_income_baseline_above==1)

df_low_hi <- df %>% 
  filter(health_income_baseline_above==0)



# 4. regression specifications
# =================================================================

# controls <- c(grep("^t_", names(df), value = T),"finbra_desp_saude_san_pcapita_neighbor","lrf")
baseline_controls <- grep("^t_", names(df), value = T)
baseline_controls <- baseline_controls[3:length(baseline_controls)-1]

tvarying_controls <- c("gdp_mun_pcapita","pbf_pcapita")

fiscal_controls <- c("finbra_desp_saude_san_pcapita_neighbor","lrf")

imr_controls <- "t_tx_mi_baseline"

controls <- c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls)


# Reduce form specification
# ------------------------------------------------

spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# Reduce form specification for IMR
# ------------------------------------------------

spec1_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# Reduced form specifications
# ------------------------------------------------

spec1_post <- paste(" ~ ","iv"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post <- paste(" ~ ","iv"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# Reduced form specifications for IMR
# ------------------------------------------------

spec1_post_imr <- paste(" ~ ","iv"," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 5. Reduced Form
# =================================================================

reduced <- function(outcome,var_name,df,regression_output,transform,year_filter,weight){
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    spec_reduced<- get(paste0("spec",spec,"_post"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
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

reduced_imr <- function(outcome,var_name,df,regression_output,transform,year_filter,weight){
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_imr"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
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


reduced_yearly <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap){
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
    
    out <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010),
             spec = as.character(spec)) %>% 
      mutate(spec = ifelse(spec=="1","Baseline",spec),
             spec = ifelse(spec=="4","+ Baseline, Time Varying and Fiscal Controls",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Baseline","+ Baseline, Time Varying and Fiscal Controls"))  
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
  }
  
  # adjusments for big confidence intervals
  table <- table %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub))
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  }else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  
  # GRAPHS
  # ---------
  
  # shapes <-  c(8,15,19)
  shapes <-  c(15,19)
  
  # graph with now bounds adjs
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = ub_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_point(aes(y = ub_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}

reduced_yearly_imr <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap){
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
    
    out <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010),
             spec = as.character(spec)) %>% 
      mutate(spec = ifelse(spec=="1","Baseline",spec),
             spec = ifelse(spec=="4","+ Baseline, Time Varying and Fiscal Controls",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Baseline","+ Baseline, Time Varying and Fiscal Controls"))  
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
  }
  
  # adjusments for big confidence intervals
  table <- table %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub))
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  }else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  
  # GRAPHS
  # ---------
  
  # shapes <-  c(8,15,19)
  shapes <-  c(15,19)
  
  # graph with now bounds adjs
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = ub_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_point(aes(y = ub_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}


# regs + graph function for heterogeneity analysis. 
reduced_yearly_het <- function(outcome,var_name,df1,df1_name,df2,df2_name,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap){
  
  transformations <- function(df_reg,transform){
    
    if(transform==1){
      # log
      ln_outcome <- paste0("ln_",outcome)
      df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
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
      select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),pop,
             peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
             finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
      filter(ano>=year_filter)
    
    df_reg <- df_reg[complete.cases(df_reg),]
    df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
    
  }
  
  ln_outcome <- paste0("ln_",outcome)
  
  df_reg1 <- df1 %>% transformations(transform = transform)
  df_reg2 <- df2 %>% transformations(transform = transform)
  
  # baseline mean of outcome for each sample
  
  baseline1 <- mean(df_reg1[ln_outcome] %>% unlist() %>%  as.numeric())
  baseline2 <- mean(df_reg2[ln_outcome] %>% unlist() %>%  as.numeric())
  
  
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    weight_vector <- df_reg1[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg1, weights = weight_vector,exactDOF = T)
    
    table1 <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      mutate(estimate = estimate/baseline1*0.10,
             lb = lb/baseline1*0.10,
             ub = ub/baseline1*0.10) %>% 
      mutate(Sample = df1_name)
    
  }
  
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    weight_vector <- df_reg2[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg2, weights = weight_vector,exactDOF = T)
    
    table2 <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      mutate(estimate = estimate/baseline2*0.10,
             lb = lb/baseline2*0.10,
             ub = ub/baseline2*0.10) %>% 
      mutate(Sample = df2_name)
    
  }
  
  
  table <- rbind(table1,table2)
  
  # adjusments for big confidence intervals
  table <- table %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub))
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  }else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  
  # GRAPHS
  # ---------
  
  # shapes <-  c(8,15,19)
  colors <-  c("#67a9cf","#ef8a62")
  
  # graph with now bounds adjs
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = lb_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = ub_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = lb_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_point(aes(y = ub_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}

reduced_yearly_imr_het <- function(outcome,var_name,df1,df1_name,df2,df2_name,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap){
  
  transformations <- function(df_reg,transform){
    
    if(transform==1){
      # log
      ln_outcome <- paste0("ln_",outcome)
      df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
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
      select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),pop,
             peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
             finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
      filter(ano>=year_filter)
    
    df_reg <- df_reg[complete.cases(df_reg),]
    df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
    
  }
  
  ln_outcome <- paste0("ln_",outcome)
  
  df_reg1 <- df1 %>% transformations(transform = transform)
  df_reg2 <- df2 %>% transformations(transform = transform)
  
  baseline1 <- mean(df_reg1[ln_outcome] %>% unlist() %>%  as.numeric())
  baseline2 <- mean(df_reg2[ln_outcome] %>% unlist() %>%  as.numeric())
  
  
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
    
    weight_vector <- df_reg1[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg1, weights = weight_vector,exactDOF = T)
    
    table1 <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      mutate(estimate = estimate/baseline1*0.10,
             lb = lb/baseline1*0.10,
             ub = ub/baseline1*0.10) %>% 
      mutate(Sample = df1_name)
    
  }
  
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
    
    weight_vector <- df_reg2[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg2, weights = weight_vector,exactDOF = T)
    
    table2 <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      mutate(estimate = estimate/baseline2*0.10,
             lb = lb/baseline2*0.10,
             ub = ub/baseline2*0.10) %>% 
      mutate(Sample = df2_name)
    
  }
  
  
  table <- rbind(table1,table2)
  
  # adjusments for big confidence intervals
  table <- table %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub))
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  }else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  
  # GRAPHS
  # ---------
  
  # shapes <-  c(8,15,19)
  colors <-  c("#67a9cf","#ef8a62")
  
  # graph with now bounds adjs
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = lb_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = ub_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = lb_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_point(aes(y = ub_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=6),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}



# 7. Output functions
# =================================================================

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
                    select(term,estimate),
                  df %>% 
                    select(term,std.error) %>% 
                    rename(estimate = std.error))
}  # formats regression outputs into article format

regress_output <- function(var,var_name,transform,year_filter,weight){
  
  # FULL SAMPLE
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_low_ineq","df_high_ineq","df_low_pov","df_high_pov","df_low_hi","df_high_hi","df_first","df_second")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  # 2sls final tables
  
  organizing_table <- function(d,sample_name,withspec){
    
    obs_1 <- d %>% slice(1) %>% select(nobs) %>% as.numeric()
    obs_2 <- d %>% slice(2) %>% select(nobs) %>% as.numeric()
    obs_3 <- d %>% slice(3) %>% select(nobs) %>% as.numeric()
    obs_4 <- d %>% slice(4) %>% select(nobs) %>% as.numeric()
    
    obs_name <- paste0("obs_",sample_name)
    
    
    if(withspec==0){
      
      table_1 <- d %>% mutate(sample = sample_name) %>% table_formating(1) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1)
      table_2 <- d %>% mutate(sample = sample_name) %>% table_formating(2) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2)
      table_3 <- d %>% mutate(sample = sample_name) %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3)
      table_4 <- d %>% mutate(sample = sample_name) %>% table_formating(4) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4)
      
      
    } else {
      
      table_1 <- d %>% mutate(sample = sample_name) %>% table_formating(1) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1) %>% mutate(spec=1)
      table_2 <- d %>% mutate(sample = sample_name) %>% table_formating(2) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2) %>% mutate(spec=2)
      table_3 <- d %>% mutate(sample = sample_name) %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3) %>% mutate(spec=3)
      table_4 <- d %>% mutate(sample = sample_name) %>% table_formating(4) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4) %>% mutate(spec=4)
      
      
    }
    
    
    name1 <- paste0("table_1_",sample_name)
    name2 <- paste0("table_2_",sample_name)
    name3 <- paste0("table_3_",sample_name)
    name4 <- paste0("table_4_",sample_name)
    
    assign(name1,table_1, envir = parent.frame()) 
    assign(name2,table_2, envir = parent.frame()) 
    assign(name3,table_3, envir = parent.frame()) 
    assign(name4,table_4, envir = parent.frame()) 
    
    
  }
  
  
  organizing_table(reg_df,"all",0)
  organizing_table(reg_df_low_ineq,"low_ineq",0)
  organizing_table(reg_df_high_ineq,"high_ineq",0)
  organizing_table(reg_df_low_pov,"low_pov",0)
  organizing_table(reg_df_high_pov,"high_pov",0)
  organizing_table(reg_df_low_hi,"low_hi",0)
  organizing_table(reg_df_high_hi,"high_hi",0)
  organizing_table(reg_df_first,"first",0)
  organizing_table(reg_df_second,"second",1)
  
  
  tables <- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
  
  binding <- function(name){
    
    output <- paste0("table_",name)
    for(i in grep(name,tables,value = T)){
      
      first <- grep("1",i,value = T)
      d <- get(i)
      
      if(length(first>0)){
        df <- d
      } else {
        df <- bind_rows(df,d)
      }
      
    }  
    
    assign(output,df,envir = parent.frame())
    
  }
  
  binding("all")
  binding("low_ineq")
  binding("high_ineq")
  binding("low_pov")
  binding("high_pov")
  binding("low_hi")
  binding("high_hi")
  binding("first")
  binding("second")
  
  table_all <- bind_cols(table_all,
                         table_low_ineq %>% select(-term),
                         table_high_ineq %>% select(-term),
                         table_low_pov %>% select(-term),
                         table_high_pov %>% select(-term),
                         table_low_hi %>% select(-term),
                         table_high_hi %>% select(-term),
                         table_first %>% select(-term),
                         table_second %>% select(-term)
  )
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects

regress_output_imr <- function(var,var_name,transform,year_filter,weight){
  
  # FULL SAMPLE
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_low_ineq","df_high_ineq","df_low_pov","df_high_pov","df_low_hi","df_high_hi","df_first","df_second")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced_imr(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  organizing_table <- function(d,sample_name,withspec){
    
    obs_1 <- d %>% slice(1) %>% select(nobs) %>% as.numeric()
    obs_2 <- d %>% slice(2) %>% select(nobs) %>% as.numeric()
    obs_3 <- d %>% slice(3) %>% select(nobs) %>% as.numeric()
    obs_4 <- d %>% slice(4) %>% select(nobs) %>% as.numeric()
    
    obs_name <- paste0("obs_",sample_name)
    
    
    if(withspec==0){
      
      table_1 <- d %>% mutate(sample = sample_name) %>% table_formating(1) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1)
      table_2 <- d %>% mutate(sample = sample_name) %>% table_formating(2) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2)
      table_3 <- d %>% mutate(sample = sample_name) %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3)
      table_4 <- d %>% mutate(sample = sample_name) %>% table_formating(4) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4)
      
      
    } else {
      
      table_1 <- d %>% mutate(sample = sample_name) %>% table_formating(1) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1) %>% mutate(spec=1)
      table_2 <- d %>% mutate(sample = sample_name) %>% table_formating(2) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2) %>% mutate(spec=2)
      table_3 <- d %>% mutate(sample = sample_name) %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3) %>% mutate(spec=3)
      table_4 <- d %>% mutate(sample = sample_name) %>% table_formating(4) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4) %>% mutate(spec=4)
      
      
    }
    
    
    name1 <- paste0("table_1_",sample_name)
    name2 <- paste0("table_2_",sample_name)
    name3 <- paste0("table_3_",sample_name)
    name4 <- paste0("table_4_",sample_name)
    
    assign(name1,table_1, envir = parent.frame()) 
    assign(name2,table_2, envir = parent.frame()) 
    assign(name3,table_3, envir = parent.frame()) 
    assign(name4,table_4, envir = parent.frame()) 
    
    
  }
  
  
  organizing_table(reg_df,"all",0)
  organizing_table(reg_df_low_ineq,"low_ineq",0)
  organizing_table(reg_df_high_ineq,"high_ineq",0)
  organizing_table(reg_df_low_pov,"low_pov",0)
  organizing_table(reg_df_high_pov,"high_pov",0)
  organizing_table(reg_df_low_hi,"low_hi",0)
  organizing_table(reg_df_high_hi,"high_hi",0)
  organizing_table(reg_df_first,"first",0)
  organizing_table(reg_df_second,"second",1)
  
  
  tables <- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
  
  binding <- function(name){
    
    output <- paste0("table_",name)
    for(i in grep(name,tables,value = T)){
      
      first <- grep("1",i,value = T)
      d <- get(i)
      
      if(length(first>0)){
        df <- d
      } else {
        df <- bind_rows(df,d)
      }
      
    }  
    
    assign(output,df,envir = parent.frame())
    
  }
  
  binding("all")
  binding("low_ineq")
  binding("high_ineq")
  binding("low_pov")
  binding("high_pov")
  binding("low_hi")
  binding("high_hi")
  binding("first")
  binding("second")
  
  table_all <- bind_cols(table_all,
                         table_low_ineq %>% select(-term),
                         table_high_ineq %>% select(-term),
                         table_low_pov %>% select(-term),
                         table_high_pov %>% select(-term),
                         table_low_hi %>% select(-term),
                         table_high_hi %>% select(-term),
                         table_first %>% select(-term),
                         table_second %>% select(-term)
  )
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects



# 10. Saving
# =================================================================

rm(raw)

save.image(paste0(dir,"regs.RData"))














