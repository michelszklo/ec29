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
         peso_r = mean(finbra_reccorr_pcapita,na.rm = T)) %>% 
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

# generating high (low) income sample and high (low) inequality sample
df <- df %>% 
  mutate(rdpc_baseline_median = median(rdpc_baseline,na.rm = T),
         gini_baseline_median = median(gini_baseline, na.rm = T)) %>% 
  mutate(rdpc_baseline_above = ifelse(rdpc_baseline>rdpc_baseline_median,1,0),
         gini_baseline_above = ifelse(gini_baseline>=gini_baseline_median,1,0))


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


# Instrument interacted with heterogeneity varaible
df <- df %>% 
  # first (second) term
  # mutate(het = (1-second_term)) %>% 
  # High (low) inequality
  mutate(het = gini_baseline_above) %>%
  mutate(iv_het = iv * het) %>% 
  mutate(t_het = t * het)



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




# 4. regression specifications
# =================================================================

baseline_controls <- grep("^t_", names(df), value = T)
baseline_controls <- baseline_controls[3:length(baseline_controls)-1]

tvarying_controls <- c("gdp_mun_pcapita","pbf_pcapita")

fiscal_controls <- c("finbra_desp_saude_san_pcapita_neighbor","lrf")

imr_controls <- "t_tx_mi_baseline"

controls <- c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls)



# Reduced form specifications
# ------------------------------------------------

spec1_post <- paste(" ~ ","iv + iv_het + t_het"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post <- paste(" ~ ","iv + iv_het + t_het"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post <- paste(" ~ ","iv + iv_het + t_het"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post <- paste(" ~ ","iv + iv_het + t_het"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# Reduced form specifications IMR
# ------------------------------------------------

spec1_post_imr <- paste(" ~ ","iv + iv_het + t_het"," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr <- paste(" ~ ","iv + iv_het + t_het"," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr <- paste(" ~ ","iv + iv_het + t_het"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr <- paste(" ~ ","iv + iv_het + t_het"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# 8. Reduced Form
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_het,t_het,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r) %>% 
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
    fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:3),fit %>% broom::glance() %>% select(nobs))
    
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>%
    mutate(coeff = term,
           term = ln_outcome)
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_het,t_het,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r) %>% 
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
    fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:3),fit %>% broom::glance() %>% select(nobs))
    
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>%
    mutate(coeff = term,
           term = ln_outcome)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
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
                    select(term,estimate,coeff,nobs) %>% filter(coeff=="iv") %>% mutate(item="b"),
                  df %>% 
                    select(term,std.error,coeff,nobs) %>% filter(coeff=="iv") %>% rename(estimate = std.error)%>% mutate(item="se"),
                  df %>%
                    select(term,estimate,coeff,nobs) %>% filter(coeff=="iv_het") %>% mutate(item="b"),
                  df %>% 
                    select(term,std.error,coeff,nobs) %>% filter(coeff=="iv_het") %>% rename(estimate = std.error) %>% mutate(item="se"),
                  df %>%
                    select(term,estimate,coeff,nobs) %>% filter(coeff=="t_het") %>% mutate(item="b"),
                  df %>% 
                    select(term,std.error,coeff,nobs) %>% filter(coeff=="t_het") %>% rename(estimate = std.error) %>% mutate(item="se"),
                  
  ) %>% 
    pivot_wider(id_cols = c("item","term","nobs"),
                names_from = "coeff",
                values_from = "estimate") %>% 
    select(item,term,iv,iv_het,t_het,everything())
}  # formats regression outputs into article format

regress_output <- function(var,var_name,transform,year_filter,weight){
  
  # REDUCED FORM REGRESSION
  # ----------------------------------------
  
  for (data in c("df")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced(var,var_name,d,obj,transform,year_filter,weight) # function for OLS regression
    
    
    print(paste0("Reduced form regs for sample ",data))
  }
  
  # Reduced form final tables
  table_all_1 <- reg_df  %>% table_formating(1) %>% mutate(spec = 1)
  table_all_2 <- reg_df  %>% table_formating(2) %>% mutate(spec = 2)
  table_all_3 <- reg_df  %>% table_formating(3) %>% mutate(spec = 3)
  table_all_4 <- reg_df  %>% table_formating(3) %>% mutate(spec = 4)
  
  
  table_all <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3,table_all_4)) %>% 
    select(-item)
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects

regress_output_imr <- function(var,var_name,transform,year_filter,weight){
  
  # REDUCED FORM REGRESSION
  # ----------------------------------------
  
  for (data in c("df")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced_imr(var,var_name,d,obj,transform,year_filter,weight) # function for OLS regression
    
    
    print(paste0("Reduced form regs for sample ",data))
  }
  
  # Reduced form final tables
  table_all_1 <- reg_df  %>% table_formating(1) %>% mutate(spec = 1)
  table_all_2 <- reg_df  %>% table_formating(2) %>% mutate(spec = 2)
  table_all_3 <- reg_df  %>% table_formating(3) %>% mutate(spec = 3)
  table_all_4 <- reg_df  %>% table_formating(3) %>% mutate(spec = 4)
  
  
  table_all <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3,table_all_4)) %>% 
    select(-item)
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects



# 9. Saving
# =================================================================
rm(raw)

save.image(paste0(dir,"regs.RData"))














