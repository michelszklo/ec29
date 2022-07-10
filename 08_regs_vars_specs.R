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
spec3_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,
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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,
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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,4)){
    
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,4)){
    
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=8),
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
  for (data in c("df","df_first","df_second","df_above","df_below","df_low_inc","df_high_inc","df_low_ineq","df_high_ineq")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  # 2sls final tables
  
  obs_all_1 <- reg_df %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_all_2 <- reg_df %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_all_3 <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_all_4 <- reg_df %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_all_1 <- reg_df %>% mutate(sample = "all") %>% table_formating(1) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_1)
  table_all_2 <- reg_df %>% mutate(sample = "all") %>% table_formating(2) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_2)
  table_all_3 <- reg_df %>% mutate(sample = "all") %>% table_formating(3) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_3)
  table_all_4 <- reg_df %>% mutate(sample = "all") %>% table_formating(4) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_4)
  
  
  obs_low_inc_1 <- reg_df_low_inc %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_low_inc_2 <- reg_df_low_inc %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_low_inc_3 <- reg_df_low_inc %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_low_inc_4 <- reg_df_low_inc %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_low_inc_1 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(1) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_1)
  table_low_inc_2 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(2) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_2)
  table_low_inc_3 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(3) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_3)
  table_low_inc_4 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(4) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_4)
  
  
  obs_high_inc_1 <- reg_df_high_inc %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_high_inc_2 <- reg_df_high_inc %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_high_inc_3 <- reg_df_high_inc %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_high_inc_4 <- reg_df_high_inc %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_high_inc_1 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(1) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_1)
  table_high_inc_2 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(2) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_2)
  table_high_inc_3 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(3) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_3)
  table_high_inc_4 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(4) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_4)
  
  
  obs_low_ineq_1 <- reg_df_low_ineq %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_low_ineq_2 <- reg_df_low_ineq %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_low_ineq_3 <- reg_df_low_ineq %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_low_ineq_4 <- reg_df_low_ineq %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_low_ineq_1 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(1) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_1)
  table_low_ineq_2 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(2) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_2)
  table_low_ineq_3 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(3) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_3)
  table_low_ineq_4 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(4) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_4)
  
  
  obs_high_ineq_1 <- reg_df_high_ineq %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_high_ineq_2 <- reg_df_high_ineq %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_high_ineq_3 <- reg_df_high_ineq %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_high_ineq_4 <- reg_df_high_ineq %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_high_ineq_1 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(1) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_1)
  table_high_ineq_2 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(2) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_2)
  table_high_ineq_3 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(3) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_3)
  table_high_ineq_4 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(4) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_4)
  
  
  obs_below_1 <- reg_df_below %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_below_2 <- reg_df_below %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_below_3 <- reg_df_below %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_below_4 <- reg_df_below %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_1)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_2)
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_3)
  table_below_4 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(4) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_4)
  
  
  obs_above_1 <- reg_df_above %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_above_2 <- reg_df_above %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_above_3 <- reg_df_above %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_above_4 <- reg_df_above %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_3)
  table_above_4 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(4) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_4)
  
  
  obs_first_1 <- reg_df_first %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_first_2 <- reg_df_first %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_first_3 <- reg_df_first %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_first_4 <- reg_df_first %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_first_1 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(1) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_1)
  table_first_2 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(2) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_2)
  table_first_3 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(3) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_3)
  table_first_4 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(4) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_4)
  
  
  obs_second_1 <- reg_df_second %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_second_2 <- reg_df_second %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_second_3 <- reg_df_second %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_second_4 <- reg_df_second %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_second_1 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(1) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_1) %>% mutate(spec=1)
  table_second_2 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(2) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_2) %>% mutate(spec=2)
  table_second_3 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(3) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_3) %>% mutate(spec=3)
  table_second_4 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(4) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_4) %>% mutate(spec=4)
  
  
  
  
  table_all <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3,table_all_4),
                         bind_rows(table_below_1,table_below_2,table_below_3,table_below_4),
                         bind_rows(table_above_1,table_above_2,table_above_3,table_above_4),
                         bind_rows(table_low_inc_1,table_low_inc_2,table_low_inc_3,table_low_inc_4),
                         bind_rows(table_high_inc_1,table_high_inc_2,table_high_inc_3,table_high_inc_4),
                         bind_rows(table_low_ineq_1,table_low_ineq_2,table_low_ineq_3,table_low_ineq_4),
                         bind_rows(table_high_ineq_1,table_high_ineq_2,table_high_ineq_3,table_high_ineq_4),
                         bind_rows(table_first_1,table_first_2,table_first_3,table_first_4),
                         bind_rows(table_second_1,table_second_2,table_second_3,table_second_4)) 
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects

regress_output_imr <- function(var,var_name,transform,year_filter,weight){
  
  # FULL SAMPLE
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_first","df_second","df_above","df_below","df_low_inc","df_high_inc","df_low_ineq","df_high_ineq")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced_imr(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  # 2sls final tables
  
  obs_all_1 <- reg_df %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_all_2 <- reg_df %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_all_3 <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_all_4 <- reg_df %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_all_1 <- reg_df %>% mutate(sample = "all") %>% table_formating(1) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_1)
  table_all_2 <- reg_df %>% mutate(sample = "all") %>% table_formating(2) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_2)
  table_all_3 <- reg_df %>% mutate(sample = "all") %>% table_formating(3) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_3)
  table_all_4 <- reg_df %>% mutate(sample = "all") %>% table_formating(4) %>% rename("all" = "estimate") %>% mutate(obs_all = obs_all_4)
  
  
  obs_low_inc_1 <- reg_df_low_inc %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_low_inc_2 <- reg_df_low_inc %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_low_inc_3 <- reg_df_low_inc %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_low_inc_4 <- reg_df_low_inc %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_low_inc_1 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(1) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_1)
  table_low_inc_2 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(2) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_2)
  table_low_inc_3 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(3) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_3)
  table_low_inc_4 <- reg_df_low_inc %>% mutate(sample = "low_inc") %>% table_formating(4) %>% rename("low_inc" = "estimate") %>% select(-term) %>% mutate(obs_low_inc = obs_low_inc_4)
  
  
  obs_high_inc_1 <- reg_df_high_inc %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_high_inc_2 <- reg_df_high_inc %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_high_inc_3 <- reg_df_high_inc %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_high_inc_4 <- reg_df_high_inc %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_high_inc_1 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(1) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_1)
  table_high_inc_2 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(2) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_2)
  table_high_inc_3 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(3) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_3)
  table_high_inc_4 <- reg_df_high_inc %>% mutate(sample = "high_inc") %>% table_formating(4) %>% rename("high_inc" = "estimate") %>% select(-term)  %>% mutate(obs_high_inc = obs_high_inc_4)
  
  
  obs_low_ineq_1 <- reg_df_low_ineq %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_low_ineq_2 <- reg_df_low_ineq %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_low_ineq_3 <- reg_df_low_ineq %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_low_ineq_4 <- reg_df_low_ineq %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_low_ineq_1 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(1) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_1)
  table_low_ineq_2 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(2) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_2)
  table_low_ineq_3 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(3) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_3)
  table_low_ineq_4 <- reg_df_low_ineq %>% mutate(sample = "low_ineq") %>% table_formating(4) %>% rename("low_ineq" = "estimate") %>% select(-term) %>% mutate(obs_low_ineq = obs_low_ineq_4)
  
  
  obs_high_ineq_1 <- reg_df_high_ineq %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_high_ineq_2 <- reg_df_high_ineq %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_high_ineq_3 <- reg_df_high_ineq %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_high_ineq_4 <- reg_df_high_ineq %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_high_ineq_1 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(1) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_1)
  table_high_ineq_2 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(2) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_2)
  table_high_ineq_3 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(3) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_3)
  table_high_ineq_4 <- reg_df_high_ineq %>% mutate(sample = "high_ineq") %>% table_formating(4) %>% rename("high_ineq" = "estimate") %>% select(-term)  %>% mutate(obs_high_ineq = obs_high_ineq_4)
  
  
  obs_below_1 <- reg_df_below %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_below_2 <- reg_df_below %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_below_3 <- reg_df_below %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_below_4 <- reg_df_below %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_1)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_2)
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_3)
  table_below_4 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(4) %>% rename("below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_4)
  
  
  obs_above_1 <- reg_df_above %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_above_2 <- reg_df_above %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_above_3 <- reg_df_above %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_above_4 <- reg_df_above %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_3)
  table_above_4 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(4) %>% rename("above" = "estimate") %>% select(-term)  %>% mutate(obs_above = obs_above_4)
  
  
  obs_first_1 <- reg_df_first %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_first_2 <- reg_df_first %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_first_3 <- reg_df_first %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_first_4 <- reg_df_first %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_first_1 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(1) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_1)
  table_first_2 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(2) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_2)
  table_first_3 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(3) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_3)
  table_first_4 <- reg_df_first %>% mutate(sample = "first") %>% table_formating(4) %>% rename("first" = "estimate") %>% select(-term) %>% mutate(obs_first = obs_first_4)
  
  
  obs_second_1 <- reg_df_second %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_second_2 <- reg_df_second %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_second_3 <- reg_df_second %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_second_4 <- reg_df_second %>% slice(4) %>% select(nobs) %>% as.numeric()
  
  table_second_1 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(1) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_1) %>% mutate(spec=1)
  table_second_2 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(2) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_2) %>% mutate(spec=2)
  table_second_3 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(3) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_3) %>% mutate(spec=3)
  table_second_4 <- reg_df_second %>% mutate(sample = "second") %>% table_formating(4) %>% rename("second" = "estimate") %>% select(-term) %>% mutate(obs_second = obs_second_4) %>% mutate(spec=4)
  
  
  
  
  table_all <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3,table_all_4),
                         bind_rows(table_below_1,table_below_2,table_below_3,table_below_4),
                         bind_rows(table_above_1,table_above_2,table_above_3,table_above_4),
                         bind_rows(table_low_inc_1,table_low_inc_2,table_low_inc_3,table_low_inc_4),
                         bind_rows(table_high_inc_1,table_high_inc_2,table_high_inc_3,table_high_inc_4),
                         bind_rows(table_low_ineq_1,table_low_ineq_2,table_low_ineq_3,table_low_ineq_4),
                         bind_rows(table_high_ineq_1,table_high_ineq_2,table_high_ineq_3,table_high_ineq_4),
                         bind_rows(table_first_1,table_first_2,table_first_3,table_first_4),
                         bind_rows(table_second_1,table_second_2,table_second_3,table_second_4)) 
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects



# 10. Saving
# =================================================================

rm(raw)

save.image(paste0(dir,"regs.RData"))














