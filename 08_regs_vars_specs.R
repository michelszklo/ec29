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
         peso_a2 = mean(pop_40_59, na.rm = T)) %>% 
  ungroup()
# mutate(post_11 = ifelse(ano==2011,1,0)) %>% 
# mutate(post_12 = ifelse(ano==2012,1,0)) %>% 
# mutate(post_13 = ifelse(ano==2013,1,0)) %>% 
# mutate(post_14 = ifelse(ano==2014,1,0)) %>% 
# mutate(post_15 = ifelse(ano==2015,1,0)) 




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


# creating year dummies
dummy_cols(select_columns = "ano", ignore_na = TRUE)

yeardummies <- grep("^ano_",names(df),value = T)


# 3. Setting regression samples
# =================================================================

# sample 1: municipalities below target (positive distance to the target)
# ------------------------------------------------------------------------
df_below <- df %>%
  filter(dist_ec29_baseline>0) #  %>%
# mutate(ln_dist_spending_pc_baseline = log(dist_spending_pc_baseline))

# interacting dummies with treatment (dist_ec29_baseline)

dummies <- c(grep("^pre",names(df_below),value = T),grep("^post_",names(df), value = T))
# yeartreat_dummies <- sapply(dummies, function(x) paste0(x,"_ln_dist_spending_pc_baseline"), simplify = "array", USE.NAMES = F)
yeartreat_dummies <- sapply(dummies, function(x) paste0(x,"_",instrument), simplify = "array", USE.NAMES = F)
df_below[yeartreat_dummies] <- df_below[dummies]

df_below["iv"] <- df_below[instrument]

df_below <- df_below %>%
  mutate_at(yeartreat_dummies, `*`,quote(iv)) %>%
  unnest(all_of(yeartreat_dummies))

# sample 2: municipalities above target
# ------------------------------------------------------------------------
df_above <- df %>%
  filter(dist_ec29_baseline<0) # %>% 

# interacting dummies with treatment (dist_ec29_baseline)

df_above[yeartreat_dummies] <- df_above[dummies]
df_above["iv"] <- df_above[instrument]

df_above <- df_above %>%
  mutate_at(yeartreat_dummies, `*`,quote(iv)) %>%
  unnest(all_of(yeartreat_dummies))


# Full sample
# ------------------------------------------------------------------------

df[yeartreat_dummies] <- df[dummies]
df["iv"] <- df[instrument]

df <- df %>% 
  mutate_at(yeartreat_dummies, `*`,quote(iv)) %>% 
  unnest(all_of(yeartreat_dummies))


# # converting df_above to sample of municipalities of mayors who ran for reelection
# df_above <- df %>%
#   filter(second_term==1)
# 
# # converting df_below to sample of municipalities of mayors who didn't run for reelection
# 
# df_below <- df %>%
#   filter(second_term==0)


# # # converting df_above to sample of municipalities with high fiscal governance
# df_above <- df %>%
#   filter(firjan_above==1)
# 
# # converting df_above to sample of municipalities with low fiscal governance
# 
# df_below <- df %>%
#   filter(firjan_above==0)


# 4. regression specifications
# =================================================================

controls <- c(grep("^t_", names(df), value = T),"finbra_desp_saude_san_pcapita_neighbor","lrf")
controls <- controls[3:length(controls)]


# Reduce form specification and first stage - yearly
# ------------------------------------------------

spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# # changing the order that controls and fixed effects are added to the specifications
# spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
# spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
# spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# Reduced form specifications and first stage
# ------------------------------------------------

# spending in per capita figures
spec1_post <- paste(" ~ ","iv"," | cod_mun + ano | 0 | cod_mun")
spec2_post <- paste(" ~ ","iv"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post <- paste(" ~ ","iv"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3)){
    
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

reduced_yearly <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,sample,below,weight){
  
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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,2,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    weight_vector <- df_reg[weight] %>% as.numeric()
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
             spec = ifelse(spec=="2","+ Controls",spec),
             spec = ifelse(spec=="3","+ State-Year FE",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Baseline","+ State-Year FE","+ Controls"))  
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
  }
  
  
  
  shapes <-  c(17,15,19)
  # color_graph <- pal_lancet("lanonc")(9)
  
  graph <- table %>%
    ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_pointrange(size = 0.5, alpha = 0.8, position = position_dodge(width=0.6),color = "grey13") +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
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


# 6. Population cap
# =================================================================

cap_pop <- function(df,npop){
  df <- df %>% 
    mutate(pop_baseline = ifelse(ano==2000,pop,NA)) %>% 
    group_by(cod_mun) %>% 
    mutate(pop_baseline = mean(pop_baseline, na.rm = T)) %>% 
    ungroup() %>% 
    filter(pop <= npop)
}

# df <- df %>% 
#   cap_pop(30000)
# 
# df_above <- df_above %>% 
#   cap_pop(30000)
# 
# df_below <- df_below %>% 
#   cap_pop(30000)

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
  for (data in c("df","df_above","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  # 2sls final tables
  
  obs_all_1 <- reg_df %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_all_2 <- reg_df %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_all_3 <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("ALL_full" = "estimate") %>% mutate(obs_full = obs_all_1)
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("ALL_full" = "estimate") %>% mutate(obs_full = obs_all_2)
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("ALL_full" = "estimate") %>% mutate(obs_full = obs_all_3)
  
  obs_below_1 <- reg_df_below %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_below_2 <- reg_df_below %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_below_3 <- reg_df_below %>% slice(3) %>% select(nobs) %>% as.numeric()
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("ALL_below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_1)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("ALL_below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_2)
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("ALL_below" = "estimate") %>% select(-term) %>% mutate(obs_below = obs_below_3)
  
  obs_above_1 <- reg_df_above %>% slice(1) %>% select(nobs) %>% as.numeric()
  obs_above_2 <- reg_df_above %>% slice(2) %>% select(nobs) %>% as.numeric()
  obs_above_3 <- reg_df_above %>% slice(3) %>% select(nobs) %>% as.numeric()
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("ALL_above" = "estimate") %>% select(-term) %>% mutate(spec=1) %>% mutate(obs_above = obs_above_1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("ALL_above" = "estimate") %>% select(-term) %>% mutate(spec=2) %>% mutate(obs_above = obs_above_2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("ALL_above" = "estimate") %>% select(-term) %>% mutate(spec=3) %>% mutate(obs_above = obs_above_1)
  
  
  table_all <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                         bind_rows(table_below_1,table_below_2,table_below_3),
                         bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  
  # SAMPLE: MUNICIPALITIES WITH UP TO 50K INHABITANTS
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_above","df_below")){
    
    d <- get(data) %>% 
      cap_pop(50000)
    
    obj <- paste0("reg_",data) # name of the output object
    reduced(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for Pop<=50 for sample ",data))
  }
  
  # OLS final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("pop50_full" = "estimate")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("pop50_full" = "estimate")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("pop50_full" = "estimate")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("pop50_below" = "estimate") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("pop50_below" = "estimate") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("pop50_below" = "estimate") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("pop50_above" = "estimate") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("pop50_above" = "estimate") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("pop50_above" = "estimate") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_pop50 <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                           bind_rows(table_below_1,table_below_2,table_below_3),
                           bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  
  # SAMPLE: MUNICIPALITIES WITH UP TO 30K INHABITANTS
  # ----------------------------------------
  
  for (data in c("df","df_above","df_below")){
    
    d <- get(data) %>% 
      cap_pop(30000)
    
    obj <- paste0("reg_",data) # name of the output object
    reduced(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    
    print(paste0("Regs for Pop<=30 for sample ",data))
  }
  
  # Reduced form final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("pop30_full" = "estimate")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("pop30_full" = "estimate")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("pop30_full" = "estimate")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("pop30_below" = "estimate") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("pop30_below" = "estimate") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("pop30_below" = "estimate") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("pop30_above" = "estimate") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("pop30_above" = "estimate") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("pop30_above" = "estimate") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_pop30 <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                           bind_rows(table_below_1,table_below_2,table_below_3),
                           bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  
  table_final <- cbind.data.frame(table_all %>% select(term,`ALL_full`),
                                  table_pop50 %>% select(pop50_full),
                                  table_pop30 %>% select(`pop30_full`),
                                  table_all %>% select(`obs_full`),
                                  
                                  table_all %>% select(`ALL_below`),
                                  table_pop50 %>% select(pop50_below),
                                  table_pop30 %>% select(`pop30_below`),
                                  table_all %>% select(`obs_below`),
                                  
                                  table_all %>% select(`ALL_above`),
                                  table_pop50 %>% select(pop50_above),
                                  table_pop30 %>% select(`pop30_above`),
                                  table_all %>% select(`obs_above`),
                                  table_pop30 %>% select(`spec`))
  
  # assigning objects to the global envir
  assign("table_all",table_final, envir = .GlobalEnv) 
  
}  # runs regressions and output objects





# 10. Saving
# =================================================================

rm(raw)

save.image(paste0(dir,"regs.RData"))














