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
df <- df %>% 
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

# Instrument interacted with first term dummy
df <- df %>% 
  mutate(firstterm = (1-second_term),
         iv_firstterm = iv * firstterm) %>%
  # baseline first term * time
  mutate(t_firstterm = t * firstterm)



# 4. regression specifications
# =================================================================

controls <- c(grep("^t_", names(df), value = T),"finbra_desp_saude_san_pcapita_neighbor","lrf")
controls <- controls[3:length(controls)]


# Reduce form specification - yearly
# ------------------------------------------------

spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# # changing the order that controls and fixed effects are added to the specifications
# spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
# spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + ano | 0 | cod_mun")
# spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# Reduced form specifications
# ------------------------------------------------

# spending in per capita figures
spec1_post <- paste(" ~ ","iv + iv_firstterm + t_firstterm"," | cod_mun + ano | 0 | cod_mun")
spec2_post <- paste(" ~ ","iv + iv_firstterm + t_firstterm"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post <- paste(" ~ ","iv + iv_firstterm + t_firstterm"," + ", paste(controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_firstterm,t_firstterm,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r) %>% 
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

reduced_yearly <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,sample,below,weight){
  
  # first term sample
  # ----------------------------------------------------------------
  df_reg <- df %>% filter(firstterm==1)
  
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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,2,3)){
    
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
             spec = ifelse(spec=="2","+ Controls",spec),
             spec = ifelse(spec=="3","+ State-Year FE",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Baseline","+ State-Year FE","+ Controls"))  
    
    if(spec==1){
      table_first <- out
    }
    else{
      table_first <- rbind(table_first,out)
    }
  }
  
  table_first <- table_first %>% mutate(sample = "First Term")
  
  # second term sample
  # ----------------------------------------------------------------
  df_reg <- df %>% filter(firstterm==0)
  
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
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,2,3)){
    
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
             spec = ifelse(spec=="2","+ Controls",spec),
             spec = ifelse(spec=="3","+ State-Year FE",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Baseline","+ State-Year FE","+ Controls"))  
    
    if(spec==1){
      table_second <- out
    }
    else{
      table_second <- rbind(table_second,out)
    }
  }
  
  table_second <- table_second %>% mutate(sample = "Second Term")
  
  table <- rbind(table_first,table_second)
  
  shapes <-  c(17,15,19)
  color_graph <- c("grey13","grey48")
  
  graph <- table %>%
    ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color=sample,shape = spec))+
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_pointrange(size = 0.5, alpha = 0.8, position = position_dodge(width=0.6)) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
    scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    scale_color_manual(values = color_graph) +
    scale_shape_manual(values = shapes) +
    theme_light() +
    labs(y = var_name,
         color = "Sample",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=8),
          legend.position="bottom",
          legend.box = "vertical",
          legend.spacing.y = unit(0.0001, "cm")) +
    guides(colour = guide_legend(order = 1), 
           shape = guide_legend(order = 2))
  
  
  
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



# 10. Variables labels dictionary
# =================================================================

dict <- rbind(cbind('finbra_desp_c_pcapita','Total Spending per capita'),
              cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita'),
              cbind('finbra_desp_investimento_pcapita','Investment Spending per capita'),
              cbind('finbra_desp_adm_pcapita','Administrative Spending per capita'),
              cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita'),
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














