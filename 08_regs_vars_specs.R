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
#Set-up path for principal directory
if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/"
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}
SRC <- paste0(dir,"source/")
DAT <- paste0(dir,"data/processed/")
TAB <- paste0(dir,"results/tables/")
FIG <- paste0(dir,"results/figures/")

# loading Folder, files and instrument setup
load(paste0(DAT,"output_setup.RData"))


if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/"
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}

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
raw <- readRDS(paste0(DAT,"CONSOL_DATA.RDS"))


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
  mutate(finbra_desp_o_pcapita = ifelse(finbra_desp_o_pcapita==0,NA,finbra_desp_o_pcapita),
         finbra_desp_c_pcapita = ifelse(finbra_desp_c_pcapita==0,NA,finbra_desp_c_pcapita),
         finbra_reccorr_pcapita = ifelse(finbra_reccorr_pcapita==0,NA,finbra_reccorr_pcapita),
         finbra_recorc_pcapita = ifelse(finbra_recorc_pcapita==0,NA,finbra_recorc_pcapita),
         finbra_desp_saude_san_pcapita = ifelse(finbra_desp_saude_san_pcapita==0,NA,finbra_desp_saude_san_pcapita),
         siops_rimpostosetransfconst = ifelse(siops_rimpostosetransfconst==0,NA,siops_rimpostosetransfconst)) %>% 
  mutate(siops_despinvest = siops_despinvest_pcapita * pop,
         siops_desppessoal = siops_desppessoal_pcapita * pop,
         siops_despservicoster = siops_despservicoster_pcapita * pop,
         siops_despoutros = siops_despoutros_pcapita * pop)


# adjusting uf code for 1996 and 1997
dict <- df %>% select(cod_uf,uf) %>% unique() %>% filter(!is.na(uf))

df <- df %>% 
  select(-uf) %>% 
  left_join(dict, by = "cod_uf") %>% 
  select(ano,cod_mun,mun_name,cod_uf,uf,everything())

rm(dict)

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
  mutate(pre_96 = ifelse(ano==1996,1,0)) %>% 
  mutate(pre_97 = ifelse(ano==1997,1,0)) %>% 
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
         peso_a3 = mean(pop_40, na.rm = T),
         peso_r = mean(finbra_reccorr_pcapita,na.rm = T),
         peso_m = mean(pop_fem_10_49,na.rm = T),
         peso_ha = mean(pop_45,na.rm = T),
         peso_ha1 = mean(pop_25_44,na.rm = T),
         peso_ha2 = mean(pop_45_54,na.rm = T),
         peso_pop = mean(pop,na.rm = T)) %>% 
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
# change in spending 2000-2005 in the baseline
# mutate(change_05_siops_despsaude_pcapita_baseline = ifelse(ano==2000,change_05_siops_despsaude_pcapita,NA)) %>% 
# group_by(cod_mun) %>% 
# mutate(change_05_siops_despsaude_pcapita_baseline = mean(change_05_siops_despsaude_pcapita_baseline,na.rm = T)) %>% 
# ungroup() 


# defining instrument/treatment
# ------------------------------------------------
df["iv"] <- df[instrument]
df <-  df %>% 
  # above and below
  mutate(iv_a = ifelse(iv<=0,iv,0),
         iv_b = ifelse(iv>0,iv,0)) %>% 
  mutate(iv_a = -iv_a) %>% 
  # binary treatment
  mutate(iv_binary = ifelse(iv>0,1,0))




# baseline controls
# ------------------------------------------------
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
# ------------------------------------------------
df <- df %>% 
  group_by(cod_mun) %>% 
  mutate(t = seq(-4,15,1)) %>% 
  ungroup()


# creating t * baseline controls
# ------------------------------------------------
interact_vars <- sapply(controlsvar_baseline, function(x) paste0("t_",x), simplify = "array", USE.NAMES = F)
df[interact_vars] <- df[controlsvar_baseline]
df <- df %>% 
  mutate_at(interact_vars,`*`,quote(t))


# creating t * baseline IMR
# ------------------------------------------------
df <- df %>% 
  mutate(t_tx_mi_baseline = tx_mi_baseline * t)



# creating year dummies and interacting with treatment
# ------------------------------------------------


df <- df %>% 
  dummy_cols(select_columns = "ano", ignore_na = TRUE)

yeardummies <- grep("^ano_",names(df),value = T)
dummies <- c(grep("^pre",names(df),value = T),grep("^post_",names(df), value = T))

# continuous and binary
yeartreat_dummies <- sapply(dummies, function(x) paste0(x,"_",instrument), simplify = "array", USE.NAMES = F)
yeartreat_dummies_binary <- sapply(dummies, function(x) paste0(x,"_",instrument,"_binary"), simplify = "array", USE.NAMES = F)


df[yeartreat_dummies] <- df[dummies]

df <- df %>% 
  mutate_at(yeartreat_dummies, `*`,quote(iv)) %>% 
  unnest(all_of(yeartreat_dummies))

df[yeartreat_dummies_binary] <- df[dummies]

df <- df %>% 
  mutate_at(yeartreat_dummies_binary, `*`,quote(iv_binary)) %>% 
  unnest(all_of(yeartreat_dummies_binary))


# above and below
yeartreat_dummies_above <- sapply(yeartreat_dummies, function(x) paste0("above_",x),simplify = "array", USE.NAMES = F)
yeartreat_dummies_below <- sapply(yeartreat_dummies, function(x) paste0("below_",x),simplify = "array", USE.NAMES = F)

df[yeartreat_dummies_above] <- df[yeartreat_dummies]
df[yeartreat_dummies_below] <- df[yeartreat_dummies]

df <- df %>% 
  mutate_at(yeartreat_dummies_above, ~ ifelse(. <= 0, ., 0)) %>% 
  mutate_at(yeartreat_dummies_below, ~ ifelse(. > 0, ., 0)) %>% 
  mutate_at(yeartreat_dummies_above, function(x) -x)

yeartreat_dummies_ab <- c(yeartreat_dummies_above,yeartreat_dummies_below)

# creating reweighting variable for average causal response (Callaway et al., 2023)
# ------------------------------------------------
dfw <- df %>% 
  filter(ano==2000) %>% 
  select(dist_ec29_baseline,cod_mun) %>% 
  mutate(max = max(dist_ec29_baseline,na.rm = T),
         min = min(dist_ec29_baseline,na.rm = T),
         varD = var(dist_ec29_baseline,na.rm = T),
         ED = mean(dist_ec29_baseline,na.rm = T),
         range = max-min,
         N = nrow(.)) %>% 
  arrange(dist_ec29_baseline) %>% 
  mutate(n = row_number()) %>% 
  mutate(grid = (n-1)/(N-1)*range + min,
         CGSw = 0)
# Loop to create CGS weights
for(i in seq.int(1,nrow(dfw))){
  ED <- dfw$ED[i]
  varD <- dfw$varD[i]
  l <- dfw$grid[i]
  dfw_l <- dfw %>% filter(dist_ec29_baseline>l | is.na(dist_ec29_baseline))
  EDl <- mean(dfw_l$dist_ec29_baseline, na.rm = T)
  PDl <- nrow(dfw_l)/dfw$N[i]
  dfw[i,"CGSw"] <- ((EDl - ED) * PDl) / varD
}
dfw <- dfw[complete.cases(dfw$dist_ec29_baseline), ]
density_values <- density(dfw$dist_ec29_baseline)
dfw$empiric <- approx(density_values$x, density_values$y, 
                      xout = dfw$dist_ec29_baseline)$y
df <- left_join(df,select(dfw,cod_mun,CGSw,empiric),by="cod_mun")
#df$CGSw[df$CGSw<0.2]<-0.2
df$reweight    <- df$empiric/df$CGSw
df$reweightPop <- ifelse(is.na(df$reweight*df$peso_pop), df$peso_pop, df$reweight*df$peso_pop)


# 3. Setting regression sample
# =================================================================

# filters to 2010
df <- df %>%
  filter(ano<=2010)

# transform treatment into treatment*post
df <- df %>%
  mutate(iv=ifelse(ano<=2000,0,iv),
         iv_a = ifelse(ano<=2000,0,iv_a),
         iv_b = ifelse(ano<=2000,0,iv_b),
         iv_binary = ifelse(ano<=2000,0,iv_binary))



# 4. Regression specifications (POST)
# =================================================================

# controls <- c(grep("^t_", names(df), value = T),"finbra_desp_saude_san_pcapita_neighbor","lrf")
baseline_controls <- grep("^t_", names(df), value = T)
baseline_controls <- baseline_controls[3:length(baseline_controls)-1]

tvarying_controls <- c("gdp_mun_pcapita","pbf_pcapita")

fiscal_controls <- c("finbra_desp_saude_san_pcapita_neighbor","lrf")

imr_controls <- "t_tx_mi_baseline"

controls <- c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls)


# 1) Continuous 
# ------------------------------------------------

# standard outcomes
spec1_post_cont_c <- paste(" ~ ","iv"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_cont_c <- paste(" ~ ","iv"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_cont_c <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_cont_c <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_imr_cont_c <- paste(" ~ ","iv"," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr_cont_c <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr_cont_c <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr_cont_c <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec5_post_imr_cont_c <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 2) Continuous above + below
# ------------------------------------------------

# standard outcomes
spec1_post_cont_a <- paste(" ~ ","iv_a + iv_b"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_cont_a <- paste(" ~ ","iv_a + iv_b"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_cont_a <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_cont_a <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_imr_cont_a <- paste(" ~ ","iv_a + iv_b"," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr_cont_a <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr_cont_a <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr_cont_a <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec5_post_imr_cont_a <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# 3) Continuous below + above
# ------------------------------------------------

# standard outcomes
spec1_post_cont_b <- paste(" ~ ","iv_b + iv_a"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_cont_b <- paste(" ~ ","iv_b + iv_a"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_cont_b <- paste(" ~ ","iv_b + iv_a"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_cont_b <- paste(" ~ ","iv_b + iv_a"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_imr_cont_b <- paste(" ~ ","iv_b + iv_a"," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr_cont_b <- paste(" ~ ","iv_b + iv_a"," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr_cont_b <- paste(" ~ ","iv_b + iv_a"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr_cont_b <- paste(" ~ ","iv_b + iv_a"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec5_post_imr_cont_b <- paste(" ~ ","iv_b + iv_a"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# 4) Binary model
# ------------------------------------------------

# standard outcomes
spec1_post_binary <- paste(" ~ ","iv_binary"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_binary <- paste(" ~ ","iv_binary"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_binary <- paste(" ~ ","iv_binary"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_binary <- paste(" ~ ","iv_binary"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_imr_binary <- paste(" ~ ","iv_binary"," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr_binary <- paste(" ~ ","iv_binary"," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr_binary <- paste(" ~ ","iv_binary"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr_binary <- paste(" ~ ","iv_binary"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec5_post_imr_binary <- paste(" ~ ","iv_binary"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# 5. Regression specifications (yearly)
# =================================================================


# 1) Continuous
# ------------------------------------------------

# standard outcomes
spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# imr outcomes
spec1_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_imr <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 2) Continuous above + below
# ------------------------------------------------

# standard outcomes
spec1_post_y_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_y_imr_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_imr_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_imr_ab <- paste(" ~ ",paste(yeartreat_dummies_ab, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")




# 3) Binary
# ------------------------------------------------

# standard outcomes
spec1_post_y_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_y_imr_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_imr_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_imr_binary <- paste(" ~ ",paste(yeartreat_dummies_binary, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# 6. Regression functions
# =================================================================

reduced <- function(outcome,var_name,df,regression_output,transform,year_filter,weight,reg_type){
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  #DC 19 Aug 2024: commented out
  #df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4,5)){
    spec_reduced<- get(paste0("spec",spec,"_post_imr_",reg_type))
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

reduced_imr <- function(outcome,var_name,df,regression_output,transform,year_filter,weight,reg_type){
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_imr_",reg_type))
    
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
  for (reg_type in c("cont_c","cont_a","cont_b","binary")){
    
    d <- df
    obj <- paste0("reg_",reg_type) # name of the output object
    
    reduced(var,var_name,d,obj,transform,year_filter,weight = weight,reg_type = reg_type) # function for reduced form regression
    
    print(paste0(reg_type," regs were run"))
  } 
  
  # 2sls final tables
  
  organizing_table <- function(d,sample_name,withspec){
    
    obs_1 <- d %>% slice(1) %>% select(nobs) %>% as.numeric()
    obs_2 <- d %>% slice(2) %>% select(nobs) %>% as.numeric()
    obs_3 <- d %>% slice(3) %>% select(nobs) %>% as.numeric()
    obs_4 <- d %>% slice(4) %>% select(nobs) %>% as.numeric()
    obs_5 <- d %>% slice(5) %>% select(nobs) %>% as.numeric()
    
    obs_name <- paste0("obs_",sample_name)
    
    
    if(withspec==0){
      
      table_1 <- d %>% mutate(sample = sample_name) %>% table_formating(1) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1)
      table_2 <- d %>% mutate(sample = sample_name) %>% table_formating(2) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2)
      table_3 <- d %>% mutate(sample = sample_name) %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3)
      table_4 <- d %>% mutate(sample = sample_name) %>% table_formating(4) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4)
      table_5 <- d %>% mutate(sample = sample_name) %>% table_formating(5) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_5)
      
    } else {
      
      table_1 <- d %>% mutate(sample = sample_name) %>% table_formating(1) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1) %>% mutate(spec=1)
      table_2 <- d %>% mutate(sample = sample_name) %>% table_formating(2) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2) %>% mutate(spec=2)
      table_3 <- d %>% mutate(sample = sample_name) %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3) %>% mutate(spec=3)
      table_4 <- d %>% mutate(sample = sample_name) %>% table_formating(4) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4) %>% mutate(spec=4)
      table_5 <- d %>% mutate(sample = sample_name) %>% table_formating(5) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_5) %>% mutate(spec=5)
      
      
    }
    
    
    name1 <- paste0("table_1_",sample_name)
    name2 <- paste0("table_2_",sample_name)
    name3 <- paste0("table_3_",sample_name)
    name4 <- paste0("table_4_",sample_name)
    name5 <- paste0("table_5_",sample_name)
    
    assign(name1,table_1, envir = parent.frame()) 
    assign(name2,table_2, envir = parent.frame()) 
    assign(name3,table_3, envir = parent.frame()) 
    assign(name4,table_4, envir = parent.frame()) 
    assign(name5,table_5, envir = parent.frame()) 
    
    
  }
  
  
  organizing_table(reg_cont_c,"cont_c",0)
  organizing_table(reg_cont_a,"cont_a",0)
  organizing_table(reg_cont_b,"cont_b",0)
  organizing_table(reg_binary,"binary",1)
  
  
  
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
  
  binding("cont_c")
  binding("cont_a")
  binding("cont_b")
  binding("binary")
  
  
  
  table_all <- bind_cols(table_cont_c,
                         table_cont_a %>% select(-term),
                         table_cont_b %>% select(-term),
                         table_binary %>% select(-term)
  )
  
  
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects

regress_output_imr <- function(var,var_name,transform,year_filter,weight){
  
  # FULL SAMPLE
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (reg_type in c("cont_c","cont_a","cont_b","binary")){
    
    d <- df
    obj <- paste0("reg_",reg_type) # name of the output object
    
    reduced_imr(var,var_name,d,obj,transform,year_filter,weight = weight,reg_type = reg_type) # function for reduced form regression
    
    print(paste0(reg_type," regs were run"))
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
  
  
  organizing_table(reg_cont_c,"cont_c",1)
  organizing_table(reg_cont_a,"cont_a",0)
  organizing_table(reg_cont_b,"cont_b",0)
  organizing_table(reg_binary,"binary",0)
  
  
  
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
  
  binding("cont_c")
  binding("cont_a")
  binding("cont_b")
  binding("binary")
  
  
  
  table_all <- bind_cols(table_cont_c,
                         table_cont_a %>% select(-term),
                         table_cont_b %>% select(-term),
                         table_binary %>% select(-term)
  )
  
  
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects




# 8. Regression graphs
# =================================================================
# 
outcome <- 'siops_despsaude_pcapita'
var_name <- 'Infant Mortality Rate'
transform <- 1
year_filter <- 1998
y0 <- -3
yf <- 9.25
ys <- 1
cont <- 1
weight <- "peso_pop"
year_cap <- 2010
label_size = 8


reduced_yearly <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  
  
  if(missing(label_size)){
    ylabel <- 11
  }else{
    ylabel <-  label_size
  }
  
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  #df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  # Descriptive
  # ------------------------------------
  if (spec == 1){
    df_2000 <- df_reg %>% filter(ano == 2000)
    
    # Calculate statistics for the year 2000
    mean_value <- mean(df_2000[[ln_outcome]], na.rm = TRUE)
    min_value <- min(df_2000[[ln_outcome]], na.rm = TRUE)
    max_value <- max(df_2000[[ln_outcome]], na.rm = TRUE)
    sd_value <- sd(df_2000[[ln_outcome]], na.rm = TRUE)
    num_obs <- nrow(df_2000)    

    p <- ggplot(df_2000, aes(x = .data[[ln_outcome]])) +
      geom_density(fill = "skyblue", alpha = 0.5) +
      labs(title = "Kernel Density Plot for Year 2000") +
      annotate("text", 
               x = Inf, y = Inf, 
               label = paste("Mean:", round(mean_value, 2), 
                             "\nMin:", round(min_value, 2), 
                             "\nMax:", round(max_value, 2), 
                             "\nSD:", round(sd_value, 2), 
                             "\nN:", num_obs),
               hjust = 1.1, vjust = 1.1, size = 4, color = "blue") +
      theme(plot.margin = unit(c(1, 1, 1.5, 1), "lines"))
    
    dname <- "desc"
    ggsave(paste0(FIG,yearly_folder,dname,"_",outcome,".pdf"),
           plot = p,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
  }  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  num_obs <- summary(fit)$N
  table <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    t <- table[i,]
    t <- t %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!var_name := estimate)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
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
  
  
  # graph with now bounds defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  return(data.frame(year= table$year, estimates=table$estimate, lb=table$lb, 
                    ub=table$ub, lb2=table$lb2, ub2=table$ub2))
  
  
  
}

reduced_yearly_imr <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3,base_year=2000){
  
  
  if(missing(label_size)){
    ylabel <- 11
  }else{
    ylabel <-  label_size
  }
  
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  #df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
    
  # Descriptive
  # ------------------------------------
  if (spec == 1){
    df_2000 <- df_reg %>% filter(ano == base_year)
  
    # Calculate statistics for the year 20''
    mean_value <- mean(df_2000[[ln_outcome]], na.rm = TRUE)
    min_value <- min(df_2000[[ln_outcome]], na.rm = TRUE)
    max_value <- max(df_2000[[ln_outcome]], na.rm = TRUE)
    sd_value <- sd(df_2000[[ln_outcome]], na.rm = TRUE)
    num_obs <- nrow(df_2000)    
    
    Title = paste("Kernel Density Plot for Year", base_year)
    p <- ggplot(df_2000, aes(x = .data[[ln_outcome]])) +
      geom_density(fill = "skyblue", alpha = 0.5) +
      labs(title = Title) +
      annotate("text", 
               x = Inf, y = Inf, 
               label = paste("Mean:", round(mean_value, 2), 
                             "\nMin:", round(min_value, 2), 
                             "\nMax:", round(max_value, 2), 
                             "\nSD:", round(sd_value, 2), 
                             "\nN:", num_obs),
               hjust = 1.1, vjust = 1.1, size = 4, color = "blue") +
      theme(plot.margin = unit(c(1, 1, 1.5, 1), "lines"))
    
    dname <- "desc"
    ggsave(paste0(FIG,yearly_folder,dname,"_",outcome,".pdf"),
           plot = p,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
  }
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_binary"))
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  print(summary(fit))  
  print(summary(fit)$N)  
  num_obs <- summary(fit)$N

table <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    t <- table[i,]
    t <- t %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!var_name := estimate)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  } else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  # graph with now bounds defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  print(fit)
  print(table)
  return(data.frame(year= table$year, estimates=table$estimate, lb=table$lb, 
                    ub=table$ub, lb2=table$lb2, ub2=table$ub2))
  
  
}

reduced_yearly_imr_ext <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  
  if(missing(label_size)){
    ylabel <- 11
  }else{
    ylabel <-  label_size
  }
  
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter) %>% 
    select(-lrf,-finbra_desp_saude_san_pcapita_neighbor,-mun_name) %>% 
    mutate(pbf_pcapita = ifelse(is.na(pbf_pcapita),0,pbf_pcapita))
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_binary"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(1:15) %>%
    select(term,estimate,std.error) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2)) %>% 
    filter(!is.na(estimate))
  
  
  
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
  
  
  # graph with now bounds defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
      # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
      geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}


reduced_yearly_ab <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  
  if(missing(label_size)){
    ylabel <- 11
  }else{
    ylabel <-  label_size
  }
  
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_ab),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  #df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_ab"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary_ab"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  num_obs <- summary(fit)$N
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(18:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  tableA <- table %>% filter(target=="Above")
  tableB <- table %>% filter(target=="Below")
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    tA <- tableA[i,]
    tA <- tA %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Above)") := estimate)
    
    tB <- tableB[i,]
    tB <- tB %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Below)") := estimate)
    
    t <- tA %>% bind_cols(tB)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  
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
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom",
            legend.title = element_blank())
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1),
                 aes(color = target)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "grey50") +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "grey50") +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1),
                 aes(color = target)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "grey50") +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "grey50") +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  return(data.frame(year= table$year, estimates=table$estimate, lb=table$lb, 
                    ub=table$ub, lb2=table$lb2, ub2=table$ub2, 
                    target=table$target))
}

reduced_yearly_ab_imr <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  if(missing(label_size)){
    ylabel <- 11
  }else{
    ylabel <-  label_size
  }
  
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_ab),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  #df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary_ab"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
print(summary(fit)) 
print(summary(fit)$N) 
num_obs <- summary(fit)$N

  table1 <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(18:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  
  tableA <- table %>% filter(target=="Above")
  tableB <- table %>% filter(target=="Below")
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    tA <- tableA[i,]
    tA <- tA %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Above)") := estimate)
    
    tB <- tableB[i,]
    tB <- tB %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Below)") := estimate)
    
    t <- tA %>% bind_cols(tB)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
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
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom",
            legend.title = element_blank())
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
     
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>% filter(!is.nan(estimate)) %>% 
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification",
           caption = paste("Number of observations:", num_obs)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 11, face = "bold"),
            axis.title.x = element_text(size=11),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 11),
            legend.position="bottom")
    
    
    ggsave(paste0(FIG,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  return(data.frame(year= table$year, estimates=table$estimate, lb=table$lb, 
                    ub=table$ub, lb2=table$lb2, ub2=table$ub2, 
                    target=table$target))
  
}




# 10. Saving
# =================================================================

rm(raw)

save.image(paste0(DAT,"regs.RData"))














