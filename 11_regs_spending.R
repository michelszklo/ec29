#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for the spending outcomes
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
            'lfe')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)

path <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"


# 1. Load data frame
# =================================================================
df <- readRDS(paste0(path,"data/CONSOL_DATA.RDS"))


df <- df %>% 
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
 


# 2. regressions' variables
# =================================================================

df <- df %>% 
  # lag of health revenue
  mutate(health_revenue = siops_rimpostosetransfconst/pop) %>% 
  mutate(health_revenue_tm1 = lag(health_revenue,1)) %>% 
  # ec29 spending target
  mutate(ec29_target_spending = health_revenue*0.15) %>% 
  # lag of own resource spending
  mutate(own_resource_spending_tm1 = lag(siops_desprecpropriosaude_pcapita),1) %>%
  # difference between per capita own resource spending in t-1 and the constitutional minimum in t
  mutate(delta_ec29 = ec29_target_spending - own_resource_spending_tm1) %>%
  # lag of total spending per capita - finbra
  mutate(finbra_health_spending_tm2 = lag(finbra_desp_saude_san_pcapita,2),
         finbra_health_spending_tm1 = lag(finbra_desp_saude_san_pcapita,1)) %>% 
  # lag of total spending per capita - siops
  mutate(siops_health_spending_tm2 = lag(siops_despsaude_pcapita,2),
         siops_health_spending_tm1 = lag(siops_despsaude_pcapita,1)) %>% 
  # state year fixed effects
  mutate(uf_y_fe = as.factor(paste0(ano,"_",uf))) %>% 
  # % of own resource spending in the baseline
  mutate(pct_ownresource_spending_baseline = ifelse(ano==2000,siops_pct_recproprios_ec29,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(pct_ownresource_spending_baseline = mean(pct_ownresource_spending_baseline, na.rm = T)) %>% 
  ungroup()
  



# 3. Setting regression samples
# =================================================================
# sample 1: municipalities below target (positive distance to the target)

df_pos <- df %>% 
  filter(delta_ec29>0)

df_neg <- df %>% 
  filter(delta_ec29<0)

  
# 4. Regression SPENDING Finbra
# =================================================================

df_reg <- df_pos %>% 
  mutate(ln_delta_ec29 = log(delta_ec29),
         ln_finbra_health_spending_tm2 = log(finbra_health_spending_tm2),
         ln_health_revenue_tm1 = log(health_revenue_tm1))


df_reg <- df_neg %>% 
  mutate(ln_delta_ec29 = log(-delta_ec29),
         ln_finbra_health_spending_tm2 = log(finbra_health_spending_tm2),
         ln_health_revenue_tm1 = log(health_revenue_tm1))

y <- "finbra_desp_saude_san_pcapita"
ln_y <- paste0("ln_",y)
df_reg[ln_y] <- df_reg[y]

df_reg <- df_reg %>% 
  mutate_at(ln_y,log)

model_formula <- as.formula(paste(ln_y,
                                  " ~ ",
                                  "ln_delta_ec29 + ln_finbra_health_spending_tm2 ",
                                  " | cod_mun + uf_y_fe | 0 | cod_mun")
)

fit <- felm(model_formula, data = df_reg, exactDOF = TRUE)


summary(fit)


