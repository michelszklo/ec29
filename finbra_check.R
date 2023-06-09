#######################################################################################################
# Author: Michel Szklo
# October 2022
# 
# This scripts plots outcome indexes by quantile of baseline distribution
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
            'ggsci',
            'gtools')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)
options(scipen = 50)

# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"



# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))

vars <- c("cod_mun","ano","dist_ec29_baseline","pop","gdp_mun_pcapita","finbra_desp_o_pcapita", "finbra_desp_pessoal_pcapita","finbra_reccorr_pcapita",
          "finbra_desp_investimento_pcapita", "finbra_desp_outros_nature_pcapita",
          "finbra_desp_saude_san_pcapita", "finbra_desp_nao_saude_pcapita", "finbra_desp_transporte_pcapita",
          "finbra_desp_educ_cultura_pcapita", "finbra_desp_hab_urb_pcapita", "finbra_desp_assist_prev_pcapita",
          "finbra_desp_outros_area_pcapita",
          "finbra_desp_educ_pcapita","finbra_desp_saude_pcapita","finbra_desp_san_pcapita")

vars2 <- c("pop","gdp_mun_pcapita","finbra_desp_o_pcapita", "finbra_desp_pessoal_pcapita", "finbra_reccorr_pcapita",
           "finbra_desp_investimento_pcapita", "finbra_desp_outros_nature_pcapita",
           "finbra_desp_saude_san_pcapita", "finbra_desp_nao_saude_pcapita", "finbra_desp_transporte_pcapita",
           "finbra_desp_educ_cultura_pcapita", "finbra_desp_hab_urb_pcapita", "finbra_desp_assist_prev_pcapita",
           "finbra_desp_outros_area_pcapita",
           "finbra_desp_educ_pcapita","finbra_desp_saude_pcapita","finbra_desp_san_pcapita")



df_teste <- df %>% 
  select(all_of(vars)) %>% 
  mutate(bin = "") %>% 
  mutate(bin = ifelse(dist_ec29_baseline < -0.074 & !is.na(dist_ec29_baseline),"1. Much Above",bin),
         bin = ifelse(dist_ec29_baseline >= -0.074 & dist_ec29_baseline < -0.023,"2. Above",bin),
         bin = ifelse(dist_ec29_baseline >= -0.023 & dist_ec29_baseline < 0,"3. Just Above",bin),
         bin = ifelse(dist_ec29_baseline >= 0 & dist_ec29_baseline < 0.0224,"4. Just Below",bin),
         bin = ifelse(dist_ec29_baseline >= 0.0224 & dist_ec29_baseline < 0.05795,"5. Below",bin),
         bin = ifelse(dist_ec29_baseline >= 0.05795 & dist_ec29_baseline < 0.0867,"6. Much Below",bin),
         bin = ifelse(dist_ec29_baseline >= 0.0867 & !is.na(dist_ec29_baseline),"7. Far Below",bin)) %>% 
  select(bin, all_of(vars)) %>% 
  mutate_at(vars2, ~na_if(., 0)) %>%
  group_by(cod_mun) %>% 
  mutate(check = finbra_desp_o_pcapita/dplyr::lag(finbra_desp_o_pcapita,1)) %>% 
  ungroup() %>%
  mutate(check_log = log(check),
         s = log(finbra_desp_o_pcapita)) %>% 
  select(check,check_log,s,everything())



df_select1 <- df_teste %>% 
  filter(ano<=2002) %>% 
  arrange(check) %>% 
  select(cod_mun) %>%
  unique() %>%
  slice(1:10) %>% 
  arrange(cod_mun)

df_select2 <- df_teste %>% 
  filter(ano<=2002) %>% 
  arrange(desc(check)) %>%
  select(cod_mun) %>%
  unique() %>%
  slice(1:10) %>% 
  arrange(cod_mun)

desv <- 5

df_select3 <- df_teste %>% 
  mutate(s1 = 7.10634965935 - 0.47612910375324*desv,
         s2 = 7.10634965935 + 0.47612910375324*desv) %>% 
  filter(s<=s1 | s>=s2) %>% 
  select(cod_mun) %>% 
  unique()


df_select_alt <- df_select3$cod_mun

df_select <- c(df_select1$cod_mun,df_select2$cod_mun) %>% unique()




df_plots <- df %>% 
  select(all_of(vars)) %>% 
  mutate(bin = "") %>% 
  mutate(bin = ifelse(dist_ec29_baseline < -0.074 & !is.na(dist_ec29_baseline),"1. Much Above",bin),
         bin = ifelse(dist_ec29_baseline >= -0.074 & dist_ec29_baseline < -0.023,"2. Above",bin),
         bin = ifelse(dist_ec29_baseline >= -0.023 & dist_ec29_baseline < 0,"3. Just Above",bin),
         bin = ifelse(dist_ec29_baseline >= 0 & dist_ec29_baseline < 0.0224,"4. Just Below",bin),
         bin = ifelse(dist_ec29_baseline >= 0.0224 & dist_ec29_baseline < 0.05795,"5. Below",bin),
         bin = ifelse(dist_ec29_baseline >= 0.05795 & dist_ec29_baseline < 0.0867,"6. Much Below",bin),
         bin = ifelse(dist_ec29_baseline >= 0.0867 & !is.na(dist_ec29_baseline),"7. Far Below",bin)) %>% 
  select(bin, all_of(vars)) %>% 
  # filter(cod_mun!=172093 & cod_mun!=171550 & cod_mun!=171525 & cod_mun!= 311300 & cod_mun!= 170980) %>%
  filter(!(cod_mun %in% df_select_alt)) %>%
  # filter(cod_mun!=311860) %>% 
  filter(!is.na(dist_ec29_baseline)) %>% 
  mutate_at(vars2, ~na_if(., 0)) %>% 
  group_by(ano, bin) %>% 
  summarise_at(vars2,mean,na.rm = TRUE) %>% 
  filter(!is.na(bin)) %>% 
  mutate(s = log(finbra_desp_o_pcapita))

# df_means <- df_plots %>% 
#   group_by(ano) %>% 
#   summarise_at(vars,mean,na.rm = TRUE)






# GDP

df_plots %>% ggplot(aes(x = ano,y = gdp_mun_pcapita,linetype = bin ,group = bin)) +
  geom_vline(xintercept = 2000, alpha = 0.3,size = 0.2) +
  geom_line(alpha = 0.7,size = 0.8) +
  scale_linetype_discrete() +
  # scale_linetype_manual(values=c("dashed","solid","solid","solid","dashed","dashed")) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
  labs(y = "") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# population

df_plots %>% ggplot(aes(x = ano,y = pop,linetype = bin ,group = bin)) +
  geom_vline(xintercept = 2000, alpha = 0.3,size = 0.2) +
  geom_line(alpha = 0.7,size = 0.8) +
  scale_linetype_discrete() +
  # scale_linetype_manual(values=c("dashed","solid","solid","solid","dashed","dashed")) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
  labs(y = "") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# spending
df_plots %>% ggplot(aes(x = ano,y = finbra_desp_o_pcapita,linetype = bin ,group = bin)) +
  geom_vline(xintercept = 2000, alpha = 0.3,size = 0.2) +
  geom_line(alpha = 0.7,size = 0.8) +
  scale_linetype_discrete() +
  # scale_color_discrete() +
  # scale_linetype_manual(values=c("dashed","solid","solid","solid","dashed","dashed")) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
  labs(y = "") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())




