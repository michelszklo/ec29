#######################################################################################################
# Author: Michel Szklo
# August 2025
# 
# This script implements descriptive analysis of presence of hospitals at the municipal level.
# 
# 
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
            'plotly',
            'ggplot2',
            'xlsx',
            'httr',
            'mapview',
            'stringdist',
            'data.table')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


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

# ------------------------------------


# 1. Loading datasets
# =================================================================

load(paste0(DAT,"regs.RData"))
sih <- readRDS(paste0(dir,"data/SIH/","sih_hosp_1996_2010.rds")) %>% 
  mutate(ANO_CMPT = ifelse(ANO_CMPT<100,ANO_CMPT+1900,ANO_CMPT))
sih_tabnet_all <- fread(paste0(dir,"data/SIH/","SIH_n_hospitais_ano.csv"),skip = 4,encoding = "Latin-1")
sih_tabnet_gen <- fread(paste0(dir,"data/SIH/","SIH_n_hospitais_ano_geral.csv"),skip = 4,encoding = "Latin-1")
sih_tabnet_esp <- fread(paste0(dir,"data/SIH/","SIH_n_hospitais_ano_especializado.csv"),skip = 4,encoding = "Latin-1")
sih_tabnet_mix <- fread(paste0(dir,"data/SIH/","SIH_n_hospitais_ano_mista.csv"),skip = 4,encoding = "Latin-1")



# vars <- names(d)[2:length(names(d))]
# 
# d <- d %>% 
#   mutate(cod_mun = as.numeric(substr(Município,1,6))) %>% 
#   select(-Município) %>% 
#   mutate_at(all_of(vars), as.numeric) %>% 
#   filter(!is.na(cod_mun)) %>% 
#   pivot_longer(cols = vars,
#                names_to = "mes",
#                values_to = "n") %>% 
#   mutate(n = ifelse(is.na(n),0,n)) %>% 
#   mutate(ano = as.numeric(substr(mes,1,4))) %>% 
#   group_by(cod_mun,ano) %>% 
#   summarise(sih_hosp = max(n)) %>% 
#   ungroup()



# 2. clean sih tabnet and merge
# =================================================================
tabnet_clean <- function(df,varname){
  
  vars <- names(df)[2:length(names(df))]
  
  df <- df %>% 
    mutate(cod_mun = as.numeric(substr(Município,1,6))) %>% 
    select(-Município) %>% 
    mutate_at(all_of(vars), as.numeric) %>% 
    filter(!is.na(cod_mun)) %>% 
    pivot_longer(cols = vars,
                 names_to = "mes",
                 values_to = "n") %>% 
    mutate(n = ifelse(is.na(n),0,n)) %>% 
    mutate(ano = as.numeric(substr(mes,1,4))) %>% 
    group_by(cod_mun,ano) %>% 
    summarise(!!sym(varname) := max(n)) %>% 
    ungroup()
  
  return(df)
  
}

sih_tabnet_all <- sih_tabnet_all %>% tabnet_clean("sih_hosp")
sih_tabnet_gen <- sih_tabnet_gen %>% tabnet_clean("sih_hosp_gen")
sih_tabnet_esp <- sih_tabnet_esp %>% tabnet_clean("sih_hosp_esp")
sih_tabnet_mix <- sih_tabnet_mix %>% tabnet_clean("sih_hosp_mix")

sih_tabnet <- sih_tabnet_all %>% 
  left_join(sih_tabnet_gen, by = c("cod_mun","ano")) %>% 
  left_join(sih_tabnet_esp, by = c("cod_mun","ano")) %>% 
  left_join(sih_tabnet_mix, by = c("cod_mun","ano")) %>% 
  mutate_at(c("sih_hosp_gen","sih_hosp_esp","sih_hosp_mix"), ~ ifelse(is.na(.), 0, .))

rm(sih_tabnet_all,sih_tabnet_gen,sih_tabnet_esp,sih_tabnet_mix)

# 2. Adjusting SIH data and collapsing by year
# =================================================================

# # List of specialized ICDs
# 
# cids_especializados_restritos <- c(
#   # Oncologia de alta concentração
#   paste0("C", str_pad(0:97, 2, pad = "0")),     # C00–C97
#   paste0("D", str_pad(0:48, 2, pad = "0")),     # D00–D48 (neoplasias benignas e incertas)
#   
#   # Psiquiatria
#   paste0("F", str_pad(0:99, 2, pad = "0")),     # F00–F99
#   
#   # Neonatal/perinatal muito específico
#   paste0("P", str_pad(0:96, 2, pad = "0")),     # P00–P96
#   
#   # Malformações congênitas
#   paste0("Q", str_pad(0:99, 2, pad = "0")),     # Q00–Q99
#   
#   # Ortopedia de alta complexidade e trauma extenso
#   paste0("M", str_pad(0:99, 2, pad = "0")),     # M00–M99
#   paste0("S", str_pad(0:99, 2, pad = "0")),     # S00–S99
#   paste0("T", str_pad(0:98, 2, pad = "0"))      # T00–T98
# )
# 
# sih_collapse_type <- sih %>% 
#   filter(ANO_CMPT>=1998) %>% 
#   mutate(CID_10 = substr(DIAG_PRINC,1,3)) %>%
#   mutate(esp = 0) %>% 
#   mutate(esp = ifelse(CID_10 %in% cids_especializados_restritos,1,esp)) %>% 
#   rename(cod_mun = MUNIC_MOV,
#          ano = ANO_CMPT) %>% 
#   group_by(cod_mun,ano,CGC_HOSP) %>% 
#   summarise(esp = max(esp,na.rm = T)) %>% 
#   ungroup() %>% 
#   group_by(cod_mun,ano,esp) %>% 
#   summarise(n = n()) %>%
#   ungroup() %>% 
#   pivot_wider(names_from = "esp",
#               values_from = "n",
#               names_prefix = "hosp_") %>%
#   mutate_at(c("hosp_geral","hosp_especializado"),~ ifelse(is.na(.), 0, .)) %>% 
#   mutate(hosp_total = hosp_geral + hosp_especializado)

sih_collapse <- sih %>% 
  filter(ANO_CMPT>=1996) %>% 
  rename(cod_mun = MUNIC_MOV,
         ano = ANO_CMPT) %>% 
  group_by(cod_mun,ano) %>% 
  summarise(n = n_distinct(CGC_HOSP))



df_hosp <- df %>% 
  select(ano,cod_mun,dist_ec29_baseline,ams_hospital_mun,ams_hospital_nmun,pop) %>% 
  left_join(sih_collapse, by = c("cod_mun","ano")) %>% 
  left_join(sih_tabnet %>% select(cod_mun,ano,sih_hosp), by = c("cod_mun","ano")) %>% 
  mutate(hospital_ams = ams_hospital_mun + ams_hospital_nmun) %>% 
  rename(hospital_sih_micro = n,
         hospital_cnes = sih_hosp) %>% 
  mutate(below =0) %>% 
  mutate(below = ifelse(dist_ec29_baseline>0,1,below)) %>% 
  group_by(ano) %>% 
  summarise(hospital_ams = sum(hospital_ams, na.rm = T),
            hospital_sih_micro = sum(hospital_sih_micro, na.rm = T),
            hospital_cnes = sum(hospital_cnes, na.rm = T)) %>% 
  mutate(hospital_ams = ifelse(hospital_ams==0,NA,hospital_ams),
         hospital_cnes = ifelse(hospital_cnes==0,NA,hospital_cnes)) %>% 
  pivot_longer(cols = c(hospital_ams,hospital_sih_micro,hospital_cnes),
               names_to = "var",
               values_to = "n")


df_hosp_ab <- df %>% 
  select(ano,cod_mun,dist_ec29_baseline,ams_hospital_mun,ams_hospital_nmun,pop) %>% 
  left_join(sih_collapse, by = c("cod_mun","ano")) %>% 
  left_join(sih_tabnet %>% select(cod_mun,ano,sih_hosp), by = c("cod_mun","ano")) %>% 
  mutate(hospital_ams = ams_hospital_mun + ams_hospital_nmun) %>% 
  rename(hospital_sih_micro = n,
         hospital_cnes = sih_hosp) %>% 
  mutate(below =0) %>% 
  mutate(below = ifelse(dist_ec29_baseline>0,1,below)) %>% 
  group_by(ano,below) %>% 
  summarise(hospital_ams = sum(hospital_ams, na.rm = T),
            hospital_sih_micro = sum(hospital_sih_micro, na.rm = T),
            hospital_cnes = sum(hospital_cnes, na.rm = T)) %>% 
  mutate(hospital_ams = ifelse(hospital_ams==0,NA,hospital_ams),
         hospital_cnes = ifelse(hospital_cnes==0,NA,hospital_cnes)) %>% 
  pivot_longer(cols = c(hospital_ams,hospital_sih_micro,hospital_cnes),
               names_to = "var",
               values_to = "n")



plot <- df_hosp %>% 
  ggplot(aes(x = ano,y=n,color = var, group = var)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  labs(x = "Year",
       y = "Number of hospitals") +
  theme_light() +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=3,byrow=TRUE))
  

filepdf <- paste0(dir,"data/SIH/","number_hospitals.pdf")
ggsave(filepdf,
       plot = plot,
       device = "pdf",
       width = 6, height = 5,
       units = "in")



plot <- df_hosp_ab %>%
  mutate(below = as.factor(below)) %>% 
  filter(!is.na(below)) %>% 
  ggplot(aes(x = ano,y=n,color = var, shape = below)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  labs(x = "Year",
       y = "Number of hospitals",
       shape = "Below") +
  theme_light() +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=3,byrow=TRUE))


filepdf <- paste0(dir,"data/SIH/","number_hospitals_ab.pdf")
ggsave(filepdf,
       plot = plot,
       device = "pdf",
       width = 6, height = 5,
       units = "in")







