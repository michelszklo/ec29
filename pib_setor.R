#######################################################################################################
# Author: Michel Szklo
# August 2024
# 
#  Consolidates municipal GDP by sector
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
            'sf',
            'sp',
            'xlsx',
            'httr',
            'mapview',
            'stringdist',
            'gridExtra',
            'binsreg',
            'lfe',
            'rJava',
            'lmtest',
            'sandwich',
            'fastDummies')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/PIB_municipal/"


# 1. Open csvs
# =================================================================

# agro
# -----------------------------------
pib_agro <- read.csv(paste0(dir,"pib_agro.csv"),skip = 1) %>% 
  select(-Sigla,-Município,-X) %>% 
  mutate(cod_mun = as.numeric(substr(as.character(Código),1,6))) %>% 
  select(cod_mun,everything(),-Código)
  

pib_agro <- pib_agro %>% 
  pivot_longer(cols = grep("X",colnames(pib_agro),value = T),
               names_to = "ano",
               values_to = "pib_agro") %>% 
  mutate(ano = as.numeric(substr(ano,2,5)))


# industry
# -----------------------------------
pib_ind <- read.csv(paste0(dir,"pib_ind.csv"),skip = 1) %>% 
  select(-Sigla,-Município,-X) %>% 
  mutate(cod_mun = as.numeric(substr(as.character(Código),1,6))) %>% 
  select(cod_mun,everything(),-Código)


pib_ind <- pib_ind %>% 
  pivot_longer(cols = grep("X",colnames(pib_ind),value = T),
               names_to = "ano",
               values_to = "pib_ind") %>% 
  mutate(ano = as.numeric(substr(ano,2,5)))


# service
# -----------------------------------
pib_serv <- read.csv(paste0(dir,"pib_serv.csv"),skip = 1) %>% 
  select(-Sigla,-Município,-X) %>% 
  mutate(cod_mun = as.numeric(substr(as.character(Código),1,6))) %>% 
  select(cod_mun,everything(),-Código)


pib_serv <- pib_serv %>% 
  pivot_longer(cols = grep("X",colnames(pib_serv),value = T),
               names_to = "ano",
               values_to = "pib_serv") %>% 
  mutate(ano = as.numeric(substr(ano,2,5)))
  


# 2. Merge and export
# =================================================================
pib_setor <- pib_agro %>% 
  left_join(pib_ind, by = c("cod_mun","ano")) %>% 
  left_join(pib_serv, by = c("cod_mun","ano"))
  
write.csv(pib_setor,paste0(dir,"pib_setor.csv"))

