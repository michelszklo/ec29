#######################################################################################################
# Author: Michel Szklo
# June 2024
# 
# This scripts downloads other economic data for robustness checks:
#     - labor: number of formal workers
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
            'ggarchery',
            'basedosdados')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"


# google cloud set for using Base dos Dados API

set_billing_id("base-dos-dados-427520")

query_dict <- bdplyr("br_me_rais.dicionario")
df_dict <- bd_collect(query_dict)



# 1. RAIS download and collapse
# =================================================================
# 
query <- bdplyr("br_me_rais.microdados_estabelecimentos")


for (i in seq.int(1996,2010)){
  print(i)
  q_i <- query %>%
    dplyr::filter(ano==i) %>%
    dplyr::select(ano,sigla_uf,id_municipio,quantidade_vinculos_ativos)
  
  df <- bd_collect(q_i)
  print(paste0(i," downloaded"))
  
  saveRDS(df,file=paste0(dir,"data/RAIS/RAIS",i,".Rds"))
  print(paste0(i," saved"))

}


# 2. RAIS consol
# =================================================================

path <- paste0(dir,"data/RAIS/")
files <- list.files(path, pattern = "\\.Rds$", full.names = TRUE)

rais <- lapply(files, readRDS)

rais <- do.call(rbind, rais)
rais <- rais %>% 
  group_by(ano,sigla_uf,id_municipio) %>% 
  summarise(rais_employees = sum(quantidade_vinculos_ativos,rm.na = T),
            rais_employeers = n()) %>% 
  ungroup() %>% 
  rename(cod_mun = id_municipio) %>% 
  mutate(cod_mun = as.numeric(substr(cod_mun,1,6)))

saveRDS(rais,file = paste0(dir,"data/RAIS/rais_consol.rds"))
write.csv(rais, file = paste0(dir,"data/RAIS/rais_consol.csv"),row.names = T)

