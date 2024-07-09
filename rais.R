#######################################################################################################
# Author: Michel Szklo
# June 2024
# 
# This scripts downloads other economic data for robustness checks:
#     - education: number of students
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



# 1. RAIS
# =================================================================
# 
query <- bdplyr("br_me_rais.microdados_vinculos")


for (i in seq.int(1996,2010)){
  print(i)
  q_i <- query %>%
    dplyr::filter(ano==i) %>%
    dplyr::select(ano,sigla_uf,id_municipio,tipo_vinculo,
                  vinculo_ativo_3112,tempo_emprego,quantidade_horas_contratadas)
  
  df <- bd_collect(q_i)
  print("downloaded")
  df <- df %>% 
    group_by(ano,sigla_uf,id_municipio,tipo_vinculo,vinculo_ativo_3112) %>% 
    summarise(tempo_emprego = mean(tempo_emprego, na.rm = T),
              quantidade_horas_contratadas = mean(quantidade_horas_contratadas, na.rm = T))
  
  saveRDS(df,file=paste0(dir,"data/RAIS/RAIS",i,".Rds"))
  print("saved")

}