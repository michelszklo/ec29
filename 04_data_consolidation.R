#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts consolidates the final dataset
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
            'readstata13')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

raw <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/"



# 1. List of municipalities
# =================================================================
mun_list <- read.csv(paste0(raw,"lista_mun/lista_mun_2000.csv"), encoding = "UTF-8") %>% 
  rowwise() %>% 
  mutate(ano = list(seq.int(1998,2010))) %>% 
  ungroup() %>% 
  unnest(ano) %>% 
  rename("cod_mun" = 1)
  


# 2. Expenditure deflator
# =================================================================
deflator <- read.csv(paste0(raw,"inflacao/igp_di.csv"), encoding = "Latin1", sep = ";") %>% 
  rename(ano = 1,
         deflator_saude = 2)


# 3. Importing FINBRA data
# =================================================================

finbra <- read.csv(paste0(raw,"Finbra/FINBRA.csv"), encoding = "UTF-8") %>% 
  select(ano,cod_mun,nome_mun,cod_uf,uf,pop,pop2000,everything()) %>% 
  select(-c(dep_gambiental,dep_orgagraria))

names(finbra) <- gsub("desp","finbra_desp",names(finbra))

finbra_receita <- read.csv(paste0(raw,"Finbra/FINBRA_receita.csv"), encoding = "UTF-8") %>% 
  rename(finbra_reccorr=reccorr) %>% 
  filter(ano<=2010)

finbra <- finbra %>% left_join(finbra_receita, by = c("ano","cod_mun"))
  
rm(finbra_receita)

# 3. Importing SIOPS data
# =================================================================

siops <- readRDS(paste0(raw,"SIOPS/SIOPS.rds")) %>% 
  select(ano, cod_mun,nome_mun,UF,cod_uf,estado,data,pop,everything())

names(siops)[9:65] <- paste("siops",names(siops)[9:65], sep = "_")


# 4. Merging Datasus Data
# =================================================================

# infra
infra <- data.frame(read.dta13(paste0(raw,"Infra/infra_psf.dta")))

# sinasc (birth - access to health)
sinasc <- read.csv(paste0(raw,"SINASC/SINASC_final.csv"), encoding = "UTF-8")

# sim
sim <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse.dta")))

# sia
sia <- read.csv(paste0(raw,"SIA/SIA_final.csv"), encoding = "UTF-8")


ams <- data.frame(read.dta13(paste0(raw,"AMS/ams.dta"))) 

# ADD VARIABLE AFTER MERGES
# %>% 
#   mutate(hospital_nmun = ifelse(!is.na(hospital_est) & !is.na(hospital_fed),0,NA)) %>% 
#   mutate(hospital_nmun = hospital_est + hospital_fed)






