#######################################################################################################
# Author: Michel Szklo
# June 2020
# 
# This script consolidates DATASUS data on mortality caused by diabetes (E10-14) and hypertension (I10-15).
# 
#
#######################################################################################################

# =================================================================
# 0. Set-up
# =================================================================

rm(list=ls())

# packages
packages<-c('readr','tidyverse','dplyr','RCurl','tidyr',
            'scales','RColorBrewer','ggplot2','xlsx',
            'stringdist','textclean')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)



# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


# working directories
path <- paste0(dir,"data/")



id_mun <- read.csv(file =paste0(path,"lista_mun/lista_mun_2000_2010.csv"), encoding = "UTF-8")


# import_tabnet_year("C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/imr_claudio_ferraz/SINASC_00_2018.csv","sinasc",2000,2018,"nasc_vivos",skip = 3)
# 
# write.table(sinasc, file = "C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/imr_claudio_ferraz/SINASC_final.csv",sep = ",", fileEncoding = "UTF-8", row.names = F)

# import_tabnet_year("C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/Health financing/sinasc/prenat0.csv","prenat0",1998,2018,"prenat0",skip = 4)
# import_tabnet_year("C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/Health financing/sinasc/prenat1_6.csv","prenat6",1998,2018,"prenat6",skip = 4)
# import_tabnet_year("C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/Health financing/sinasc/prenat7.csv","prenat7",1998,2018,"prenat7",skip = 4)
# 
# prenat <- prenat0 %>% 
#   full_join(prenat6, by = c("cod_mun","ano")) %>% 
#   full_join(prenat7, by = c("cod_mun","ano")) %>% 
#   rename(mun  = cod_mun)
# 
# write.table(prenat, "C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/Health financing/prenat.csv", sep = ",", fileEncoding = "UTF-8",row.names = F)
# 
#   

# =================================================================
# 1. IMPORT FUNCTION
# =================================================================

import_tabnet_year <- function(csv,object,year_start,year_end,varname,skip){
  
  
  # data set with municipalities IDs
  # id_mun <- read.csv(file =paste0(path,"lista_mun/lista_mun_2000_2010.csv"), encoding = "UTF-8")
  # id_mun <- id_mun %>% filter(ano>=year_start & ano<=year_end)
  # 
  #
  df_cols_n <-year_end - year_start + 1 
  
  df <- read.csv(file = csv, encoding = "Latim-1", sep = ";", skip = skip)
  df <- df %>% 
    mutate(cod_mun = substr(Município,1,6)) %>% 
    select(-c("Município","Total")) %>% 
    mutate(cod_mun = as.numeric(cod_mun)) %>%
    filter(!is.na(cod_mun) & substr(cod_mun,3,6)!="0000")
  
  df[,1:df_cols_n] <- lapply(df[,1:df_cols_n], function(x) as.numeric(gsub("-","0",x)))
  
  df <- df %>% 
    pivot_longer(cols = colnames(df[1:df_cols_n]),
                 names_to = "ano",
                 values_to = varname) %>% 
    mutate(ano = as.numeric(substr(ano,2,5))) # %>% 
    #right_join(id_mun, by = c("cod_mun","ano"))
  
  df[is.na(df)] <- 0
  
  assign(paste0(object),df,envir = .GlobalEnv)
  
}


# =================================================================
# 2. IMPORT SINASC
# =================================================================

temp <- list.files(path = paste0(path,"SINASC/"), pattern = "*.csv")
temp <- temp[!temp %in% "nasc_vivos_11_15.csv"]

for (i in seq.int(1,length(temp))){
  name <- strsplit(temp[i],"[.]") %>% unlist()
  name <- name[1]
  if (name=="nasc_vivos"){
    skip <- 3
  }else{
    skip <- 4
  }
  name2 <- paste0("birth_",name)
  import_tabnet_year(paste0(path,"SINASC/",name,".csv"),name2,1998,2010,name2,skip)
  
}

sinasc <- Filter(function(x) is(x, "data.frame"), mget(ls()))
sinasc <- Reduce(function(x, y) merge(x, y, all = T, by = c("cod_mun", "ano")), sinasc, accumulate = F)
sinasc <- sinasc %>% 
  mutate(birth_apgar1 = (birth_apgar1_0_2 * 1 + birth_apgar1_3_5 * 4 + birth_apgar1_6_7 * 6.5 + birth_apgar1_8_10 * 9)/ (birth_apgar1_0_2 + birth_apgar1_3_5 + birth_apgar1_6_7 + birth_apgar1_8_10),
         birth_apgar5 = (birth_apgar5_0_2 * 1 + birth_apgar5_3_5 * 4 + birth_apgar5_6_7 * 6.5 + birth_apgar5_8_10 * 9)/ (birth_apgar5_0_2 + birth_apgar5_3_5 + birth_apgar5_6_7 + birth_apgar5_8_10)) %>% 
  mutate(birth_c_sections = birth_c_sections / birth_nasc_vivos,
         birth_gest_37plus = birth_gest_37plus / birth_nasc_vivos,
         birth_hospital = birth_hospital / birth_nasc_vivos,
         birth_low_weight_2500g = birth_low_weight_2500g / birth_nasc_vivos,
         birth_prenat_0 = birth_prenat_0 / birth_nasc_vivos,
         birth_prenat_1_6 = birth_prenat_1_6 / birth_nasc_vivos,
         birth_prenat_7_plus = birth_prenat_7_plus / birth_nasc_vivos) %>% 
  select(-c("birth_apgar1_0_2","birth_apgar1_3_5","birth_apgar1_6_7","birth_apgar1_8_10","birth_apgar5_0_2","birth_apgar5_3_5","birth_apgar5_6_7","birth_apgar5_8_10"))


import_tabnet_year(paste0(path,"SINASC/nasc_vivos_11_15.csv"),"birth_nasc_vivos2",2011,2015,"birth_nasc_vivos",3)

sinasc <- sinasc %>% bind_rows(birth_nasc_vivos2)

# =================================================================
# 3. IMPORT SIA
# =================================================================

import_tabnet_year(paste0(path,"SIA/","SIA.csv"),"sia_07",1996,2007,"sia",3)
import_tabnet_year(paste0(path,"SIA/","SIA2.csv"),"sia_15",2008,2015,"sia",3)





import_tabnet_year(paste0(path,"SIA/","SIA_ab.csv"),"sia_ab_07",1996,2007,"sia_ab",4)
import_tabnet_year(paste0(path,"SIA/","SIA2_ab.csv"),"sia_ab_15",2008,2015,"sia_ab",4)




# ----------------------
# SIA extension for health spendin inequality analysis

# import_tabnet_year(paste0(path,"SIA/","SIA3.csv"),"sia_19",2008,2019,"sia",3)
# import_tabnet_year(paste0(path,"SIA/","SIA3_ab.csv"),"sia_ab_19",2008,2019,"sia_ab",4)
# 
# sia <- rbind(sia_07,sia_19) 
# sia_ab <- rbind(sia_ab_07,sia_ab_19)
# 
# sia <- sia %>% 
#   left_join(sia_ab, by = c("cod_mun","ano"))
# 
# write.table(sia,
#             file = paste0("C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/Health spending inequality/","SIA.csv"),
#             sep = ",",
#             fileEncoding = "UTF-8",
#             row.names = F)

# ----------------------

import_tabnet_year(paste0(path,"SIA/","sia_ab_acoesbasicas.csv"),"sia_ab_acoesbasicas",1999,2007,"sia_ab_acoesbasicas",4)
import_tabnet_year(paste0(path,"SIA/","sia_ab_enfermagem.csv"),"sia_ab_enfermagem",1999,2007,"sia_ab_enfermagem",4)
import_tabnet_year(paste0(path,"SIA/","sia_ab_odonto.csv"),"sia_ab_odonto",1999,2007,"sia_ab_odonto",4)
import_tabnet_year(paste0(path,"SIA/","sia_ab_nsuperior.csv"),"sia_ab_nsuperior",1999,2007,"sia_ab_nsuperior",4)

sia <- rbind(sia_07,sia_15) 
sia_ab <- rbind(sia_ab_07,sia_ab_15)

import_tabnet_year(paste0(path,"SIA/","ativ_grupo_98_99.csv"),"sia_ativ_grupo_98_99",1998,1999,"sia_ativ_grupo_98_99",4)
import_tabnet_year(paste0(path,"SIA/","ativ_grupo_99_07.csv"),"sia_ativ_grupo_99_07",1999,2007,"sia_ativ_grupo_99_07",4)
import_tabnet_year(paste0(path,"SIA/","ativ_grupo_08_10.csv"),"sia_ativ_grupo_08_10",2008,2010,"sia_ativ_grupo_08_10",4)

import_tabnet_year(paste0(path,"SIA/","visita_medio_98_99.csv"),"sia_visita_medio_98_99",1998,1999,"sia_visita_medio_98_99",4)
import_tabnet_year(paste0(path,"SIA/","visita_medio_99_07.csv"),"sia_visita_medio_99_07",1999,2007,"sia_visita_medio_99_07",4)
import_tabnet_year(paste0(path,"SIA/","visita_medio_08_10.csv"),"sia_visita_medio_08_10",2008,2010,"sia_visita_medio_08_10",4)

import_tabnet_year(paste0(path,"SIA/","visita_superior_98_99.csv"),"sia_visita_superior_98_99",1998,1999,"sia_visita_superior_98_99",4)
import_tabnet_year(paste0(path,"SIA/","visita_superior_99_07.csv"),"sia_visita_superior_99_07",1999,2007,"sia_visita_superior_99_07",4)
import_tabnet_year(paste0(path,"SIA/","visita_superior_08_10.csv"),"sia_visita_superior_08_10",2008,2010,"sia_visita_superior_08_10",4)


sia <- id_mun %>% 
  left_join(sia, by = c("cod_mun","ano")) %>% 
  left_join(sia_ab, by = c("cod_mun","ano")) %>% 
  left_join(sia_ab_acoesbasicas, by = c("cod_mun","ano")) %>% 
  left_join(sia_ab_enfermagem, by = c("cod_mun","ano")) %>% 
  left_join(sia_ab_nsuperior, by = c("cod_mun","ano")) %>% 
  left_join(sia_ab_odonto, by = c("cod_mun","ano")) %>% 
  left_join(sia_ativ_grupo_98_99, by = c("cod_mun","ano")) %>% 
  left_join(sia_ativ_grupo_99_07, by = c("cod_mun","ano")) %>% 
  left_join(sia_ativ_grupo_08_10, by = c("cod_mun","ano")) %>% 
  left_join(sia_visita_medio_98_99, by = c("cod_mun","ano")) %>% 
  left_join(sia_visita_medio_99_07, by = c("cod_mun","ano")) %>% 
  left_join(sia_visita_medio_08_10, by = c("cod_mun","ano")) %>% 
  left_join(sia_visita_superior_98_99, by = c("cod_mun","ano")) %>% 
  left_join(sia_visita_superior_99_07, by = c("cod_mun","ano")) %>% 
  left_join(sia_visita_superior_08_10, by = c("cod_mun","ano")) %>% 
  group_by(cod_mun, ano) %>% 
  mutate(sia_ativ_grupo = sum(sia_ativ_grupo_98_99,sia_ativ_grupo_99_07,sia_ativ_grupo_08_10,na.rm = T),
         sia_visita_medio = sum(sia_visita_medio_98_99,sia_visita_medio_99_07,sia_visita_medio_08_10,na.rm = T),
         sia_visita_superior = sum(sia_visita_superior_98_99,sia_visita_superior_99_07,sia_visita_superior_08_10,na.rm = T)) %>% 
  ungroup() %>% 
  select(-c("sia_ativ_grupo_98_99","sia_ativ_grupo_99_07","sia_ativ_grupo_08_10","sia_visita_medio_98_99","sia_visita_medio_99_07","sia_visita_medio_08_10","sia_visita_superior_98_99","sia_visita_superior_99_07","sia_visita_superior_08_10"))
  
  

# =================================================================
# 4. POP 40+
# =================================================================

import_tabnet_year2 <- function(csv,object,year_start,year_end,varname,skip){
  
  
  # data set with municipalities IDs
  # id_mun <- read.csv(file =paste0(path,"lista_mun/lista_mun_2000_2010.csv"), encoding = "UTF-8")
  # id_mun <- id_mun %>% filter(ano>=year_start & ano<=year_end)
  
  #
  df_cols_n <-year_end - year_start + 1 
  
  df <- read.csv(file = csv, encoding = "Latim-1", sep = ";", skip = skip)
  df <- df %>% 
    mutate(cod_mun = substr(Município,1,6)) %>% 
    select(-c("Município")) %>% 
    mutate(cod_mun = as.numeric(cod_mun)) %>%
    filter(!is.na(cod_mun) & substr(cod_mun,3,6)!="0000")
  
  df[,1:df_cols_n] <- lapply(df[,1:df_cols_n], function(x) as.numeric(gsub("-","0",x)))
  
  df <- df %>% 
    pivot_longer(cols = colnames(df[1:df_cols_n]),
                 names_to = "ano",
                 values_to = varname) %>% 
    mutate(ano = as.numeric(substr(ano,2,5)))
  
  df[is.na(df)] <- 0
  
  assign(paste0(object),df,envir = .GlobalEnv)
  
}
import_tabnet_year2(paste0(path,"SIM/","pop.csv"),"pop40",2000,2015,"pop",4)

# =================================================================
# 4. EXPORTING
# =================================================================

write.table(sia, file = paste0(path,"SIA/SIA_final.csv"),sep = ",", fileEncoding = "UTF-8", row.names = F)
write.table(sinasc, file = paste0(path,"SINASC/SINASC_final.csv"),sep = ",", fileEncoding = "UTF-8", row.names = F)
write.table(pop40, file = paste0(path,"SIM/pop40.csv"),sep = ",", fileEncoding = "UTF-8", row.names = F)







