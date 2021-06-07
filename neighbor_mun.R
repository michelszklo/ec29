
#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts creates matrix of  municipalities neighbors
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
            'ggsci',
            'sf','sp')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


# 1. Load municipalities shapefiles 
# =================================================================

shape_mun <- read_sf("C:/Users/Michel/Google Drive/DOUTORADO FGV/IEPS/Agua/shapefiles/Brazil_s.shp")
shape_mun <- shape_mun %>% 
  st_transform(4326) %>% 
  mutate(ibgeID = as.numeric(substr(CD_GEOCODM,1,6))) %>% 
  select(c("ibgeID","geometry","NM_MUNICIP")) %>% 
  mutate(delete_mun = substr(as.character(ibgeID),3,6)) %>% 
  filter(delete_mun!="0000") %>% 
  select(-delete_mun) %>% 
  mutate(id = seq.int(1,5565))


# 2. Neighbors matrix
# =================================================================

neighbors_matrix <- st_intersects(shape_mun,shape_mun) %>%
  as.data.frame() %>% 
  left_join(shape_mun %>% select(ibgeID,id) %>% rename(row.id = id) , by = "row.id") %>% 
  rename(cod_mun = ibgeID) %>% 
  select(-geometry) %>% 
  left_join(shape_mun %>% select(ibgeID,id) %>% rename(col.id = id) , by = "col.id") %>%
  rename(cod_mun_neighbor = ibgeID) %>% 
  select(-geometry) %>% 
  select(cod_mun,cod_mun_neighbor)




# 3. saving
# =================================================================

saveRDS(neighbors_matrix, file = "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/mun_neighbors.RDS")

  

