#######################################################################################################
# Author: Michel Szklo
# June 2022
# 
# This script creates maps for Ec29 in the baseline year of 2000
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
            'maptools',
            'raster',
            'geomerge',
            'cowplot',
            'stringdist',
            'gridExtra')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
output <- "C:/Users/Michel/Documents/GitHub/ec29/outputs/map_plots/"


load(paste0(dir,"regs.RData"))
# --------------------------------------------------------------------------------------------------


# 1. Loading Municipalities shape
# =================================================================
munbr_shape <- read_sf(paste0(dir,"data/shapefiles/Brazil_s.shp"))
munbr_shape <- munbr_shape %>%
  mutate(cod_mun = as.numeric(substr(CD_GEOCODM,1,6)))

ufbr_shape <- read_sf(paste0(dir,"data/shapefiles/br_estados/estados_2010.shp")) %>% 
  dplyr::select(sigla,geometry) %>% 
  st_as_sf() %>%
  st_transform(4326)


# 2. Merging with dataframe
# =================================================================

df_map <- df %>% 
  right_join(munbr_shape, by = "cod_mun") %>% 
  filter(ano==2000) %>% 
  dplyr::select(cod_mun,ec29_baseline,geometry,second_term) %>% 
  mutate(`% of OR spent in Health` = "",
         `% of OR spent in Health` = ifelse(ec29_baseline<0.05,"0-5%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.05 & ec29_baseline<0.1,"05-10%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.1 & ec29_baseline<0.15,"10-15%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.15 & ec29_baseline<0.2,"15-20%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.2 & ec29_baseline<0.25,"20-25%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.25,"25+%",`% of OR spent in Health`)) %>% 
  st_as_sf() %>%
  st_transform(4326)


df_map_samples <- df %>% 
  filter(ano==2000) %>% 
  mutate(term = ifelse(second_term==0,"1. First Term","2. Second Term"),
         inequality = ifelse(gini_baseline_above==0,"1. Low Inequality","2. High Inequality"),
         poverty = ifelse(pmpob_baseline_above==0,"1. Low Poverty","2. High Poverty")) %>% 
  dplyr::select(cod_mun,term,cod_uf,inequality,poverty) %>% 
  right_join(munbr_shape, by = "cod_mun") %>% 
  st_as_sf() %>%
  st_transform(4326)


# 3. Plotting map
# =================================================================

color_map <- c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4')

map <- df_map %>% 
  filter(!is.na(`% of OR spent in Health`)) %>% 
  ggplot() +
  geom_sf(aes(fill = `% of OR spent in Health`),size = 0.00001,color = NA)+
  geom_sf(data = ufbr_shape,fill=NA,size = 0.00001) +
  scale_fill_manual(values = color_map,na.value = "White") +
  theme_light()+
  labs(fill = "% of Own Resource spent in Health")+
  theme(legend.position="bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.line =  element_blank(),
        axis.ticks =  element_blank()) +
  guides(fill=guide_legend(nrow = 2,byrow=TRUE))
  
file <- paste0(output,"ec29_map.pdf")
ggsave(file,
       plot = map,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

file <- paste0(output,"ec29_map.png")
ggsave(file,
       plot = map,
       device = "png",
       width = 7, height = 5,
       units = "in")



map <- df_map %>% 
  filter(!is.na(`% of OR spent in Health`)) %>%
  filter(second_term==0) %>% 
  ggplot() +
  geom_sf(aes(fill = `% of OR spent in Health`),size = 0.00001,color = NA)+
  geom_sf(data = ufbr_shape,fill=NA,size = 0.00001) +
  scale_fill_manual(values = color_map,na.value = "White") +
  theme_light()+
  labs(fill = "% of Own Resource spent in Health")+
  theme(legend.position="bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.line =  element_blank(),
        axis.ticks =  element_blank()) +
  coord_sf(ndiscr = F) +
  guides(fill=guide_legend(nrow = 2,byrow=TRUE))

file <- paste0(output,"ec29_elect1_map.pdf")
ggsave(file,
       plot = map,
       device = "pdf",
       width = 4, height = 4.5,
       units = "in")

file <- paste0(output,"ec29_elect1_map.png")
ggsave(file,
       plot = map,
       device = "png",
       width = 4, height = 4.5,
       units = "in")


map <- df_map %>% 
  filter(!is.na(`% of OR spent in Health`)) %>%
  filter(second_term==1) %>% 
  ggplot() +
  geom_sf(aes(fill = `% of OR spent in Health`),size = 0.00001,color = NA)+
  geom_sf(data = ufbr_shape,fill=NA,size = 0.00001) +
  scale_fill_manual(values = color_map,na.value = "White") +
  theme_light()+
  labs(fill = "% of Own Resource spent in Health")+
  theme(legend.position="bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.line =  element_blank(),
        axis.ticks =  element_blank()) +
  guides(fill=guide_legend(nrow = 2,byrow=TRUE))

file <- paste0(output,"ec29_elect2_map.pdf")
ggsave(file,
       plot = map,
       device = "pdf",
       width = 4, height = 4.5,
       units = "in")

file <- paste0(output,"ec29_elect2_map.png")
ggsave(file,
       plot = map,
       device = "png",
       width = 4, height = 4.5,
       units = "in")




# 4. Plotting map to show samples
# =================================================================

color_map <- c("#67a9cf","#ef8a62")


# Electoral Term
map <- df_map_samples %>%
  filter(!is.na(term)) %>% 
  ggplot() +
  geom_sf(aes(fill = term),size = 0.00001,color = NA)+
  geom_sf(data = ufbr_shape,fill=NA,size = 0.00001) +
  scale_fill_manual(values = color_map,na.value = "White") +
  theme_light()+
  theme(legend.position="bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.line =  element_blank(),
        axis.ticks =  element_blank()) +
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))

file <- paste0(output,"term_map.pdf")
ggsave(file,
       plot = map,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

file <- paste0(output,"term_map.png")
ggsave(file,
       plot = map,
       device = "png",
       width = 7, height = 5,
       units = "in")



# Inequality
map <- df_map_samples %>%
  filter(!is.na(inequality)) %>% 
  ggplot() +
  geom_sf(aes(fill = inequality),size = 0.00001,color = NA)+
  geom_sf(data = ufbr_shape,fill=NA,size = 0.00001) +
  scale_fill_manual(values = color_map,na.value = "White") +
  theme_light()+
  theme(legend.position="bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.line =  element_blank(),
        axis.ticks =  element_blank()) +
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))

file <- paste0(output,"ineq_map.pdf")
ggsave(file,
       plot = map,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

file <- paste0(output,"ineq_map.png")
ggsave(file,
       plot = map,
       device = "png",
       width = 7, height = 5,
       units = "in")



# Poverty
map <- df_map_samples %>%
  filter(!is.na(poverty)) %>% 
  ggplot() +
  geom_sf(aes(fill = poverty),size = 0.00001,color = NA)+
  geom_sf(data = ufbr_shape,fill=NA,size = 0.00001) +
  scale_fill_manual(values = color_map,na.value = "White") +
  theme_light()+
  theme(legend.position="bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.line =  element_blank(),
        axis.ticks =  element_blank()) +
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))

file <- paste0(output,"pov_map.pdf")
ggsave(file,
       plot = map,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

file <- paste0(output,"pov_map.png")
ggsave(file,
       plot = map,
       device = "png",
       width = 7, height = 5,
       units = "in")




# 5. Within State variation?
# =================================================================

df_var <- df %>% 
  filter(ano==2000) %>% 
  mutate(term = ifelse(second_term==0,"1. First Term","2. Second Term"),
         inequality = ifelse(gini_baseline_above==0,"1. Low Inequality","2. High Inequality"),
         poverty = ifelse(pmpob_baseline_above==0,"1. Low Poverty","2. High Poverty")) %>% 
  dplyr::select(cod_mun,term,cod_uf,inequality,poverty)


var_term <- df_var %>% 
  dplyr::select(cod_uf,term) %>% 
  group_by(cod_uf,term) %>% 
  summarise(var = sum(!is.na(term))) %>% 
  filter(term!="NA") %>% 
  ungroup() %>% 
  group_by(cod_uf) %>%
  mutate(total = sum(var)) %>% 
  ungroup() %>% 
  mutate(var = var/total)


var_inequality <- df_var %>% 
  dplyr::select(cod_uf,inequality) %>% 
  group_by(cod_uf,inequality) %>% 
  summarise(var = sum(!is.na(inequality))) %>% 
  ungroup() %>% 
  group_by(cod_uf) %>%
  mutate(total = sum(var)) %>% 
  ungroup() %>% 
  mutate(var = var/total)

var_poverty <- df_var %>% 
  dplyr::select(cod_uf,poverty) %>% 
  group_by(cod_uf,poverty) %>% 
  summarise(var = sum(!is.na(poverty))) %>% 
  ungroup() %>% 
  group_by(cod_uf) %>%
  mutate(total = sum(var)) %>% 
  ungroup() %>% 
  mutate(var = var/total)


