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
  dplyr::select(cod_mun,ec29_baseline,geometry) %>% 
  mutate(`% of OR spent in Health` = "",
         `% of OR spent in Health` = ifelse(ec29_baseline<0.05,"0-5%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.05 & ec29_baseline<0.1,"05-10%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.1 & ec29_baseline<0.15,"10-15%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.15 & ec29_baseline<0.2,"15-20%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.2 & ec29_baseline<0.25,"20-25%",`% of OR spent in Health`),
         `% of OR spent in Health` = ifelse(ec29_baseline>=0.25,"25+%",`% of OR spent in Health`)) %>% 
  st_as_sf() %>%
  st_transform(4326)


# 3. Plotting map
# =================================================================

color_map <- c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4')

map <- df_map %>% 
  ggplot() +
  geom_sf(aes(fill = `% of OR spent in Health`),size = 0.00001,color = NA)+
  geom_sf(data = ufbr_shape,fill=NA,size = 0.00001) +
  scale_fill_manual(values = color_map,na.value = "White") +
  theme_light()+
  theme(legend.position="bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank()) +
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))
  
file <- paste0(output,"ec29_map.pdf")
ggsave(file,
       plot = map,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

file <- paste0(output,"ec29_map.pdf.png")
ggsave(file,
       plot = map,
       device = "png",
       width = 7, height = 5,
       units = "in")


