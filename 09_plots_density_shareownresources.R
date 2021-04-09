#######################################################################################################
# Author: Michel Szklo
# April 2020
# 
# This script creates time density plots for "own resources / total spending" with SIOPS data 
# 
#
#
#######################################################################################################

# =================================================================
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
            'stringdist')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/Plots/"


SIOPS <- readRDS("data/SIOPS.rds")



# =================================================================
# 1. Density: own resource / total spending
# =================================================================



SIOPS_temp <- SIOPS %>% filter(ano<=2005) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#11A579","#3969AC","#E73F74","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")

hist7 <- SIOPS_temp %>% ggplot(aes(x = desprecproprio_desptotal_pc, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  scale_x_continuous(limits = c(-0.5,1.5)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


SIOPS_temp <- SIOPS %>% filter(ano==2000 | ano==2005) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#008695")

hist8 <- SIOPS_temp %>% ggplot(aes(x = desprecproprio_desptotal_pc, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  scale_x_continuous(limits = c(-0.5,1.5)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



filePNG <- paste0(output,"hist7.png")
filepdf <- paste0(output,"hist7.pdf")
ggsave(filePNG,
       plot = hist7,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist7,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")

filePNG <- paste0(output,"hist8.png")
filepdf <- paste0(output,"hist8.pdf")
ggsave(filePNG,
       plot = hist8,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist8,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")




