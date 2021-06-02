#######################################################################################################
# Author: Michel Szklo
# April 2020
# 
# This script creates time density plots for "spending own resources" with SIOPS data 
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

output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/ec29/outputs/density_plots/"



SIOPS <- readRDS("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/SIOPS/SIOPS.rds")



# =================================================================
# 1. Density: Health spending (% of own resource) - 2000 - 2004
# =================================================================


SIOPS_temp <- SIOPS %>% filter(ano<=2004) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#11A579","#3969AC","#E73F74","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")
color_graph <- c("#E68310","#E73F74","#3969AC","#11A579","#7F3C8D")

hist_ec29 <- SIOPS_temp %>% ggplot(aes(x = pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(xintercept = 7, linetype = "dotted", size = 0.6, color = "grey63") +
  geom_vline(xintercept = 15, linetype = "dashed", size = 0.6, color = "grey43") +
  scale_x_continuous(breaks = seq(-5,45,5), limits = c(-5,45)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  labs(x = "Health Spending (% of municipalities' own resource)",
       y = "Density") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size=12),
        legend.position="bottom", legend.box = "horizontal")


filePNG <- paste0(output,"hist_ec29.png")
filepdf <- paste0(output,"hist_ec29.pdf")
ggsave(filePNG,
       plot = hist_ec29,
       device = "png",
       width = 7, height = 5,
       units = "in")
ggsave(filepdf,
       plot = hist_ec29,
       device = "pdf",
       width = 7, height = 5,
       units = "in")



# =================================================================
# 2. Density: total health spending per capita - 2000-2004
# =================================================================

deflator <- read.csv("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/deflator/deflator.csv")

SIOPS_temp <- SIOPS %>% filter(ano<=2004) %>%  
  left_join(deflator,by = "ano") %>% 
  mutate(ano  = as.factor(ano)) %>%
  mutate(despsaude_pcapita = despsaude_pcapita/deflator_saude)

color_graph <- c("#7F3C8D","#11A579","#3969AC","#E73F74","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")
color_graph <- c("#E68310","#E73F74","#3969AC","#11A579","#7F3C8D")


hist_pc <- SIOPS_temp %>% ggplot(aes(x = despsaude_pcapita, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  scale_x_continuous(breaks = seq(0,1000,100),limits = c(0,1000)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  labs(x = "Health Spending per capita",
       y = "Density") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size=12),
        legend.position="bottom", legend.box = "horizontal")



filePNG <- paste0(output,"hist_pc.png")
filepdf <- paste0(output,"hist_pc.pdf")
ggsave(filePNG,
       plot = hist_pc,
       device = "png",
       width = 7, height = 5,
       units = "in")
ggsave(filepdf,
       plot = hist_pc,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

