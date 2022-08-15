#######################################################################################################
# Author: Michel Szklo
# June 2022
# 
# This script creates graph for Average Causal Response Weights vs Treatment Distribution
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
            'ggsci')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
output <- "C:/Users/Michel/Documents/GitHub/ec29/outputs/density_plots/"


load(paste0(dir,"regs.RData"))

# 1. Creating simpler data frame and variables to estimate weights
# =================================================================

dfw <- df %>% 
  filter(dist_ec29_baseline>-0.5) %>% 
  filter(ano==2000) %>% 
  select(dist_ec29_baseline) %>% 
  mutate(max = max(dist_ec29_baseline,na.rm = T),
         min = min(dist_ec29_baseline,na.rm = T),
         varD = var(dist_ec29_baseline,na.rm = T),
         ED = mean(dist_ec29_baseline,na.rm = T),
         range = max-min,
         N = nrow(.)) %>% 
  arrange(dist_ec29_baseline) %>% 
  mutate(n = row_number()) %>% 
  mutate(grid = (n-1)/(N-1)*range + min,
         CGSw = 0)


# 2. Loop to creat CGS weights
# =================================================================

for(i in seq.int(1,nrow(dfw))){
  
  ED <- dfw$ED[i]
  varD <- dfw$varD[i]
  l <- dfw$grid[i]
  dfw_l <- dfw %>% filter(dist_ec29_baseline>l | is.na(dist_ec29_baseline))
  EDl <- mean(dfw_l$dist_ec29_baseline, na.rm = T)
  PDl <- nrow(dfw_l)/dfw$N[i]
  dfw[i,"CGSw"] <- ((EDl - ED) * PDl) / varD
  
}


# 3. plot
# =================================================================

plot <- dfw %>%
  ggplot() +
  geom_density(aes(x = dist_ec29_baseline), color = "grey", size = 1.5) +
  geom_line(aes(x = grid,y = CGSw),color = "black", linetype = "dashed", size = 1.5) +
  labs(y = "",
       x = "Dose (d)") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        legend.position="none") +
  annotate("text", x = -0.01, y = 6.5, label = "f[(D)] (d)", parse = T, color = "grey40") +
  annotate("text", x = -0.07, y = 4.2, label = "TWFE", color = "black")

  
filePNG <- paste0(output,"acr_weights.png")
filePDF <- paste0(output,"acr_weights.pdf")
ggsave(filePNG,
       plot = plot,
       device = "png",
       width = 7, height = 6,
       units = "in")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 7, height = 6,
       units = "in")


