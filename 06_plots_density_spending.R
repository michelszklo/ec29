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

output <- "C:/Users/Michel/Documents/GitHub/ec29/outputs/density_plots/"
dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

df <- readRDS(paste0(dir,"data/CONSOL_DATA.rds"))


# =================================================================
# 1. Density: Health spending (% of own resource) - 2000 - 2004
# =================================================================


SIOPS_temp <- df %>% filter(ano<=2005) %>% mutate(ano  = as.factor(ano)) %>% select(siops_pct_recproprios_ec29, ano)

color_graph <- c("#008695","#E68310","#E73F74","#3969AC","#11A579","#7F3C8D")

hist_ec29 <- SIOPS_temp %>% ggplot(aes(x = siops_pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(xintercept = 0.15, linetype = "dashed", size = 0.6, color = "grey43") +
  scale_x_continuous(breaks = seq(-0.05,0.45,0.05), limits = c(-0.05,0.45)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  labs(x = "Health Spending (share of municipalities' own resource)",
       y = "Density") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))


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

SIOPS_temp <- df %>% filter(ano<=2005) %>%  
  mutate(ano  = as.factor(ano)) %>%
  select(ano,siops_despsaude_pcapita)

color_graph <- c("#008695","#E68310","#E73F74","#3969AC","#11A579","#7F3C8D")


hist_pc <- SIOPS_temp %>% ggplot(aes(x = siops_despsaude_pcapita, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  scale_x_continuous(breaks = seq(0,800,100),limits = c(0,800)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  labs(x = "Health Spending per capita",
       y = "Density") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))



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

