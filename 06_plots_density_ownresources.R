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

output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/Plots/"



SIOPS <- readRDS("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/SIOPS/SIOPS.rds")



# =================================================================
# 1. Density: 2000 - 2005
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
  labs(x = "Health Spending (% of own resource spending)",
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





SIOPS_temp <- SIOPS %>% filter(ano<=2005) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#11A579","#3969AC","#E73F74","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")

hist1 <- SIOPS_temp %>% ggplot(aes(x = pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(aes(xintercept = target_ec29, color = ano), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-20,50,5), limits = c(-20,50)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


SIOPS_temp <- SIOPS %>% filter(ano==2000 | ano==2003 | ano==2005) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#E73F74", "#008695")

hist2 <- SIOPS_temp %>% ggplot(aes(x = pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(aes(xintercept = target_ec29, color = ano), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-20,50,5), limits = c(-20,50)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


filePNG <- paste0(output,"hist1.png")
filepdf <- paste0(output,"hist1.pdf")
ggsave(filePNG,
       plot = hist1,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist1,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")

filePNG <- paste0(output,"hist2.png")
filepdf <- paste0(output,"hist2.pdf")
ggsave(filePNG,
       plot = hist2,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist2,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")



# =================================================================
# 2. Density: 2010 - 2014
# =================================================================

SIOPS_temp <- SIOPS %>% filter(ano>=2010 & ano<=2014) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#11A579","#3969AC","#E73F74","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")

hist1_alt <- SIOPS_temp %>% ggplot(aes(x = pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(aes(xintercept = target_ec29, color = ano), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-20,50,5), limits = c(-20,50)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


SIOPS_temp <- SIOPS %>% filter(ano==2010 | ano==2012 | ano==2014) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#3969AC", "#E68310")

hist2_alt <- SIOPS_temp %>% ggplot(aes(x = pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(aes(xintercept = target_ec29, color = ano), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-20,50,5), limits = c(-20,50)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


filePNG <- paste0(output,"hist1_alt.png")
filepdf <- paste0(output,"hist1_alt.pdf")
ggsave(filePNG,
       plot = hist1_alt,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist1_alt,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")

filePNG <- paste0(output,"hist2_alt.png")
filepdf <- paste0(output,"hist2_alt.pdf")
ggsave(filePNG,
       plot = hist2_alt,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist2_alt,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")




# =================================================================
# 3. Density: 2000 - 2014
# =================================================================


SIOPS_temp <- SIOPS %>% filter(ano>=2000 & ano<=2014) %>% mutate(ano  = as.factor(ano))



color_graph <- c("#5F4690","#1D6996","#38A6A5","#0F8554","#73AF48","#EDAD08","#E17C05","#CC503E","#94346E","#6F4070","#994E95","#666666", "#5F4690","#1D6996","#38A6A5")

hist1_all <- SIOPS_temp %>% ggplot(aes(x = pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(aes(xintercept = target_ec29, color = ano), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-20,50,5), limits = c(-20,50)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


SIOPS_temp <- SIOPS %>% filter(ano==2000 | ano==2003 | ano==2005 | ano==2007 | ano == 2009 | ano == 2012 | ano == 2014) %>% mutate(ano  = as.factor(ano))

color_graph <- c("#7F3C8D","#11A579","#3969AC","#E73F74","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")

hist2_all <- SIOPS_temp %>% ggplot(aes(x = pct_recproprios_ec29, color = ano, fill = ano)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_vline(aes(xintercept = target_ec29, color = ano), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-20,50,5), limits = c(-20,50)) +
  scale_color_manual(values = color_graph) +
  scale_fill_manual(values = color_graph) +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



filePNG <- paste0(output,"hist1_all.png")
filepdf <- paste0(output,"hist1_all.pdf")
ggsave(filePNG,
       plot = hist1_all,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist1_all,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")

filePNG <- paste0(output,"hist2_all.png")
filepdf <- paste0(output,"hist2_all.pdf")
ggsave(filePNG,
       plot = hist2_all,
       device = "png",
       width = 7, height = 3.5,
       units = "in")
ggsave(filepdf,
       plot = hist2_all,
       device = "pdf",
       width = 7, height = 3.5,
       units = "in")



