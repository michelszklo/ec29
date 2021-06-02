#######################################################################################################
# Author: Michel Szklo
# April 2020
# 
# This script creates time trends plots with SIOPS data 
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
            'stringdist',
            'gtools',
            'gridExtra')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/ec29/outputs/time_trends/"


SIOPS <- readRDS("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/SIOPS/SIOPS.rds")


# =================================================================
# 1. New variables
# =================================================================

deflator <- read.csv("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/deflator/deflator.csv")
SIOPS_temp <- left_join(SIOPS, deflator, by = "ano")

SIOPS_temp <- SIOPS_temp %>% 
  group_by(ano) %>% 
  mutate(pct_recproprios_ec29_mean = mean(pct_recproprios_ec29, na.rm = T),
         quantile = ifelse(ano==2000,quantcut(pct_recproprios_ec29,4),NA)) %>% 
  ungroup() %>% 
  group_by(cod_mun) %>% 
  mutate(quantile = mean(quantile, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(quantile = as.character(quantile)) %>% 
  filter(quantile != 'NaN')



for(i in seq(1,4,1)){
  name <- 25*i
  value <- paste0(name,"%")
  i <- as.character(i)
  SIOPS_temp <- SIOPS_temp %>%
    mutate(quantile = ifelse(quantile==i,value,quantile))
}

SIOPS_temp <- SIOPS_temp %>% 
  mutate(quantile = as.factor(quantile),
         quantile = factor(quantile, levels = c('25%','50%','75%','100%'))) %>% 
  group_by(ano,quantile) %>% 
  summarise(despsaude_pcapita = mean(despsaude_pcapita, na.rm = T),
            despexrecproprio_pcapita = mean(despexrecproprio_pcapita, na.rm = T),
            desprecpropriosaude_pcapita = mean(desprecpropriosaude_pcapita, na.rm = T),
            deflator_saude = mean(deflator_saude)) %>%
  ungroup() %>% 
  mutate(date = as.Date(as.character(ano), format = "%Y"),
         despsaude_pcapita = despsaude_pcapita / deflator_saude,
         desprecpropriosaude_pcapita = desprecpropriosaude_pcapita / deflator_saude,
         despexrecproprio_pcapita = despexrecproprio_pcapita / deflator_saude) %>% 
  group_by(quantile) %>% 
  mutate(var1 = ifelse(ano==2000,1,despsaude_pcapita / dplyr::lag(despsaude_pcapita,1)),
         var2 = ifelse(ano==2000,1,desprecpropriosaude_pcapita / dplyr::lag(desprecpropriosaude_pcapita,1)),
         var3 = ifelse(ano==2000,1,despexrecproprio_pcapita / dplyr::lag(despexrecproprio_pcapita,1))) %>%
  mutate(`Health spending per capita index (2000 = 100)` = 100 * cumprod(var1),
         `Own resources health spending per capita index (2000 = 100)` = 100 * cumprod(var2),
         `Health spending - own resources per capita index (2000 = 100)` = 100 * cumprod(var3)) %>% 
  ungroup() %>% 
  mutate(quantile = as.character(quantile),
         quantile = ifelse(quantile=="25%","Bottom quartile",quantile),
         quantile = ifelse(quantile=="100%","Top quartile",quantile))







# =================================================================
# 2. Time trends
# =================================================================

# total health spending per capita

color_graph <- c("#a50026","#313695")

plot1 <- SIOPS_temp %>%
  filter(ano<=2010) %>%
  filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
  ggplot(aes(x = ano, y = `Health spending per capita index (2000 = 100)`)) +
  geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
  scale_y_continuous(breaks = seq(100,350,50), limits = c(90,350)) +
  theme_light() +
  labs(x = "Year",
       y = "Spending per capita index (2000 = 100)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())


plot2 <- SIOPS_temp %>%
  filter(ano<=2010) %>% 
  filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
  ggplot(aes(x = ano, y = `Own resources health spending per capita index (2000 = 100)`)) +
  geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
  scale_y_continuous(breaks = seq(100,500,50), limits = c(90,500)) +
  labs(x = "Year",
       y = "Spending per capita index (2000 = 100)")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())




plot3 <- SIOPS_temp %>%
  filter(ano<=2010) %>% 
  filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
  ggplot(aes(x = ano, y = `Health spending - own resources per capita index (2000 = 100)`)) +
  geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
  scale_y_continuous(breaks = seq(100,500,50), limits = c(90,500)) +
  labs(x = "Year",
       y = "Spending per capita index (2000 = 100)")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())


filePNG <- paste0(output,"plot_total.png")
filepdf <- paste0(output,"plot_total.pdf")
ggsave(filePNG,
       plot = plot1,
       device = "png",
       width = 12, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot1,
       device = "pdf",
       width = 12, height = 5,
       units = "in")



filePNG <- paste0(output,"plot_own.png")
filepdf <- paste0(output,"plot_own.pdf")
ggsave(filePNG,
       plot = plot2,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot2,
       device = "pdf",
       width = 6, height = 5,
       units = "in")

filePNG <- paste0(output,"plot_transf.png")
filepdf <- paste0(output,"plot_transf.pdf")
ggsave(filePNG,
       plot = plot3,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot3,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot1 <- SIOPS_temp %>%
#   filter(ano<=2010) %>%
#   filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
#   ggplot(aes(x = ano, y = `Health spending per capita index (2000 = 100)`)) +
#   geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
#   scale_color_manual(values = color_graph) +
#   scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
#   scale_y_continuous(breaks = seq(100,500,25), limits = c(100,500)) +
#   labs(title = "Total health spending",
#        subtitle = "by quantiles of the baseline % of own resource spending off total health spending") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=8),
#         legend.position=c(0.1,0.7),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 8),
#         legend.key.size = unit(0.8, "cm"),
#         legend.key.width = unit(0.2, "cm"),
#         plot.title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 7))
# 
# 
# 
# # own resources per capita
# 
# plot2 <- SIOPS_temp %>%
#   filter(ano<=2010) %>% 
#   filter(quantile == "20%" | quantile == "80%") %>% 
#   ggplot(aes(x = ano, y = `Own resources health spending per capita index (2000 = 100)`)) +
#   geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
#   scale_color_manual(values = color_graph) +
#   scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
#   scale_y_continuous(breaks = seq(100,500,25), limits = c(100,500)) +
#   labs(title = "Own resources health spending",
#        subtitle = "by quantiles of the baseline % of own resource spending off total health spending") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=8),
#         legend.position=c(0.1,0.7),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 8),
#         legend.key.size = unit(0.8, "cm"),
#         legend.key.width = unit(0.2, "cm"),
#         plot.title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 7))
# 
# 
# # besides own resources per capita
# 
# plot3 <- SIOPS_temp %>%
#   filter(ano<=2010) %>% 
#   filter(quantile == "20%" | quantile == "80%") %>% 
#   ggplot(aes(x = ano, y = `Health spending - own resources per capita index (2000 = 100)`)) +
#   geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
#   scale_color_manual(values = color_graph) +
#   scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
#   scale_y_continuous(breaks = seq(100,500,25), limits = c(100,500)) +
#   labs(title = "Health spending from other sources",
#        subtitle = "by quantiles of the baseline % of own resource spending off total health spending") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=8),
#         legend.position=c(0.1,0.7),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 8),
#         legend.key.size = unit(0.8, "cm"),
#         legend.key.width = unit(0.2, "cm"),
#         plot.title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 7))
# 
# 
# 
# grid <- grid.arrange(plot1,plot2,plot3,ncol = 3)
# 
# 
# filePNG <- paste0(output,"plot.png")
# filepdf <- paste0(output,"plot.pdf")
# ggsave(filePNG,
#        plot = grid,
#        device = "png",
#        width = 12, height = 5,
#        units = "in")
# ggsave(filepdf,
#        plot = grid,
#        device = "pdf",
#        width = 12, height = 5,
#        units = "in")
# 
# 









# 
# 
# SIOPS_agg <- SIOPS %>% 
#   group_by(ano) %>% 
#   summarise(target_ec29 = mean(target_ec29, na.rm = T),
#             pct_recproprios_ec29 = mean(pct_recproprios_ec29, na.rm = T),
#             achieved_ec29 = mean(achieved_ec29, na.rm = T),
#             dist_ec29_desp = mean(dist_ec29_desp, na.rm = T),
#             dist_ec29_desp_pc = mean(dist_ec29_desp_pc, na.rm = T))
# 
# plot1 <- SIOPS_agg %>% ggplot(aes(x = ano, y = achieved_ec29)) +
#   geom_line(color = "blue", size = 2) +
#   scale_y_continuous(breaks = seq(0,1,0.05), limits = c(0.8,1), labels = percent) +
#   scale_x_continuous(breaks = seq(2000,2018,1), limits = c(2000,2018)) +
#   theme(axis.text.x = element_text(angle=90))
# 
# plot2 <- SIOPS_agg %>% ggplot(aes(x = ano, y = pct_recproprios_ec29)) +
#   geom_line(color = "blue", size = 2) +
#   geom_line(aes(x = ano, y = target_ec29), color = "red", size = 2, linetype = "dotted") +
#   geom_line(aes(x = ano, y = pct_dist_ec29), color = "green", size = 2, linetype = "dashed") +
#   scale_y_continuous(breaks = seq(0,100,5), limits = c(0,25)) +
#   scale_x_continuous(breaks = seq(2000,2018,1), limits = c(2000,2018)) +
#   theme(axis.text.x = element_text(angle=90))
# 
# filePNG <- paste0(output,"plot1.png")
# filepdf <- paste0(output,"plot1.pdf")
# ggsave(filePNG,
#        plot = plot1,
#        device = "png",
#        width = 7, height = 3.5,
#        units = "in")
# ggsave(filepdf,
#        plot = plot1,
#        device = "pdf",
#        width = 7, height = 3.5,
#        units = "in")
# 
# filePNG <- paste0(output,"plot2.png")
# filepdf <- paste0(output,"plot2.pdf")
# ggsave(filePNG,
#        plot = plot2,
#        device = "png",
#        width = 7, height = 3.5,
#        units = "in")
# ggsave(filepdf,
#        plot = plot2,
#        device = "pdf",
#        width = 7, height = 3.5,
#        units = "in")
# 
# 
# 
# 
# 
