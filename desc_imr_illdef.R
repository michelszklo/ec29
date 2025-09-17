#######################################################################################################
# Author: Michel Szklo
# Julyu 2025
# 
# This scripts provide descriptives for ill-defined infant mortality
#
#
#######################################################################################################

# 0. Set-up
# =================================================================


rm(list=ls())

#Set-up path for principal directory
if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/"
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}
SRC <- paste0(dir,"source/")
DAT <- paste0(dir,"data/processed/")
TAB <- paste0(dir,"results/tables/")
FIG <- paste0(dir,"results/figures/")


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
            'plm')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)
options(digits = 15)


folder <- "desc_imr_illdef/"

# 1. Load data
# =================================================================
load(paste0(DAT,"regs.RData"))

# adjusting region
df <- df %>% 
  group_by(cod_mun) %>%
  mutate(region = as.character(region)) %>% 
  mutate(region = ifelse(is.na(region) & ano == 1996, dplyr::lead(region,2), region),
         region = ifelse(is.na(region) & ano == 1997, dplyr::lead(region,1), region)) %>% 
  mutate(region = as.factor(region)) %>% 
  ungroup()

# generating dummy for below
df <- df %>% 
  mutate(below = NA) %>% 
  mutate(below = ifelse(dist_ec29_baseline>0,"1. Below",below)) %>% 
  mutate(below = ifelse(dist_ec29_baseline<=0,"2. Above",below))


# 2. Time Series 1996
# =================================================================

# simple time series with total imr
vars <- c("tx_mi", "tx_mi_illdef")

df_plot <- df %>%
  filter(sample_rm_outlier_imr_96==1) %>% 
  group_by(ano) %>% 
  summarise_at(vars, ~ weighted.mean(.,peso_pop, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = all_of(vars),
               names_to = "var",
               values_to = "IMR")

plot <- df_plot %>%
  ggplot(aes(x = ano, y = IMR, color = var, group = var)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,30,5), limits = c(0,30)) +
  scale_x_continuous(breaks = seq(1996,2010,1), limits = c(1996,2010)) +
  theme_light() +
  labs(x = "Year")+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 



filePDF <- paste0(FIG,folder,"desc_imr_illdef_1.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")



# ill def above and below
df_plot <- df %>%
  filter(sample_rm_outlier_imr_96==1) %>% 
  group_by(ano,below) %>% 
  summarise_at("tx_mi_illdef", ~ weighted.mean(.,peso_pop, na.rm = T)) %>% 
  filter(!is.na(below)) %>% 
  ungroup() %>% 
  rename("IMR" = 3)

plot <- df_plot %>%
  ggplot(aes(x = ano, y = IMR, linetype = below)) +
  geom_line(size = 0.8, alpha = 0.6, color = "blue") +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1996,2010,1), limits = c(1996,2010)) +
  theme_light() +
  labs(x = "Year")+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 



filePDF <- paste0(FIG,folder,"desc_imr_illdef_2.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")




# ill def by region
df_plot <- df %>%
  filter(sample_rm_outlier_imr_96==1) %>% 
  group_by(ano,region) %>% 
  summarise_at("tx_mi_illdef", ~ weighted.mean(.,peso_pop, na.rm = T)) %>% 
  ungroup() %>% 
  rename("IMR" = 3)

plot <- df_plot %>%
  ggplot(aes(x = ano, y = IMR, color = region)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,9,0.5), limits = c(0,9)) +
  scale_x_continuous(breaks = seq(1996,2010,1), limits = c(1996,2010)) +
  theme_light() +
  labs(x = "Year")+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 



filePDF <- paste0(FIG,folder,"desc_imr_illdef_3.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")




# ill def by region above and below
df_plot <- df %>%
  filter(sample_rm_outlier_imr_96==1) %>% 
  group_by(ano,region,below) %>%
  summarise_at("tx_mi_illdef", ~ weighted.mean(.,peso_pop, na.rm = T)) %>% 
  filter(!is.na(below)) %>% 
  ungroup() %>% 
  rename("IMR" = 4)

plot <- df_plot %>%
  ggplot(aes(x = ano, y = IMR, color = region, linetype = below)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,10,0.5), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(1996,2010,1), limits = c(1996,2010)) +
  theme_light() +
  labs(x = "Year")+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 



filePDF <- paste0(FIG,folder,"desc_imr_illdef_4.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")




# 3. Scatterplots
# =================================================================

df_plot <- df %>% 
  filter(sample_rm_outlier_imr_96==1) %>% 
  group_by(cod_mun) %>% 
  mutate(tx_mi_illdef_00_05_shift = dplyr::lead(tx_mi_illdef,5) - tx_mi_illdef,
         tx_mi_illdef_00_10_shift = dplyr::lead(tx_mi_illdef,10) - tx_mi_illdef) %>% 
  filter(ano==2000)



plot <- ggplot(df_plot %>% filter(dist_ec29_baseline>-0.5),
                      aes(x = dist_ec29_baseline, y = tx_mi_illdef)) +
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
      geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
      # scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
      scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
      labs(y = "IMR - Ill-Defined",
           x = "Distance to the EC29 target") +
      theme_light() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title = element_text(size=12),
            axis.text = element_text(size = 13),
            legend.position="none")


filePDF <- paste0(FIG,folder,"desc_imr_illdef_5.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")




plot <- ggplot(df_plot %>%
                 filter(dist_ec29_baseline>-0.5) %>%
                 filter(region=="North" | region == "NorthEast"),
               aes(x = dist_ec29_baseline, y = tx_mi_illdef)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  # scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "IMR - Ill-Defined",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 13),
        legend.position="none")


filePDF <- paste0(FIG,folder,"desc_imr_illdef_6.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")



plot <- ggplot(df_plot %>%
                 filter(dist_ec29_baseline>-0.5) %>%
                 filter(region!="North" & region != "NorthEast"),
               aes(x = dist_ec29_baseline, y = tx_mi_illdef)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  # scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "IMR - Ill-Defined",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 13),
        legend.position="none")


filePDF <- paste0(FIG,folder,"desc_imr_illdef_7.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")










plot <- ggplot(df_plot %>% filter(dist_ec29_baseline>-0.5),
               aes(x = dist_ec29_baseline, y = tx_mi_illdef_00_05_shift)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  # scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "IMR - Ill-Defined - 2000-2005 shift",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 13),
        legend.position="none")


filePDF <- paste0(FIG,folder,"desc_imr_illdef_8.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")


plot <- ggplot(df_plot %>% filter(dist_ec29_baseline>-0.5),
               aes(x = dist_ec29_baseline, y = tx_mi_illdef_00_10_shift)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  # scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "IMR - Ill-Defined - 2000-2010 shift",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 13),
        legend.position="none")


filePDF <- paste0(FIG,folder,"desc_imr_illdef_9.pdf")

ggsave(filePDF,
       plot = plot,
       device = "pdf",
       width = 8, height = 5,
       units = "in")













