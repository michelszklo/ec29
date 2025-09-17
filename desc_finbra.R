#######################################################################################################
# Author: Michel Szklo
# April 2025
# 
# This scripts generates descriptives for FINBRA Data
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


options(digits = 15)




# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/"
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}
SRC <- paste0(dir,"source/")
DAT <- paste0(dir,"data/processed/")
TAB <- paste0(dir,"results/tables/")
FIG <- paste0(dir,"results/figures/")

# ------------------------------------

# 1. Load data
# =================================================================
load(paste0(DAT,"regs.RData"))

# generating delta of liabilities (stock to flows)
df <- df %>% 
  group_by(cod_mun) %>% 
  mutate(finbra_passivo_pcapita_delta = finbra_passivo_pcapita - dplyr::lag(finbra_passivo_pcapita,1),
         finbra_passivo_fin_pcapita_delta = finbra_passivo_fin_pcapita - dplyr::lag(finbra_passivo_fin_pcapita,1))


# generating revenue-spending
df <- df %>% 
  mutate(finbra_rec_desp_pcapita = finbra_recorc_pcapita - finbra_desp_o_pcapita)

# generating dummy for below
df <- df %>% 
  mutate(below = NA) %>% 
  mutate(below = ifelse(dist_ec29_baseline>0,"1. Below",below)) %>% 
  mutate(below = ifelse(dist_ec29_baseline<=0,"2. Above",below))




# removing spending outliers from the sample
# ----------------------------------------------
# calculating SD withing municipalities, across years

out <- df %>% 
  group_by(cod_mun) %>% 
  summarise(std_dev = sd(finbra_desp_o_pcapita, na.rm = TRUE))

# setting the 95 percentile threshold
threshold <- quantile(out$std_dev, 0.95, na.rm = TRUE)

# filtering cod_mun of outliers
out <- out %>% 
  mutate(outlier = ifelse(std_dev>threshold,1,0)) %>% 
  filter(outlier==1) %>% 
  select(cod_mun) %>% 
  pull()

length(out)

# removing spending outliers from the sample
# ----------------------------------------------
df_nout<- df %>% 
  filter(!(cod_mun %in% out))



# removing population outliers from the sample
# ----------------------------------------------
# calculating SD withing municipalities, across years

out <- df_nout %>% 
  group_by(cod_mun) %>% 
  summarise(std_dev = sd(pop, na.rm = TRUE))

# setting the 95 percentile threshold
threshold <- quantile(out$std_dev, 0.95, na.rm = TRUE)

# filtering cod_mun of outliers
out <- out %>% 
  mutate(outlier = ifelse(std_dev>threshold,1,0)) %>% 
  filter(outlier==1) %>% 
  select(cod_mun) %>% 
  pull()

length(out)

# removing outliers from the sample
df_nout <- df_nout %>% 
  filter(!(cod_mun %in% out))




df_balance_finbra <- df[complete.cases(df[c('finbra_recorc_pcapita',
                                            'finbra_desp_o_pcapita',
                                            'finbra_desp_saude_san_pcapita',
                                            'finbra_desp_nao_saude_pcapita',
                                            'finbra_despsocial_pcapita',
                                            'finbra_desp_outros_area_pcapita',
                                            'gdp_mun_pcapita',
                                            'pbf_pcapita',
                                            't_tx_mi_baseline',
                                            'dist_ec29_baseline')]) & 
                          df$finbra_recorc_pcapita != 0 & 
                          df$finbra_desp_o_pcapita != 0 & 
                          df$finbra_desp_saude_san_pcapita != 0 & 
                          df$finbra_desp_nao_saude_pcapita != 0 & 
                          df$finbra_despsocial_pcapita != 0 & 
                          df$finbra_desp_outros_area_pcapita != 0, ]


# 2. Time trend plots
# =================================================================

ts <- function(folder,sample){
  vars_name <- vars_map[,1]
  vars_label <- vars_map[,2]
  
  
  # setting df for plotting: collapese and long format
  df_plot <- sample %>%
    filter(ano>1997) %>% 
    ungroup() %>% 
    group_by(ano) %>% 
    summarise_at(vars_name, ~ weighted.mean(.,peso_pop, na.rm = T)) %>% 
    ungroup()
  
  last_col <- ncol(df_plot)
  names(df_plot)[2:last_col] <- vars_label
  
  df_plot <- df_plot %>% 
    pivot_longer(cols = all_of(vars_label),
                 names_to = "var",
                 values_to = "Spending per capita (R$ 2010)") %>% 
    mutate(var = factor(var, levels = vars_label))
  
  
  df_plot_ab <- sample %>%
    filter(ano>1997 & !is.na(below)) %>% 
    ungroup() %>% 
    group_by(ano,below) %>% 
    summarise_at(vars_name, ~ weighted.mean(.,peso_pop, na.rm = T)) %>% 
    ungroup()
  
  last_col <- ncol(df_plot_ab)
  names(df_plot_ab)[3:last_col] <- vars_label
  
  df_plot_ab <- df_plot_ab %>% 
    pivot_longer(cols = all_of(vars_label),
                 names_to = "var",
                 values_to = "Spending per capita (R$ 2010)") %>% 
    mutate(var = factor(var, levels = vars_label))
  
  
  # plot 1: Revenues and spending
  plot <- df_plot %>%
    filter(var == "Total Revenue" | var == "Current Revenue" | var == "Total Spending" | var == "Current Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra1.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Revenue" | var == "Current Revenue" | var == "Total Spending" | var == "Current Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra1_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  
  # plot 1: Revenues and spending - Index
  plot <- df_plot %>%
    filter(var == "Total Revenue" | var == "Current Revenue" | var == "Total Spending" | var == "Current Spending") %>% 
    left_join(df_plot %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var) %>% 
                rename(v2000 = 1),
              by = "var") %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index:  2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra1_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Revenue" | var == "Current Revenue" | var == "Total Spending" | var == "Current Spending") %>% 
    left_join(df_plot_ab %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var,below) %>% 
                rename(v2000 = 1),
              by = c("var","below")) %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra1_ab_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  

  # plot 2: Revenues and spending, restrict
  plot <- df_plot %>%
    filter(var == "Total Revenue" | var == "Total Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra2.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Revenue" | var == "Total Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra2_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  # plot 2: Revenues and spending, restrict - Index
  plot <- df_plot %>%
    filter(var == "Total Revenue" | var == "Total Spending") %>% 
    left_join(df_plot %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var) %>% 
                rename(v2000 = 1),
              by = "var") %>% 
  mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
  ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra2_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Revenue" | var == "Total Spending") %>% 
    left_join(df_plot_ab %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var,below) %>% 
                rename(v2000 = 1),
              by = c("var","below")) %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra2_ab_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  # plot 3: Liabilities
  plot <- df_plot %>%
    filter(var == "Total Liabilities" | var == "Financial Liabilities") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra3.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Liabilities" | var == "Financial Liabilities") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra3_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  # plot 3: Liabilities - Index
  plot <- df_plot %>%
    filter(var == "Total Liabilities" | var == "Financial Liabilities") %>% 
    left_join(df_plot %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var) %>% 
                rename(v2000 = 1),
              by = "var") %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra3_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Liabilities" | var == "Financial Liabilities") %>% 
    left_join(df_plot_ab %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var,below) %>% 
                rename(v2000 = 1),
              by = c("var","below")) %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100") +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra3_ab_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  
  
  
  
  # plot 4: Rev - Spend + delta liabilities
  plot <- df_plot %>%
    filter(var == "Revenue - Spending" | var == "Delta Total Liabilities" | var == "Delta Financial Liabilities") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra4.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Revenue - Spending" | var == "Delta Total Liabilities" | var == "Delta Financial Liabilities") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra4_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  # plot 4: Rev - Spend + delta liabilities - Index
  plot <- df_plot %>%
    filter(var == "Revenue - Spending" | var == "Delta Total Liabilities" | var == "Delta Financial Liabilities") %>% 
    left_join(df_plot %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var) %>% 
                rename(v2000 = 1),
              by = "var") %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra4_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Revenue - Spending" | var == "Delta Total Liabilities" | var == "Delta Financial Liabilities") %>% 
    left_join(df_plot_ab %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var,below) %>% 
                rename(v2000 = 1),
              by = c("var","below")) %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>%
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra4_ab_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  # plot 5: Total Spending, Healh, Non-Health
  plot <- df_plot %>%
    filter(var == "Total Spending" | var == "Health and Sanitation Spending" | var == "Non-Health Spending" | var == "Non-Health Social Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra5.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Spending" | var == "Health and Sanitation Spending" | var == "Non-Health Spending" | var == "Non-Health Social Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra5_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  # plot 5: Total Spending, Healh, Non-Health - Index
  plot <- df_plot %>%
    filter(var == "Total Spending" | var == "Health and Sanitation Spending" | var == "Non-Health Spending" | var == "Non-Health Social Spending") %>% 
    left_join(df_plot %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var) %>% 
                rename(v2000 = 1),
              by = "var") %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra5_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Total Spending" | var == "Health and Sanitation Spending" | var == "Non-Health Spending" | var == "Non-Health Social Spending") %>% 
    left_join(df_plot_ab %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var,below) %>% 
                rename(v2000 = 1),
              by = c("var","below")) %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra5_ab_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  
  
  
  # plot 6: Healh, Non-Health as share of total spending
  plot <- df_plot %>%
    filter(var == "Health and Sanitation Spending" | var == "Non-Health Spending" | var == "Non-Health Social Spending") %>% 
    left_join(df_plot %>%
                filter(var=="Total Spending") %>%
                select(`Spending per capita (R$ 2010)`,ano) %>%
                rename(total=1),
              by = "ano") %>%
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/total) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Share of Total Spending")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra6.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Health and Sanitation Spending" | var == "Non-Health Spending" | var == "Non-Health Social Spending") %>% 
    left_join(df_plot_ab %>%
                filter(var=="Total Spending") %>%
                select(`Spending per capita (R$ 2010)`,ano, below) %>%
                rename(total=1),
              by = c("ano",'below')) %>%
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/total) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Share of Total Spending")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra6_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  # plot 7: Spending by Category
  plot <- df_plot %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra7.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra7_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  # plot 7: Spending by Category - Index
  plot <- df_plot %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    left_join(df_plot %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var) %>% 
                rename(v2000 = 1),
              by = "var") %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra7_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    left_join(df_plot_ab %>% 
                filter(ano==2000) %>% 
                select(`Spending per capita (R$ 2010)`,var,below) %>% 
                rename(v2000 = 1),
              by = c("var","below")) %>% 
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/v2000*100) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Index: 2000 = 100")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra7_ab_i.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  # plot 8: By Category as share of total spending
  plot <- df_plot %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    left_join(df_plot %>%
                filter(var=="Total Spending") %>%
                select(`Spending per capita (R$ 2010)`,ano) %>%
                rename(total=1),
              by = "ano") %>%
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/total) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Share of Total Spending")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra8.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    left_join(df_plot_ab %>%
                filter(var=="Total Spending") %>%
                select(`Spending per capita (R$ 2010)`,ano, below) %>%
                rename(total=1),
              by = c("ano",'below')) %>%
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/total) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Share of Total Spending")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra8_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  # plot 8: By Category as share of total spending - Index
  plot <- df_plot %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    left_join(df_plot %>%
                filter(var=="Total Spending") %>%
                select(`Spending per capita (R$ 2010)`,ano) %>%
                rename(total=1),
              by = "ano") %>%
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/total) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, group = var)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Share of Total Spending")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra8.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  plot <- df_plot_ab %>%
    filter(var == "Health and Sanitation Spending" | var == "Education and Culture Spending" | var == "Housing and Urbanization Spending" | var == "Social Assistence and Social Security Spending" | var == "Transportation Spending" | var == "Public Safety Spending") %>% 
    left_join(df_plot_ab %>%
                filter(var=="Total Spending") %>%
                select(`Spending per capita (R$ 2010)`,ano, below) %>%
                rename(total=1),
              by = c("ano",'below')) %>%
    mutate(`Spending per capita (R$ 2010)` = `Spending per capita (R$ 2010)`/total) %>% 
    ggplot(aes(x = ano, y = `Spending per capita (R$ 2010)`, color = var, linetype = below)) +
    geom_line(size = 0.8, alpha = 0.6) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    theme_light() +
    labs(x = "Year",
         y = "Share of Total Spending")+
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE,override.aes = list(size = 3))) 
  
  
  filePDF <- paste0(FIG,folder,"desc_finbra8_ab.pdf")
  
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 8, height = 5,
         units = "in")
  
  
  
  
  
  
  
  
  
  
  
}

vars_map <- rbind(cbind('finbra_recorc_pcapita','Total Revenue'),
                  cbind('finbra_reccorr_pcapita','Current Revenue'),
                  cbind('finbra_desp_o_pcapita','Total Spending'),
                  cbind('finbra_desp_c_pcapita', 'Current Spending'),
                  cbind('finbra_rec_desp_pcapita', 'Revenue - Spending'),
                  cbind('finbra_passivo_pcapita','Total Liabilities'),
                  cbind('finbra_passivo_fin_pcapita','Financial Liabilities'),
                  cbind('finbra_passivo_pcapita_delta','Delta Total Liabilities'),
                  cbind('finbra_passivo_fin_pcapita_delta','Delta Financial Liabilities'),
                  cbind('finbra_impostos_total_pcapita', 'Total Tax Revenue'),
                  cbind('finbra_desp_saude_san_pcapita',"Health and Sanitation Spending"),
                  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending'),
                  cbind('finbra_despsocial_pcapita','Non-Health Social Spending'),
                  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending'),
                  cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending'),
                  cbind('finbra_desp_hab_urb_pcapita','Housing and Urbanization Spending'),
                  cbind('finbra_desp_assist_prev_pcapita','Social Assistence and Social Security Spending'),
                  cbind('finbra_desp_transporte_pcapita','Transportation Spending'),
                  cbind('finbra_desp_seguranca_pcapita', 'Public Safety Spending'))


ts("desc_finbra/",df_nout)
ts("desc_finbra_bal/",df_balance_finbra)





