#######################################################################################################
# Author: Michel Szklo
# October 2022
# 
# This scripts plots outcome indexes by quantile of baseline distribution
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
            'ggsci',
            'gtools')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)


# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
graphs_folder <- paste0(dir,"regs_outputs/parallel/")
graphs_folder2 <- paste0(dir,"regs_outputs/parallel_w/")


# ------------------------------------


# choose step

# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))
index <- data.frame(read.dta13("C:/Users/Michel/Documents/GitHub/ec29/indexes.dta"))


# merge indexes to main df
all_df <- c("df","df_low_ineq","df_high_ineq","df_low_pov","df_high_pov","df_low_hi","df_high_hi","df_first","df_second")

imerge <- function(df){
  df <- df %>% 
    left_join(index, by = c("ano","cod_mun","cod_uf"))
}

for(d in all_df){
  df_merge <- get(d)
  df_merge <- df_merge %>% imerge()
  assign(d,df_merge,envir = .GlobalEnv)
}



df_quantiles <- df %>% 
  mutate(dist_quantile_interval = quantcut(dist_ec29_baseline,6),
         dist_quantile_group = ifelse(ano==2000,quantcut(dist_ec29_baseline,6),NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_quantile_group = mean(dist_quantile_group, na.rm = T)) %>% 
  ungroup() %>% 
  select(cod_mun,ano,dist_ec29_baseline,dist_quantile_interval,dist_quantile_group,
         access_index,access_pc_index,access_npc_index,
         input_index,hr_index,hospital_index,
         birth_index,birth_others_index,imr_index,
         finbra_desp_saude_san_pcapita,
         ACS_popprop, eSF_popprop,
         siab_accomp_especif_pcapita, siab_accomp_especif_pacs_pcapita,
         siab_accomp_especif_psf_pcapita, siab_visit_cons_pcapita,
         siab_visit_cons_pacs_pcapita, siab_visit_cons_psf_pcapita,
         sia_ncnes_acs_pcapita, sia_ncnes_medcom_pcapita, sia_ncnes_enfacs_pcapita,
         sia_ncnes_psf_pcapita, sia_ncnes_medpsf_pcapita, sia_ncnes_enfpsf_pcapita,
         sia_ncnes_outpsf_pcapita, sia_pcapita, sia_ab_pcapita, sia_nprod_amb_lc_mun_pcapita,
         sia_nprod_amb_hc_mun_pcapita, birth_prenat_ig, birth_prenat_0, birth_prenat_1_6, 
         birth_prenat_7_plus, tx_sih_maternal, tx_sih_infant_icsap, tx_sih_infant_nicsap,
         tx_mi,pop,ams_hospital_mun_pcapita) %>% 
  filter(!is.na(dist_quantile_interval))


df_quantiles %>% select(dist_quantile_interval) %>% unique()
groups <- df_quantiles %>% 
  group_by(dist_quantile_interval) %>% 
  summarise(dist_quantile_group = mean(dist_quantile_group,na.rm=T))


vars <- c('finbra_desp_saude_san_pcapita',
          'access_index','access_pc_index','access_npc_index',
          'input_index','hr_index','hospital_index',
          'birth_index','birth_others_index','imr_index',
          'ACS_popprop', 'eSF_popprop',
          'siab_accomp_especif_pcapita', 'siab_accomp_especif_pacs_pcapita',
          'siab_accomp_especif_psf_pcapita', 'siab_visit_cons_pcapita',
          'siab_visit_cons_pacs_pcapita', 'siab_visit_cons_psf_pcapita',
          'sia_ncnes_acs_pcapita', 'sia_ncnes_medcom_pcapita', 'sia_ncnes_enfacs_pcapita',
          'sia_ncnes_psf_pcapita', 'sia_ncnes_medpsf_pcapita', 'sia_ncnes_enfpsf_pcapita',
          'sia_ncnes_outpsf_pcapita', 'sia_pcapita', 'sia_ab_pcapita', 'sia_nprod_amb_lc_mun_pcapita',
          'sia_nprod_amb_hc_mun_pcapita', 'birth_prenat_ig', 'birth_prenat_0', 'birth_prenat_1_6', 
          'birth_prenat_7_plus', 'tx_sih_maternal', 'tx_sih_infant_icsap', 'tx_sih_infant_nicsap',
          'tx_mi')





for(v in vars){
  plot1 <- df_quantiles %>% 
    group_by(ano,dist_quantile_group) %>% 
    summarise(!!v := mean(eval(parse(text = v)),na.rm = T)) %>% 
    ungroup() %>% 
    mutate(dist_quantile_group = as.factor(dist_quantile_group)) %>% 
    filter(!is.nan(eval(parse(text = v)))) %>% 
    ggplot(aes(x = ano,y = eval(parse(text = v)),color = dist_quantile_group,linetype = dist_quantile_group ,group = dist_quantile_group)) +
    geom_vline(xintercept = 2000, alpha = 0.3,size = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.3,size = 0.2) +
    geom_line(alpha = 0.7,size = 0.5) +
    scale_color_discrete() +
    scale_linetype_manual(values=c("dashed","solid","solid","solid","dashed","dashed")) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    labs(y = v,
         group = "Quantile") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  plot2 <- df_quantiles %>% 
    filter(ano<=2000) %>% 
    group_by(ano,dist_quantile_group) %>% 
    summarise(!!v := mean(eval(parse(text = v)),na.rm = T)) %>% 
    ungroup() %>% 
    mutate(dist_quantile_group = as.factor(dist_quantile_group)) %>% 
    filter(!is.nan(eval(parse(text = v)))) %>% 
    ggplot(aes(x = ano,y = eval(parse(text = v)),color = dist_quantile_group,linetype = dist_quantile_group ,group = dist_quantile_group)) +
    geom_vline(xintercept = 2000, alpha = 0.3,size = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.3,size = 0.2) +
    geom_line(alpha = 0.7,size = 0.5) +
    scale_color_discrete() +
    scale_linetype_manual(values=c("dashed","solid","solid","solid","dashed","dashed")) +
    scale_x_continuous(breaks = seq(1998,2000,1), limits = c(1998,2000)) +
    labs(y = v,
         group = "Quantile") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  
  number <- grep(v,vars)
  
  filePDF <- paste0(graphs_folder,number,"_",v,".pdf")
  filePNG <- paste0(graphs_folder,number,"_",v,".png")
  ggsave(filePDF,
         plot = plot1,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  ggsave(filePNG,
         plot = plot1,
         device = "png",
         width = 7, height = 5,
         units = "in")
  
  filePDF <- paste0(graphs_folder,number,"_",v,"_pre.pdf")
  filePNG <- paste0(graphs_folder,number,"_",v,"_pre.png")
  ggsave(filePDF,
         plot = plot2,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  ggsave(filePNG,
         plot = plot2,
         device = "png",
         width = 7, height = 5,
         units = "in")
  
  
}

vars <- c('ams_hospital_mun_pcapita')

for(v in vars){
  plot1 <- df_quantiles %>% 
    group_by(ano,dist_quantile_group) %>% 
    summarise(!!v := weighted.mean(eval(parse(text = v)),w = pop,na.rm = T)) %>% 
    ungroup() %>% 
    mutate(dist_quantile_group = as.factor(dist_quantile_group)) %>% 
    filter(!is.nan(eval(parse(text = v)))) %>% 
    ggplot(aes(x = ano,y = eval(parse(text = v)),color = dist_quantile_group,linetype = dist_quantile_group ,group = dist_quantile_group)) +
    geom_vline(xintercept = 2000, alpha = 0.3,size = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.3,size = 0.2) +
    geom_line(alpha = 0.7,size = 0.5) +
    scale_color_discrete() +
    scale_linetype_manual(values=c("dashed","solid","solid","solid","dashed","dashed")) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    labs(y = v,
         group = "Quantile") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  plot2 <- df_quantiles %>% 
    filter(ano<=2000) %>% 
    group_by(ano,dist_quantile_group) %>% 
    summarise(!!v := weighted.mean(eval(parse(text = v)),w = pop,na.rm = T)) %>% 
    ungroup() %>% 
    mutate(dist_quantile_group = as.factor(dist_quantile_group)) %>% 
    filter(!is.nan(eval(parse(text = v)))) %>% 
    ggplot(aes(x = ano,y = eval(parse(text = v)),color = dist_quantile_group,linetype = dist_quantile_group ,group = dist_quantile_group)) +
    geom_vline(xintercept = 2000, alpha = 0.3,size = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.3,size = 0.2) +
    geom_line(alpha = 0.7,size = 0.5) +
    scale_color_discrete() +
    scale_linetype_manual(values=c("dashed","solid","solid","solid","dashed","dashed")) +
    scale_x_continuous(breaks = seq(1998,2000,1), limits = c(1998,2000)) +
    labs(y = v,
         group = "Quantile") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  
  number <- grep(v,vars)
  
  filePDF <- paste0(graphs_folder2,number,"_",v,".pdf")
  filePNG <- paste0(graphs_folder2,number,"_",v,".png")
  ggsave(filePDF,
         plot = plot1,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  ggsave(filePNG,
         plot = plot1,
         device = "png",
         width = 7, height = 5,
         units = "in")
  
  filePDF <- paste0(graphs_folder2,number,"_",v,"_pre.pdf")
  filePNG <- paste0(graphs_folder2,number,"_",v,"_pre.png")
  ggsave(filePDF,
         plot = plot2,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  ggsave(filePNG,
         plot = plot2,
         device = "png",
         width = 7, height = 5,
         units = "in")
  
  
}



