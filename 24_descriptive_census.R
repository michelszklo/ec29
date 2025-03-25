#######################################################################################################
# Author: Michel Szklo
# December 2023
# 
# This script creates scatter plots for control variables shifts between census (1991 - 2000)
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
            'gridExtra',
            'binsreg',
            'lfe',
            'rJava',
            'lmtest',
            'sandwich',
            'fastDummies')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
raw <- paste0(dir,"data/")
output <- "C:/Users/mszklo/Documents/GitHub/ec29/outputs/scatter_plots/fixed effects/"
options(scipen = 999)

# =================================================================
# 1. Loading data
# =================================================================

# complete dataset
load(paste0(dir,"regs.RData"))

outliers <- df %>% 
  mutate(s = log(finbra_desp_o_pcapita)) %>% 
  select(s,everything())

ndesv <- 5
x <- mean(outliers$s, na.rm = T)
sd <- sd(outliers$s, na.rm = T)
outliers <- outliers %>% 
  mutate(s1 = x - sd * ndesv,
         s2 = x + sd * ndesv) %>% 
  filter(s<=s1 | s>=s2) %>% 
  select(cod_mun) %>% 
  unique()

outliers <- outliers$cod_mun

df2 <- df %>% 
  filter(!(cod_mun %in% outliers))

# creating subdataset with only distance to ec29 and municipality code
df3 <- df %>% 
  filter(ano==2000)

df <- df %>% 
  # filter(ano==2000)
  filter(ano==2000) %>%
  select(cod_mun,dist_ec29_baseline,dist_ec29_baseline_below,pop,uf) %>% 
  mutate(dist_ec29_baseline_above = ifelse(!is.nan(dist_ec29_baseline),0,NaN)) %>% 
  mutate(dist_ec29_baseline_above = ifelse(dist_ec29_baseline<0,dist_ec29_baseline,dist_ec29_baseline_above)) %>% 
  mutate(below = ifelse(dist_ec29_baseline>0,1,0))



# loading control data
atlas <- read.csv(paste0(raw,"Atlas2013/atlas_data.csv"), encoding = "UTF-8", sep = ";") %>%
  rename(ano=1) %>%
  filter(ano!=2010) %>% 
  rename(cod_mun = Codmun6)
colnames(atlas) <- tolower(colnames(atlas))
atlas <- atlas %>% 
  select(cod_mun,ano,e_anosestudo,t_analf18m,gini,pind,pmpob,rdpc,t_agua,t_lixo,t_luz,agua_esgoto,idhm) %>% 
  mutate(pind = pind/100,
         pmpob = pmpob/100)

# analysis df
df <- atlas %>% 
  left_join(df, by = "cod_mun")



# some descriptive analysis

summary1991 <- df %>% 
  filter(ano==1991) %>% 
  select(-cod_mun,-ano,-dist_ec29_baseline_above,-below,-dist_ec29_baseline,-dist_ec29_baseline_below,-uf,-pop) %>% 
  summarise_all(~mean(.,rm.na = T)) %>% 
  pivot_longer(cols = e_anosestudo:idhm,
               names_to = "var",
               values_to = "mean1991")
  
summary1991_ab <- df %>% 
  filter(ano==1991) %>% 
  select(-cod_mun,-ano,-dist_ec29_baseline_above,-dist_ec29_baseline,-dist_ec29_baseline_below,-uf,-pop) %>% 
  group_by(below) %>% 
  summarise_all(~mean(.,rm.na = T)) %>% 
  ungroup() %>% 
  filter(!is.na(below)) %>% 
  pivot_longer(cols = e_anosestudo:idhm,
               names_to = "var",
               values_to = "mean1991") %>% 
  pivot_wider(id_cols = "var",
              names_from = "below",
              values_from = "mean1991",
              names_prefix = "mean1991_")


summary2000 <- df %>% 
  filter(ano==2000) %>% 
  select(-cod_mun,-ano,-dist_ec29_baseline_above,-below,-dist_ec29_baseline,-dist_ec29_baseline_below,-uf,-pop) %>% 
  summarise_all(~mean(.,rm.na = T)) %>% 
  pivot_longer(cols = e_anosestudo:idhm,
               names_to = "var",
               values_to = "mean2000")

summary2000_ab <- df %>% 
  filter(ano==2000) %>% 
  select(-cod_mun,-ano,-dist_ec29_baseline_above,-dist_ec29_baseline,-dist_ec29_baseline_below,-uf,-pop) %>% 
  group_by(below) %>% 
  summarise_all(~mean(.,rm.na = T)) %>% 
  ungroup() %>% 
  filter(!is.na(below)) %>% 
  pivot_longer(cols = e_anosestudo:idhm,
               names_to = "var",
               values_to = "mean2000") %>% 
  pivot_wider(id_cols = "var",
              names_from = "below",
              values_from = "mean2000",
              names_prefix = "mean2000_")

summary <- summary1991 %>% 
  left_join(summary2000, by = "var") %>% 
  left_join(summary1991_ab, by = "var") %>% 
  left_join(summary2000_ab, by = "var")

write.csv(summary, paste0(dir,"data/censo/stats.csv"))


# =================================================================
# 2. Calculating shifts in variables  - 1991-2000
# =================================================================

vars <- names(df)[3:(ncol(df)-6)]

for (v in vars){
  
  varname1 <- paste0(v,"_91_00_shift")
  varname2 <- paste0(v,"_91_00_shift_pc")
  df <- df %>% 
    group_by(cod_mun) %>% 
    mutate(!!varname1 := dplyr::lead(get(v),1) - get(v)) %>% 
    mutate(!!varname2 := (dplyr::lead(get(v),1) - get(v)) / get(v)) %>% 
    mutate(!!varname2 := ifelse(get(v)==0,0, get(!!varname2)))
}

df <- df %>% 
  filter(ano==1991) %>% 
  select(-ano,-all_of(vars))



map <- rbind(
  
  cbind("e_anosestudo_91_00_shift","Change in Average Years of Study \n 1991-2000"),
  cbind("e_anosestudo_91_00_shift_pc","% Change in Average Years of Study \n 1991-2000"),
  
  cbind("t_analf18m_91_00_shift","Change in Illiteracy Rates \n 1991-2000"),
  cbind("t_analf18m_91_00_shift_pc","% Change in Illiteracy Rates \n 1991-2000"),
  
  cbind("gini_91_00_shift","Change in Gini Coefficient \n 1991-2000"),
  cbind("gini_91_00_shift_pc","% Change in Gini Coefficient \n 1991-2000"),
  
  cbind("pind_91_00_shift","Change in the Share of Pop in Extreme Poverty \n 1991-2000"),
  cbind("pind_91_00_shift_pc","% Change in the Share of Pop in Extreme Poverty \n 1991-2000"),
  
  cbind("pmpob_91_00_shift","Change in the Share of Pop in Poverty \n 1991-2000"),
  cbind("pmpob_91_00_shift_pc","% Change in the Share of Pop in Poverty \n 1991-2000"),
  
  cbind("rdpc_91_00_shift","Change in Income Per Capita \n 1991-2000"),
  cbind("rdpc_91_00_shift_pc","% Change in Income Per Capita \n 1991-2000"),
  
  cbind("t_agua_91_00_shift","Change in the Share of Pop with Access to Water \n 1991-2000"),
  cbind("t_agua_91_00_shift_pc","% Change in the Share of Pop with Access to Water \n 1991-2000"),
  
  cbind("t_lixo_91_00_shift","Change in the Share of Pop with Access to Garbage Collection \n 1991-2000"),
  cbind("t_lixo_91_00_shift_pc","% Change in the Share of Pop with Access to Garbage Collection \n 1991-2000"),
  
  cbind("t_luz_91_00_shift","Change in the Share of Pop with Access to Eletricity \n 1991-2000"),
  cbind("t_luz_91_00_shift_pc","% Change in the Share of Pop with Access to Eletricity \n 1991-2000"),
  
  cbind("agua_esgoto_91_00_shift","Change in the Share of Pop with Access to Adequate Water and Sewage \n 1991-2000"),
  cbind("agua_esgoto_91_00_shift_pc","% Change in the Share of Pop with Access to Adequate Water and Sewage \n 1991-2000"),
  
  cbind("idhm_91_00_shift","Change in HDI \n 1991-2000"),
  cbind("idhm_91_00_shift_pc","% Change in HDI \n 1991-2000")
)


final_table <- data.frame()
final_table_ab <- data.frame()

for (i in 1: nrow(map)){
  
  var <- map[i,1]
  var_name <- map[i,2]
  
  pc <- grep("pc",var)
  
  if(is.null(pc)){
    var2 <- substr(var,1,nchar(var)-3)
  } else{
    var2 <- var
  }
  
  
  outliers <- df %>% 
    mutate(s = log(get(var2))) %>% 
    select(s,everything())
  
  ndesv <- 2
  x <- mean(outliers$s, na.rm = T)
  sd <- sd(outliers$s, na.rm = T)
  outliers <- outliers %>% 
    mutate(s1 = x - sd * ndesv,
           s2 = x + sd * ndesv) %>% 
    filter(s<=s1 | s>=s2) %>% 
    select(cod_mun) %>% 
    unique()
  
  outliers <- outliers$cod_mun
  
  df_plot <- df %>% 
    filter(!(cod_mun %in% outliers)) %>% 
    filter(!is.infinite(get(var)))
  
  
  if(i %% 2 == 0){
    scatter <- ggplot(df_plot %>% 
                        filter(dist_ec29_baseline>-0.5)
                      ,
                      aes(x = dist_ec29_baseline, y = get(var))) +
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
      geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
      scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
      scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
      labs(y = var_name,
           x = "Distance to the EC29 target") +
      theme_light() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title = element_text(size=12),
            axis.text = element_text(size = 13),
            legend.position="none")
    
  } else {
    scatter <- ggplot(df_plot %>% 
                        filter(dist_ec29_baseline>-0.5)
                      ,
                      aes(x = dist_ec29_baseline, y = get(var))) +
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
      geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
      # scale_y_continuous(limits = c(-400,950), breaks = seq(-400,900,100)) +
      scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
      labs(y = var_name,
           x = "Distance to the EC29 target") +
      theme_light() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title = element_text(size=12),
            axis.text = element_text(size = 13),
            legend.position="none")
  }
  
  
  filePNG <- paste0(output,var,"_scatter_dist_ec29_baseline.png")
  filePDF <- paste0(output,var,"_scatter_dist_ec29_baseline_change05.pdf")
  ggsave(filePNG,
         plot = scatter,
         device = "png",
         width = 7, height = 6,
         units = "in")
  
  ggsave(filePDF,
         plot = scatter,
         device = "pdf",
         width = 7, height = 6,
         units = "in")
  

 fit <- lm(data = df_plot,
     formula = get(var) ~ dist_ec29_baseline)
  
  
  table <- summary(fit)
  table <- table$coefficients %>% as.data.frame()
  table <- table %>% 
    mutate(var = var_name)
  
  rse <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
  table[,2] <- rse[, 2]
  table[,3]  <- rse[, 3]
  table[,4]  <- rse[, 4]
  
  
  final_table <- rbind(final_table,table)
  
  print(var_name)
}



# with fixed effects

for (i in 1: nrow(map)){
  
  var <- map[i,1]
  var_name <- map[i,2]
  
  # pc <- grep("pc",var)
  # 
  # if(is.null(pc)){
  #   var2 <- substr(var,1,nchar(var)-3)
  # } else{
  #   var2 <- var
  # }
  # 
  
  outliers <- df %>% 
    mutate(s = log(get(var))) %>% 
    mutate(s = get(var)) %>% 
    select(s,everything()) %>% 
    mutate(s = ifelse(is.infinite(s),NaN,s))
  
  ndesv <- 3
  x <- mean(outliers$s, na.rm = T)
  sd <- sd(outliers$s, na.rm = T)
  outliers <- outliers %>% 
    mutate(s1 = x - sd * ndesv,
           s2 = x + sd * ndesv) %>% 
    filter(s>=s2) %>% 
    select(cod_mun) %>% 
    unique()
  
  outliers <- outliers$cod_mun
  
  df_plot <- df %>% 
    filter(!(cod_mun %in% outliers)) %>% 
    filter(!is.infinite(get(var)))
  
  df_reg <- df %>% 
    filter(!is.infinite(get(var)))
  
  df_plot <- df_plot %>% 
    dummy_cols(select_columns = "uf", ignore_na = TRUE)
  
  fe <- grep("^uf_",names(df_plot),value = T)
  
  
  fit <- felm(as.formula(paste(var," ~ ",paste(fe, collapse = " + "))),data = df_plot)
  
  df_plot2 <- df_plot %>% 
    filter(!is.na(uf))
  
  df_plot2$residuals <- fit$residuals %>% unlist()
  
  # 
  # 
  # if(i %% 2 == 0){
  #   scatter <- ggplot(df_plot2 
  #                     ,
  #                     aes(x = dist_ec29_baseline, y = residuals)) +
  #     geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  #     geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  #     geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  #     geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  #     # scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
  #     scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  #     labs(y = var_name,
  #          x = "Distance to the EC29 target") +
  #     theme_light() +
  #     theme(panel.grid.major = element_blank(), 
  #           panel.grid.minor = element_blank(),
  #           axis.title = element_text(size=12),
  #           axis.text = element_text(size = 13),
  #           legend.position="none")
  #   
  # }else{
  #   scatter <- ggplot(df_plot2 
  #                     ,
  #                     aes(x = dist_ec29_baseline, y = residuals)) +
  #     geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  #     geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  #     geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  #     geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  #     # scale_y_continuous(limits = c(-400,950), breaks = seq(-400,900,100)) +
  #     scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  #     labs(y = var_name,
  #          x = "Distance to the EC29 target") +
  #     theme_light() +
  #     theme(panel.grid.major = element_blank(), 
  #           panel.grid.minor = element_blank(),
  #           axis.title = element_text(size=12),
  #           axis.text = element_text(size = 13),
  #           legend.position="none")
  # }
  # 
  # filePDF <- paste0(output,var,"_scatter_dist_ec29_baseline_change05_fe.pdf")
  # ggsave(filePDF,
  #        plot = scatter,
  #        device = "pdf",
  #        width = 7, height = 6,
  #        units = "in")
  
  
  # above and below
  colors <-  c("#ef8a62","#ef8a62")
  
  scatter <- ggplot(df_plot2 %>%
                      mutate(below = ifelse(below==1,"Below","Above"))
                    ,
                    aes(x = dist_ec29_baseline, y = residuals, color = below, fill = below,group = below)) +
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_point(aes(size = pop), alpha = 0.2, color = 'steelblue4') +
    geom_smooth(method='lm', formula= y~x, alpha = 0.3, se = F, size = 0.7)+
    # scale_y_continuous(limits = c(-5,10), breaks = seq(-5,10,1)) + # i <- 18
    # scale_y_continuous(limits = c(-2,3), breaks = seq(-2,3,1)) + # i <- 8
    # scale_y_continuous(limits = c(-20,80), breaks = seq(-20,80,10)) + # i <- 20
    # scale_y_continuous(limits = c(-20,60), breaks = seq(-20,60,10)) + # i <- 16
    # scale_y_continuous(limits = c(-20,40), breaks = seq(-20,40,10)) + # i <- 14
    scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
    scale_color_manual(values = colors) +
    labs(y = '% Change 1991-2000',
         x = "Distance to the EC29 target") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=18),
          axis.text = element_text(size = 13),
          legend.position="none")
  
  filePDF <- paste0(output,var,"_scatter_dist_ec29_baseline_change05_fe_AB.pdf")
  ggsave(filePDF,
         plot = scatter,
         device = "pdf",
         width = 7, height = 6,
         units = "in")
  
  
  fit <- lm(data = df_plot2,
            formula = residuals ~ dist_ec29_baseline)
  table <- summary(fit)
  table <- table$coefficients %>% as.data.frame()
  table <- table %>% 
    mutate(var = var_name)
  
  rse <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
  table[,2] <- rse[, 2]
  table[,3]  <- rse[, 3]
  table[,4]  <- rse[, 4]
  
  final_table <- rbind(final_table,table)
  
  
  fit2 <- lm(data = df_plot2,
             formula = residuals ~ dist_ec29_baseline_above:dist_ec29_baseline + dist_ec29_baseline_below + dist_ec29_baseline_below:dist_ec29_baseline)
  
  table <- summary(fit2)
  table <- table$coefficients %>% as.data.frame()
  table <- table %>% 
    mutate(var = var_name)
  
  rse <- coeftest(fit2, vcov = vcovHC(fit2, type = "HC3"))
  
  table[,2] <- rse[, 2]
  table[,3]  <- rse[, 3]
  table[,4]  <- rse[, 4]
  
  final_table_ab <- rbind(final_table_ab,table)
  
  print(var_name)
}


# df2 <- df2 %>% 
#   select(cod_mun,ano,dist_ec29_baseline,finbra_desp_saude_san_pcapita,pop)
# 
# df2_98 <- df2 %>% filter(ano==1998) %>% 
#   rename(spending98 = finbra_desp_saude_san_pcapita)
# 
# df2_00 <- df2 %>% filter(ano==2000) %>% 
#   rename(spending00 = finbra_desp_saude_san_pcapita) %>% 
#   left_join(df2_98 %>% select(-ano,-pop), by = c("cod_mun","dist_ec29_baseline")) %>% 
#   mutate(`Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000` = spending00 - spending98,
#          `% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000` = (spending00 - spending98)/spending98)
# 
# 
# scatter <- ggplot(df2_00 %>% 
#                     filter(dist_ec29_baseline>-0.5)
#                   ,
#                   aes(x = dist_ec29_baseline, y = `Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`)) +
#   geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
#   geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
#   geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
#   geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
#   scale_y_continuous(limits = c(-1000,1000), breaks = seq(-1000,1000,250)) +
#   scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
#   labs(x = "Distance to the EC29 target") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size = 13),
#         legend.position="none")
# 
# filePNG <- paste0(output,"finbra","_scatter_dist_ec29_baseline.png")
# filePDF <- paste0(output,"finbra","_scatter_dist_ec29_baseline.pdf")
# ggsave(filePNG,
#        plot = scatter,
#        device = "png",
#        width = 7, height = 6,
#        units = "in")
# 
# ggsave(filePDF,
#        plot = scatter,
#        device = "pdf",
#        width = 7, height = 6,
#        units = "in")
# 
# 
# fit <- lm(data = df2_00,
#           formula = `Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000` ~ dist_ec29_baseline)
# table <- summary(fit)
# table <- table$coefficients %>% as.data.frame()
# table <- table %>% 
#   mutate(var = "Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000")
# 
# rse <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
# table[,2] <- rse[, 2]
# table[,3]  <- rse[, 3]
# table[,4]  <- rse[, 4]
# 
# final_table <- rbind(final_table,table)




# with fixed effects


df2 <- df2 %>% 
  select(cod_mun,ano,uf,dist_ec29_baseline,finbra_desp_saude_san_pcapita,pop,dist_ec29_baseline_below) %>% 
  mutate(dist_ec29_baseline_above = ifelse(!is.nan(dist_ec29_baseline),0,NaN)) %>% 
  mutate(dist_ec29_baseline_above = ifelse(dist_ec29_baseline<0,dist_ec29_baseline,dist_ec29_baseline_above)) %>% 
  mutate(below = ifelse(dist_ec29_baseline>0,1,0))

df2_98 <- df2 %>% filter(ano==1998) %>% 
  rename(spending98 = finbra_desp_saude_san_pcapita)

df2_00 <- df2 %>% filter(ano==2000) %>% 
  rename(spending00 = finbra_desp_saude_san_pcapita) %>% 
  left_join(df2_98 %>% select(-ano,-pop,-dist_ec29_baseline_below,-dist_ec29_baseline_above,-below), by = c("cod_mun","dist_ec29_baseline","uf")) %>% 
  mutate(`Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000` = spending00 - spending98,
         `% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000` = (spending00 - spending98)/spending98)



df2_00 <- df2_00 %>% 
  dummy_cols(select_columns = "uf", ignore_na = TRUE)

fe <- grep("^uf_",names(df2_00),value = T)




# level shift
# remove outliers
outliers <- df2_00 %>% 
  mutate(s = log(`Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`)) %>% 
  mutate(s = `Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`) %>% 
  select(s,everything()) %>% 
  mutate(s = ifelse(is.infinite(s),NaN,s))

ndesv <- 3
x <- mean(outliers$s, na.rm = T)
sd <- sd(outliers$s, na.rm = T)
outliers <- outliers %>% 
  mutate(s1 = x - sd * ndesv,
         s2 = x + sd * ndesv) %>% 
  filter(s>=s2) %>% 
  select(cod_mun) %>% 
  unique()

outliers <- outliers$cod_mun

df_plot <- df2_00 %>% 
  filter(!(cod_mun %in% outliers)) %>% 
  filter(!is.infinite(`Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`))

fit <- felm(as.formula(paste("`Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`"," ~ ",paste(fe, collapse = " + "))),data = df_plot)

df_plot <- df_plot %>% 
  filter(!is.na(uf)) %>% 
  filter(!is.na(`Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`))

df_plot$residuals <- fit$residuals %>% unlist()



scatter <- ggplot(df_plot 
                  ,
                  aes(x = dist_ec29_baseline, y = residuals)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  scale_y_continuous(limits = c(-1000,1000), breaks = seq(-1000,1000,250)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(x = "Distance to the EC29 target",
       y = "Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 13),
        legend.position="none")

filePDF <- paste0(output,"finbra","_scatter_dist_ec29_baseline_fe.pdf")
ggsave(filePDF,
       plot = scatter,
       device = "pdf",
       width = 7, height = 6,
       units = "in")


fit <- lm(data = df_plot,
          formula = residuals ~ dist_ec29_baseline)
table <- summary(fit)
table <- table$coefficients %>% as.data.frame()
table <- table %>% 
  mutate(var = "Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000")

rse <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
table[,2] <- rse[, 2]
table[,3]  <- rse[, 3]
table[,4]  <- rse[, 4]

final_table <- rbind(final_table,table)



# % shift
# remove outliers
outliers <- df2_00 %>% 
  mutate(s = log(`% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`)) %>% 
  mutate(s = `% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`) %>% 
  select(s,everything()) %>% 
  mutate(s = ifelse(is.infinite(s),NaN,s))

ndesv <- 3
x <- mean(outliers$s, na.rm = T)
sd <- sd(outliers$s, na.rm = T)
outliers <- outliers %>% 
  mutate(s1 = x - sd * ndesv,
         s2 = x + sd * ndesv) %>% 
  filter(s>=s2) %>% 
  select(cod_mun) %>% 
  unique()

outliers <- outliers$cod_mun

df_plot <- df2_00 %>% 
  filter(!(cod_mun %in% outliers)) %>% 
  filter(!is.infinite(`% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`))



fit <- felm(as.formula(paste("`% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`"," ~ ",paste(fe, collapse = " + "))),data = df_plot)

df_plot <- df_plot %>% 
  filter(!is.na(uf)) %>% 
  filter(!is.na(`% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`))

df_plot$residuals <- fit$residuals %>% unlist()



scatter <- ggplot(df_plot 
                  ,
                  aes(x = dist_ec29_baseline, y = residuals)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method='lm', formula= y~x,color = "#ef8a62",fill = "#ef8a62", alpha = 0.3, se = F, size = 0.7)+
  scale_y_continuous(limits = c(-5,15), breaks = seq(-5,15,5)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(x = "Distance to the EC29 target",
       y = "% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 13),
        legend.position="none")

filePDF <- paste0(output,"finbra_pc","_scatter_dist_ec29_baseline_fe.pdf")
ggsave(filePDF,
       plot = scatter,
       device = "pdf",
       width = 7, height = 6,
       units = "in")



# above and below
colors <-  c("#ef8a62","#ef8a62")
scatter <- ggplot(df_plot %>% 
                    mutate(below = ifelse(below==1,"Below","Above"))
                  ,
                  aes(x = dist_ec29_baseline, y = residuals, color = below, fill = below,group = below)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop), alpha = 0.2, color = 'steelblue4') +
  geom_smooth(method='lm', formula= y~x, alpha = 0.3, se = F, size = 0.7)+
  scale_y_continuous(limits = c(-5,15), breaks = seq(-5,15,5)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  scale_color_manual(values = colors) +
  labs(x = "Distance to the EC29 target",
       y = "% Change 1998-2000") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text = element_text(size = 13),
        legend.position="none")

filePDF <- paste0(output,"finbra_pc","_scatter_dist_ec29_baseline_fe_AB.pdf")
ggsave(filePDF,
       plot = scatter,
       device = "pdf",
       width = 7, height = 6,
       units = "in")


fit <- lm(data = df_plot,
          formula = residuals ~ dist_ec29_baseline)
table <- summary(fit)
table <- table$coefficients %>% as.data.frame()
table <- table %>% 
  mutate(var = "% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000")

rse <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
table[,2] <- rse[, 2]
table[,3]  <- rse[, 3]
table[,4]  <- rse[, 4]

final_table <- rbind(final_table,table)


fit2 <- lm(data = df_plot,
          formula = residuals ~ dist_ec29_baseline_above:dist_ec29_baseline + dist_ec29_baseline_below + dist_ec29_baseline_below:dist_ec29_baseline)
table <- summary(fit2)
table <- table$coefficients %>% as.data.frame()
table <- table %>% 
  mutate(var = "% Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000")

rse <- coeftest(fit2, vcov = vcovHC(fit2, type = "HC3"))
table[,2] <- rse[, 2]
table[,3]  <- rse[, 3]
table[,4]  <- rse[, 4]

final_table_ab <- rbind(final_table_ab,table)







# save final table
# write.xlsx2(final_table, file = paste0(output,"scatter_regs.xlsx"),sheetName = "scatter_regs_fe",row.names = T,append = T)


final_table2 <- final_table[seq(2,nrow(final_table),2),]

final_table_level <- final_table2[seq(1,nrow(final_table2)-1,2),]
final_table_pcent <- final_table2[seq(2,nrow(final_table2),2),]

final_table_ab_pcent <- final_table_ab[grep("%",final_table_ab$var),]


write.csv(final_table, file = paste0(output,"scatter_regs_fe_rse.csv"),row.names = T)
write.csv(final_table_ab_pcent, file = paste0(output,"scatter_regs_fe_rse_ab.csv"),row.names = T)
write.csv(final_table_level, file = paste0(output,"scatter_regs_fe_rse_level.csv"),row.names = T)
write.csv(final_table_pcent, file = paste0(output,"scatter_regs_fe_rse_pcent.csv"),row.names = T)


