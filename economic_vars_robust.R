#######################################################################################################
# Author: Michel Szklo
# June 2024
# 
# This scripts analyses run regressions for GDP and Workers for 1996 - 2010
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

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

load(paste0(dir,"regs.RData"))

yearly_folder <- "regs_plots_trend/robust_other/"


# 1. Regression functions
# =================================================================

set_spec <- 2

reduced_yearly2 <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=set_spec){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  # df_reg <- df
  df_reg <- df %>% 
    filter(cod_uf!=22 & cod_uf!=23 & cod_uf!=29)
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano,cod_mun,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter) %>% 
    select(-finbra_desp_saude_san_pcapita_neighbor,-lrf) %>% 
    mutate(pbf_pcapita = ifelse(ano<1998 & is.na(pbf_pcapita),0,pbf_pcapita))
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(1:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec))
  
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    t <- table[i,]
    t <- t %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!var_name := estimate)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  
  
  
  # graph with now bounds defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  
  graph <- table %>% filter(!is.nan(estimate)) %>%
    ggplot(aes(x = year, y = estimate))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
    geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=ylabel),
          axis.text = element_text(size = 10),
          legend.position="bottom")
  
  
  
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  
}

reduced_yearly_ab2 <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=set_spec){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano,cod_mun,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies_ab),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter) %>% 
    select(-finbra_desp_saude_san_pcapita_neighbor,-lrf) %>% 
    mutate(pbf_pcapita = ifelse(ano<1998 & is.na(pbf_pcapita),0,pbf_pcapita))
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_ab"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary_ab"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(1:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(16:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec))
  
  
  tableA <- table %>% filter(target=="Above")
  tableB <- table %>% filter(target=="Below")
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    tA <- tableA[i,]
    tA <- tA %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Above)") := estimate)
    
    tB <- tableB[i,]
    tB <- tB %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Below)") := estimate)
    
    t <- tA %>% bind_cols(tB)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  # graphs variation
  
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  
  # 
  graph <- table %>% filter(!is.nan(estimate)) %>%
    ggplot(aes(x = year, y = estimate, color = target, group = target))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    scale_color_manual(values = colors) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=ylabel),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank())
  
  
  
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  
}



reduced_yearly_imr2 <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=set_spec){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  # df_reg <- df
  df_reg <- df %>% 
    filter(cod_uf!=22 & cod_uf!=23 & cod_uf!=29)
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano,cod_mun,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter) %>% 
    select(-finbra_desp_saude_san_pcapita_neighbor,-lrf) %>% 
    mutate(pbf_pcapita = ifelse(ano<1998 & is.na(pbf_pcapita),0,pbf_pcapita))
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_binary"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(1:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec))
  
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    t <- table[i,]
    t <- t %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!var_name := estimate)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  
  
  
  # graph with now bounds defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  
  graph <- table %>% filter(!is.nan(estimate)) %>%
    ggplot(aes(x = year, y = estimate))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
    geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=ylabel),
          axis.text = element_text(size = 10),
          legend.position="bottom")



  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  
}

reduced_yearly_ab_imr2 <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=set_spec){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano,cod_mun,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies_ab),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter) %>% 
    select(-finbra_desp_saude_san_pcapita_neighbor,-lrf) %>% 
    mutate(pbf_pcapita = ifelse(ano<1998 & is.na(pbf_pcapita),0,pbf_pcapita))
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary_ab"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(1:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(16:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec))
  
  
  tableA <- table %>% filter(target=="Above")
  tableB <- table %>% filter(target=="Below")
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    tA <- tableA[i,]
    tA <- tA %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Above)") := estimate)
    
    tB <- tableB[i,]
    tB <- tB %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Below)") := estimate)
    
    t <- tA %>% bind_cols(tB)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  # graphs variation
  
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  
  # 
  graph <- table %>% filter(!is.nan(estimate)) %>%
    ggplot(aes(x = year, y = estimate, color = target, group = target))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    scale_color_manual(values = colors) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=ylabel),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank())



  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")

  
  
}


reduced_yearly <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=1){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    t <- table[i,]
    t <- t %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!var_name := estimate)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
 
  
  
  # graph with now bounds defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  graph <- table %>% filter(!is.nan(estimate)) %>% 
    ggplot(aes(x = year, y = estimate))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
    geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=ylabel),
          axis.text = element_text(size = 10),
          legend.position="bottom")
  
  
  
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  
  
  
  
}

reduced_yearly_ab <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=1){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_ab),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_ab"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary_ab"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(18:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  tableA <- table %>% filter(target=="Above")
  tableB <- table %>% filter(target=="Below")
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    tA <- tableA[i,]
    tA <- tA %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Above)") := estimate)
    
    tB <- tableB[i,]
    tB <- tB %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Below)") := estimate)
    
    t <- tA %>% bind_cols(tB)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  graph <- table %>% filter(!is.nan(estimate)) %>% 
    ggplot(aes(x = year, y = estimate, color = target, group = target))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    scale_color_manual(values = colors) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=ylabel),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank())
  
  
  
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  
  
}




# 2. Variables
# =================================================================

df <- df %>% 
  mutate(rais_employees_pcapita = rais_employees/pop)

var_map <-  rbind(cbind('gdp_mun_pcapita','GDP Per Capita (log)'),
                  # cbind('gdp_mun','GDP (log)'),
                  cbind('rais_employees_pcapita','Formal Workers Per Capita (log)'))
                  # cbind('rais_employees','Formal Workers (log)'))


var_map2 <-  rbind(cbind('gdp_mun_pcapita','GDP Per Capita'),
                  # cbind('gdp_mun','GDP'),
                  cbind('rais_employees_pcapita','Formal Workers Per Capita'))
                  # cbind('rais_employees','Formal Workers'))



# 3. Running regs
# =================================================================

# outliers in IMR for 1996 and 1997
df <- df %>% 
  group_by(cod_mun) %>%
  mutate(ln_gdp_mun_pcapita = log(gdp_mun_pcapita)) %>% 
  mutate(mean = mean(ln_gdp_mun_pcapita,na.rm = T),
         sd = sd(ln_gdp_mun_pcapita, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(p95 = quantile(sd, probs = 0.95, na.rm = T),
         out3 = 0) %>% 
  mutate(out3 = ifelse(sd>p95,1,out3)) %>%
  mutate(out4 = 0) %>% 
  mutate(out4 = ifelse(ln_gdp_mun_pcapita>mean+2*sd,1,0)) %>% 
  select(mean,sd,out3,out4,everything())

df3 <- df %>% filter(out3==0)
df4 <- df %>% filter(out4==0)


# log with trend, spec = 1
for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr2(var,var_name,df,1,1996,-100,200,20,paste0("1_cont_log_trend_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr2(var,var_name,df,1,1996,-100,200,20,paste0("1_ab_log_trend_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}

# log without trend, spec = 1
for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly2(var,var_name,df,1,1996,-100,200,20,paste0("2_cont_log_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab2(var,var_name,df,1,1996,-100,200,20,paste0("2_ab_log_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


# level with trend, spec = 1

for (i in seq(1,2,1)){
  var <- var_map2[i,1]
  var_name <- var_map2[i,2]
  print(var_name)
  reduced_yearly_imr2(var,var_name,df,3,1996,-100,200,20,paste0("3_cont_level_trend_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


for (i in seq(1,2,1)){
  var <- var_map2[i,1]
  var_name <- var_map2[i,2]
  print(var_name)
  reduced_yearly_ab_imr2(var,var_name,df,3,1996,-100,200,20,paste0("3_ab_level_trend_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


# level without trend, spec = 1
for (i in seq(1,2,1)){
  var <- var_map2[i,1]
  var_name <- var_map2[i,2]
  print(var_name)
  reduced_yearly2(var,var_name,df,3,1996,-100,200,20,paste0("4_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


for (i in seq(1,2,1)){
  var <- var_map2[i,1]
  var_name <- var_map2[i,2]
  print(var_name)
  reduced_yearly_ab2(var,var_name,df,3,1996,-100,200,20,paste0("4_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}



# 1996 -> 
# log without trend, spec = 1


for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-100,200,20,paste0("5_cont_log_1998_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab(var,var_name,df,1,1998,-100,200,20,paste0("5_ab_log_1998_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


# level without trend, spec = 1

for (i in seq(1,2,1)){
  var <- var_map2[i,1]
  var_name <- var_map2[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-100,200,20,paste0("6_cont_level_1998_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab(var,var_name,df,3,1998,-100,200,20,paste0("6_ab_level_1998_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}





