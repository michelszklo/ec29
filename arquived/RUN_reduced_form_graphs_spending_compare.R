#######################################################################################################
# Author: Michel Szklo
# April 2022
# 
# This scripts runs reduced form graphs for all outcomes
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

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


# 1. Load data, folder and heterogeneity set up
# =================================================================
load(paste0(dir,"regs.RData"))

# dropping municipalities with outliers in spending

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

df <- df %>% 
  filter(!(cod_mun %in% outliers))


# creating above and below target sample

# df_above = df %>% filter(ec29_baseline>=0.15)
# df_below = df %>% filter(ec29_baseline<0.15)
# 
# df_second <- df_above
# df_first <- df_below


# below and above samples, graph groups
df_below <- df %>% filter(dist_ec29_baseline>0)
df_above <- df %>%
  filter(dist_ec29_baseline<=0) %>% 
  mutate_at(yeartreat_dummies, function(x) -x)



df1 <- df_below
df1_name <- "1. Below sample"
df2 <- df_above
df2_name <- "3. Above sample"

df3_name <- "4. Above"
df4_name <- "2. Below"

gf_name <- "_abovebelow"


# new specs
yeartreat_dummies_above <- sapply(yeartreat_dummies, function(x) paste0("above_",x),simplify = "array", USE.NAMES = F)
yeartreat_dummies_below <- sapply(yeartreat_dummies, function(x) paste0("below_",x),simplify = "array", USE.NAMES = F)

df[yeartreat_dummies_above] <- df[yeartreat_dummies]
df[yeartreat_dummies_below] <- df[yeartreat_dummies]


df <- df %>% 
  mutate_at(yeartreat_dummies_above, ~ ifelse(. <= 0, ., 0)) %>% 
  mutate_at(yeartreat_dummies_below, ~ ifelse(. > 0, ., 0)) %>% 
  mutate_at(yeartreat_dummies_above, function(x) -x)

yeartreat_dummies2 <- c(yeartreat_dummies_above,yeartreat_dummies_below)

spec1_post_y2 <- paste(" ~ ",paste(yeartreat_dummies2, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y2 <- paste(" ~ ",paste(yeartreat_dummies2, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y2 <- paste(" ~ ",paste(yeartreat_dummies2, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y2 <- paste(" ~ ",paste(yeartreat_dummies2, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")




# graph function to compare regressions
reduced_yearly_het_c <- function(outcome,var_name,df,df1,df1_name,df2,df2_name,df3_name,df4_name,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap,label_size){
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  transformations <- function(df_reg,transform){
    
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
      select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,
             all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),all_of(yeartreat_dummies_above), all_of(yeartreat_dummies_below),
             pop,peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_pop,
             finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
      filter(ano>=year_filter)
    
    df_reg <- df_reg[complete.cases(df_reg),]
    df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
    
  }
  
  transformations_sep <- function(df_reg,transform){
    
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
      select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,
             all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),
             pop,peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_pop,
             finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
      filter(ano>=year_filter)
    
    df_reg <- df_reg[complete.cases(df_reg),]
    df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
    
  }
  
  ln_outcome <- paste0("ln_",outcome)
  
  df_reg1 <- df1 %>% transformations_sep(transform = transform)
  df_reg2 <- df2 %>% transformations_sep(transform = transform)
  df_reg <- df %>% transformations(transform = transform)
  
  
  
  # baseline mean of outcome for each sample
  
  baseline1 <- mean(df_reg1[ln_outcome] %>% unlist() %>%  as.numeric())
  baseline2 <- mean(df_reg2[ln_outcome] %>% unlist() %>%  as.numeric())
  
  
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    weight_vector <- df_reg1[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg1, weights = weight_vector,exactDOF = T)
    
    table1 <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      # mutate(estimate = estimate/baseline1*0.10,
      #        lb = lb/baseline1*0.10,
      #        ub = ub/baseline1*0.10) %>% 
      mutate(Sample = df1_name)
    
  }
  
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    weight_vector <- df_reg2[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg2, weights = weight_vector,exactDOF = T)
    
    table2 <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      # mutate(estimate = estimate/baseline2*0.10,
      #        lb = lb/baseline2*0.10,
      #        ub = ub/baseline2*0.10) %>% 
      mutate(Sample = df2_name)
    
  }
  
  
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y2"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
    
    table3 <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(is.nan(estimate),0,estimate)) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      # mutate(estimate = estimate/baseline2*0.10,
      #        lb = lb/baseline2*0.10,
      #        ub = ub/baseline2*0.10) %>% 
      mutate(Sample = df3_name)
    
    table4 <- fit %>% 
      broom::tidy() %>%
      slice(14:26) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(is.nan(estimate),0,estimate)) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010)) %>% 
      # mutate(estimate = estimate/baseline2*0.10,
      #        lb = lb/baseline2*0.10,
      #        ub = ub/baseline2*0.10) %>% 
      mutate(Sample = df4_name)
    
  }
  
  
  
  table <- rbind(table1,table4,table2,table3)
  
  # adjusments for big confidence intervals
  table <- table %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub))
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  }else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  
  # GRAPHS
  # ---------
  
  # shapes <-  c(8,15,19)
  colors <-  c("#67a9cf","#67a9cf","#ef8a62","#ef8a62")
  
  # graph with now bounds adjs
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,shape = Sample ,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_shape_manual(values = c(8,19,8,19)) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,shape = Sample ,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = lb_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_shape_manual(values = c(8,19,8,19)) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,shape = Sample ,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = ub_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_shape_manual(values = c(8,19,8,19)) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, color = Sample,shape = Sample ,group=Sample))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_point(aes(y = lb_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_point(aes(y = ub_adj,x = year,group=Sample),
                 position=position_dodge(width=0.6),
                 shape = 24,
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.6)) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_shape_manual(values = c(8,19,8,19)) +
      # scale_colour_manual(values = color_graph) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,outcome,"_",instrument,"_",instrument,"_",sample,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}



yearly_folder <- "yearly_reduced_form_dist_ec29_compare/"




# 2. Spending
# =================================================================



var_map <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (2010 R$)'),
                 cbind('finbra_reccorr_pcapita','Current Revenue per capita (2010 R$)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (2010 R$)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                 cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (2010 R$)'),
                 cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (2010 R$)'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita (2010 R$)'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (2010 R$)'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (2010 R$)'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita (2010 R$)'),
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'))


# logs
# -----------------------------------
for (i in c(1,3)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df,df1,df1_name,df2,df2_name,df3_name,df4_name,1,1998,-4,4,1,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}



for (i in seq(7,10,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df,df1,df1_name,df2,df2_name,df3_name,df4_name,1,1998,-4,4,1,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
}



for (i in c(7,seq(15,21,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df,df1,df1_name,df2,df2_name,df3_name,df4_name,1,1998,-6,14,2,"log2",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
}






# level
# -----------------------------------

for (i in c(1,3)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df,df1,df1_name,df2,df2_name,df3_name,df4_name,3,1998,-1500,1500,500,"level",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}



for (i in seq(7,10,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df,df1,df1_name,df2,df2_name,df3_name,df4_name,3,1998,-1500,1500,500,"level",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
}



for (i in c(7,seq(15,21,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_het_c(var,var_name,df,df1,df1_name,df2,df2_name,df3_name,df4_name,3,1998,-1500,1500,500,"level2",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
}



