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


# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))


yearly_folder <- "yearly_reduced_form_dist_ec29_spending/"

# 2. Spending
# =================================================================

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



var_map <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (log)'),
                 cbind('finbra_reccorr_pcapita','Current Revenue per capita (log)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (log)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (log)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (log)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (log)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
                 cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)'),
                 cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (log)'),
                 cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (log)'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita (log)'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (log)'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (log)'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita (log)'),
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (log)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (log)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (log)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (log)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (log)'))

# logs
# -----------------------------------

for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-2,2,0.5,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}


for (i in seq(3,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-2,2,0.5,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}


for (i in seq(7,10,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-2,2,0.5,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}

for (i in c(7,seq(15,21,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-4,10,1,"log2",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
}



# level
# -----------------------------------

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



for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1500,1500,500,"level",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}


for (i in seq(3,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1500,1500,500,"level",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}


for (i in seq(7,10,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1500,1500,500,"level",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}

for (i in c(7,seq(15,21,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-500,500,100,"level2",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
}





# =============================================================================

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





load(paste0(dir,"regs.RData"))


yearly_folder <- "yearly_reduced_form_dist_ec29_spending_robust/"


outliers <- df %>% 
  mutate(s = log(finbra_desp_o_pcapita)) %>% 
  select(s,everything())


for(i in c(0,5,3.5,2.5)){
  
  if(i==0){
    outliers0 <- c()
  } else{
    
    obj <- paste0("outliers",i)
    ndesv <- i
    x <- mean(outliers$s, na.rm = T)
    sd <- sd(outliers$s, na.rm = T)
    out <- outliers %>% 
      mutate(s1 = x - sd * ndesv,
             s2 = x + sd * ndesv) %>% 
      filter(s<=s1 | s>=s2) %>% 
      select(cod_mun) %>% 
      unique()
    
    out <- out$cod_mun
    assign(obj,out,envir = .GlobalEnv)
    
  }
  
}


reduced_yearly_robust <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap,label_size){
  
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (outlier in c(0,5,3.5,2.5)){
    
    spec_reduced<- get(paste0("spec",3,"_post_y"))
    
    remove <- get(paste0("outliers",outlier))
    
    df_reg_out <- df_reg %>% 
      filter(!(cod_mun %in% remove))
    
    weight_vector <- df_reg_out[weight] %>% unlist() %>% as.numeric()
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg_out, weights = weight_vector,exactDOF = T)
    
    out <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010),
             outlier = as.character(outlier)) %>% 
      mutate(outlier = ifelse(outlier=="0","1. Full Sample",outlier),
             outlier = ifelse(outlier=="5","2. Removed 5 sd",outlier),
             outlier = ifelse(outlier=="3.5","3. Removed 3.5 sd",outlier),
             outlier = ifelse(outlier=="2.5","4. Removed 2.5 sd",outlier)) %>% 
      mutate(outlier = as.factor(outlier)) 
    
    # out$spec <- factor(out$spec,levels = c("Baseline","+ Baseline and Time Varying Controls"))  
    
    if(outlier==0){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
  }
  
  weight <- "peso_eq"
  
  outlier <- 2.5
  # reg without weight
  spec_reduced<- get(paste0("spec",3,"_post_y"))
  
  remove <- get(paste0("outliers",outlier))
  
  df_reg_out <- df_reg %>% 
    filter(!(cod_mun %in% remove))
  
  weight_vector <- df_reg_out[weight] %>% unlist() %>% as.numeric()
  # second stage regression
  # ------------------------------
  
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg_out, weights = weight_vector,exactDOF = T)
  
  out <- fit %>% 
    broom::tidy() %>%
    slice(1:13) %>%
    select(term,estimate,std.error) %>% 
    mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           year = seq.int(year_filter,2010),
           outlier = as.character(outlier)) %>% 
    mutate(outlier = "5. Removed 2.5 sd, no weights") %>% 
    mutate(outlier = as.factor(outlier)) 
  
  
  table <- rbind(table,out)
  
  
  
  
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
  shapes <-  c(15,16,17,18,8)
  
  # graph with now bounds adjs
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = outlier,group=outlier))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.8),color = "grey20") +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
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
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 8))
    
    
    
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
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = outlier,group=outlier))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.8),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=outlier),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
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
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 8))
    
    
    
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
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = outlier,group=outlier))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.8),color = "grey20") +
      geom_point(aes(y = ub_adj,x = year,group=outlier),
                 position=position_dodge(width=0.8),
                 shape = 24,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
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
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 8))
    
    
    
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
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = outlier,group=outlier))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.2, alpha = 1, position = position_dodge(width=0.8),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=outlier),
                 position=position_dodge(width=0.8),
                 shape = 25,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_point(aes(y = ub_adj,x = year,group=outlier),
                 position=position_dodge(width=0.8),
                 shape = 24,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      # scale_colour_manual(values = color_graph) +
      scale_shape_manual(values = shapes) +
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
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 8))
    
    
    
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



for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_robust(var,var_name,df,1,1998,-2,2,0.5,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}


for (i in seq(3,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_robust(var,var_name,df,1,1998,-2,2,0.5,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}


for (i in seq(7,10,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_robust(var,var_name,df,1,1998,-2,2,0.5,"log",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
  
}

for (i in c(7,seq(15,21,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_robust(var,var_name,df,1,1998,-4,10,1,"log2",below = below,weight = "peso_pop",year_cap = 2010) # ec29baseline
}




