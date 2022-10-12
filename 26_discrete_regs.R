#######################################################################################################
# Author: Michel Szklo
# October 2022
# 
# This scripts runs regressions with discrete treatment variable
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

# creating discrete treatment variable
all_df <- c("df","df_low_ineq","df_high_ineq","df_low_pov","df_high_pov","df_low_hi","df_high_hi","df_first","df_second")

discrete <- function(df){
  df <- df %>% 
    mutate(iv_p1 = 0,
           iv_p2 = 0,
           iv_p3 = 0,
           iv_0 = 0,
           iv_m1 = 0,
           iv_m2 = 0,
           iv_m3 = 0,
           iv_m4 = 0) %>% 
    mutate(iv_p1 = ifelse(dist_ec29_baseline>0 & dist_ec29_baseline<=0.05 & ano>2000,1,iv_p1),
           iv_p2 = ifelse(dist_ec29_baseline>0.05 & dist_ec29_baseline<=0.1 & ano>2000,1,iv_p2),
           iv_p3 = ifelse(dist_ec29_baseline>0.1 & dist_ec29_baseline<=0.15 & ano>2000,1,iv_p3),
           iv_m1 = ifelse(dist_ec29_baseline<0 & dist_ec29_baseline>-0.05 & ano>2000,1,iv_m1),
           iv_m2 = ifelse(dist_ec29_baseline<=-0.05 & dist_ec29_baseline>-0.1 & ano>2000,1,iv_m2),
           iv_m3 = ifelse(dist_ec29_baseline<=-0.1 & dist_ec29_baseline>-0.15 & ano>2000,1,iv_m3),
           iv_m4 = ifelse(dist_ec29_baseline<=-0.15 & ano>2000,1,iv_m4)) %>% 
    mutate(iv_m1 = 0)
}

for(d in all_df){
  df_add <- get(d)
  df_add <- df_add %>% discrete()
  assign(d,df_add,envir = .GlobalEnv)
}


# merging indexes
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


# 2. Regression specs
# =================================================================

# spec1_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec2_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec3_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec4_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


spec1_post_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



spec1_post_imr_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr_d <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 2. Regression formulas
# =================================================================

reduced <- function(outcome,var_name,df,regression_output,transform,year_filter,weight){
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),
           iv,iv_m4,iv_m3,iv_m2,iv_m1,iv_0,iv_p1,iv_p2,iv_p3,
           all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
 
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    # discrete treatment
    # ------------------------------
    spec_reduced<- get(paste0("spec",spec,"_post_d"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:7),fit %>% broom::glance() %>% select(nobs))
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(var = var_name)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
  table_graph <- table %>% filter(spec==1 | spec == 3) %>% 
    mutate(estimate = ifelse(term=="iv_m1",0,estimate)) %>% 
    mutate(term = ifelse(term=="iv_m4","< -0.15",term),
           term = ifelse(term=="iv_m3","-0.15 to -0.10",term),
           term = ifelse(term=="iv_m2","-0.10 to -0.05",term),
           term = ifelse(term=="iv_m1","-0.05 to 0",term),
           term = ifelse(term=="iv_p1","0 to 0.05",term),
           term = ifelse(term=="iv_p2","0.05 to 0.10",term),
           term = ifelse(term=="iv_p3","0.10 to 0.15",term)) %>% 
    mutate(spec = as.character(spec)) %>% 
    mutate(spec = ifelse(spec=="1","Baseline",spec),
           spec = ifelse(spec=="3","+ Baseline and Time Varying Controls",spec)) %>% 
    mutate(spec = as.factor(spec),
           term = as.factor(term)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error)
  
  table_graph$term <- factor(table_graph$term, levels = c("< -0.15","-0.15 to -0.10","-0.10 to -0.05","-0.05 to 0",
                                                          "0 to 0.05","0.05 to 0.10","0.10 to 0.15"))

  
  # GRAPHS
  # ---------
  
  # shapes <-  c(8,15,19)
  shapes <-  c(15,19)
  
 
  graph <- table_graph %>%
    ggplot(aes(x = term, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec)) +
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    # scale_colour_manual(values = color_graph) +
    scale_shape_manual(values = shapes) +
    theme_light() +
    labs(y = var_name,
         x = "Distance to the EC29 Target",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=6),
          legend.position="bottom")
  
  
  
  ggsave(paste0(dir,main_folder,"discrete_treatment/",i,"_",outcome,"_",instrument,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,"discrete_treatment/",i,"_",outcome,"_",instrument,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
}

reduced_imr <- function(outcome,var_name,df,regression_output,transform,year_filter,weight){
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),
           iv,iv_m4,iv_m3,iv_m2,iv_m1,iv_0,iv_p1,iv_p2,iv_p3,
           all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    # discrete treatment
    # ------------------------------
    spec_reduced<- get(paste0("spec",spec,"_post_imr_d"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:7),fit %>% broom::glance() %>% select(nobs))
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(var = var_name)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
  table_graph <- table %>% filter(spec==1 | spec == 3) %>% 
    mutate(estimate = ifelse(term=="iv_m1",0,estimate)) %>% 
    mutate(term = ifelse(term=="iv_m4","< -0.15",term),
           term = ifelse(term=="iv_m3","-0.15 to -0.10",term),
           term = ifelse(term=="iv_m2","-0.10 to -0.05",term),
           term = ifelse(term=="iv_m1","-0.05 to 0",term),
           term = ifelse(term=="iv_p1","0 to 0.05",term),
           term = ifelse(term=="iv_p2","0.05 to 0.10",term),
           term = ifelse(term=="iv_p3","0.10 to 0.15",term)) %>% 
    mutate(spec = as.character(spec)) %>% 
    mutate(spec = ifelse(spec=="1","Baseline",spec),
           spec = ifelse(spec=="3","+ Baseline and Time Varying Controls",spec)) %>% 
    mutate(spec = as.factor(spec),
           term = as.factor(term)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error)
  
  table_graph$term <- factor(table_graph$term, levels = c("< -0.15","-0.15 to -0.10","-0.10 to -0.05","-0.05 to 0",
                                                          "0 to 0.05","0.05 to 0.10","0.10 to 0.15"))
  
  
  # GRAPHS
  # ---------
  
  # shapes <-  c(8,15,19)
  shapes <-  c(15,19)
  
  
  graph <- table_graph %>%
    ggplot(aes(x = term, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec)) +
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    # scale_colour_manual(values = color_graph) +
    scale_shape_manual(values = shapes) +
    theme_light() +
    labs(y = var_name,
         x = "Distance to the EC29 Target",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=6),
          legend.position="bottom")
  
  
  
  ggsave(paste0(dir,main_folder,"discrete_treatment/",i,"_",outcome,"_",instrument,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,"discrete_treatment/",i,"_",outcome,"_",instrument,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
}




# 3. Running regs
# =================================================================

var_map <- rbind(cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'),
                 
                 
                 cbind('pc_index','Primary Care Index'),
                 cbind('input_index','Health Inputs Index (HR, Infra)'),
                 cbind('access_index','Access to Health Care Index'),
                 cbind('hosp_index','Hospitalization Index'),
                 cbind('birth_index','Birth Outcomes Index'),
                 cbind('imr_index','Infant Mortality Index')
                 
)




for (i in seq(1,11,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  reduced(var,var_name,df,"table_all",3,1998,"peso_eq")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}


for (i in seq(12,12,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  reduced(var,var_name,df,"table_all",3,1998,"peso_b")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}


for (i in seq(13,13,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  reduced_imr(var,var_name,df,"table_all",3,1998,"peso_b")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}















