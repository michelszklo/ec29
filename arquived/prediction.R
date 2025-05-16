#######################################################################################################
# Author: Michel Szklo
# September 2022
# 
# This scripts 
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
            'gtools',
            'grid',
            'gridExtra')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)

# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

graphs_folder <- paste0(dir,"regs_outputs/prediction/")

# ------------------------------------




# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))

yearly_folder <- "yearly_reduced_form_dist_ec29_robust/"

index <- data.frame(read.dta13("C:/Users/Michel/Documents/GitHub/ec29/indexes.dta"))

df_low_ineq <- df_below
df_high_ineq <- df_above

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



df <- df %>% 
  mutate(dist_quantile_interval = quantcut(dist_ec29_baseline,4),
         dist_quantile_group = ifelse(ano==2000,quantcut(dist_ec29_baseline,4),NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(dist_quantile_group = mean(dist_quantile_group, na.rm = T)) %>% 
  ungroup()



# 2. regs function and graphs
# =================================================================

# specs
spec1_post <- paste(" ~ ","iv"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# inputs

prediction <- function(outcome,var_name,df,year_filter) {
  
  # data set up
  df_reg <- df
  ln_outcome <- outcome
  
  df_reg <- df_reg %>% 
    mutate(t_outcome = ifelse(ano==2000,eval(parse(text = ln_outcome)),NA)) %>% 
    mutate(t_outcome = ifelse(ano==2000 & is.na(t_outcome),dplyr::lag(eval(parse(text = ln_outcome)),1),t_outcome)) %>% 
    group_by(cod_mun) %>% 
    mutate(t_outcome = mean(t_outcome,na.rm = T)) %>% 
    ungroup() %>% 
    mutate(t_outcome = t_outcome * t)
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(yeartreat_dummies),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,
           dist_quantile_interval,dist_quantile_group) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # specs and weight
  spec_reduced<- get(paste0("spec",1,"_post"))
  
  weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()
  
  
  # regression 
  
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  df_out <- df_reg %>% select(cod_mun,ano,dist_quantile_group)
  df_out$fit_treat <- fit[["fitted.values"]][,1]
  
  # regression without treatment
  
  df_reg <- df_reg %>% mutate(iv=0)
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  df_out$fit_placebo <- fit[["fitted.values"]][,1]
  
  
  # average effect by quartile
  df_out <- df_out %>% 
    group_by(ano,dist_quantile_group) %>% 
    summarise(fit_treat = mean(fit_treat,na.rm=T),
              fit_placebo = mean(fit_placebo,na.rm=T)) %>% 
    ungroup() %>% 
    rename(`with treatment` = fit_treat,
           `without treatment` = fit_placebo) %>% 
    pivot_longer(cols = c(`with treatment`,`without treatment`),
                 names_to = "Predicted yhat",
                 values_to = "fit") %>% 
    mutate(`Predicted yhat` = as.factor(`Predicted yhat`),
           dist_quantile_group = as.factor(dist_quantile_group)) %>% 
    filter(!is.na(dist_quantile_group))
  
  
  
  plot1 <- df_out %>% filter(dist_quantile_group==1) %>% 
    ggplot(aes(x=ano,y=fit,linetype = `Predicted yhat`)) +
    geom_line() +
    scale_linetype_manual(values = c("solid","dashed")) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    labs(title = "1st Quartile",
         subtitle = "Distance to EC29: -0.652 to -0.0232") +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="bottom")
  
  plot2 <- df_out %>% filter(dist_quantile_group==2) %>% 
    ggplot(aes(x=ano,y=fit,linetype = `Predicted yhat`)) +
    geom_line() +
    scale_linetype_manual(values = c("solid","dashed")) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    labs(title = "2nd Quartile",
         subtitle = "Distance to EC29: -0.0232 to 0.0224") +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="bottom")
  
  plot3 <- df_out %>% filter(dist_quantile_group==3) %>% 
    ggplot(aes(x=ano,y=fit,linetype = `Predicted yhat`)) +
    geom_line() +
    scale_linetype_manual(values = c("solid","dashed")) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    labs(title = "3rd Quartile",
         subtitle = "Distance to EC29: 0.0224 to 0.0579]") +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="bottom")
  
  plot4 <- df_out %>% filter(dist_quantile_group==4) %>% 
    ggplot(aes(x=ano,y=fit,linetype = `Predicted yhat`)) +
    geom_line() +
    scale_linetype_manual(values = c("solid","dashed")) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1998,2010)) +
    labs(title = "4th Quartile",
         subtitle = "Distance to EC29: 0.0579 to 0.15") +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="bottom")
  
  
  plot <- grid.arrange(plot1,plot2,plot3,plot4,
                       top = textGrob(var_name,gp=gpar(fontsize=15)))
  
  filePDF <- paste0(graphs_folder,outcome,".pdf")
  filePNG <- paste0(graphs_folder,outcome,".png")
  ggsave(filePDF,
         plot = plot,
         device = "pdf",
         width = 14, height = 10,
         units = "in")
  ggsave(filePNG,
         plot = plot,
         device = "png",
         width = 14, height = 10,
         units = "in")
  
  
}


# 3. Running Regs
# =================================================================


var_map <-  rbind(cbind('access_index','Access and Production of Health Services Index'),
                  cbind('access_pc_index','Primary Care Access and Production Index'),
                  cbind('access_npc_index','Non-Primary Care Access and Production Index'),
                  cbind('input_index','Health Inputs Index'),
                  cbind('hr_index','Human Resources Index'),
                  cbind('hospital_index','Hospitals Index'),
                  cbind('birth_index','Birth Outcomes Index'),
                  cbind('imr_index','Infant Mortality Index'),
                  cbind('birth_others_index','Other Birth Outcomes Index')
)



for (i in 1:9){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  prediction(var,var_name,df,1998)
  
}
