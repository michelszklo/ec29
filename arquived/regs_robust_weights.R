#######################################################################################################
# Author: Michel Szklo
# September 2022
# 
# This scripts runs regressions for variables index created based on Anderson (2008) and main spending
#   variables for 3 different specification
#   1) preferred no weights
#   2) 1 + population weights
#   3) 1 + trend for underreporting
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


# 4. Override regression functions
# =================================================================


# Reduce form specification
# ------------------------------------------------
outcome_trend <- "t_outcome"

spec1_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,outcome_trend), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")




reduced_yearly <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap,label_size){
  
  
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
  
  
  
  # creating baseline outcome interacted with time
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
           t_outcome) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,2,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    if(spec==1){
      weight_vector <- df_reg["peso_eq"] %>% unlist() %>% as.numeric()
    } else {
      weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()
    }
    
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
    
    out <- fit %>% 
      broom::tidy() %>%
      slice(1:13) %>%
      select(term,estimate,std.error) %>% 
      mutate(estimate = ifelse(term==paste0("post_00_",instrument),0,estimate)) %>% 
      mutate(lb = estimate - 1.96 * std.error,
             ub = estimate + 1.96 * std.error,
             year = seq.int(year_filter,2010),
             spec = as.character(spec)) %>% 
      mutate(spec = ifelse(spec=="1","Preferred",spec),
             spec = ifelse(spec=="2","+ Population weights",spec),
             spec = ifelse(spec=="3","+ Outcome in 2000 * t",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Preferred","+ Population weights","+ Outcome in 2000 * t"))  
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
  }
  
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
  shapes <-  c(15,19,17)
  
  # graph with now bounds adjs
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
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
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=spec),
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
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = ub_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
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
      ggplot(aes(x = year, y = estimate, ymin = lb, ymax = ub, shape = spec,group=spec))+
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
      geom_pointrange(size = 0.4, alpha = 1, position = position_dodge(width=0.6),color = "grey20") +
      geom_point(aes(y = lb_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
                 shape = 25,
                 color = "grey50",
                 fill = "white",
                 size = 1.7,
                 stroke = 0.5) +
      geom_point(aes(y = ub_adj,x = year,group=spec),
                 position=position_dodge(width=0.6),
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




# 3. Define outcomes output name and output functions
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



for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.0,1.50,0.25,"20",below = below,weight = w,year_cap = 2010) # ec29baseline
}


for (i in seq(4,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-1.0,2.25,0.25,"20",below = below,weight = w,year_cap = 2010) # ec29baseline
}

for (i in seq(7,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.0,1.5,0.25,"20",below = below,weight = w,year_cap = 2010) # ec29baseline
}




var_map <- rbind(cbind('finbra_reccorr_pcapita','Total Revenue per capita (2010 R$)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (2010 R$)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                 cbind('finbra_desp_nao_saude_pcapita','All Other Spending per capita (2010 R$)'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita (2010 R$)'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (2010 R$)'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (2010 R$)'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_area_pcapita','Other Areas Spending per capita (2010 R$)'),
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'))


for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-4500,2500,1000,"B1",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
  
}

for (i in seq(6,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-2500,1000,500,"B2",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


for (i in seq(8,12,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1000,1000,500,"B2",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


for (i in c(6,seq(13,15,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-300,800,100,"6",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


for (i in seq(16,19,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-200,400,50,"7",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}



var_map <- rbind(cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
                 cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
                 cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
                 cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
                 cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
                 cbind('siab_visit_cons_pcapita','N. of Household Visits and Appointments (per capita)'),
                 cbind('siab_visit_cons_pacs_pcapita','N. of Household Visits and Appointments from Community Health Agents (per capita)'),
                 cbind('siab_visit_cons_psf_pcapita','N. of Household Visits and Appointments from Family Health Agents (per capita)'),
                 
                 cbind('ams_hr_superior_pcapita','N. of Doctors (per capita*1000)'),
                 cbind('ams_hr_technician_pcapita','N. of Nurses (per capita*1000)'),
                 cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants (per capita*1000)'),
                 cbind('ams_hr_admin_pcapita','N. of Administrative Professionals (per capita*1000)'),
                 
                 cbind('ams_hospital_mun_pcapita','N. of Municipal Hospitals (per capita*1000)'),
                 cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals (per capita*1000)'),
                 cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                 cbind('ams_hospital_mun_esp_pcapita', 'N. of Specialty Hospitals (per capita*1000)')
)


# figure 8 (ex 11)

for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.5,0.5,0.1,"8",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# figure 9 (ex 12)
for (i in seq(3,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.5,2.5,0.5,"9",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

for (i in seq(7,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.5,2.5,0.5,"9",below = below,weight = "peso_eq",year_cap = 2010,label_size = 6.9) # ec29baseline
}


for (i in seq(8,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.5,2.5,0.5,"9",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# figure 10
for (i in seq(9,12,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0) ,3,1998,-4,10,1,"10",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


# figure 11
for (i in seq(13,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-0.1,0.3,0.1,"11",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}




var_map <- rbind(cbind('birth_prenat_ig','Prenatal Visits - Ignored'),
                 cbind('birth_prenat_0','Prenatal Visits None'),
                 cbind('birth_prenat_1_6','Prenatal Visits 1-6'),
                 cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
                 cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
                 cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_low_skill_mun_pcapita','N. Outpatient Procedures by Low Skilled Workers (per capita)'),
                 cbind('sia_nprod_med_skill_mun_pcapita','N. Outpatient procedures by Mid Skilled Workers (per capita)')
                 
)



# figure 13 (ex 14)
for (i in seq(1,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.2,0.2,0.05,"14",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}


# figure 14 (ex 15)
for (i in seq(5,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-6,10,2,"13",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

for (i in seq(7,10,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-6,10,2,"13",below = below,weight = "peso_eq",year_cap = 2007) # ec29baseline
}




var_map <- rbind(cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)'),
                 cbind('tx_sih_adult','Adult Hospitalization Rate (pop 40+y * 1000)'),
                 cbind('tx_sih_adult_icsap','Adult Hospitalization Rate - APC (pop 40+y * 1000)'),
                 cbind('tx_sih_adult_nicsap','Adult Hospitalization Rate - non-APC (pop 40+y * 1000)')
)


for (i in seq(1,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-700,700,100,"15",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}

for (i in seq(5,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-50,50,10,"15",below = below,weight = "peso_m",year_cap = 2010) # ec29baseline
}


for (i in seq(6,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-100,100,10,"15",below = below,weight = "peso_ha",year_cap = 2010) # ec29baseline
}




var_map <-  rbind(cbind('tx_mi','Infant Mortality Rate'),
                  cbind('tx_mi_icsap','Infant Mortality Rate - APC'),
                  cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC'),
                  cbind('tx_mi_infec','Infant Mortality Rate - Infectious'),
                  cbind('tx_mi_resp','Infant Mortality Rate - Respiratory'),
                  cbind('tx_mi_perinat','Infant Mortality Rate - Perinatal'),
                  cbind('tx_mi_cong','Infant Mortality Rate - Congenital'),
                  cbind('tx_mi_ext','Infant Mortality Rate - External'),
                  cbind('tx_mi_nut','Infant Mortality Rate - Nutritional'),
                  cbind('tx_mi_out','Infant Mortality Rate - Other'),
                  cbind('tx_mi_illdef','Infant Mortality Rate - Ill-Defined'),
                  cbind('tx_mi_fet','Infant Mortality Rate - Fetal'),
                  cbind('tx_mi_24h','Infant Mortality Rate - Within 24h'),
                  cbind('tx_mi_27d','Infant Mortality Rate - 1 to 27 days'),
                  cbind('tx_mi_ano','Infant Mortality Rate - 27 days to 1 year'),
                  cbind('tx_mm',"Maternal Mortality Rate"))



for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-20,20,5,"16",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}


for (i in seq(12,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-20,20,5,"17",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}


for (i in seq(4,11,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-20,20,5,"18",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}

for (i in seq(12,12,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-20,20,5,"22",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}



var_map <- rbind(cbind('tx_ma5','Adult Mortality Rate (40+ y)'),
                 cbind('tx_ma5_icsap','Adult Mortality Rate (40+ y) - APC'),
                 cbind('tx_ma5_nicsap','Adult Mortality Rate (40+ y) - non-APC'))


for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-6,4,1,"22",below = below,weight = "peso_a3",year_cap = 2010) # ec29baseline
}





var_map <- rbind(cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
                 cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_premature','Premature Birth'),
                 cbind('birth_sexratio',"Sex Ratio at Birth"))



# Figure 18 (ex 19)
for (i in c(1,seq(4,6,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.2,0.2,0.05,"19",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}

for (i in seq(2,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.5,1.5,0.5,"19",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}






var_map <- rbind(cbind('tx_sih_in_hosp_total','Hospitalization Outflow rate (pop * 1000)'),
                 cbind('tx_sih_in_hosp_icsap','Hospitalization Outflow rate - APC (pop * 1000)'),
                 cbind('tx_sih_in_hosp_nicsap','Hospitalization Outflow rate - non-APC (pop * 1000)'),
                 cbind('tx_sih_out_hosp_total','Hospitalization Inflow rate (pop * 1000)'),
                 cbind('tx_sih_out_hosp_icsap','Hospitalization Inflow rate - APC (pop * 1000)'),
                 cbind('tx_sih_out_hosp_nicsap','Hospitalization Inflow rate - non-APC (pop * 1000)'),
                 cbind('cobertura_plano','Private Insurance Coverage')
                 
                 
)

for (i in seq(1,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-20,20,5,"21",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

for (i in 7){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.1,0.1,0.02,"21",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}






