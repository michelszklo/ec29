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
if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/ec29/"
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}

# ------------------------------------

rnames <- seq.int(1998,2010) %>%
  as.data.frame() %>% 
  slice(rep(1:n(), each = 2)) %>% 
  mutate(b = paste0("DistEC29 * ",.)) %>% 
  select(b) %>% 
  rbind("obs")

table_main <- cbind(rnames,data.frame(matrix(nrow = 27, ncol = 0)))
table_ab <- cbind(rnames,data.frame(matrix(nrow = 27, ncol = 0)))



# ------------------------------------
# Plotting functions
robustPlot <- function(var, combined_df) {
  graph <- ggplot(combined_df, aes(x=year, y=estimates, color = controls,linetype = controls)) + 
    geom_line(size=1.2) +
    geom_errorbar(aes(ymin = lb2, ymax = ub2), width=0.2, color = "gray") +
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
    theme_light() +
    scale_color_viridis_d() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank())
  ggsave(paste0(dir,robust_folder,var,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
       units = "in")
}

robustPlotAB <- function(var, combined_df) {
  graph <- ggplot(combined_df, aes(x=year, y=estimates, color = target,linetype = controls)) + 
    # geom_line(size=1.2) +
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_pointrange(aes(ymin = lb2, ymax = ub2), size=0.3, position = position_dodge(width=0.8),shape=15) +
    scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 9),
          panel.grid = element_blank()) +
    guides(color=guide_legend(nrow=2,byrow=TRUE),
           linetype=guide_legend(nrow=2,byrow=TRUE)           ) 
  ggsave(paste0(dir,robust_folder,var,"_ab.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
}
# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))


df2 <- df


robust_folder <- "regs_outputs/regs_plots_trend/robust/"




# 2. Spending
# =================================================================

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

# redefines folder
yearly_folder <- "regs_plots_trend/fiscal_response/"

var_map1 <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (log)'),
                  cbind('finbra_desp_o_pcapita','Total Spending per capita (log)'),
                  
                  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
                  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)'),
                  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (log)'),
                  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (log)'),
                  
                  cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
                  cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
                  cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)'),
                  cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (log)'),
                  cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (log)'),
                  cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (log)'),
                  cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (log)'))



# per capita level
var_map2 <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (2010 R$)'),
                  cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                  
                  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (2010 R$)'),
                  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (2010 R$)'),
                  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (2010 R$)'),
                  
                  cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                  cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                  cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (2010 R$)'),
                  cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (2010 R$)'),
                  cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                  cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (2010 R$)'),
                  cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (2010 R$)'))


# continuous

for (i in seq(1,13,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()
  
  res <- reduced_yearly_imr(var,var_name,df,1,1998,-1000,1000,10,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,1,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)  
  
  table_main <- table_main %>% cbind(table_final)
  
}

for (i in seq(1,3,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-1,2.5,0.25,paste0("1_cont_log_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}

for (i in seq(3,6,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-1,2.5,0.25,paste0("1_cont_log_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}

for (i in seq(3,3,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-3,9.25,1,paste0("1_cont_log2_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}

for (i in seq(7,13,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-3,9.25,1,paste0("1_cont_log_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_main <- table_main %>% bind_cols(table_final)
}


# continuous above and below
for (i in seq(1,13,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()
  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,1,1998,-1000,1000,10,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}

for (i in seq(1,3,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,1,1998,-2.5,2.5,0.5,paste0("2_ab_log_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}


for (i in seq(3,6,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,1,1998,-2.5,3.5,0.5,paste0("2_ab_log_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}

for (i in seq(3,3,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,1,1998,-10,14,2,paste0("2_ab_log2_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}

for (i in seq(7,13,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,1,1998,-10,14,2,paste0("2_ab_log_",i),weight = "peso_pop",year_cap = 2010,cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}



# binary

for (i in seq(1,2,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-0.1,0.25,0.025,paste0("3_binary_log_",i),weight = "peso_pop",year_cap = 2010,cont = 0) # ec29baseline
  
}


for (i in seq(3,6,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-0.1,0.25,0.025,paste0("3_binary_log_",i),weight = "peso_pop",year_cap = 2010,cont = 0) # ec29baseline
}

for (i in seq(3,3,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-0.3,1.3,0.1,paste0("3_binary_log2_",i),weight = "peso_pop",year_cap = 2010,cont = 0) # ec29baseline
}

for (i in seq(7,13,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-0.3,1.3,0.1,paste0("3_binary_log_",i),weight = "peso_pop",year_cap = 2010,cont = 0) # ec29baseline
}




# 3. Access and Production
# =================================================================

df <- df2
# creating missing SIA variable
df <- df %>% 
  mutate(sia_nab_pcapita = sia_pcapita - sia_ab_pcapita)
yearly_folder <- "regs_plots_trend/access_production/"

var_map <- rbind(cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
                 cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
                 cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
                 cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
                 cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
                 cbind('siab_visit_cons_pcapita','N. of Household Visits and Appointments (per capita)'),
                 cbind('siab_visit_cons_pacs_pcapita','N. of Household Visits and Appointments from Community Health Agents (per capita)'),
                 cbind('siab_visit_cons_psf_pcapita','N. of Household Visits and Appointments from Family Health Agents (per capita)'),
                 
                 cbind('sia_ncnes_amb_mun_pcapita','N. of Health Facilities with Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_acs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Teams (per capita*1000)'),
                 cbind('sia_ncnes_psf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Teams (per capita*1000)'),
                 cbind('sia_ncnes_medcom_pcapita','N. of Health Facilities with Ambulatory Service and Community Doctors (per capita*1000)'),
                 cbind('sia_ncnes_medpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Doctors (per capita*1000)'),
                 cbind('sia_ncnes_enfacs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Nurses (per capita*1000)'),
                 cbind('sia_ncnes_enfpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nurses (per capita*1000)'),
                 cbind('sia_ncnes_outpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nursing Assistants (per capita*1000)'),
                 
                 cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
                 cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
                 cbind('sia_nab_pcapita','N. Non-Primary Care Outpatient Procedures (per capita)'), # precisa criar
                 cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
                 
                 cbind('birth_prenat_ig','Proportion of births with unknown prenatal care coverage'),
                 cbind('birth_prenat_0','Proportion of births with 0 prenatal visits'),
                 cbind('birth_prenat_1_6','Proportion of births with 1-6 prenatal visits'),
                 cbind('birth_prenat_7_plus','Proportion of births with 7+ prenatal visits')
                 
                 
)

# continuous
#reduced_yearly_imr(var,var_name,df,3,1998,-0.5,0.75,0.1,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
for (i in seq(1,25,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  res <- reduced_yearly_imr(var,var_name,df,3,1998,-0.5,0.75,0.1,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,3,1998,-25,15,5,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)  
  
  table_main <- table_main %>% cbind(table_final)
}

for (i in seq(3,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1,1.5,0.5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}


for (i in seq(6,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1.5,3,0.5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}


for (i in seq(9,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-0.5,0.5,0.1,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2007, cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}


for (i in seq(17,21,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-7,14,1,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}


for (i in seq(22,25,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-0.3,0.3,0.1,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
}



# continuous above and below 2_ab_log_
for (i in seq(1,25,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,1,1998,-1000,1000,10,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}

for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-1,1.4,0.2,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}

for (i in seq(3,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-1.75,1.5,0.25,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}


for (i in seq(6,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-4.5,4.5,0.5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}


for (i in seq(9,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-0.8,0.4,0.2,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2007, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}


for (i in seq(17,21,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-20,30,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}


for (i in seq(22,25,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-0.5,0.5,0.1,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}





# binary

for (i in seq(1,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.075,0.15,0.025,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}

for (i in seq(3,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.1,0.2,0.05,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


for (i in seq(6,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.15,0.45,0.05,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


for (i in seq(9,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.05,0.05,0.01,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2007, cont = 0) # ec29baseline
}


for (i in seq(17,21,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-1.25,2.25,0.5,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


for (i in seq(22,25,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.06,0.06,0.01,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}





# 4. Inputs
# =================================================================

yearly_folder <- "regs_plots_trend/inputs/"


var_map <- rbind(cbind('ams_hospital_mun_pcapita','N. of Municipal Hospitals (per capita*1000)'),
                 cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals (per capita*1000)'),
                 cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                 
                 cbind('ams_hr_all_pcapita',"N. of Health Professionals (per capita*1000)"),
                 cbind('ams_hr_superior_pcapita','N. of Doctors (per capita*1000)'),
                 cbind('ams_hr_technician_pcapita','N. of Nurses (per capita*1000)'),
                 cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants (per capita*1000)'),
                 cbind('ams_hr_admin_pcapita','N. of Administrative Professionals (per capita*1000)')
                 
                 # cbind('ams_hospital_mun_esp_pcapita', 'N. of Specialty Hospitals (per capita*1000)'),
                 # cbind('ams_unity_mun_pcapita','N. of Health Facilities (per capita*1000)'),
                 # cbind('ams_therapy_mun_pcapita','N. of Therapy Units (per capita*1000)')
)


# continuous
#reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-0.04,0.1,0.02,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline

for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  output_list <- list()
  
  res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                            3,1998,-15,30,5,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                              3,1998,-15,30,5,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlot(var,df_nona)  
  
  table_main <- table_main %>% cbind(table_final)
}


for (i in 4){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-15,30,5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
  
}

for (i in seq(5,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-10,15,5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main <- table_main %>% cbind(table_final)
  
}



# continuous above and below
for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,
                                 df %>% mutate(above_pre_99_dist_ec29_baseline=0,below_pre_99_dist_ec29_baseline=0),
                                 1,1998,-1000,1000,10,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}


for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df %>% mutate(above_pre_99_dist_ec29_baseline=0,below_pre_99_dist_ec29_baseline=0),3,1998,-0.1,0.125,0.025,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}


for (i in 4){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df %>% mutate(above_pre_99_dist_ec29_baseline=0,below_pre_99_dist_ec29_baseline=0),3,1998,-40,80,10,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}

for (i in seq(5,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df %>% mutate(above_pre_99_dist_ec29_baseline=0,below_pre_99_dist_ec29_baseline=0),3,1998,-15,35,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}



# Binary

for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline_binary=0),3,1998,-0.006,0.01,0.002,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


for (i in 4){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline_binary=0),3,1998,-5,5,1,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}

for (i in seq(4,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline_binary=0),3,1998,-3,3,1,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


# 5. Hospitalization
# =================================================================

yearly_folder <- "regs_plots_trend/hosp/"

var_map <- rbind(cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)'),
                 
                 cbind('sih_infant','Infant Hospitalization - Total (log)'),
                 cbind('sih_infant_icsap','Infant Hospitalization - APC (log)'),
                 cbind('sih_infant_nicsap','Infant Hospitalization - non-APC (log)'),
                 cbind('sih_maternal','Maternal Hospitalization - Total (log)')
)

for (i in seq(1,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlot(var,combined_df)  
}

# continuous
for (i in seq(1,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-600,1000,200,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
  
}

for (i in seq(5,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-50,50,10,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont =1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}


# continuous above and below
# continuous above and below 2_ab_log_
for (i in seq(1,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,1,1998,-10000,10000,1000,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}

for (i in seq(1,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-1500,3500,500,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}

for (i in seq(5,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-50,50,10,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont =1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}



# binary

for (i in seq(1,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-60,100,20,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}

for (i in seq(5,5,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-5,5,1,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}



for (i in seq(6,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,1,1998,-1,1,0.2,paste0("4_cont_log_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
}


# 6. Fertility and Birth
# =================================================================

yearly_folder <- "regs_plots_trend/birth/"


var_map <- rbind(cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
                 cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_premature','Premature Birth'),
                 cbind('birth_sexratio',"Sex Ratio at Birth"))


# continuous

for (i in c(1,seq(4,6,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-0.1,0.15,0.05,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}

for (i in seq(2,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1,1.5,0.5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}


# continuous above and below

for (i in c(1,seq(4,6,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-0.2,0.25,0.05,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}

for (i in seq(2,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-1.5,3,0.5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}

# binary

for (i in c(1,seq(4,6,1))){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.02,0.015,0.005,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}

for (i in seq(2,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.1,0.25,0.05,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


# 7. IMR
# =================================================================

yearly_folder <- "regs_plots_trend/imr/"

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
# continuous

for (i in seq(1,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  res <- reduced_yearly_imr(var,var_name,df,3,1998,-25,15,5,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,3,1998,-25,15,5,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)  
  
  table_main<- table_main %>% cbind(table_final)
}

for (i in seq(4,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-15,10,5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}

for (i in seq(16,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-15,10,5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}

# continuous above and below
for (i in seq(1,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,-30,30,5,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlotAB(var,df_nona)
}


for (i in seq(4,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-20,20,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}

for (i in seq(16,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-15,15,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}




# binary

for (i in seq(1,3,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-2,2,0.5,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


for (i in seq(4,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-2,2,0.5,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}

# DC Comment out Jan 22 -- Matrix indefinite...
#for (i in seq(16,16,1)){
#  var <- var_map[i,1]
#  var_name <- var_map[i,2]
#  print(var_name)
#  reduced_yearly_imr(var,var_name,df,3,1998,-2,2,0.5,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
#}



# 8. Indexes
# =================================================================
if(Sys.getenv("USERNAME")=="dcc213") {
  index <- data.frame(read.dta13("/home/dcc213/investigacion/2021/decentralization/github/ec29/indexes.dta"))
} else {
  index <- data.frame(read.dta13("C:/Users/mszklo/Documents/GitHub/ec29/indexes.dta"))
}
# merge indexes to main df
all_df <- c("df")

imerge <- function(df){
  df <- df %>% 
    left_join(index, by = c("ano","cod_mun","cod_uf"))
}

for(d in all_df){
  df_merge <- get(d)
  df_merge <- df_merge %>% imerge()
  assign(d,df_merge,envir = .GlobalEnv)
}

yearly_folder <- "regs_plots_trend/indexes/"


var_map <-  rbind(cbind('access_index','Access and Production of Health Services Index','peso_pop'),
                  cbind('access_pc_index','Primary Care Access and Production Index','peso_pop'),
                  cbind('access_npc_index','Non-Primary Care Access and Production Index','peso_pop'),
                  cbind('input_index','Health Inputs Index','peso_pop'),
                  cbind('hr_index','Human Resources Index','peso_pop'),
                  cbind('hospital_index','Hospitals Index','peso_pop'),
                  cbind('birth_index','Birth Outcomes Index','peso_pop'),
                  cbind('imr_index','Infant Mortality Index','peso_pop'),
                  cbind('birth_others_index','Other Birth Outcomes Index','peso_pop')
)


# continous

for (i in seq(1,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  if(i<4|i>6) {
    res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3)
  } else {
    res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                              3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3)
  }
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    if(i<4|i>6) {
      res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    } else {
      res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                                3,1998,-1000,1000,10,
                                paste0("1_cont_level_",i),weight = "peso_pop",
                                year_cap = 2010, cont = 1, spec=control)
    }
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlot(var,df_nona)  
  
  table_main<- table_main %>% cbind(table_final)
}

for (i in c(seq(1,3,1),7,9)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1,1.75,0.25,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}
for (i in seq(4,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-1,2.5,0.25,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}


for (i in 8){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1,1.75,0.25,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}


# continous above and below
# continuous above and below 2_ab_log_
for (i in seq(1,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    if(i<4|i>6) {
      res <- reduced_yearly_ab_imr(var,var_name,df,1,1998,-1000,1000,10,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    } else {
      res <- reduced_yearly_ab_imr(var,var_name,
                                   df %>% mutate(above_pre_99_dist_ec29_baseline=0, below_pre_99_dist_ec29_baseline=0),
                                   1,1998,-1000,1000,10,
                                   paste0("2_ab_level_",i),weight = "peso_pop",
                                   year_cap = 2010, cont = 1, spec=control)
    }
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}

for (i in c(seq(1,3,1),7,9)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-2,2.5,0.5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}

for (i in seq(4,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df %>% mutate(above_pre_99_dist_ec29_baseline=0, below_pre_99_dist_ec29_baseline=0),3,1998,-4,6,1,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}


for (i in 8){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-1,1,0.25,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}




# binary

for (i in c(seq(1,3,1),7,9)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.15,0.25,0.025,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}

for (i in seq(4,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline_binary=0),3,1998,-0.1,0.25,0.025,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}


for (i in 8){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-0.1,0.175,0.025,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
}




# 9. System
# =================================================================

  yearly_folder <- "regs_plots_trend/system/"
  
  var_map <- rbind(cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                   cbind('cobertura_plano','Private Insurance Coverage'),
                   cbind('tx_sih_in_hosp_total','Hospitalization Inflow rate (pop * 1000)'),
                   cbind('tx_sih_in_hosp_icsap','Hospitalization Inflow rate - APC (pop * 1000)'),
                   cbind('tx_sih_in_hosp_nicsap','Hospitalization Inflow rate - non-APC (pop * 1000)'),
                   cbind('tx_sih_out_hosp_total','Hospitalization Outflow rate (pop * 1000)'),
                   cbind('tx_sih_out_hosp_icsap','Hospitalization Outflow rate - APC (pop * 1000)'),
                   cbind('tx_sih_out_hosp_nicsap','Hospitalization Outflow rate - non-APC (pop * 1000)'))
  
  # continuous
  
  for (i in seq(1,8,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    output_list <- list()
  
    if (i==1) {
      res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                                3,1998,-1000,1000,10,
                                paste0("1_cont_level_",i),weight = "reweightPop",
                                year_cap = 2010, cont = 1, spec=3)
      
    } else { 
      res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                                paste0("1_cont_level_",i),weight = "reweightPop",
                                year_cap = 2010, cont = 1, spec=3)
    }
    print(res)
    res$con <-5
    output_list[[1]] <- res
    iter <- 2
    for (control in c(1,2,4,3)) {
      print(control)
      if (i==1) {
        res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-1000,1000,10,
                                  paste0("1_cont_level_",i),weight = "peso_pop",
                                  year_cap = 2010, cont = 1, spec=control)
        } else {
          res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                                    paste0("1_cont_level_",i),weight = "peso_pop",
                                    year_cap = 2010, cont = 1, spec=control)
        }    
      print(res)
      res$con <-control 
      output_list[[iter]] <- res
      iter <- iter + 1
    }
    combined_df <- do.call(rbind, output_list)
    combined_df <- combined_df %>%
      mutate(controls = case_when(
        con == 1 ~ "(1) Baseline",
        con == 2 ~ "(2) + Municipal char.",
        con == 3 ~ "(3) + Economic",
        con == 4 ~ "(4) + Spending",
        con == 5 ~ "(5) Reweight",
      ))
    
    combined_df$con2<-as.character(combined_df$con)
    combined_df$year<- combined_df$year+combined_df$con/10
    ## Robustness plot
    df_nona <- combined_df[!is.na(combined_df$estimates),]
    robustPlot(var,df_nona)  
    
    table_main<- table_main %>% cbind(table_final)
  }
  
  
  for (i in seq(1,1,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-0.06,0.06,0.02,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
    
    table_main<- table_main %>% cbind(table_final)
  }
  
  for (i in seq(2,2,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly_imr(var,var_name,df,3,1998,-0.15,0.15,0.05,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
    
    table_main<- table_main %>% cbind(table_final)
    
  }
  
  
  for (i in seq(3,8,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    reduced_yearly_imr(var,var_name,df,3,1998,-10,20,2,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
    
    table_main<- table_main %>% cbind(table_final)
    
  }
  
  
  

# continuous above and below
  # continuous above and below 2_ab_log_
  for (i in seq(1,8,1)){
    var <- var_map[i,1]
    var_name <- var_map[i,2]
    print(var_name)
    output_list <- list()
    iter <- 1
    for (control in c(1,2,4,3)) {
      print(control)
      if (i==1) {
        res <- reduced_yearly_ab_imr(var,var_name,
                                     df %>% mutate(pre_99_dist_ec29_baseline=0),
                                     1,1998,-1000,1000,10,paste0("2_ab_level_",i),
                                     weight = "peso_pop",year_cap = 2010, 
                                     cont = 1, spec=control)
      } else {
        res <- reduced_yearly_ab_imr(var,var_name,df,1,1998,-1000,1000,10,
                                     paste0("2_ab_level_",i),weight = "peso_pop",
                                     year_cap = 2010, cont = 1, spec=control)
      }
      print(res)
      res$con <-control 
      output_list[[iter]] <- res
      iter <- iter + 1
    }
    combined_df <- do.call(rbind, output_list)
    combined_df <- combined_df %>%
      mutate(controls = case_when(
        con == 1 ~ "(1) Baseline",
        con == 2 ~ "(2) + Municipal char.",
        con == 3 ~ "(3) + Economic",
        con == 4 ~ "(4) + Spending",
        con == 5 ~ "(5) Reweight",
      ))
    table_ab<- table_ab %>% cbind(table_final)
    combined_df$con2<-as.character(combined_df$con)
    combined_df$year<- combined_df$year+combined_df$con/10
    ## Robustness plot
    df_nona <- combined_df[!is.na(combined_df$estimates),]
    robustPlotAB(var,df_nona)
  }
  
for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-0.04,0.1,0.02,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
  
}

for (i in seq(2,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-0.30,0.20,0.05,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}


for (i in seq(3,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-40,30,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab<- table_ab %>% cbind(table_final)
}




# binary
for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),3,1998,-0.006,0.006,0.002,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
  
}

for (i in seq(2,2,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-0.04,0.04,0.01,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
  
  
}


for (i in seq(3,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-4,6,1,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
  

}



# 10. Adult
# =================================================================

yearly_folder <- "regs_plots_trend/adult/"

var_map <- rbind(cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_adult','Adult Hospitalization Rate (pop 40+y * 1000)'),
                 cbind('tx_sih_adult_icsap','Adult Hospitalization Rate - APC (pop 40+y * 1000)'),
                 cbind('tx_sih_adult_nicsap','Adult Hospitalization Rate - non-APC (pop 40+y * 1000)'),
                 cbind('tx_mm',"Maternal Mortality Rate"),
                 cbind('tx_ma5','Adult Mortality Rate (40+ y)'),
                 cbind('tx_ma5_icsap','Adult Mortality Rate (40+ y) - APC'),
                 cbind('tx_ma5_nicsap','Adult Mortality Rate (40+ y) - non-APC'))

# continuous
for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlot(var,df_nona)  
  
  table_main<- table_main %>% cbind(table_final)
}


for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,200,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}

for (i in seq(2,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-100,100,20,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}


for (i in seq(5,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-8,4,2,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_main<- table_main %>% cbind(table_final)
}




# continuous above and below
for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,1,1998,-1000,1000,10,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}

for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-2500,3500,500,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab <- table_ab %>% cbind(table_final)
}

for (i in seq(2,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-300,300,50,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab <- table_ab %>% cbind(table_final)
}


for (i in seq(5,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr(var,var_name,df,3,1998,-20,20,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  
  table_ab <- table_ab %>% cbind(table_final)
}



# binary

for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-200,200,50,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline

}

for (i in seq(2,4,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-40,40,10,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline

}


for (i in seq(5,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-1.4,1,0.2,paste0("3_binary_level_",i),weight = "peso_pop",year_cap = 2010, cont = 0) # ec29baseline
  
}



# 11. Other
# =================================================================

yearly_folder <- "regs_plots_trend/robust_other/"

var_map <- rbind(cbind('gdp_mun_pcapita','GDP per capita'))



for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-50,50,10,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1, spec = 2) # ec29baseline
  
  # table_main<- table_main %>% cbind(table_final)
}






# 12. Tables output
# =================================================================

output_file <- "regression_tables_raw.xlsx"

write.xlsx2(table_main, file = paste0(dir,main_folder,output_file),sheetName = "event_study",row.names = F,append = T)
write.xlsx2(table_ab, file = paste0(dir,main_folder,output_file),sheetName = "event_study_ab",row.names = F,append = T)


