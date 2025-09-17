#######################################################################################################
# Author: Michel Szklo
# May 2025
# 
# This scripts runs regressions for public spending
#
#
#######################################################################################################

# 0. Set-up
# =================================================================


rm(list=ls())

#Set-up path for principal directory
if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/"
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}
SRC <- paste0(dir,"source/")
DAT <- paste0(dir,"data/processed/")
TAB <- paste0(dir,"results/tables/")
FIG <- paste0(dir,"results/figures/")


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
            'plm')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)
options(digits = 15)




YEAR <- 1996

# 1. Ploting Function
# =================================================================
#---    Accepts: variable name (var)
#---             dataframe with estimates, LB, UB, etc. (combined_df)
#--------------------------------------------------------------------------------


# Plotting functions
robustPlot <- function(var, combined_df, folder) {
  graph <- ggplot(combined_df, aes(x=year, y=estimates, color = controls, linetype = controls)) + 
    geom_line(size=1.2) +
    geom_errorbar(aes(ymin = lb2, ymax = ub2), width=0.2, color = "gray") +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", linewidth = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(YEAR,2020,1), limits = c(1995.5,2010+0.5)) +
    theme_light() +
    scale_color_viridis_d() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x = element_text(size=11),
          axis.text = element_text(size = 11),
          legend.position="bottom",
          legend.title = element_blank())
  ggsave(paste0(FIG,folder,"/",var,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
}

robustPlotAB <- function(var, combined_df,folder) {
  graph <- ggplot(combined_df, aes(x=year, y=estimates, color = target,linetype = controls)) + 
    geom_point(shape=15) +
    geom_errorbar(aes(ymin = lb2, ymax = ub2, color = target,linetype = controls), width=0.2) +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", linewidth = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(YEAR,2010,1), limits = c(1995.5,2010+0.5)) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x = element_text(size=11),
          axis.text = element_text(size = 11),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 9)) +
    guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
  ggsave(paste0(FIG,folder,"/",var,"_ab.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
}

# 
# # 2. Loads data, remove outliers, balances samples
# # =================================================================
# 
# 
load(paste0(DAT,"regs.RData"))
# 
# 
# if(YEAR==1998){
#   df <- df %>% 
#     filter(ano>=1998)
# }
# 
# 
# 
# df2 <- df
# 
# 
# # As a first method of examining data quality issues and to remove sample observations with abnor
# # mal values we consider the relative variation of IMR over time. We first identified municipalities
# # with abnormally high variations in IMR across years by calculating the standard deviation of IMR
# # within municipalities. While we would expect to observe substantial variation in IMR both across
# # municipalities and over time, we should not observe abnormal variation in IMR from year to year
# # for a given municipality. We thus estimated the withinmunicipality IMR standard deviation (SD)
# # for the whole sample, and flagged the municipalities above the 95th percentile of the SD distribu
# # tion
# 
# 
# # removing spending outliers from the sample
# # ----------------------------------------------
# # calculating SD withing municipalities, across years
# 
# out <- df %>% 
#   group_by(cod_mun) %>% 
#   summarise(std_dev = sd(finbra_desp_o_pcapita, na.rm = TRUE))
# 
# # setting the 95 percentile threshold
# threshold <- quantile(out$std_dev, 0.95, na.rm = TRUE)
# 
# # filtering cod_mun of outliers
# out <- out %>% 
#   mutate(outlier = ifelse(std_dev>threshold,1,0)) %>% 
#   filter(outlier==1) %>% 
#   select(cod_mun) %>% 
#   pull()
# 
# length(out)
# 
# # removing spending outliers from the sample
# # ----------------------------------------------
# df <- df %>% 
#   filter(!(cod_mun %in% out))
# 
# 
# 
# # removing population outliers from the sample
# # ----------------------------------------------
# # calculating SD withing municipalities, across years
# 
# out <- df %>% 
#   group_by(cod_mun) %>% 
#   summarise(std_dev = sd(pop, na.rm = TRUE))
# 
# # setting the 95 percentile threshold
# threshold <- quantile(out$std_dev, 0.95, na.rm = TRUE)
# 
# # filtering cod_mun of outliers
# out <- out %>% 
#   mutate(outlier = ifelse(std_dev>threshold,1,0)) %>% 
#   filter(outlier==1) %>% 
#   select(cod_mun) %>% 
#   pull()
# 
# length(out)
# 
# # removing outliers from the sample
# df <- df %>% 
#   filter(!(cod_mun %in% out))
# 
# 
# # balancing finbra sample within year
# df_balance_finbra <- df[complete.cases(df[c('finbra_recorc_pcapita',
#                                             'finbra_desp_o_pcapita',
#                                             'finbra_desp_saude_san_pcapita',
#                                             'finbra_desp_nao_saude_pcapita',
#                                             'finbra_despsocial_pcapita',
#                                             'finbra_desp_outros_area_pcapita',
#                                             'gdp_mun_pcapita',
#                                             'pbf_pcapita',
#                                             't_tx_mi_baseline',
#                                             'dist_ec29_baseline')]) & 
#                           df$finbra_recorc_pcapita != 0 & 
#                           df$finbra_desp_o_pcapita != 0 & 
#                           df$finbra_desp_saude_san_pcapita != 0 & 
#                           df$finbra_desp_nao_saude_pcapita != 0 & 
#                           df$finbra_despsocial_pcapita != 0 & 
#                           df$finbra_desp_outros_area_pcapita != 0, ]
# 


# 3. Runs regressions
# =================================================================


# a) Variables
# -----------------------------------------------------------------

var_map1 <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (log)'),
                  cbind('finbra_desp_o_pcapita','Total Spending per capita (log)'),
                  
                  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
                  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)'),
                  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (log)'),
                  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (log)'),
                  
                  
                  # cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
                  # cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
                  # cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)'),
                  # cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (log)'),
                  # cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (log)'),
                  # cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (log)'),
                  # cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (log)'),
                  
                  cbind('finbra_impostos_total_pcapita', 'Total Tax Revenue (log)'),
                  cbind('finbra_iptu_pcapita', 'Property Tax Revenue (log)'),
                  cbind('finbra_iss_pcapita', 'Services Tax Revenue (log)'),
                  
                  cbind('finbra_passivo_pcapita','Total Liabilities (log)'),
                  cbind('finbra_passivo_fin_pcapita','Financial Liabilities (log)')
)



# per capita level
var_map2 <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (2010 R$)'),
                  cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                  
                  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (2010 R$)'),
                  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (2010 R$)'),
                  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (2010 R$)'),
                  
                  
                  # cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                  # cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                  # cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (2010 R$)'),
                  # cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (2010 R$)'),
                  # cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                  # cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (2010 R$)'),
                  # cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (2010 R$)'),
                  
                  cbind('finbra_impostos_total_pcapita', 'Total Tax Revenue (2010 R$)'),
                  cbind('finbra_iptu_pcapita', 'Property Tax Revenue (2010 R$)'),
                  cbind('finbra_iss_pcapita', 'Services Tax Revenue (2010 R$)'),
                  
                  cbind('finbra_passivo_pcapita','Total Liabilities (2010 R$)'),
                  cbind('finbra_passivo_fin_pcapita','Financial Liabilities (2010 R$)')
                  
)


if(YEAR==1998){
  
  # a) sample without outliers
  # -----------------------------------------------------------------
  
  
  yearly_folder <- "fiscal_response/"
  
  # continuous
  
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    
    res <- reduced_yearly_imr(var,var_name,df %>%
                                filter(sample_rm_outlier_spending_98==1) %>% 
                                filter(sample_rm_outlier_pop_98==1),1,1998,-2,2,0.25,
                              paste0("1_cont_log_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3)
    print(res)
    res$con <-5
    output_list[[1]] <- res
    iter <- 2
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_imr(var,var_name,df %>%
                                  filter(sample_rm_outlier_spending_98==1) %>% 
                                  filter(sample_rm_outlier_pop_98==1),1,1998,-2,2,0.25,
                                paste0("1_cont_log_",i),weight = "peso_pop",
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
    robustPlot(var,combined_df,"robust_fiscal_response")  
  }
  
  # above and below
  
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    iter <- 1
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_ab_imr(var,var_name,df %>%
                                     filter(sample_rm_outlier_spending_98==1) %>% 
                                     filter(sample_rm_outlier_pop_98==1),1,1998,-4,4,0.5,
                                   paste0("2_ab_log_",i),weight = "peso_pop",
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
    ##table_ab<- table_ab %>% cbind(table_final)
    combined_df$con2<-as.character(combined_df$con)
    combined_df$year<- combined_df$year+combined_df$con/10-0.4
    combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
    ## Robustness plot
    df_nona <- combined_df[!is.na(combined_df$estimates),]
    robustPlotAB(var,df_nona,"robust_fiscal_response")
  }
  
  
  
  # b) finbra balanced
  # -----------------------------------------------------------------
  
  yearly_folder <- "fiscal_response_bal/"
  
  # continuous
  
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    
    res <- reduced_yearly_imr(var,var_name,df %>%
                                filter(sample_rm_outlier_spending_98==1) %>%
                                filter(sample_rm_outlier_pop_98==1) %>% 
                                filter(sample_balance_finbra==1),1,1998,-1.5,2.5,0.25,
                              paste0("1_cont_log_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3)
    print(res)
    res$con <-5
    output_list[[1]] <- res
    iter <- 2
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_imr(var,var_name,df %>%
                                  filter(sample_rm_outlier_spending_98==1) %>%
                                  filter(sample_rm_outlier_pop_98==1) %>% 
                                  filter(sample_balance_finbra==1),1,1998,-2,2,0.25,
                                paste0("1_cont_log_",i),weight = "peso_pop",
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
    robustPlot(var,combined_df,"robust_fiscal_response_bal")  
  }
  
  
  # above and below
  
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    iter <- 1
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_ab_imr(var,var_name,df %>%
                                     filter(sample_rm_outlier_spending_98==1) %>%
                                     filter(sample_rm_outlier_pop_98==1) %>% 
                                     filter(sample_balance_finbra==1),1,1998,-4,4,0.5,
                                   paste0("2_ab_log_",i),weight = "peso_pop",
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
    ##table_ab<- table_ab %>% cbind(table_final)
    combined_df$con2<-as.character(combined_df$con)
    combined_df$year<- combined_df$year+combined_df$con/10-0.4
    combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
    ## Robustness plot
    df_nona <- combined_df[!is.na(combined_df$estimates),]
    robustPlotAB(var,df_nona,"robust_fiscal_response") 
  }
  
  
  
  
  
  
} else {
  
  # a) sample without outliers
  # -----------------------------------------------------------------
  
  yearly_folder <- "fiscal_response_96/"
  
  
  # continuous
  
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    
    res <- reduced_yearly_imr_ext(var,var_name,df %>%
                                    filter(sample_rm_outlier_spending_96==1) %>% 
                                    filter(sample_rm_outlier_pop_96==1),1,1996,-2,2,0.25,
                                  paste0("1_cont_log_",i),weight = "reweightPop",
                                  year_cap = 2010, cont = 1, spec=3)
    print(res)
    res$con <-5
    output_list[[1]] <- res
    iter <- 2
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_imr_ext(var,var_name,df %>%
                                      filter(sample_rm_outlier_spending_96==1) %>% 
                                      filter(sample_rm_outlier_pop_96==1),1,1996,-2,2,0.25,
                                    paste0("1_cont_log_",i),weight = "peso_pop",
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
    robustPlot(var,combined_df,"robust_fiscal_response_96")  
  }
  
  
  # above and below
  
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    iter <- 1
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_ab_imr_ext(var,var_name,df %>%
                                         filter(sample_rm_outlier_spending_96==1) %>% 
                                         filter(sample_rm_outlier_pop_96==1),1,1996,-4,4,0.5,
                                   paste0("2_ab_log_",i),weight = "peso_pop",
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
    ##table_ab<- table_ab %>% cbind(table_final)
    combined_df$con2<-as.character(combined_df$con)
    combined_df$year<- combined_df$year+combined_df$con/10-0.4
    combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
    ## Robustness plot
    df_nona <- combined_df[!is.na(combined_df$estimates),]
    robustPlotAB(var,df_nona,"robust_fiscal_response_96")
  }
  
  
  
  
  
  # b) finbra balanced
  # -----------------------------------------------------------------
  
  yearly_folder <- "fiscal_response_96_bal/"
  
  # continous
  
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    
    res <- reduced_yearly_imr_ext(var,var_name,df %>%
                                    filter(sample_rm_outlier_spending_96==1) %>% 
                                    filter(sample_rm_outlier_pop_96==1) %>% 
                                    filter(sample_balance_finbra==1),1,1996,-1.5,2.5,0.25,
                                  paste0("1_cont_log_",i),weight = "reweightPop",
                                  year_cap = 2010, cont = 1, spec=3)
    print(res)
    res$con <-5
    output_list[[1]] <- res
    iter <- 2
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_imr_ext(var,var_name,df %>%
                                      filter(sample_rm_outlier_spending_96==1) %>% 
                                      filter(sample_rm_outlier_pop_96==1) %>% 
                                      filter(sample_balance_finbra==1),1,1996,-2,2,0.25,
                                    paste0("1_cont_log_",i),weight = "peso_pop",
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
    robustPlot(var,combined_df,"robust_fiscal_response_96_bal")  
  }
  
  # above and below
  for (i in seq(1,11,1)){
    var <- var_map1[i,1]
    var_name <- var_map1[i,2]
    print(var_name)
    output_list <- list()
    iter <- 1
    for (control in c(1,2,4,3)) {
      print(control)
      res <- reduced_yearly_ab_imr_ext(var,var_name,df %>%
                                         filter(sample_rm_outlier_spending_96==1) %>% 
                                         filter(sample_rm_outlier_pop_96==1) %>% 
                                         filter(sample_balance_finbra==1),1,1996,-4,4,0.5,
                                       paste0("2_ab_log_",i),weight = "peso_pop",
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
    ##table_ab<- table_ab %>% cbind(table_final)
    combined_df$con2<-as.character(combined_df$con)
    combined_df$year<- combined_df$year+combined_df$con/10-0.4
    combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
    ## Robustness plot
    df_nona <- combined_df[!is.na(combined_df$estimates),]
    robustPlotAB(var,df_nona,"robust_fiscal_response_96")
  }
  
  
  
}














