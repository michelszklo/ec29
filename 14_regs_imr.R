#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public Infant Mortality rates
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


# 2. Define outcomes output name and output functions
# =================================================================

var_map <-  rbind(cbind('tx_mi','Infant Mortality Rate (log)'),
                  cbind('tx_mi_icsap','Infant Mortality Rate - APC (log)'),
                  cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC (log)'),
                  cbind('tx_mi_infec','Infant Mortality Rate - Infectious (log)'),
                  cbind('tx_mi_resp','Infant Mortality Rate - Respiratory (log)'),
                  cbind('tx_mi_perinat','Infant Mortality Rate - Perinatal (log)'),
                  cbind('tx_mi_cong','Infant Mortality Rate - Congenital (log)'),
                  cbind('tx_mi_ext','Infant Mortality Rate - External (log)'),
                  cbind('tx_mi_nut','Infant Mortality Rate - Nutritional (log)'),
                  cbind('tx_mi_out','Infant Mortality Rate - Other (log)'),
                  cbind('tx_mi_illdef','Infant Mortality Rate - Ill-Defined (log)'),
                  cbind('tx_mi_fet','Infant Mortality Rate - Fetal (log)'),
                  cbind('tx_mi_24h','Infant Mortality Rate - Within 24h (log)'),
                  cbind('tx_mi_27d','Infant Mortality Rate - 1 to 27 days (log)'),
                  cbind('tx_mi_ano','Infant Mortality Rate - 27 days to 1 year (log)'))



table_formating <- function(df){
  df <- df %>% 
    filter(spec==3) %>%
    select(-spec) %>% 
    mutate(term=var_name) %>% 
    mutate(sig = ifelse(p.value<=0.01,"***",""),
           sig = ifelse(p.value<=0.05 & p.value>0.01,"**",sig),
           sig = ifelse(p.value<=0.1 & p.value>0.05,"*",sig)) %>% 
    mutate(std.error = paste0("(",round(std.error,digits = 3),")"),
           estimate = paste0(round(estimate,digits = 3),sig))
  
  df <- bind_rows(df %>%
                    select(term,estimate) %>%
                    rename(`2SLS` = estimate),
                  df %>% 
                    select(term,std.error) %>% 
                    rename(`2SLS` = std.error))
}  # formats regression outputs into article format

graph_formatting <- function(df){
  df <- df %>% 
    mutate(lb = estimate - 1.96*std.error,
           ub = estimate + 1.96*std.error,
           term = var_name) %>% 
    select(term,estimate,lb,ub,spec)
} # formats regression outputs into dataframe ready for graph

regress_output <- function(var,var_name,transform,year_filter){
  
  # IV REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_above","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    iv(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for IV regression and bootstrap estimating of SE
    
    print(paste0("IV regs for sample ",data))
  } 
  
  # 2sls final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("2SLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("2SLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("2SLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("2SLS_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("2SLS_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("2SLS_below" = "2SLS") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("2SLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("2SLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("2SLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_2sls <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                          bind_rows(table_below_1,table_below_2,table_below_3),
                          bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  # 2sls dataframe input for coefficients graph
  graph_all <- reg_df %>% graph_formatting()
  graph_below <- reg_df_below %>% graph_formatting()
  graph_above <-  reg_df_above %>% graph_formatting() 
  
  
  # OLS REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_above","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    ols(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for OLS regression
    
    print(paste0("OLS regs for sample ",data))
  }
  
  # OLS final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("OLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("OLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("OLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("OLS_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("OLS_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("OLS_below" = "2SLS") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("OLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("OLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("OLS_above" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_ols <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                         bind_rows(table_below_1,table_below_2,table_below_3),
                         bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  
  # REDUCED FORM REGRESSION
  # ----------------------------------------
  
  for (data in c("df","df_above","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced(var,var_name,d,obj,transform,year_filter) # function for OLS regression
    
    
    print(paste0("Reduced form regs for sample ",data))
  }
  
  # Reduced form final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("RF_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("RF_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("RF_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("RF_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("RF_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("RF_below" = "2SLS") %>% select(-term) 
  
  
  table_above_1 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(1) %>% rename("RF_above" = "2SLS") %>% select(-term) %>% mutate(spec=1)
  table_above_2 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(2) %>% rename("RF_above" = "2SLS") %>% select(-term) %>% mutate(spec=2)
  table_above_3 <- reg_df_above %>% mutate(sample = "above") %>% table_formating(3) %>% rename("RF_above" = "2SLS") %>% select(-term) %>% mutate(spec=3)
  
  
  
  table_rf <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                        bind_rows(table_below_1,table_below_2,table_below_3),
                        bind_rows(table_above_1,table_above_2,table_above_3)) 
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  table_all <- cbind.data.frame(table_ols %>% select(term,OLS_full),
                                table_2sls %>% select(`2SLS_full`),
                                table_rf %>% select(`RF_full`),
                                table_ols %>% select(OLS_below),
                                table_2sls %>% select(`2SLS_below`),
                                table_rf %>% select(`RF_below`),
                                table_ols %>% select(OLS_above),
                                table_2sls %>% select(`2SLS_above`),
                                table_rf %>% select(`RF_above`),
                                table_rf %>% select(`spec`))
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  assign("graph_all",graph_all, envir = .GlobalEnv)
  assign("graph_below",graph_below, envir = .GlobalEnv)
  assign("graph_above",graph_above, envir = .GlobalEnv)
}  # runs regressions and output objects

regress_output_below <- function(var,var_name,transform,year_filter){
  
  # IV REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    iv(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for IV regression and bootstrap estimating of SE
    
    print(paste0("IV regs for sample ",data))
  } 
  
  # 2sls final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("2SLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("2SLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("2SLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("2SLS_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("2SLS_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("2SLS_below" = "2SLS") %>% select(-term) 
  
  
  table_2sls <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                          bind_rows(table_below_1,table_below_2,table_below_3)) 
  
  # 2sls dataframe input for coefficients graph
  graph_all <- reg_df %>% graph_formatting()
  graph_below <- reg_df_below %>% graph_formatting()
  
  
  # OLS REGRESSION
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    ols(var,"finbra_desp_saude_san_pcapita",d,obj,transform,year_filter) # function for OLS regression
    
    print(paste0("OLS regs for sample ",data))
  }
  
  # OLS final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("OLS_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("OLS_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("OLS_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("OLS_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("OLS_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("OLS_below" = "2SLS") %>% select(-term) 
  
  
  table_ols <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                         bind_rows(table_below_1,table_below_2,table_below_3)) 
  
  
  # REDUCED FORM REGRESSION
  # ----------------------------------------
  
  for (data in c("df","df_below")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    reduced(var,var_name,d,obj,transform,year_filter) # function for OLS regression
    
    
    print(paste0("Reduced form regs for sample ",data))
  }
  
  # Reduced form final tables
  table_all_1 <- reg_df %>% mutate(sample = "full") %>% table_formating(1) %>% rename("RF_full" = "2SLS")
  table_all_2 <- reg_df %>% mutate(sample = "full") %>% table_formating(2) %>% rename("RF_full" = "2SLS")
  table_all_3 <- reg_df %>% mutate(sample = "full") %>% table_formating(3) %>% rename("RF_full" = "2SLS")
  
  table_below_1 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(1) %>% rename("RF_below" = "2SLS") %>% select(-term)
  table_below_2 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(2) %>% rename("RF_below" = "2SLS") %>% select(-term) 
  table_below_3 <- reg_df_below %>% mutate(sample = "below") %>% table_formating(3) %>% rename("RF_below" = "2SLS") %>% select(-term) 
  
  
  table_rf <- bind_cols(bind_rows(table_all_1,table_all_2,table_all_3),
                        bind_rows(table_below_1,table_below_2,table_below_3)) 
  
  
  # IV + OLS + reduced form table
  # ----------------------------------------
  table_all <- cbind.data.frame(table_ols %>% select(term,OLS_full),
                                table_2sls %>% select(`2SLS_full`),
                                table_rf %>% select(`RF_full`),
                                table_ols %>% select(OLS_below),
                                table_2sls %>% select(`2SLS_below`),
                                table_rf %>% select(`RF_below`),
                                table_rf %>% select(`spec`))
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  assign("graph_all",graph_all, envir = .GlobalEnv)
  assign("graph_below",graph_below, envir = .GlobalEnv)
}  # runs regressions and output objects


# 3. Run and ouput
# =================================================================

df <- df %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano==2000,0,iv)) 
df_below <- df_below %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano==2000,0,iv)) 
df_above <- df_above %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano==2000,0,iv))  



for (i in seq(1,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  if(below==1){
    
    regress_output_below(var,var_name,1,1998)
    
    
    if(exists("df_table_all")){
      df_table_all <- rbind(df_table_all,table_all)
      df_graph_all <- rbind(df_graph_all,graph_all)
      df_graph_below <- rbind(df_graph_below,graph_below)
    } else {
      
      df_table_all <- table_all
      df_graph_all <- graph_all
      df_graph_below <- graph_below
    }
    
  }else{
    regress_output(var,var_name,1,1998)
    
    
    if(exists("df_table_all")){
      df_table_all <- rbind(df_table_all,table_all)
      df_graph_all <- rbind(df_graph_all,graph_all)
      df_graph_below <- rbind(df_graph_below,graph_below)
      df_graph_above <- rbind(df_graph_above,graph_above)
    } else {
      
      df_table_all <- table_all
      df_graph_all <- graph_all
      df_graph_below <- graph_below
      df_graph_above <- graph_above
    }
    
  }
  
}


# exporting results
# ---------------------

if(below==1){
  write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "imr_b",row.names = F,append = T)
  
}else{
  
  write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "imr",row.names = F,append = T)
}





# # 5. Specifications graph
# # =================================================================
# 
# scale_f <- -7
# scale_l <- 11
# scale_s <- 1
# 
# color_graph <- pal_lancet("lanonc")(9)
# 
# graph <- df_graph_all %>% 
#   mutate(spec = as.factor(spec)) %>%
#   mutate(spec = ifelse(spec=="1","1. municipality + time FE",spec),
#          spec = ifelse(spec=="2", "2. municipality + state-time FE",spec),
#          spec = ifelse(spec=="3", "3. municipality + state-time FE, with controls",spec)) %>% 
#   mutate(term = as.factor(term)) %>% 
#   mutate(term = fct_relevel(term,
#                             var_map[15,2],
#                             var_map[14,2],
#                             var_map[13,2],
#                             var_map[12,2],
#                             var_map[11,2],
#                             var_map[10,2],
#                             var_map[9,2],
#                             var_map[8,2],
#                             var_map[7,2],
#                             var_map[6,2],
#                             var_map[5,2],
#                             var_map[4,2],
#                             var_map[3,2],
#                             var_map[2,2],
#                             var_map[1,2])) %>%  
#   ggplot(aes(color = spec)) +
#   geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.35, alpha = 1, linetype = "dotted") +
#   geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.35, alpha = 1, linetype = "dotted") +
#   geom_errorbar(aes(x= term, ymin = lb, ymax = ub, width = 0.4),
#                 size = 0.7,
#                 alpha = 0.7,
#                 position = position_dodge(width=0.6)) +
#   geom_point(aes(y = estimate, x = term),position = position_dodge(width=0.6), size = 3, alpha = 0.7) +
#   scale_x_discrete() +
#   scale_y_continuous(breaks = seq(scale_f,scale_l,scale_s), limits = c(scale_f,scale_l))+
#   scale_colour_manual(values = color_graph) +
#   coord_flip() +
#   labs(x = "") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position="bottom", legend.box = "horizontal",
#         # legend.title = element_blank(),
#         plot.title = element_text(size = 10),
#         plot.subtitle = element_text(size = 8),
#         legend.text = element_text(size = 12),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size=10)) +
#   guides(color=guide_legend(ncol=1,byrow=T, title = "Specification",title.position="top"))
# 
# 
# 
# ggsave(paste0(dir,main_folder,robust_folder,"imr_all.png"),
#        plot = graph,
#        device = "png",
#        width = 10, height = 12,
#        units = "in")
# ggsave(paste0(dir,main_folder,robust_folder,"imr_all.pdf"),
#        plot = graph,
#        device = "pdf",
#        width = 10, height = 12,
#        units = "in")
# 
# 
# 
# graph <- df_graph_below %>% 
#   mutate(spec = as.factor(spec)) %>%
#   mutate(spec = ifelse(spec=="1","1. municipality + time FE",spec),
#          spec = ifelse(spec=="2", "2. municipality + state-time FE",spec),
#          spec = ifelse(spec=="3", "3. municipality + state-time FE, with controls",spec)) %>% 
#   mutate(term = as.factor(term)) %>% 
#   mutate(term = fct_relevel(term,
#                             var_map[15,2],
#                             var_map[14,2],
#                             var_map[13,2],
#                             var_map[12,2],
#                             var_map[11,2],
#                             var_map[10,2],
#                             var_map[9,2],
#                             var_map[8,2],
#                             var_map[7,2],
#                             var_map[6,2],
#                             var_map[5,2],
#                             var_map[4,2],
#                             var_map[3,2],
#                             var_map[2,2],
#                             var_map[1,2])) %>%  
#   ggplot(aes(color = spec)) +
#   geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.35, alpha = 1, linetype = "dotted") +
#   geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.35, alpha = 1, linetype = "dotted") +
#   geom_errorbar(aes(x= term, ymin = lb, ymax = ub, width = 0.4),
#                 size = 0.7,
#                 alpha = 0.7,
#                 position = position_dodge(width=0.6)) +
#   geom_point(aes(y = estimate, x = term),position = position_dodge(width=0.6), size = 3, alpha = 0.7) +
#   scale_x_discrete() +
#   scale_y_continuous(breaks = seq(scale_f,scale_l,scale_s), limits = c(scale_f,scale_l))+
#   scale_colour_manual(values = color_graph) +
#   coord_flip() +
#   labs(x = "") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position="bottom", legend.box = "horizontal",
#         # legend.title = element_blank(),
#         plot.title = element_text(size = 10),
#         plot.subtitle = element_text(size = 8),
#         legend.text = element_text(size = 12),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size=10)) +
#   guides(color=guide_legend(ncol=1,byrow=T, title = "Specification",title.position="top"))
# 
# 
# 
# ggsave(paste0(dir,main_folder,robust_folder,"imr_below.png"),
#        plot = graph,
#        device = "png",
#        width = 10, height = 12,
#        units = "in")
# ggsave(paste0(dir,main_folder,robust_folder,"imr_below.pdf"),
#        plot = graph,
#        device = "pdf",
#        width = 10, height = 12,
#        units = "in")
# 
# 
# graph <- df_graph_above %>% 
#   mutate(spec = as.factor(spec)) %>%
#   mutate(spec = ifelse(spec=="1","1. municipality + time FE",spec),
#          spec = ifelse(spec=="2", "2. municipality + state-time FE",spec),
#          spec = ifelse(spec=="3", "3. municipality + state-time FE, with controls",spec)) %>% 
#   mutate(term = as.factor(term)) %>% 
#   mutate(term = fct_relevel(term,
#                             var_map[15,2],
#                             var_map[14,2],
#                             var_map[13,2],
#                             var_map[12,2],
#                             var_map[11,2],
#                             var_map[10,2],
#                             var_map[9,2],
#                             var_map[8,2],
#                             var_map[7,2],
#                             var_map[6,2],
#                             var_map[5,2],
#                             var_map[4,2],
#                             var_map[3,2],
#                             var_map[2,2],
#                             var_map[1,2])) %>%  
#   ggplot(aes(color = spec)) +
#   geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.35, alpha = 1, linetype = "dotted") +
#   geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.35, alpha = 1, linetype = "dotted") +
#   geom_errorbar(aes(x= term, ymin = lb, ymax = ub, width = 0.4),
#                 size = 0.7,
#                 alpha = 0.7,
#                 position = position_dodge(width=0.6)) +
#   geom_point(aes(y = estimate, x = term),position = position_dodge(width=0.6), size = 3, alpha = 0.7) +
#   scale_x_discrete() +
#   scale_y_continuous(breaks = seq(scale_f,scale_l,scale_s), limits = c(scale_f,scale_l))+
#   scale_colour_manual(values = color_graph) +
#   coord_flip() +
#   labs(x = "") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position="bottom", legend.box = "horizontal",
#         # legend.title = element_blank(),
#         plot.title = element_text(size = 10),
#         plot.subtitle = element_text(size = 8),
#         legend.text = element_text(size = 12),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size=10)) +
#   guides(color=guide_legend(ncol=1,byrow=T, title = "Specification",title.position="top"))
# 
# 
# 
# ggsave(paste0(dir,main_folder,robust_folder,"imr_above.png"),
#        plot = graph,
#        device = "png",
#        width = 10, height = 12,
#        units = "in")
# ggsave(paste0(dir,main_folder,robust_folder,"imr_above.pdf"),
#        plot = graph,
#        device = "pdf",
#        width = 10, height = 12,
#        units = "in")
# 
# 
