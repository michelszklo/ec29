#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for hospitalization
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

set.seed(28121989)

# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))

# removing variables that will not be used
#------------------------------------------

# select outcomes
outcomes <- c("tx_mi","siops_despsaude_pcapita","finbra_desp_o_pcapita")

df <- df %>% 
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(outcomes),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
         all_of(yeartreat_dummies),all_of(yeartreat_dummies_ab),all_of(yeartreat_dummies_binary),
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
         finbra_desp_saude_san_pcapita_neighbor,lrf)

# "finbra_desp_saude_san_pcapita","siops_despsaude_pcapita","tx_mi"

# 2. Functions
# =================================================================

# function to run regression for each dataframe and variable
f1 <- function(df,var,year_filter,table_output){
  
  df_reg <- df
  
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(var),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  
  
  
  spec <- 3
  spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
  weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()
  
  regformula <- as.formula(paste(var,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(year = seq.int(year_filter,2010)) %>%
    filter(!is.na(estimate))
  
  cols <- names(table)[2:2]
  cols <- sapply(cols, function(x) paste0(x,"_",var), simplify = "array", USE.NAMES = F)
  names(table)[2:2] <- cols
  
  assign(table_output,table,envir = .GlobalEnv)
  
  
}

f1_ab <- function(df,var,year_filter,table_output){
  
  df_reg <- df
  
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(var),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies_ab),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  
  
  
  spec <- 3
  spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
  weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()
  
  regformula <- as.formula(paste(var,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  tableA <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate) %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(year = seq.int(year_filter,2010),
           target = "Above") %>%
    filter(!is.na(estimate))
  
  tableB <- fit %>% 
    broom::tidy() %>%
    slice(18:30) %>%
    select(term,estimate) %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(year = seq.int(year_filter,2010),
           target = "Below") %>%
    filter(!is.na(estimate))
  
  table <- rbind(tableA,tableB)
  
  cols <- names(table)[2:2]
  cols <- sapply(cols, function(x) paste0(x,"_",var), simplify = "array", USE.NAMES = F)
  names(table)[2:2] <- cols
  
  assign(table_output,table,envir = .GlobalEnv)
  
  
}


# function that creates samples (with, without outlier), run regressions and outputs table
f2 <- function(df,var1,var2,year_filter,output){
  
  # var1: health outcome
  # var2: spending outcome
  
  # create sample without spending outliers
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
  
  
  # baseline mean for var1
  main_var <- c(var1)
  
  df_baseline <- df %>% 
    filter(ano==2000) %>% 
    select(all_of(main_var)) %>% 
    summarise_all(list(mean = ~ mean(.,na.rm = T)))
  
  # baseline mean for var2
  main_var <- c(var2)
  
  df_baseline2 <- df2 %>% 
    filter(ano==2000) %>% 
    select(all_of(main_var)) %>% 
    summarise_all(list(mean = ~ mean(.,na.rm = T)))
  
  f1(df,var1,1998,"table_health")
  f1(df2,var2,1998,"table_spending")
  
  table <- table_health %>% 
    left_join(table_spending, by = c("term","year")) %>% 
    select(term,year,everything())
  
  assign(output, table, envir = .GlobalEnv)
  
}

f2_ab <- function(df,var1,var2,year_filter,output){
  
  # var1: health outcome
  # var2: spending outcome
  
  # create sample without spending outliers
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
  
  
  f1_ab(df,var1,1998,"table_health")
  f1_ab(df2,var2,1998,"table_spending")
  
  table <- table_health %>% 
    left_join(table_spending, by = c("term","year","target")) %>% 
    select(term,year,everything())
  
  assign(output, table, envir = .GlobalEnv)
  
}

# boot strap function
f3 <- function(df,n) {      #define function
  new_df <- data.frame(matrix(nrow = 82605, ncol = 71))
  unique_ids <- unique(n)      #unique firms
  sample_ids <- sample(unique_ids, size=length(unique_ids), replace=T ) #choose from unique firms randomly with replacement
  new_df <- do.call(rbind, lapply(sample_ids, function(x)  df[df$cod_mun==x,] ))  #fetch all years for each randomly picked firm and rbind
  return(new_df)
}

# 3. Main regressions
# =================================================================

f2(df,"tx_mi","siops_despsaude_pcapita",1998,"elasticity_main")

f2_ab(df,"tx_mi","siops_despsaude_pcapita",1998,"elasticity_main_ab")


# 4. Bootstrap main regression
# =================================================================

var1 <- "tx_mi"
var2 <- "siops_despsaude_pcapita"

boots <- 1000
elasticity_boots <- data.frame(matrix(nrow = 13*boots, ncol = 5))
colnames(elasticity_boots) <- c("term","year",paste0("estimate_",var1),paste0("estimate_",var2),"boot")

boot_results <- lapply(1:boots, function(i) {
  print(paste0("Boot # ", i))
  set.seed(28121989 + i)
  
  elasticity_i <- f2(df %>% f3(df$cod_mun), var1, var2, 1998, "elasticity_i")
  elasticity_i <- elasticity_i %>% mutate(boot = i)
  
  r1 <- (i - 1) * 13 + 1
  r2 <- r1 + 12
  elasticity_i
})

# Combine results into a single data frame
elasticity_boots <- do.call(rbind, boot_results)




# saving bootstrap estimates
# saveRDS(elasticity_boots, paste0(dir,"regs_outputs/elasticity/bootstrap_estimates.rds"))
# elasticity_boots <- readRDS(paste0(dir,"regs_outputs/elasticity/bootstrap_estimates.rds"))


# 5. Bootstrap Above and Below regression
# =================================================================

var1 <- "tx_mi"
var2 <- "siops_despsaude_pcapita"

boots <- 1000
elasticity_boots2 <- data.frame(matrix(nrow = 13*boots*2, ncol = 5))
colnames(elasticity_boots2) <- c("term","year",paste0("estimate_",var1),paste0("estimate_",var2),"boot")

boot_results <- lapply(1:boots, function(i) {
  print(paste0("Boot # ", i))
  set.seed(28121989 + i)
  
  elasticity_i <- f2_ab(df %>% f3(df$cod_mun), var1, var2, 1998, "elasticity_i")
  elasticity_i <- elasticity_i %>% mutate(boot = i)
  
  r1 <- (i - 1) * 26 + 1
  r2 <- r1 + 25
  elasticity_i
})

# Combine results into a single data frame
elasticity_boots2 <- do.call(rbind, boot_results)




# saving bootstrap estimates
saveRDS(elasticity_boots2, paste0(dir,"regs_outputs/elasticity/bootstrap_estimates_ab.rds"))
# elasticity_boots <- readRDS(paste0(dir,"regs_outputs/elasticity/bootstrap_estimates_ab.rds"))





# 6. Variables of interest and baseline mean
# =================================================================

main_var <- c("siops_despsaude_pcapita","tx_mi")

df_baseline <- df %>% 
  filter(ano==2000) %>% 
  select(all_of(main_var)) %>% 
  summarise_all(list(mean = ~ mean(.,na.rm = T)))




# 7. Calculating elasticities  and elasticities CIs
# =================================================================

elasticity_main <- elasticity_main %>% 
  mutate(mean_siops = df_baseline$siops_despsaude_pcapita_mean,
         mean_tx_mi = df_baseline$tx_mi_mean) %>% 
  mutate(e = (estimate_tx_mi/mean_tx_mi)/(estimate_siops_despsaude_pcapita/mean_siops))

elasticity_main_ab <- elasticity_main_ab %>% 
  mutate(mean_siops = df_baseline$siops_despsaude_pcapita_mean,
         mean_tx_mi = df_baseline$tx_mi_mean) %>% 
  mutate(e = (estimate_tx_mi/mean_tx_mi)/(estimate_siops_despsaude_pcapita/mean_siops))



# main regs CI
elasticity_ci <- elasticity_boots %>% 
  mutate(mean_siops = df_baseline$siops_despsaude_pcapita_mean,
         mean_tx_mi = df_baseline$tx_mi_mean)

elasticity_ci <- elasticity_ci %>% 
  mutate(e = (estimate_tx_mi/mean_tx_mi)/(estimate_siops_despsaude_pcapita/mean_siops)) %>% 
  group_by(year) %>% 
  summarize(e_p025 = quantile(e, probs = 0.025, na.rm = T),
            p975 = quantile(e, probs = 0.975, na.rm = T))

elasticity <- elasticity_main %>% 
  left_join(elasticity_ci, by = "year")

# Above and Below regs CI
elasticity_ci_ab <- elasticity_boots2 %>% 
  mutate(mean_siops = df_baseline$siops_despsaude_pcapita_mean,
         mean_tx_mi = df_baseline$tx_mi_mean)

elasticity_ci_ab <- elasticity_ci_ab %>% 
  mutate(e = (estimate_tx_mi/mean_tx_mi)/(estimate_siops_despsaude_pcapita/mean_siops)) %>% 
  group_by(year,target) %>% 
  summarize(e_p025 = quantile(e, probs = 0.025, na.rm = T),
            p975 = quantile(e, probs = 0.975, na.rm = T))

elasticity_ab <- elasticity_main_ab %>% 
  left_join(elasticity_ci_ab, by = c("year","target"))



# 7. Graph
# =================================================================


# main regs

graph <- elasticity %>% 
  ggplot(aes(x = year, y = e))+
  geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
  geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
  geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
  geom_ribbon(aes(ymin = e_p025, ymax = p975),color = NA, alpha = 0.1) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010+0.5)) +
  scale_y_continuous(breaks = seq(-0.6,0.3,0.1), limits = c(-0.6,0.3), labels = comma) +
  theme_light() +
  labs(y = "IMR Elasticity",
       x = "Year") +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=8),
        axis.text = element_text(size = 10),
        legend.position="bottom")


yearly_folder <- "regs_plots_trend/"
ggsave(paste0(dir,"regs_outputs/elasticity/","imr_elasticity2.png"),
       plot = graph,
       device = "png",
       width = 7, height = 5,
       units = "in")
ggsave(paste0(dir,"regs_outputs/elasticity/","imr_elasticity2.pdf"),
       plot = graph,
       device = "pdf",
       width = 7, height = 5,
       units = "in")


# Above and Below
colors <-  c("#ef8a62","#67a9cf")

graph <- elasticity_ab %>% 
  ggplot(aes(x = year, y = e, color = target, group = target))+
  geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
  geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
  geom_point(size = 1, alpha = 1,shape=0,stroke = 0.8, position = position_dodge(width=0.1)) +
  geom_ribbon(aes(ymin = e_p025, ymax = p975, fill = target),color = NA, alpha = 0.1) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010+0.5)) +
  scale_y_continuous(breaks = seq(-1.4,0.8,0.2), limits = c(-1.4,0.8), labels = comma) +
  scale_color_manual(values = colors) +
  theme_light() +
  labs(y = "IMR Elasticity",
       x = "Year") +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=8),
        axis.text = element_text(size = 10),
        legend.position="bottom",
        legend.title = element_blank())

yearly_folder <- "regs_plots_trend/"
ggsave(paste0(dir,"regs_outputs/elasticity/","imr_elasticity_ab.png"),
       plot = graph,
       device = "png",
       width = 7, height = 5,
       units = "in")
ggsave(paste0(dir,"regs_outputs/elasticity/","imr_elasticity_ab.pdf"),
       plot = graph,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

