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


# 1. Load data
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



# 2. Variables of interest and baseline mean
# =================================================================

main_var <- c("finbra_desp_saude_san_pcapita","siops_despsaude_pcapita","tx_mi")

df_baseline <- df %>% 
  filter(ano==2000) %>% 
  select(all_of(main_var)) %>% 
  summarise_all(list(mean = ~ mean(.,na.rm = T)))



# 3. Regressions MAIN
# =================================================================

year_filter <- 1998
# FINBRA
# ------------------------------------

df_reg <- df


# filtering regression variables
df_reg <- df_reg %>% 
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,finbra_desp_saude_san_pcapita,iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
         all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
         finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
  filter(ano>=year_filter)

df_reg <- df_reg[complete.cases(df_reg),]



spec <- 3
spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()

regformula <- as.formula(paste("finbra_desp_saude_san_pcapita",spec_reduced))
fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)

table1 <- fit %>% 
  broom::tidy() %>%
  slice(3:15) %>%
  select(term,estimate,std.error) %>%
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         year = seq.int(year_filter,2010)) %>%
  filter(!is.na(estimate))

cols <- names(table1)[2:ncol(table1)]
cols <- sapply(cols, function(x) paste0(x,"_finbra_desp_saude_san_pcapita"), simplify = "array", USE.NAMES = F)
names(table1)[2:ncol(table1)] <- cols

table1 <- table1 %>% 
  mutate(mean_finbra_desp_saude_san_pcapita = as.numeric(df_baseline[1,1]))



# SIOPS
# ------------------------------------

df_reg <- df

# filtering regression variables
df_reg <- df_reg %>% 
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,siops_despsaude_pcapita,iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
         all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
         finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
  filter(ano>=year_filter)

df_reg <- df_reg[complete.cases(df_reg),]

spec <- 3
spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()

regformula <- as.formula(paste("siops_despsaude_pcapita",spec_reduced))
fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)

table2 <- fit %>% 
  broom::tidy() %>%
  slice(3:15) %>%
  select(term,estimate,std.error) %>%
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         year = seq.int(year_filter,2010)) %>%
  filter(!is.na(estimate))

cols <- names(table2)[2:ncol(table2)]
cols <- sapply(cols, function(x) paste0(x,"_siops_despsaude_pcapita"), simplify = "array", USE.NAMES = F)
names(table2)[2:ncol(table2)] <- cols

table2 <- table2 %>% 
  mutate(mean_siops_despsaude_pcapita = as.numeric(df_baseline[1,2]))



# IMR
# ------------------------------------
df_reg <- df


# filtering regression variables
df_reg <- df_reg %>% 
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,tx_mi,iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
         all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
         finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
  filter(ano>=year_filter)

df_reg <- df_reg[complete.cases(df_reg),]

spec <- 3
spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()

regformula <- as.formula(paste("tx_mi",spec_reduced))
fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)

table3 <- fit %>% 
  broom::tidy() %>%
  slice(3:15) %>%
  select(term,estimate,std.error) %>%
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         year = seq.int(year_filter,2010)) %>%
  filter(!is.na(estimate))

cols <- names(table3)[2:ncol(table3)]
cols <- sapply(cols, function(x) paste0(x,"_tx_mi"), simplify = "array", USE.NAMES = F)
names(table3)[2:ncol(table3)] <- cols
  
table3 <- table3 %>% 
  mutate(mean_tx_mi = as.numeric(df_baseline[1,3]))




# 4. GRAPH
# =================================================================


df_elasticity <- table2 %>% 
  left_join(table3, by ="term") %>% 
  mutate(e = (estimate_tx_mi/mean_tx_mi)/(estimate_siops_despsaude_pcapita/mean_siops_despsaude_pcapita),
         e_lb = (lb_tx_mi/mean_tx_mi)/(lb_siops_despsaude_pcapita/mean_siops_despsaude_pcapita),
         e_ub = (ub_tx_mi/mean_tx_mi)/(ub_siops_despsaude_pcapita/mean_siops_despsaude_pcapita)) %>% 
  mutate(year = seq.int(2000,2010)) %>% 
  select(year,e,e_lb,e_ub)
  

graph <- df_elasticity %>% 
  ggplot(aes(x = year, y = e))+
  geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
  geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
  geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
  geom_ribbon(aes(ymin = e_lb, ymax = e_ub),color = NA, alpha = 0.1) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010+0.5)) +
  scale_y_continuous(breaks = seq(-0.9,0.3,0.1), limits = c(-0.9,0.3), labels = comma) +
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
ggsave(paste0(dir,main_folder,yearly_folder,"imr_elasticity.png"),
       plot = graph,
       device = "png",
       width = 7, height = 5,
       units = "in")
ggsave(paste0(dir,main_folder,yearly_folder,"imr_elasticity.pdf"),
       plot = graph,
       device = "pdf",
       width = 7, height = 5,
       units = "in")







# 5. Regressions ABOVE and BELOW
# =================================================================

# FINBRA
# ------------------------------------

df_reg <- df


# filtering regression variables
df_reg <- df_reg %>% 
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,finbra_desp_saude_san_pcapita,iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
         all_of(yeartreat_dummies_ab),
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
         finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
  filter(ano>=year_filter)

df_reg <- df_reg[complete.cases(df_reg),]



spec <- 3
spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()

regformula <- as.formula(paste("finbra_desp_saude_san_pcapita",spec_reduced))
fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)

tableA <- fit %>% 
  broom::tidy() %>%
  slice(3:15) %>%
  select(term,estimate,std.error) %>%
  mutate(year = seq.int(year_filter,2010)) %>% 
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         target = "Above") %>%
  filter(!is.na(estimate))


tableB <- fit %>% 
  broom::tidy() %>%
  slice(18:30) %>%
  select(term,estimate,std.error) %>%
  mutate(year = seq.int(year_filter,2010)) %>% 
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         target = "Below") %>%
  filter(!is.na(estimate))

table1 <- rbind(tableA,tableB)

cols <- names(table1)[2:ncol(table1)-1]
cols <- sapply(cols, function(x) paste0(x,"_finbra_desp_saude_san_pcapita"), simplify = "array", USE.NAMES = F)
names(table1)[2:ncol(table1)-1] <- cols

table1 <- table1 %>% 
  mutate(mean_finbra_desp_saude_san_pcapita = as.numeric(df_baseline[1,1]))



# SIOPS
# ------------------------------------

df_reg <- df

# filtering regression variables
df_reg <- df_reg %>% 
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,siops_despsaude_pcapita,iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
         all_of(yeartreat_dummies_ab),
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
         finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
  filter(ano>=year_filter)

df_reg <- df_reg[complete.cases(df_reg),]

spec <- 3
spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()

regformula <- as.formula(paste("siops_despsaude_pcapita",spec_reduced))
fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)

tableA <- fit %>% 
  broom::tidy() %>%
  slice(3:15) %>%
  select(term,estimate,std.error) %>%
  mutate(year = seq.int(year_filter,2010)) %>% 
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         target = "Above") %>%
  filter(!is.na(estimate))


tableB <- fit %>% 
  broom::tidy() %>%
  slice(18:30) %>%
  select(term,estimate,std.error) %>%
  mutate(year = seq.int(year_filter,2010)) %>% 
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         target = "Below") %>%
  filter(!is.na(estimate))

table2 <- rbind(tableA,tableB)

cols <- names(table2)[2:ncol(table2)-1]
cols <- sapply(cols, function(x) paste0(x,"_siops_despsaude_pcapita"), simplify = "array", USE.NAMES = F)
names(table2)[2:ncol(table2)-1] <- cols

table2 <- table2 %>% 
  mutate(mean_siops_despsaude_pcapita = as.numeric(df_baseline[1,2]))



# IMR
# ------------------------------------
df_reg <- df


# filtering regression variables
df_reg <- df_reg %>% 
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,tx_mi,iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
         all_of(yeartreat_dummies_ab),
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
         finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
  filter(ano>=year_filter)

df_reg <- df_reg[complete.cases(df_reg),]

spec <- 3
spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()

regformula <- as.formula(paste("tx_mi",spec_reduced))
fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)

tableA <- fit %>% 
  broom::tidy() %>%
  slice(3:15) %>%
  select(term,estimate,std.error) %>%
  mutate(year = seq.int(year_filter,2010)) %>% 
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         target = "Above") %>%
  filter(!is.na(estimate))


tableB <- fit %>% 
  broom::tidy() %>%
  slice(18:30) %>%
  select(term,estimate,std.error) %>%
  mutate(year = seq.int(year_filter,2010)) %>% 
  mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
  mutate(lb = estimate - 1.96 * std.error,
         ub = estimate + 1.96 * std.error,
         target = "Below") %>%
  filter(!is.na(estimate))

table3 <- rbind(tableA,tableB)

cols <- names(table3)[2:ncol(table3)-1]
cols <- sapply(cols, function(x) paste0(x,"_tx_mi"), simplify = "array", USE.NAMES = F)
names(table3)[2:ncol(table3)-1] <- cols

table3 <- table3 %>% 
  mutate(mean_tx_mi = as.numeric(df_baseline[1,3]))


# 6. GRAPH
# =================================================================


df_elasticity <- table2 %>%
  mutate(merge = substr(term_siops_despsaude_pcapita,7,nchar(term_siops_despsaude_pcapita))) %>% 
  left_join(table3 %>% 
              mutate(merge = substr(term_tx_mi,7,nchar(term_tx_mi))), by =c("merge","target")) %>% 
  select(-merge) %>% 
  mutate(e = (estimate_tx_mi/mean_tx_mi)/(estimate_siops_despsaude_pcapita/mean_siops_despsaude_pcapita),
         e_lb = (lb_tx_mi/mean_tx_mi)/(lb_siops_despsaude_pcapita/mean_siops_despsaude_pcapita),
         e_ub = (ub_tx_mi/mean_tx_mi)/(ub_siops_despsaude_pcapita/mean_siops_despsaude_pcapita)) %>%
  mutate(year = year_tx_mi) %>% 
  select(year,e,e_lb,e_ub,target)


colors <-  c("#ef8a62","#67a9cf")

graph <- df_elasticity %>% 
  ggplot(aes(x = year, y = e, color = target, group = target))+
  geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
  geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
  geom_point(size = 1, alpha = 1,shape=0,stroke = 0.8, position = position_dodge(width=0.1)) +
  geom_ribbon(aes(ymin = e_lb, ymax = e_ub,fill = target),color = NA, alpha = 0.1) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010+0.5)) +
  # scale_y_continuous(breaks = seq(-0.9,0.3,0.1), limits = c(-0.9,0.3), labels = comma) +
  scale_color_manual(values = colors) +
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
ggsave(paste0(dir,main_folder,yearly_folder,"imr_elasticity.png"),
       plot = graph,
       device = "png",
       width = 7, height = 5,
       units = "in")
ggsave(paste0(dir,main_folder,yearly_folder,"imr_elasticity.pdf"),
       plot = graph,
       device = "pdf",
       width = 7, height = 5,
       units = "in")

