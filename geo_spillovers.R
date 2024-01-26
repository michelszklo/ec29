#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public infrastructure
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

raw <- paste0(dir,"data/")

# ------------------------------------


# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))



# 2. interacting year dummies with neighbours distance to the target
# =================================================================

mun_neighbors <- readRDS(paste0(raw,"mun_neighbors.RDS"))

df_neighbor <- df %>%
  select(ano,cod_mun) %>%
  full_join(mun_neighbors, by = "cod_mun") %>% 
  left_join(df %>% 
              select(ano,cod_mun,dist_ec29_baseline, peso_pop) %>% 
              rename(cod_mun_neighbor = cod_mun),
            by = c("cod_mun_neighbor","ano")) %>% 
  filter(cod_mun!=cod_mun_neighbor) %>% 
  group_by(cod_mun,ano) %>% 
  summarise(mean_nb_dist_ec29_baseline = weighted.mean(dist_ec29_baseline,w = peso_pop, na.rm = T),
            max_nb_dist_ec29_baseline = max(dist_ec29_baseline)) %>% 
  ungroup() 

df <- df %>% 
  left_join(df_neighbor, by = c("ano","cod_mun"))



# neighbours mean distance
yeartreat_dummies_mean_nb <- sapply(dummies, function(x) paste0(x,"_","mean_nb_dist_ec29_baseline"), simplify = "array", USE.NAMES = F)
df[yeartreat_dummies_mean_nb] <- df[dummies]

#neighbours max distance
yeartreat_dummies_max_nb <- sapply(dummies, function(x) paste0(x,"_","max_nb_dist_ec29_baseline"), simplify = "array", USE.NAMES = F)
df[yeartreat_dummies_max_nb] <- df[dummies]

df <- df %>% 
  mutate_at(yeartreat_dummies_mean_nb, `*`,quote(mean_nb_dist_ec29_baseline)) %>% 
  unnest(all_of(yeartreat_dummies_mean_nb)) %>% 
  mutate_at(yeartreat_dummies_max_nb, `*`,quote(max_nb_dist_ec29_baseline)) %>% 
  unnest(all_of(yeartreat_dummies_max_nb))


df2000 <- df %>% filter(ano==2000) %>% 
  summarize(mean = mean(mean_nb_dist_ec29_baseline, na.rm = T),
            max = mean(max_nb_dist_ec29_baseline, na.rm = T),
            dist = mean(dist_ec29_baseline, na.rm = T))

# 3. Regression specifications (yearly)
# =================================================================

# 1) Mean
# ------------------------------------------------

# standard outcomes
spec1_post_y_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_y_imr_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_imr_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_imr_mean_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_mean_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 1) Max
# ------------------------------------------------

# standard outcomes
spec1_post_y_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_y_imr_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_imr_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_imr_max_nb <- paste(" ~ ",paste(yeartreat_dummies, collapse = " + ")," + ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 1) Only Neighbours - MAX
# ------------------------------------------------

# standard outcomes
spec1_post_y_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_post_y_imr_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_y_imr_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_y_imr_max_nb_only <- paste(" ~ ",paste(yeartreat_dummies_max_nb, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")




outcome <- 'tx_mi'
var_name <- 'Infant Mortality Rate'
transform <- 3
year_filter <- 1998
# y0 <- -3
# yf <- 9.25
# ys <- 1
# weight <- "peso_pop"
year_cap <- 2010
label_size = 8
mean <- 2

yearly_folder <- "regs_plots_trend/"


geospill <- function(df, outcome,var_name,transform,year_filter,year_cap,label_size,mean){
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_max_nb),all_of(yeartreat_dummies_mean_nb),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  spec <- 3
  if (mean == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_mean_nb"))
  } else if (mean == 2){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_max_nb"))
  }else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_max_nb_only"))
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Municipality",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(18:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Neighbours",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  if (mean == 3){
    table <- fit %>% 
      broom::tidy() %>%
      slice(3:15) %>%
      select(term,estimate,std.error,p.value) %>%
      mutate(target = "Neighbours",
             year = seq.int(year_filter,2010))
  }
  
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec))
  # mutate(lb_adj = NA,
  #        ub_adj = NA) %>% 
  # mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
  #        ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
  # mutate(lb = ifelse(lb<y0,y0,lb),
  #        ub = ifelse(ub>yf,yf,ub),
  #        lb2 = ifelse(lb2<y0,y0,lb2),
  #        ub2 = ifelse(ub2>yf,yf,ub2))
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#762a83","#1b7837")
  
  
  graph <- table %>% filter(!is.nan(estimate)) %>% 
    ggplot(aes(x = year, y = estimate, color = target, group = target))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
    # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
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
          legend.title = element_blank())
  
  
  if(mean==1){
    suffix = "mean"
  } else if (mean == 2){
    suffix = "max"
  } else {
    suffix = "max_only"
  }
  
  ggsave(paste0(dir,main_folder,yearly_folder,"geospill_",outcome,"_",suffix,".png"),
         plot = graph,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0(dir,main_folder,yearly_folder,"geospill_",outcome,"_",suffix,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
}


geospill(df,'tx_mi','Infant Mortality Rate',
         3,1998,2010,8,1)

geospill(df,'tx_mi','Infant Mortality Rate',
         3,1998,2010,8,2)

geospill(df,'tx_mi','Infant Mortality Rate',
         3,1998,2010,8,3)


geospill(df,'tx_mi_icsap','Infant Mortality Rate - APC',
         3,1998,2010,8,1)

geospill(df,'tx_mi_icsap','Infant Mortality Rate - APC',
         3,1998,2010,8,2)

geospill(df,'tx_mi_icsap','Infant Mortality Rate - APC',
         3,1998,2010,8,3)


geospill(df,'tx_mi_nicsap','Infant Mortality Rate - non-APC',
         3,1998,2010,8,1)

geospill(df,'tx_mi_nicsap','Infant Mortality Rate - non-APC',
         3,1998,2010,8,2)

geospill(df,'tx_mi_nicsap','Infant Mortality Rate - non-APC',
         3,1998,2010,8,3)


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

df2 <- df %>% 
  filter(!(cod_mun %in% outliers))



geospill(df2,'finbra_desp_o_pcapita','Total Spending per capita (log)',
         1,1998,2010,8,1)

geospill(df2,'finbra_desp_o_pcapita','Total Spending per capita (log)',
         1,1998,2010,8,2)

geospill(df2,'finbra_desp_o_pcapita','Total Spending per capita (log)',
         1,1998,2010,8,3)


geospill(df2,'finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)',
         1,1998,2010,8,1)

geospill(df2,'finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)',
         1,1998,2010,8,2)

geospill(df2,'finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)',
         1,1998,2010,8,3)



geospill(df2,'finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)',
         1,1998,2010,8,1)

geospill(df2,'finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)',
         1,1998,2010,8,2)

geospill(df2,'finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)',
         1,1998,2010,8,3)


geospill(df2,'siops_despsaude_pcapita','Health Spending per capita - Total (log)',
         1,1998,2010,8,1)

geospill(df2,'siops_despsaude_pcapita','Health Spending per capita - Total (log)',
         1,1998,2010,8,2)

geospill(df2,'siops_despsaude_pcapita','Health Spending per capita - Total (log)',
         1,1998,2010,8,3)



geospill(df2,'siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)',
         1,1998,2010,8,1)

geospill(df2,'siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)',
         1,1998,2010,8,2)

geospill(df2,'siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)',
         1,1998,2010,8,3)



geospill(df2,'siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)',
         1,1998,2010,8,1)

geospill(df2,'siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)',
         1,1998,2010,8,2)

geospill(df2,'siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)',
         1,1998,2010,8,3)






