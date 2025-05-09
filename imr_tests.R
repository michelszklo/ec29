#######################################################################################################
# Author: Michel Szklo
# June 2024
# 
# This scripts analyses IMR data in 1996 and 1997 to remove outliers and run regressions for the period
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

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

load(paste0(dir,"regs.RData"))


# adjust regions
df <- df %>% 
  mutate(region = as.character(region)) %>% 
  mutate(region = ifelse(ano==1996, dplyr::lead(region,2),region)) %>% 
  mutate(region = ifelse(ano==1997, dplyr::lead(region,1),region))


yearly_folder <- "regs_plots_trend/imr_extend/"

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

rnames <- seq.int(1996,2010) %>%
  as.data.frame() %>% 
  slice(rep(1:n(), each = 2)) %>% 
  mutate(b = paste0("DistEC29 * ",.)) %>% 
  select(b) %>% 
  rbind("obs")

table_main <- cbind(rnames,data.frame(matrix(nrow = 31, ncol = 0)))
table_ab <- cbind(rnames,data.frame(matrix(nrow = 31, ncol = 0)))

df <- df %>% 
  select(tx_mi,birth_nasc_vivos,mi,everything())

# df_collapse <- df %>%
#   filter(ano<=2005) %>%
#   group_by(ano, uf) %>%
#   summarize(imr = weighted.mean(tx_mi,pop,na.rm = T)) %>%
#   pivot_wider(id_cols = "uf",
#               names_from = "ano",
#               values_from = "imr") %>%
#   ungroup() %>%
#   as.data.frame()
# write.xlsx2(df_collapse, file = paste0(dir,"data/SIM_corrections/","sim_correct.xlsx"),sheetName = "full_uf",row.names = F,append = T)


# df_collapse <- df %>%
#   filter(ano<=2005) %>%
#   group_by(ano, region) %>%
#   summarize(imr = weighted.mean(tx_mi,pop,na.rm = T)) %>%
#   pivot_wider(id_cols = "region",
#               names_from = "ano",
#               values_from = "imr") %>%
#   ungroup() %>%
#   as.data.frame()
# write.xlsx2(df_collapse, file = paste0(dir,"data/SIM_corrections/","sim_correct.xlsx"),sheetName = "full",row.names = F,append = T)



# # remove municipalities with mortality greater than births
# filter <- df %>%
#   filter(tx_mi>1000) %>%
#   select(cod_mun) %>%
#   pull()
# 
# df <- df %>%
#   filter(!(cod_mun %in% filter))
# 
# df_collapse <- df %>%
#   filter(ano<=2005) %>%
#   group_by(ano, region) %>%
#   summarize(imr = weighted.mean(tx_mi,pop,na.rm = T)) %>%
#   pivot_wider(id_cols = "region",
#               names_from = "ano",
#               values_from = "imr") %>% 
#   ungroup() %>% 
#   as.data.frame()
# write.xlsx2(df_collapse, file = paste0(dir,"data/SIM_corrections/","sim_correct.xlsx"),sheetName = "remove_imr_1k_plus",row.names = F,append = T)


# adjust municipalities with mortality greater than births

# df <- df %>%
#   mutate(tx_mi = ifelse(tx_mi>1000,1000,tx_mi))
# 
# df_collapse <- df %>%
#   filter(ano<=2005) %>%
#   group_by(ano, region) %>%
#   summarize(imr = weighted.mean(tx_mi,pop,na.rm = T)) %>%
#   pivot_wider(id_cols = "region",
#               names_from = "ano",
#               values_from = "imr") %>%
#   ungroup() %>%
#   as.data.frame()
# write.xlsx2(df_collapse, file = paste0(dir,"data/SIM_corrections/","sim_correct.xlsx"),sheetName = "adj_imr_1k_plus",row.names = F,append = T)
# 

# 
# df_collapse <- df %>%
#   filter(ano<=2005) %>%
#   group_by(ano, region) %>%
#   summarize(imr = weighted.mean(tx_mi,pop,na.rm = T)) %>%
#   pivot_wider(id_cols = "region",
#               names_from = "ano",
#               values_from = "imr") %>% 
#   ungroup() %>% 
#   as.data.frame()
# write.xlsx2(df_collapse, file = paste0(dir,"data/SIM_corrections/","sim_correct.xlsx"),sheetName = "outliers_5",row.names = F,append = T)
# 



# # remove municipalities with big population and 0 mortaliy in the 1990s
# filter <- df %>% 
#   filter(ano<2000) %>% 
#   filter(tx_mi==0) %>%
#   filter(pop>10000) %>% 
#   select(cod_mun) %>% 
#   unique() %>% 
#   pull()
# 
# df <- df %>%
#   filter(!(cod_mun %in% filter))
# 
# 
# df_collapse <- df %>% 
#   filter(ano<=2005) %>% 
#   group_by(ano, region) %>% 
#   summarize(imr = weighted.mean(tx_mi,pop,na.rm = T)) %>% 
#   pivot_wider(id_cols = "region",
#               names_from = "ano",
#               values_from = "imr") %>% 
#   ungroup() %>% 
#   as.data.frame()
# write.xlsx2(df_collapse, file = paste0(dir,"data/SIM_corrections/","sim_correct.xlsx"),sheetName = "big_mun_0",row.names = F,append = T)
# 


# alternative outliers removal (median and MAD)
df <- df %>% 
  # mutate(tx_mi2 = tx_mi) %>% 
  # mutate(tx_mi2 = ifelse(tx_mi2==0,0.2,tx_mi2)) %>% 
  mutate(ln_tx_mi = log(tx_mi)) %>% 
  mutate(ln_tx_mi = ifelse(is.infinite(ln_tx_mi),NA,ln_tx_mi)) %>% 
  select(ln_tx_mi, everything()) %>% 
  # group_by(cod_mun) %>% 
  group_by(ano) %>% 
  mutate(median = median(ln_tx_mi,na.rm = T),
         mad = mad(ln_tx_mi, na.rm = T),
         mean = mean(ln_tx_mi, na.rm = T),
         sd = sd(ln_tx_mi, na.rm = T)) %>%
  ungroup() %>% 
  mutate(out = 0) %>% 
  # mutate(out = ifelse(ln_tx_mi>(median+2*mad),1,out)) %>% 
  # mutate(out = ifelse(ln_tx_mi<(median-2*mad),1,out)) %>%
  mutate(out = ifelse(ln_tx_mi>(mean+2*sd),1,out)) %>%
  group_by(cod_mun) %>% 
  mutate(out2 = max(out,na.rm = T)) %>% 
  ungroup() %>% 
  select(median,mad,out,out2,everything())

plot_out <- df %>%
  mutate(out = ifelse(is.na(out),0,out)) %>% 
  group_by(ano) %>% 
  summarize(outs = mean(out))

plot_out2 <- df %>% 
  filter(out==1) %>% 
  group_by(ano) %>% 
  summarize(tx_mi_outliers = mean(tx_mi,na.rm = T))

plot_out <- plot_out %>% 
  left_join(plot_out2, by = "ano")

rm(plot_out2)


plot <- plot_out %>% 
  ggplot(aes(x = ano)) +
  geom_line(size = 1.2, color = "#440154",aes(y = outs)) +
  geom_line(size = 1.2, color = "#21918c",aes(y = tx_mi_outliers/20000)) +
  scale_y_continuous(
    name = "Share of outliers",
    sec.axis = sec_axis(~ . * 20000, name = "Avg. IMR of outliers"),
    breaks = seq(0,0.1,0.02)# Adjust the scale back
  ) +
  scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
  theme_light() +
  labs(x = "Year") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size=10),
        axis.text = element_text(size = 10),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.title.y.left = element_text(color = "#440154"),
        axis.text.y.left = element_text(color = "#440154"),
        axis.title.y.right = element_text(color = "#21918c"),
        axis.text.y.right = element_text(color = "#21918c"),
        panel.grid = element_blank())
ggsave(paste0(dir,main_folder,yearly_folder,"outliers.pdf"),
       plot = plot,
       device = "pdf",
       width = 7, height = 5,
       units = "in")



# unbalanced painel
df1 <- df %>% filter(out==0)

# balanced painel
df2 <- df %>% filter(out2==0)


# Rudi's approach
df <- df %>% 
  group_by(cod_mun) %>% 
  mutate(mean = mean(tx_mi,na.rm = T),
         sd = sd(tx_mi, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(p95 = quantile(sd, probs = 0.95, na.rm = T),
         out3 = 0) %>% 
  mutate(out3 = ifelse(sd>p95,1,out3)) %>% 
  select(mean,sd,out,out2,out3,everything())

# descriptive on samples
desc <- df %>% 
  filter(ano==1996) %>% 
  group_by(out3) %>% 
  summarize(mean = mean(tx_mi, na.rm = T),
            sd = sd(tx_mi,na.rm = T),
            n = n())



plot_out2 <- df %>%
  group_by(ano, out3) %>% 
  summarize(tx_mi = mean(tx_mi, na.rm = T),
            number = n()) %>% 
  mutate(out3 = as.character(out3)) %>% 
  mutate(out3 = ifelse(out3==1,"1. Outliers Sample (95 %ile of within Municipality SD)","2. Rest of the Sample"))



plot <- plot_out2 %>% 
  ggplot(aes(x = ano, y = tx_mi, color = out3)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
  theme_light() +
  labs(x = "Year",
       y = "Average IMR") +
  # scale_color_viridis_d() +
  scale_color_manual(values = c("#440154","#21918c")) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size=10),
        axis.text = element_text(size = 10),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
ggsave(paste0(dir,main_folder,yearly_folder,"outliers.pdf"),
       plot = plot,
       device = "pdf",
       width = 7, height = 5,
       units = "in")



plot_out3 <- df %>% 
  mutate(share = 0) %>% 
  mutate(share = ifelse(tx_mi>mean+2*sd,1,share)) %>% 
  group_by(ano,out3) %>% 
  summarize(share = mean(share,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(line = ifelse(out3==0,
                       "if IMR > Mean + 2 SD (within municipalities)",
                       "if IMR > Mean + 2 SD (within municipalities), and SD > %ile 95"))


plot <- plot_out3 %>% 
  ggplot(aes(x = ano, y = share, color = line, group = line)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
  theme_light() +
  labs(x = "Year",
       y = "Share of outliers") +
  # scale_color_viridis_d() +
  scale_color_manual(values = c("#440154","#21918c")) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size=10),
        axis.text = element_text(size = 10),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
ggsave(paste0(dir,main_folder,yearly_folder,"outliers2.pdf"),
       plot = plot,
       device = "pdf",
       width = 7, height = 5,
       units = "in")






# balanced painel
df3 <- df %>% filter(out3==0)

# 
# 
# # removing outliers (as in spending data)
# outliers <- df %>%
#   mutate(s = log(tx_mi)) %>%
#   select(s,everything()) %>%
#   mutate(s = ifelse(is.infinite(s),NA,s))
# 
# ndesv <- 3
# x <- mean(outliers$s, na.rm = T)
# sd <- sd(outliers$s, na.rm = T)
# outliers <- outliers %>%
#   mutate(s1 = x - sd * ndesv,
#          s2 = x + sd * ndesv) %>%
#   # filter(s<=s1 | s>=s2) %>%
#   filter(s>=s2) %>%
#   select(cod_mun) %>%
#   unique() %>%
#   pull()
# 
# 
# df1 <- df %>%
#   filter(!(cod_mun %in% outliers))
# 
# 
# 
# # removing outliers states with data issues (AM, MG, PB, PI)
# df2 <- df %>%
#   filter(uf!="AM" & uf!="MG" & uf!="PB" & uf!="PI")
# 
# 
# # removing outliers states with data issues (AM, MG, PB, PI), only the years with issues
# df <- df %>% 
#   mutate(id = row_number())
# 
# remove1 <- df %>% 
#   filter(uf=="AM" & ano < 1997) %>% 
#   select(id) %>% 
#   pull()
# 
# remove2 <- df %>% 
#   filter(uf=="MG" & ano < 1998) %>% 
#   select(id) %>% 
#   pull()
# 
# remove3 <- df %>% 
#   filter(uf=="PB" & ano < 1998) %>% 
#   select(id) %>% 
#   pull()
# 
# remove4 <- df %>% 
#   filter(uf=="PI" & ano < 1998) %>% 
#   select(id) %>% 
#   pull()
# 
# remove <- c(remove1,remove2,remove3,remove4)
# 
# 
# df3 <- df %>%
#   filter(!(id %in% remove))
# 
# 




reduced_yearly_imr <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  
  
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
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_binary"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec))
  
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    t <- table[i,]
    t <- t %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!var_name := estimate)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  out <- table %>% filter(!is.nan(estimate))
  assign("table", table, envir = .GlobalEnv)
  
}

reduced_yearly_ab_imr <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  
  
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
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_ab),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary_ab"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(3:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(18:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec))
  
  
  tableA <- table %>% filter(target=="Above")
  tableB <- table %>% filter(target=="Below")
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    tA <- tableA[i,]
    tA <- tA %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Above)") := estimate)
    
    tB <- tableB[i,]
    tB <- tB %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Below)") := estimate)
    
    t <- tA %>% bind_cols(tB)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  out <- table %>% filter(!is.nan(estimate))
  assign("table", table, envir = .GlobalEnv)
  
  
}


reduced_yearly_imr2 <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  # df_reg <- df
  df_reg <- df %>% 
    filter(cod_uf!=22 & cod_uf!=23 & cod_uf!=29)
  
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
    select(ano,cod_mun,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter) %>% 
    select(-finbra_desp_saude_san_pcapita_neighbor,-lrf) %>% 
    mutate(pbf_pcapita = ifelse(ano<1998 & is.na(pbf_pcapita),0,pbf_pcapita))
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_binary"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table <- fit %>% 
    broom::tidy() %>%
    slice(1:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           year = seq.int(year_filter,2010),
           spec = as.character(spec))
  
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    t <- table[i,]
    t <- t %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!var_name := estimate)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  
  
  
  # graph with now bounds defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  
  # graph <- table %>% filter(!is.nan(estimate)) %>% 
  #   ggplot(aes(x = year, y = estimate))+
  #   geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
  #   geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
  #   geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
  #   geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.1) +
  #   geom_ribbon(aes(ymin = lb2, ymax = ub2),color = NA, alpha = 0.2) +
  #   scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
  #   # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
  #   theme_light() +
  #   labs(y = var_name,
  #        x = "Year",
  #        shape = "Specification") +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         plot.title = element_text(size = 10, face = "bold"),
  #         axis.title.x = element_text(size=10),
  #         axis.title.y = element_text(size=ylabel),
  #         axis.text = element_text(size = 10),
  #         legend.position="bottom")
  # 
  # 
  # 
  # ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
  #        plot = graph,
  #        device = "png",
  #        width = 7, height = 5,
  #        units = "in")
  # ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
  #        plot = graph,
  #        device = "pdf",
  #        width = 7, height = 5,
  #        units = "in")
  
  
  out <- table %>% filter(!is.nan(estimate))
  assign("table", table, envir = .GlobalEnv)
  
  
  # return(data.frame(year= table$year, estimates=table$estimate, lb=table$lb, 
  #                   ub=table$ub, lb2=table$lb2, ub2=table$ub2))
  # 
  
  
  
}

reduced_yearly_ab_imr2 <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont,spec=3){
  
  
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
    select(ano,cod_mun,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies_ab),all_of(yeartreat_dummies_binary),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,reweight,reweightPop) %>% 
    filter(ano>=year_filter) %>% 
    select(-finbra_desp_saude_san_pcapita_neighbor,-lrf) %>% 
    mutate(pbf_pcapita = ifelse(ano<1998 & is.na(pbf_pcapita),0,pbf_pcapita))
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr_ab"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary_ab"))
    
  }
  weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(1:15) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(16:30) %>%
    select(term,estimate,std.error,p.value) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,7,13)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec))
  
  
  tableA <- table %>% filter(target=="Above")
  tableB <- table %>% filter(target=="Below")
  table_final <- data.frame()
  for(i in 1:nrow(table)){
    tA <- tableA[i,]
    tA <- tA %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Above)") := estimate)
    
    tB <- tableB[i,]
    tB <- tB %>% table_formating(3) %>%
      select(estimate) %>% 
      rename(!!paste0(var_name," (Below)") := estimate)
    
    t <- tA %>% bind_cols(tB)
    
    table_final <- bind_rows(table_final,t)
  }
  
  table_final <- table_final %>% rbind(fit %>% broom::glance() %>% select(nobs) %>% as.character())
  
  assign("table_final",table_final, envir = .GlobalEnv)
  
  
  # graphs variation
  
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  
  # 
  # graph <- table %>% filter(!is.nan(estimate)) %>% 
  #   ggplot(aes(x = year, y = estimate, color = target, group = target))+
  #   geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
  #   geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
  #   geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
  #   geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
  #   geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
  #   scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
  #   # scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
  #   scale_color_manual(values = colors) +
  #   theme_light() +
  #   labs(y = var_name,
  #        x = "Year",
  #        shape = "Specification") +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         plot.title = element_text(size = 10, face = "bold"),
  #         axis.title.x = element_text(size=10),
  #         axis.title.y = element_text(size=ylabel),
  #         axis.text = element_text(size = 10),
  #         legend.position="bottom",
  #         legend.title = element_blank())
  # 
  # 
  # 
  # ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
  #        plot = graph,
  #        device = "png",
  #        width = 7, height = 5,
  #        units = "in")
  # ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
  #        plot = graph,
  #        device = "pdf",
  #        width = 7, height = 5,
  #        units = "in")
  #   
  
  out <- table %>% filter(!is.nan(estimate))
  assign("table", table, envir = .GlobalEnv)
  
  
  # return(data.frame(year= table$year, estimates=table$estimate, lb=table$lb, 
  #                   ub=table$ub, lb2=table$lb2, ub2=table$ub2, 
  #                   target=table$target))
  # 
}



for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr2(var,var_name,df3,3,1996,-100,200,20,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  table1 <- table %>% 
    mutate(sample = "2. Remove Outliers (95 %ile of within Municipality SD)")
  
  reduced_yearly_imr(var,var_name,df3 %>% filter(ano>1997),3,1998,-100,200,20,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  table2 <- table %>% 
    mutate(sample = "3. Benchmark w/o Outliers")
  
  reduced_yearly_imr(var,var_name,df %>% filter(ano>1997),3,1998,-100,200,20,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  table3 <- table %>% 
    mutate(sample = "1. Benchmark (main regression)")
  
  table_final <- bind_rows(table1, table2, table3)
  rm(table1, table2, table3)
  
  
  graph <- table_final %>% 
    ggplot(aes(x=year, y=estimate, color = sample,linetype = sample)) + 
    geom_line(size=1.2) +
    geom_errorbar(aes(ymin = lb2, ymax = ub2), width=0.1, color = "gray", position = position_dodge(width=0.4)) +
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    theme_light() +
    scale_color_viridis_d() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank(),
          panel.grid = element_blank()) +
    guides(color=guide_legend(nrow=2,byrow=TRUE))
  ggsave(paste0(dir,main_folder,yearly_folder,"1_cont_level_",i,"_",var,"_","robust.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  graph <- table_final %>% 
    ggplot(aes(x=year, y=estimate, color = sample)) + 
    # geom_point(size=1.2,position = position_dodge(width=0.4)) +
    geom_pointrange(aes(ymin = lb2, ymax = ub2), size=0.3, position = position_dodge(width=0.8),shape=15) +
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    theme_light() +
    scale_color_viridis_d() +
    labs(y = var_name,
         x = "Year",
         color = "Specification") +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank(),
          panel.grid = element_blank()) +
    guides(shape=guide_legend(nrow=2,byrow=TRUE),
           color=guide_legend(nrow=2,byrow=TRUE)) 
  ggsave(paste0(dir,main_folder,yearly_folder,"1_cont_level_",i,"_",var,"_","robust_2.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  # separate by sample
  graph <- table_final %>% filter(sample=="1. Benchmark (main regression)") %>% 
    ggplot(aes(x = year, y = estimate))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
    geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    scale_y_continuous(breaks = seq(-30,30,10), limits = c(-30,30)) +
    theme_light() +
    labs(y = "Infant Mortality Rate",
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=10),
          plot.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 10),
          legend.position="bottom")
  
  ggsave(paste0(dir,main_folder,yearly_folder,"1_cont_level_",i,"_",var,"_","robust_sample1.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  graph <- table_final %>% filter(sample=="2. Remove Outliers (95 %ile of within Municipality SD)") %>% 
    ggplot(aes(x = year, y = estimate))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
    geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    scale_y_continuous(breaks = seq(-30,30,10), limits = c(-30,30)) +
    theme_light() +
    labs(y = "Infant Mortality Rate",
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=10),
          plot.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 10),
          legend.position="bottom")
  
  ggsave(paste0(dir,main_folder,yearly_folder,"1_cont_level_",i,"_",var,"_","robust_sample2.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  graph <- table_final %>% filter(sample=="3. Benchmark w/o Outliers") %>% 
    ggplot(aes(x = year, y = estimate))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1.2, alpha = 1,color = "grey20",shape=0,stroke = 0.8) +
    geom_ribbon(aes(ymin = lb, ymax = ub),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    scale_y_continuous(breaks = seq(-30,30,10), limits = c(-30,30)) +
    theme_light() +
    labs(y = "Infant Mortality Rate",
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=10),
          plot.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 10),
          legend.position="bottom")
  
  ggsave(paste0(dir,main_folder,yearly_folder,"1_cont_level_",i,"_",var,"_","robust_sample3.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
}


for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_ab_imr2(var,var_name,df3,3,1996,-20,20,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  table1 <- table %>% 
    mutate(sample = "2. Remove Outliers (95 %ile of within Municipality SD)")
  
  reduced_yearly_ab_imr(var,var_name,df3 %>% filter(ano>1997),3,1998,-20,20,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  table2 <- table %>% 
    mutate(sample = "3. Benchmark w/o Outliers")
  
  reduced_yearly_ab_imr(var,var_name,df %>% filter(ano>1997),3,1998,-20,20,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1) # ec29baseline
  table3 <- table %>% 
    mutate(sample = "1. Benchmark (main regression)")
  
  table_final <- bind_rows(table1, table2, table3)
  rm(table1, table2, table3)
  
  
  graph <- table_final %>% 
    ggplot(aes(x=year, y=estimate, color = target,linetype = sample)) + 
    geom_line(size=0.8) +
    geom_errorbar(aes(ymin = lb2, ymax = ub2), width=0.1, position = position_dodge(width=0.3)) +
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         color = "target",
         linetype = 'sample') +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 9),
          panel.grid = element_blank()) +
    guides(linetype=guide_legend(nrow=2,byrow=TRUE),
           color=guide_legend(nrow=2,byrow=TRUE))
  ggsave(paste0(dir,main_folder,yearly_folder,"2_ab_level_",i,"_",var,"_","robust.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  graph <- table_final %>% 
    ggplot(aes(x=year, y=estimate, color = target, linetype = sample)) + 
    # geom_point(size=1.2,position = position_dodge(width=0.4)) +
    geom_pointrange(aes(ymin = lb2, ymax = ub2), size=0.2, position = position_dodge(width=0.9),shape=15) +
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         color = "target",
         linetype = "spec") +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank(),
          panel.grid = element_blank()) +
    guides(linetype=guide_legend(nrow=2,byrow=TRUE),
           color=guide_legend(nrow=2,byrow=TRUE)) 
  ggsave(paste0(dir,main_folder,yearly_folder,"2_ab_level_",i,"_",var,"_","robust_2.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  # separate by sample
  graph <- table_final %>% filter(sample=="1. Benchmark (main regression)") %>% 
    ggplot(aes(x = year, y = estimate, color = target, group = target))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    scale_y_continuous(breaks = seq(-40,60,20), limits = c(-42,67)) +
    # scale_color_manual(values = colors) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank())
  
  ggsave(paste0(dir,main_folder,yearly_folder,"2_ab_level_",i,"_",var,"_","robust_sample1.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  
  graph <- table_final %>% filter(sample=="2. Remove Outliers (95 %ile of within Municipality SD)") %>% 
    ggplot(aes(x = year, y = estimate, color = target, group = target))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    scale_y_continuous(breaks = seq(-40,60,20), limits = c(-42,67)) +
    # scale_color_manual(values = colors) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank())
  
  ggsave(paste0(dir,main_folder,yearly_folder,"2_ab_level_",i,"_",var,"_","robust_sample2.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
  graph <- table_final %>% filter(sample=="3. Benchmark w/o Outliers") %>% 
    ggplot(aes(x = year, y = estimate, color = target, group = target))+
    geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
    geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.2) +
    scale_x_continuous(breaks = seq(1996,year_cap,1), limits = c(1995.5,year_cap+0.5)) +
    scale_y_continuous(breaks = seq(-40,60,20), limits = c(-42,67)) +
    # scale_color_manual(values = colors) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          legend.position="bottom",
          legend.title = element_blank())
  
  ggsave(paste0(dir,main_folder,yearly_folder,"2_ab_level_",i,"_",var,"_","robust_sample3.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
}





