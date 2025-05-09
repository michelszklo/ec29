#######################################################################################################
# Author: Michel Szklo
# September 2022
# 
# This scripts runs regressions for baseline per capita spending interacted with distance
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


# 2. New Regression spec (only for spec 3)
# =================================================================

df <- df %>% 
  mutate(spending_baseline = ifelse(ano==2000,siops_despsaude_pcapita,NA)) %>% 
  group_by(cod_mun) %>% 
  mutate(spending_baseline = mean(spending_baseline, na.rm = T)) %>% 
  mutate(sp = spending_baseline * post) %>% 
  mutate(iv_sp = iv * spending_baseline) %>% 
  ungroup()

# calculating 1st quartile average baseline spending
quartile <- df %>% 
  filter(ano==2000) %>% 
  mutate(quantile = quantcut(siops_pct_recproprios_ec29,q=4,na.rm = T)) %>% 
  select(cod_mun, ano, quantile,spending_baseline) %>% 
  filter(quantile=="[0,0.0921]") %>%
  summarise(mean(spending_baseline,na.rm=T))
  

# Reduce form specification
spec3_post <- paste(" ~ ","iv_sp + iv + sp"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# Reduce form specification for IMR
spec3_post_imr <- paste(" ~ ","iv_sp + iv + sp"," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 3. regs function
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,sp,iv_sp,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  spec <- 3
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:3),fit %>% broom::glance() %>% select(nobs))
    
    
    out <- cbind(out,spec)
    table <- out
    
  }
  
  table <- table %>% mutate(outcome = ln_outcome)
  
  assign(regression_output,table, envir = .GlobalEnv)
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,sp,iv_sp,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  spec <- 3
  for (spec in c(3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_imr"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:3),fit %>% broom::glance() %>% select(nobs))
    
    
    out <- cbind(out,spec)
    table <- out
    
  }
  
  table <- table %>% mutate(outcome = ln_outcome)
  
  assign(regression_output,table, envir = .GlobalEnv)
}

table_formating <- function(df,s){
  df <- df %>% 
    filter(spec==s) %>%
    select(-spec) %>% 
    mutate(term=var_name) %>% 
    mutate(sig = ifelse(p.value<=0.01,"***",""),
           sig = ifelse(p.value<=0.05 & p.value>0.01,"**",sig),
           sig = ifelse(p.value<=0.1 & p.value>0.05,"*",sig)) %>% 
    mutate(std.error = paste0("(",round(std.error,digits = 3),")"),
           estimate = paste0(round(estimate,digits = 3),sig))
  
  df <- bind_rows(df %>%
                    select(term,estimate),
                  df %>% 
                    select(term,std.error) %>% 
                    rename(estimate = std.error))
}  # formats regression outputs into article format

regress_output <- function(var,var_name,transform,year_filter,weight){
  
  # FULL SAMPLE
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in "df"){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  # 2sls final tables
  sample_name <- "all"
  
  obs <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_name <- paste0("obs_",sample_name)
  table <- reg_df  %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs)
  
  
  
  table_all <- bind_cols(table[1:2,1],
                         rbind(table[1,2],table[4,2]) %>% as.data.frame(),
                         rbind(table[2,2],table[5,2]) %>% as.data.frame(),
                         rbind(table[3,2],table[6,2]) %>% as.data.frame(),
                         table[1:2,3]) %>% 
    as.data.frame()
  
  colnames(table_all) <- c("term","dist_spending","dist","spending","obs")
  
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects

regress_output_imr <- function(var,var_name,transform,year_filter,weight){
  
  # FULL SAMPLE
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in "df"){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced_imr(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  # 2sls final tables
  sample_name <- "all"
  
  obs <- reg_df %>% slice(3) %>% select(nobs) %>% as.numeric()
  obs_name <- paste0("obs_",sample_name)
  table <- reg_df  %>% table_formating(3) %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs)
  
  
  table_all <- bind_cols(table[1:2,1],
                         rbind(table[1,2],table[4,2]) %>% as.data.frame(),
                         rbind(table[2,2],table[5,2]) %>% as.data.frame(),
                         rbind(table[3,2],table[6,2]) %>% as.data.frame(),
                         table[1:2,3]) %>% 
    as.data.frame()
  
  names(table_all) <- c("term","dist_spending","dist","spending","obs")
  
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}  # runs regressions and output objects




# 4. Spending Regressions
# =================================================================

var_map <- rbind(cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'))

for (i in seq(1,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998,"peso_eq")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}


# 5. Index Regressions
# =================================================================

var_map <-  rbind(cbind('access_index','Access and Production of Health Services Index'),
                  cbind('access_pc_index','Primary Care Access and Production Index'),
                  cbind('access_npc_index','Non-Primary Care Access and Production Index'),
                  cbind('input_index','Health Inputs Index'),
                  cbind('hr_index','Human Resources Index'),
                  cbind('hospital_index','Hospitals Index'),
                  cbind('birth_index','Birth Outcomes Index'),
                  cbind('birth_others_index','Other Birth Outcomes Index'),
                  cbind('imr_index','Infant Mortality Index')
)

for (i in seq(1,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998,"peso_eq")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}

for (i in seq(7,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,3,1998,"peso_b")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}

for (i in seq(9,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output_imr(var,var_name,3,1998,"peso_b")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}



# 6. exporting results
# =================================================================

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "interaction",row.names = F,append = T)



# 7. dynamic regressions function
# =================================================================

# creating yearly dummies interacted with spending and distance to EC29
yeardummies <- grep("^ano_",names(df),value = T)
dummies <- c(grep("^pre",names(df),value = T),grep("^post_",names(df), value = T))
dummies <- dummies[grep("dist",dummies,invert = T)]
sp_dummies <- sapply(dummies, function(x) paste0(x,"_","sp"), simplify = "array", USE.NAMES = F)
iv_sp_dummies <- sapply(dummies, function(x) paste0(x,"_","iv_sp"), simplify = "array", USE.NAMES = F)

df[sp_dummies] <- df[dummies]
df[iv_sp_dummies] <- df[dummies]

df <- df %>% 
  mutate_at(sp_dummies, `*`,quote(spending_baseline)) %>% 
  unnest(all_of(sp_dummies)) %>% 
  mutate_at(iv_sp_dummies, `*`,quote(spending_baseline)) %>% 
  mutate_at(iv_sp_dummies, `*`,quote(dist_ec29_baseline)) %>%
  unnest(all_of(iv_sp_dummies))

df <- df %>% 
  mutate(post_00_sp = 0,
         post_00_iv_sp = 0,
         post_00_dist_ec29_baseline = 0)




# specifications
all_treat_dummies <- c(iv_sp_dummies,yeartreat_dummies,sp_dummies)

spec1_post_y <- paste(" ~ ",paste(all_treat_dummies, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y <- paste(" ~ ",paste(all_treat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

spec1_post_y_imr <- paste(" ~ ",paste(all_treat_dummies, collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_y_imr <- paste(" ~ ",paste(all_treat_dummies, collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



y0 <- -0.02
yf <- 0.02
ys <- 0.005

# regressions' graph functions
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
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(all_treat_dummies),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
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
      mutate(spec = ifelse(spec=="1","Baseline",spec),
             spec = ifelse(spec=="3","+ Baseline and Time Varying Controls",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Baseline","+ Baseline and Time Varying Controls"))  
    
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
  shapes <-  c(15,19)
  
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

reduced_yearly_imr <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,sample,below,weight,year_cap,label_size){
  
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),all_of(all_treat_dummies),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  for (spec in c(1,3)){
    
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
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
      mutate(spec = ifelse(spec=="1","Baseline",spec),
             spec = ifelse(spec=="3","+ Baseline and Time Varying Controls",spec)) %>% 
      mutate(spec = as.factor(spec)) 
    
    out$spec <- factor(out$spec,levels = c("Baseline","+ Baseline and Time Varying Controls"))  
    
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
  shapes <-  c(15,19)
  
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



# graphs
var_map <- rbind(cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'),
                 
                 
                 cbind('access_index','Access and Production of Health Services Index'),
                 cbind('access_pc_index','Primary Care Access and Production Index'),
                 cbind('access_npc_index','Non-Primary Care Access and Production Index'),
                 cbind('input_index','Health Inputs Index'),
                 cbind('hr_index','Human Resources Index'),
                 cbind('hospital_index','Hospitals Index'),
                 cbind('birth_index','Birth Outcomes Index'),
                 cbind('birth_others_index','Other Birth Outcomes Index'),
                 cbind('imr_index','Infant Mortality Index'))




for (i in seq(1,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-2,2.5,0.5,"22",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}


for (i in seq(8,10,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.015,0.015,0.005,"22",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

for (i in seq(11,13,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0,pre_99_iv_sp=0,pre_99_sp=0),3,1998,-0.015,0.015,0.005,"22",below = below,weight = "peso_eq",year_cap = 2010) # ec29baseline
}

for (i in seq(14,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly(var,var_name,df,3,1998,-0.015,0.015,0.005,"22",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}

for (i in seq(16,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-0.015,0.015,0.005,"22",below = below,weight = "peso_b",year_cap = 2010) # ec29baseline
}


