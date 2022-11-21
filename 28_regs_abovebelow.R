#######################################################################################################
# Author: Michel Szklo
# October 2022
# 
# This scripts runs regressions with discrete treatment variable
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


# choose step

# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))

# creating discrete treatment variable
all_df <- c("df","df_low_ineq","df_high_ineq","df_low_pov","df_high_pov","df_low_hi","df_high_hi","df_first","df_second")

discrete <- function(df){
  
  df <- df %>% 
    mutate(iv_0 = ifelse(dist_ec29_baseline>-0.025 & dist_ec29_baseline<=0 & ano>2000,0,0),
           iv_a = ifelse(dist_ec29_baseline<=-0.025 & ano>2000,1,0),
           iv_b = ifelse(dist_ec29_baseline>0 & ano>2000,1,0))
}
  
  

for(d in all_df){
  df_add <- get(d)
  df_add <- df_add %>% discrete()
  assign(d,df_add,envir = .GlobalEnv)
}

discrete <- grep("^iv_",names(df),value = T)


# merging indexes
index <- data.frame(read.dta13("C:/Users/Michel/Documents/GitHub/ec29/indexes.dta"))


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


# 2. Regression specs
# =================================================================

# spec1_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec2_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec3_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
# spec4_post <- paste(" ~ ","iv_m4 + iv_m3 + iv_m2 + iv_m1 + iv_0 + iv_p1 + iv_p2 + iv_p3"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


spec1_post_d <- paste(" ~ ",paste(discrete,collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_d <- paste(" ~ ",paste(discrete,collapse = " + ")," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_d <- paste(" ~ ",paste(discrete,collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_d <- paste(" ~ ",paste(discrete,collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



spec1_post_imr_d <- paste(" ~ ",paste(discrete,collapse = " + ")," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post_imr_d <- paste(" ~ ",paste(discrete,collapse = " + ")," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post_imr_d <- paste(" ~ ",paste(discrete,collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post_imr_d <- paste(" ~ ",paste(discrete,collapse = " + ")," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


# 2. Regression formulas
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),
           all_of(discrete),
           all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    # discrete treatment
    # ------------------------------
    spec_reduced<- get(paste0("spec",spec,"_post_d"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:3),fit %>% broom::glance() %>% select(nobs))
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(var = var_name)
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),
           all_of(discrete),
           all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  # Regressions
  # ------------------------------------
  
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    # discrete treatment
    # ------------------------------
    spec_reduced<- get(paste0("spec",spec,"_post_imr_d"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:3),fit %>% broom::glance() %>% select(nobs))
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(var = var_name)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
 
}




# 3. Running regs
# =================================================================

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
                 cbind('imr_index','Infant Mortality Index')
                 
)




for (i in seq(1,13,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  reduced(var,var_name,df,"table_all",3,1998,"peso_eq")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}


for (i in seq(14,15,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  reduced(var,var_name,df,"table_all",3,1998,"peso_b")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}


for (i in seq(16,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  reduced_imr(var,var_name,df,"table_all",3,1998,"peso_b")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}


df_table_all <- df_table_all %>% 
  mutate(estimate = ifelse(is.nan(estimate),0,estimate),
         statistic = ifelse(is.nan(statistic),0,statistic),
         p.value = ifelse(is.nan(p.value),0,p.value)) %>% 
  mutate(sig = ifelse(p.value<=0.01,"***",""),
         sig = ifelse(p.value<=0.05 & p.value>0.01,"**",sig),
         sig = ifelse(p.value<=0.1 & p.value>0.05,"*",sig)) %>% 
  mutate(std.error = paste0("(",round(std.error,digits = 3),")"),
         estimate = paste0(round(estimate,digits = 3),sig))
  
  



# 4. Exporting Table
# =================================================================
write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "above_below",row.names = F,append = T)
















