#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public spending
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


# creating above and below target sample

df_above = df %>% filter(ec29_baseline>=0.15)
df_below = df %>% filter(ec29_baseline<0.15)

df_second <- df_above
df_first <- df_below


# 2. Define outcomes output name and output functions
# =================================================================

var_map <- rbind(cbind('finbra_recorc_pcapita','Total Revenue per capita (2010 R$)'),
                 cbind('finbra_reccorr_pcapita','Current Revenue per capita (2010 R$)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                 cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (2010 R$)'),
                 cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (2010 R$)'),
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'))


# 3. Run and ouput
# =================================================================

for (i in seq(1,14,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output(var,var_name,1,1998,"peso_pop")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}




# 4. exporting results
# =================================================================

main_folder <- "regs_outputs/finbra_check/"

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "log",row.names = F,append = T)




# 5. Run and ouput ABOVE BELOW, SAME REG
# =================================================================

rm(df_table_all)

# new specs and functions
spec1_post <- paste(" ~ ","iv_a + iv_b"," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_post <- paste(" ~ ","iv_a + iv_b"," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_post <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_post <- paste(" ~ ","iv_a + iv_b"," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

table_formating_above_below <- function(df,s,ab){
  df <- df %>% 
    filter(spec==s) %>%
    filter(sample==ab) %>% 
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
                    rename(estimate = std.error)) %>% 
    mutate(sample = ab)
}

reduced_abovebelow <- function(outcome,var_name,df,regression_output,transform,year_filter,weight){
  
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
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,all_of(controls),pop,
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  df_reg <- df_reg %>%
    mutate(iv_a = ifelse(iv<=0,iv,0),
           iv_b = ifelse(iv>0,iv,0)) %>% 
    mutate(iv_a = -iv_a)
  
  
  # Regressions
  # ------------------------------------
  
  # for (spec in c(1)){
  for (spec in c(1,2,3,4)){
    
    spec_reduced<- get(paste0("spec",spec,"_post"))
    
    weight_vector <- df_reg[weight] %>% unlist() %>% as.numeric()
    
    # second stage regression
    # ------------------------------
    
    regformula <- as.formula(paste(ln_outcome,spec_reduced))
    fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
    
    out <- cbind(fit %>% broom::tidy() %>% slice(1:2),fit %>% broom::glance() %>% select(nobs))
    
    
    out <- cbind(out,spec)
    
    if(spec==1){
      table <- out
    }
    else{
      table <- rbind(table,out)
    }
    
    
    
  }
  
  table <- table %>% mutate(sample = ifelse(term=="iv_a",paste0("above"),paste0("below")),
                            term = ln_outcome)
  
  assign(regression_output,table, envir = .GlobalEnv)
  
  
}

regress_output_abovebelow <- function(var,var_name,transform,year_filter,weight){
  
  # FULL SAMPLE
  # ----------------------------------------
  
  # loop through full database and subsamples
  for (data in c("df")){
    
    d <- get(data)
    obj <- paste0("reg_",data) # name of the output object
    
    reduced_abovebelow(var,var_name,d,obj,transform,year_filter,weight = weight) # function for reduced form regression
    
    print(paste0("Regs for sample ",data))
  } 
  
  # 2sls final tables
  
  organizing_table <- function(d,sample_name,withspec){
    
    obs_1 <- d %>% slice(1) %>% select(nobs) %>% as.numeric()
    obs_2 <- d %>% slice(2) %>% select(nobs) %>% as.numeric()
    obs_3 <- d %>% slice(3) %>% select(nobs) %>% as.numeric()
    obs_4 <- d %>% slice(4) %>% select(nobs) %>% as.numeric()
    
    obs_name <- paste0("obs_",sample_name)
    
    
    
    table_1_a <- d  %>% table_formating_above_below(1,"above") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1)
    table_1_b <- d  %>% table_formating_above_below(1,"below") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_1)
    table_1 <- bind_rows(table_1_a,table_1_b) %>% mutate(spec=1)
    
    table_2_a <- d%>% table_formating_above_below(2,"above") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2)
    table_2_b <- d %>% table_formating_above_below(2,"below") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_2)
    table_2 <- bind_rows(table_2_a,table_2_b) %>% mutate(spec=2)
    
    table_3_a <- d %>% table_formating_above_below(3,"above") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3)
    table_3_b <- d %>% table_formating_above_below(3,"below") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_3)
    table_3 <- bind_rows(table_3_a,table_3_b) %>% mutate(spec=3)
    
    table_4_a <- d %>% table_formating_above_below(4,"above") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4)
    table_4_b <- d %>% table_formating_above_below(4,"below") %>% rename(!!sample_name := "estimate") %>% mutate(!!obs_name := obs_4)
    table_4 <- bind_rows(table_4_a,table_4_b) %>% mutate(spec=4)
    
    
    name1 <- paste0("table_1_",sample_name)
    name2 <- paste0("table_2_",sample_name)
    name3 <- paste0("table_3_",sample_name)
    name4 <- paste0("table_4_",sample_name)
    
    assign(name1,table_1, envir = parent.frame()) 
    assign(name2,table_2, envir = parent.frame()) 
    assign(name3,table_3, envir = parent.frame()) 
    assign(name4,table_4, envir = parent.frame()) 
    
    
  }
  
  
  organizing_table(reg_df,"all",0)
  
  
  tables <- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
  
  binding <- function(name){
    
    output <- paste0("table_",name)
    for(i in grep(name,tables,value = T)){
      
      first <- grep("1",i,value = T)
      d <- get(i)
      
      if(length(first>0)){
        df <- d
      } else {
        df <- bind_rows(df,d)
      }
      
    }  
    
    assign(output,df,envir = parent.frame())
    
  }
  
  binding("all")
  
  
  # assigning objects to the global envir
  assign("table_all",table_all, envir = .GlobalEnv) 
  
}






for (i in seq(1,14,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  
  regress_output_abovebelow(var,var_name,3,1998,"peso_pop")
  
  
  if(exists("df_table_all")){
    df_table_all <- rbind(df_table_all,table_all)
    
  } else {
    
    df_table_all <- table_all
    
  }
  
}




# 6. exporting results
# =================================================================

main_folder <- "regs_outputs/finbra_check/"

write.xlsx2(df_table_all, file = paste0(dir,main_folder,output_file),sheetName = "level_ab",row.names = F,append = T)






