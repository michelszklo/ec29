#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts inputs CNES data and runs regressions for birth outcomes
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

reelect <- read.csv(paste0(dir,"data/TSE/reelect.csv"), encoding = "UTF-8") 
  


# 2. Reelection data
# =================================================================

df_reelect <- reelect %>% 
  mutate(ano=2004) %>% 
  left_join(df, by = c("cod_mun","ano")) %>% 
  select(cod_mun,ano,reelect,pop,iv,all_of(controls))



# 3. regression specs
# =================================================================

spec1 <- as.formula("reelect ~ iv")
spec2 <- as.formula(paste("reelect ~ iv + ",paste(baseline_controls, collapse = " + ")))
spec3 <- as.formula(paste("reelect ~ iv + ",paste(c(baseline_controls,tvarying_controls), collapse = " + ")))
spec4 <- as.formula(paste("reelect ~ iv + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")))



# 4. regs models
# =================================================================


table_formating_elect <- function(df,s){
  df <- df %>% 
    filter(spec==s) %>%
    select(-spec) %>% 
    # mutate(term=var_name) %>% 
    mutate(sig = ifelse(p.value<=0.01,"***",""),
           sig = ifelse(p.value<=0.05 & p.value>0.01,"**",sig),
           sig = ifelse(p.value<=0.1 & p.value>0.05,"*",sig)) %>% 
    mutate(std.error = paste0("(",round(std.error,digits = 3),")"),
           estimate = paste0(round(estimate,digits = 3),sig),
           nobs = as.character(nobs))
  
  df <- bind_rows(df %>%
                    select(term,estimate),
                  df %>% 
                    select(term,std.error) %>% 
                    rename(estimate = std.error),
                  df %>% 
                    select(term,nobs) %>% 
                    rename(estimate = nobs))
}  # formats regression outputs into article format


# full sample

for(i in seq.int(1,4)){
  
  spec <- get(paste0("spec",i))
  fit <- glm(spec,
             family = binomial(link = "probit"), 
             data = df_reelect)
  
  out <- cbind(fit %>% broom::tidy() %>% slice(2), fit %>% broom::glance() %>% select(nobs)) %>% 
    mutate(spec = i)
  
  if(i==1){
    table <- out
  }
  else{
    table <- rbind(table,out)
  }
  
}


table_final <- bind_cols(table %>% table_formating_elect(1) %>% rename(spec1=estimate),
                         table %>% table_formating_elect(2) %>% rename(spec2=estimate) %>% select(-term),
                         table %>% table_formating_elect(3) %>% rename(spec3=estimate) %>% select(-term),
                         table %>% table_formating_elect(4) %>% rename(spec4=estimate) %>% select(-term))

# pop<=50,000

for(i in seq.int(1,4)){
  
  spec <- get(paste0("spec",i))
  fit <- glm(spec,
             family = binomial(link = "probit"), 
             data = df_reelect %>% filter(pop<=50000))
  
  out <- cbind(fit %>% broom::tidy() %>% slice(2), fit %>% broom::glance() %>% select(nobs)) %>% 
    mutate(spec = i)
  
  if(i==1){
    table <- out
  }
  else{
    table <- rbind(table,out)
  }
  
}


table_final_50 <- bind_cols(table %>% table_formating_elect(1) %>% rename(spec1=estimate),
                         table %>% table_formating_elect(2) %>% rename(spec2=estimate) %>% select(-term),
                         table %>% table_formating_elect(3) %>% rename(spec3=estimate) %>% select(-term),
                         table %>% table_formating_elect(4) %>% rename(spec4=estimate) %>% select(-term))


# pop<=50,000

for(i in seq.int(1,4)){
  
  spec <- get(paste0("spec",i))
  fit <- glm(spec,
             family = binomial(link = "probit"), 
             data = df_reelect %>% filter(pop<=30000))
  
  out <- cbind(fit %>% broom::tidy() %>% slice(2), fit %>% broom::glance() %>% select(nobs)) %>% 
    mutate(spec = i)
  
  if(i==1){
    table <- out
  }
  else{
    table <- rbind(table,out)
  }
  
}


table_final_30 <- bind_cols(table %>% table_formating_elect(1) %>% rename(spec1=estimate),
                            table %>% table_formating_elect(2) %>% rename(spec2=estimate) %>% select(-term),
                            table %>% table_formating_elect(3) %>% rename(spec3=estimate) %>% select(-term),
                            table %>% table_formating_elect(4) %>% rename(spec4=estimate) %>% select(-term))



# exporting results
# ---------------------

write.xlsx2(table_final, file = paste0(dir,main_folder,output_file),sheetName = "reelect",row.names = F,append = T)



