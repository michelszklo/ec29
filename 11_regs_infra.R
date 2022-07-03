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

# ------------------------------------


# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))


# 2. Define outcomes output name and output functions
# =================================================================

var_map <- rbind(cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
                 cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
                 cbind('siab_regist_pers_pcapita','N. of People Register in the Primary Care System (per capita)'),
                 cbind('siab_regist_pers_pacs_pcapita','N. of People Register in the CH Program (per capita)'),
                 cbind('siab_regist_pers_psf_pcapita','N. of People Register in the FH Program (per capita)'),
                 cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
                 cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
                 cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
                 cbind('siab_visit_cha_pcapita','N. of Household Visits (per capita)'),
                 cbind('siab_visit_cha_pacs_pcapita','N. of Household Visits by Community Health Agents (per capita)'),
                 cbind('siab_visit_cha_psf_pcapita','N. of Household Visits by Family Health Agents (per capita)'),
                 cbind('siab_cons_especif_pcapita','N. of Appointments (per capita)'),
                 cbind('siab_cons_especif_pacs_pcapita','N. of Appointments from Community Health Program (per capita)'),
                 cbind('siab_cons_especif_pcapita','N. of Appointments from Family Health Program (per capita)')
)



adjust_years <- function(df){
  df <- df
}

df <- df %>% adjust_years()
df_below <- df_below %>% adjust_years()
df_above <- df_above %>% adjust_years()


# 3. Run and ouput
# =================================================================
df <- df %>%
  filter(ano<=2010) %>%
  mutate(iv=ifelse(ano<=2000,0,iv),
         iv_firstterm = ifelse(ano<=2000,0,iv_firstterm),
         firstterm = ifelse(ano<=2000,0,firstterm)) 




for (i in 1:14){
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


# exporting results
# ---------------------

write.xlsx2(df_table_all %>% as.data.frame(), file = paste0(dir,main_folder,output_file),sheetName = "infra",row.names = F,append = T)


