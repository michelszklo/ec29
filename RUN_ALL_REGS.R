#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs ALL regressions SCRIPTS
#
# 
#   BEFORE RUNNING THIS SCRIPT, RUN `08_regs_var_spec` AND SELECT OUTPUT FOLDERS
#   MAKE SURE THAT THE SELECTED FOLDERS ALREADY EXIST.
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

dir <- "C:/Users/Michel/Documents/GitHub/"
dir2 <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------

# 1. Folder, files and instrument setup
# =================================================================

# running_map <- rbind(
#   cbind("regs_outputs/","yearly_reduced_form_ec29/","ec29_baseline",0,"results_ec29_baseline.xlsx"),
#   cbind("regs_outputs/","yearly_reduced_form_ec29_b/","ec29_baseline_below",1,"results_ec29_baseline.xlsx"),
#   cbind("regs_outputs/","yearly_reduced_form_dist_ec29/","dist_ec29_baseline",0,"results_dist_ec29_baseline.xlsx"),
#   cbind("regs_outputs/","yearly_reduced_form_dist_ec29_b/","dist_ec29_baseline_below",1,"results_dist_ec29_baseline.xlsx"),
#   cbind("regs_outputs/","yearly_reduced_form_dist_spending/","dist_spending_pc_baseline",0,"results_dist_spending_baseline.xlsx"),
#   cbind("regs_outputs/","yearly_reduced_form_dist_spending_b/","dist_spending_pc_baseline_below",1,"results_dist_spending_baseline.xlsx"),
#   cbind("regs_outputs/","yearly_reduced_form_dist_spending2/","dist_spending_baseline",0,"results_dist_spending2_baseline.xlsx"),
#   cbind("regs_outputs/","yearly_reduced_form_dist_spending2_b/","dist_spending_baseline_below",1,"results_dist_spending2_baseline.xlsx")
# )


# running_map <- rbind(
#   cbind("regs_outputs/","yearly_reduced_form_dist_ec29/","dist_ec29_baseline",0,"results_dist_ec29_baseline.xlsx")
#   
# )

running_map <- rbind(
  cbind("regs_outputs/","yearly_reduced_form_dist_ec29_elect/","dist_ec29_baseline",0,"results_dist_ec29_baseline_elect.xlsx")
  
)

# running_map <- rbind(
#   cbind("regs_outputs/","yearly_reduced_form_dist_ec29_governance/","dist_ec29_baseline",0,"results_dist_ec29_baseline_governance.xlsx")
#   
# )


# 2. Run all loop
# =================================================================

for(i in 1){
  # for(i in seq.int(5,nrow(running_map))){
  
  rm(list= ls()[!(ls() %in% c("dir","dir2","running_map","i"))])
  
  # Regressions' outputs main folder
  # ------------------------------------
  main_folder <- running_map[i,1]
  print(paste0("Main folder: ", main_folder))
  
  # Reduced form yearly estimates figures folder
  # ------------------------------------
  yearly_folder <- running_map[i,2]
  print(paste0("RF yearly graphs folder: ", yearly_folder))
  
  # Instrumental variable (uncoment the selected one)
  # ------------------------------------
  instrument <- running_map[i,3]
  print(paste0("Instrumental Variable: ", instrument))
  
  # is the instrument restricted to the sample below the target?
  below <- running_map[i,4] %>% as.numeric()
  if(below==0){
    p <- "No"
  } else {
    p <- "Yes"
  }
  print(paste0("Instrument variation restricted to below target sample? ", p))
  
  # Regression output excel file
  # ------------------------------------
  output_file <- running_map[i,5]
  print(paste0("Output file name: ", output_file))
  
  print("")
  print("")
  print("")
  
  # Saving set up
  # ------------------------------------
  save.image(paste0(dir2,"output_setup.RData"))
  
  
  # Run all regressions
  # ------------------------------------
  
  scripts <- "ec29/"
  dir <- "C:/Users/Michel/Documents/GitHub/"
  source(paste0(dir,scripts,"08_regs_vars_specs.R"))
  print("specs script: ok")
  # scripts <- "ec29/"
  # dir <- "C:/Users/Michel/Documents/GitHub/"
  # source(paste0(dir,scripts,"09_regs_firststage.R"))
  # print("first stage: ok")
  scripts <- "ec29/"
  dir <- "C:/Users/Michel/Documents/GitHub/"
  source(paste0(dir,scripts,"10_regs_spending.R"))
  scripts <- "ec29/"
  dir <- "C:/Users/Michel/Documents/GitHub/"
  source(paste0(dir,scripts,"11_regs_infra.R"))
  scripts <- "ec29/"
  dir <- "C:/Users/Michel/Documents/GitHub/"
  source(paste0(dir,scripts,"12_regs_sia.R"))
  scripts <- "ec29/"
  dir <- "C:/Users/Michel/Documents/GitHub/"
  source(paste0(dir,scripts,"14_regs_imr.R"))
  scripts <- "ec29/"
  dir <- "C:/Users/Michel/Documents/GitHub/"
  source(paste0(dir,scripts,"15_regs_amr.R"))
  # scripts <- "ec29/"
  # dir <- "C:/Users/Michel/Documents/GitHub/"
  # source(paste0(dir,scripts,"16_regs_cmr.R"))
  # scripts <- "ec29/"
  # dir <- "C:/Users/Michel/Documents/GitHub/"
  # source(paste0(dir,scripts,"17_regs_emr.R"))
  scripts <- "ec29/"
  dir <- "C:/Users/Michel/Documents/GitHub/"
  source(paste0(dir,scripts,"18_regs_birth.R"))
  # scripts <- "ec29/"=
  # dir <- "C:/Users/Michel/Documents/GitHub/"
  # source(paste0(dir,scripts,"20_regs_cnes_cs.R"))
  
}
