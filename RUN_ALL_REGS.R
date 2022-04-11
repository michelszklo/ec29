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

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------

# 1. Folder, files and instrument setup
# =================================================================

# Regressions' outputs main folder
# ------------------------------------
main_folder <- "regs_outputs/"

# Reduced form yearly estimates figures folder
# ------------------------------------
yearly_folder <- "yearly_reduced_form/"

# Instrumental variable (uncoment the selected one)
# ------------------------------------

instrument <- "ec29_baseline"
# instrument <- "ec29_baseline_below"
# instrument <- "dist_ec29_baseline"
# instrument <- "dist_ec29_baseline_below"
# instrument <- "dist_spending_pc_baseline"
# instrument <- "dist_spending_pc_baseline_below"

# is the instrument restricted to the sample below the target?
below <- 0

# Regression output excel file
# ------------------------------------
output_file <- "results_ec29_baseline.xlsx"


save.image(paste0(dir,"output_setup.RData"))



# 2. Run all
# =================================================================

scripts <- "ec29/"
source(paste0(dir,scripts,"08_regs_vars_specs.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"09_regs_firststage.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"10_regs_spending.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"11_regs_infra.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"12_regs_sia.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"13_regs_hr.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"14_regs_imr.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"15_regs_amr.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"16_regs_imr_lag.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"17_regs_amr_lag.R"))
scripts <- "ec29/"
source(paste0(dir,scripts,"18_regs_birth.R"))



