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

scripts <- "ec29/"


# ------------------------------------


# 1. Run all
# =================================================================

source(paste0(dir,scripts,"09_regs_firststage.R"))
source(paste0(dir,scripts,"10_regs_spending.R"))
source(paste0(dir,scripts,"11_regs_infra.R"))
source(paste0(dir,scripts,"12_regs_sia.R"))
source(paste0(dir,scripts,"13_regs_hr.R"))
source(paste0(dir,scripts,"14_regs_imr.R"))
source(paste0(dir,scripts,"15_regs_amr.R"))
source(paste0(dir,scripts,"16_regs_imr_lag.R"))
source(paste0(dir,scripts,"17_regs_amr_lag.R"))



