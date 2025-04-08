#######################################################################################################
# Author: Michel Szklo
# April 2025
# 
# This script creates time scatter plots for "% of own resource spending in base line vs 
# change in variables between 1998-2000, 2000-2005 and 2000-2010 
# 
# 
#
#######################################################################################################

# =================================================================
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
            'plotly',
            'ggplot2',
            'xlsx',
            'httr',
            'stringdist',
            'gridExtra',
            'binsreg')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
output <- "C:/Users/mszklo/Documents/GitHub/ec29/outputs/new_scatters/"
raw <- paste0(dir,"data/")


load(paste0(dir,"regs.RData"))
# --------------------------------------------------------------------------------------------------

# 1. Census Data
# =================================================================


census <- read.csv(paste0(raw,"Atlas2013/atlas_data.csv"), encoding = "UTF-8", sep = ";") %>%
  rename(ano=1) %>%
  # filter(ano!=2010) %>% 
  rename(cod_mun = Codmun6)
colnames(census) <- tolower(colnames(census))
census <- census %>% 
  select(cod_mun,ano,e_anosestudo,t_analf18m,gini,pind,pmpob,rdpc,t_agua,t_lixo,t_luz,agua_esgoto,idhm) %>% 
  mutate(pind = pind/100,
         pmpob = pmpob/100)



# 2. New variables
# =================================================================

# main data frame
#------------------------------

variables <- c("finbra_desp_saude_san_pcapita",
               "siops_despsaude_pcapita",
               "siops_pct_recproprios_ec29",
               "tx_mi",
               "tx_mi_icsap",
               "tx_mi_nicsap")

# changes in spending 2000 -> 2005, 2010, and 1998 -> 2000
df_shift <- df %>% 
  group_by(cod_mun) %>% 
  mutate(across(all_of(variables), ~ lead(.x, 5) - .x, .names = "shift05_{.col}")) %>% 
  mutate(across(all_of(variables), ~ lead(.x, 10) - .x, .names = "shift10_{.col}")) %>% 
  mutate(across(all_of(variables), ~ .x - lag(.x, 2), .names = "shift98_{.col}")) %>% 
  ungroup() %>% 
  filter(ano==2000)


# census data
#------------------------------

census_var <- names(census)[3:ncol(census)]

census_shift <- census %>%
  group_by(cod_mun) %>% 
  mutate(across(all_of(census_var), ~ .x - lag(.x,1), .names = "shift91_{.col}")) %>% 
  mutate(across(all_of(census_var), ~ lead(.x,1) - .x, .names = "shift10_{.col}")) %>% 
  ungroup() %>% 
  filter(ano==2000) %>% 
  left_join(df %>% select(cod_mun,dist_ec29_baseline, ano) %>% filter(ano==2000),by = "cod_mun")


# 2. binscatters 
# =================================================================

df_shift <- df_shift[complete.cases(df_shift[,"dist_ec29_baseline"]),]

# Binscatter plot function
#------------------------------

bs <- function(df,var,label,file){
  
  df_plot <- df[complete.cases(df[,var]),]
  df_plot <- df_plot %>% 
    filter(dist_ec29_baseline>-0.2)
  
  est <- binsreg(df_plot[,var] %>% unlist(),
                 df_plot$dist_ec29_baseline,
                 data = df_plot, 
                 weights = df_plot$peso_pop,
                 randcut = 1,
                 line = c(3,3),
                 ci = c(3,3),
                 cb = c(3,3),
                 nbins = 35,
                 binspos = "es")
  
  est$bins_plot
  
  
  bins_ci <- est$data.plot$`Group Full Sample`$data.ci
  bins_dots <- est$data.plot$`Group Full Sample`$data.dots
  bins_cb <- est$data.plot$`Group Full Sample`$data.cb
  bins_line <- est$data.plot$`Group Full Sample`$data.line
  
  bins_dots <- bins_dots %>% cbind(bins_ci %>% select(ci.l,ci.r)) %>% 
    mutate(below = 0) %>% 
    mutate(below = ifelse(x>0,1,below))
  bins_line <- bins_line %>% cbind(bins_cb %>% select(cb.l,cb.r))
  
  
  
  
  scatter <- ggplot(bins_line,
                    aes(x = x, y = fit)) +
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_line(color = "sienna2", alpha = 1) +
    geom_ribbon(aes(ymin = cb.l, ymax = cb.r),alpha = 0.3, fill = "sienna2") +
    geom_pointrange(size = 0.1, alpha = 0.7,color = "steelblue4",data = bins_dots,aes(x = x, y = fit, ymin = ci.l, ymax = ci.r)) +
    geom_smooth(color = "steelblue4", size = 0.6, data = bins_dots %>% filter(below==0), aes(x = x, y = fit), method = "lm",se = FALSE)+
    geom_smooth(color = "steelblue4", size = 0.6, data = bins_dots %>% filter(below==1), aes(x = x, y = fit), method = "lm",se = FALSE)+
    scale_x_continuous(limits = c(-0.2,0.15),breaks = seq(-0.2,0.15,0.05)) +
    labs(y = label,
         x = "Distance to the EC29 target") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=12),
          axis.text = element_text(size = 13),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank())
  
  filePDF <- paste0(output,"binscatter_",file,".pdf")
  
  ggsave(filePDF,
         plot = scatter,
         device = "pdf",
         width = 7, height = 6,
         units = "in")
  
  
}


# baseline IMR
#------------------------------

bs(df_shift,"tx_mi","Baseline IMR","imr_baseline")


# Shifts 2000-2005
#------------------------------

variables <- cbind(grep("shift05",names(df_shift), value = T),
                   c("Shifts in Health and Sanitation Spending per capita \n 2000-2005",
                     "Shifts in Health Spending per capita \n 2000-2005",
                     "Shifts (p.p.) in % of Own Resource Spent on Health \n 2000-2005",
                     "Shifts in Infant Mortality Rate \n 2000-2005",
                     "Shifts in Infant Mortality Rate - Amenable to Primary Care \n 2000-2005",
                     "Shifts in Infant Mortality Rate - Non-Amenable to Primary Care \n 2000-2005"),
                   c("shift05_health_finbra",
                     "shift05_health_siops",
                     "shift05_health_siops_pct_own",
                     "shift05_imr",
                     "shift05_imr_icsap",
                     "shift05_imr_nicsap"))

for(i in 1:nrow(variables)){
  bs(df_shift,variables[i,1],variables[i,2],variables[i,3])
}


# Shifts 2000-2010
#------------------------------

variables <- cbind(grep("shift10",names(df_shift), value = T),
                   c("Shifts in Health and Sanitation Spending per capita \n 2000-2010",
                     "Shifts in Health Spending per capita \n 2000-2010",
                     "Shifts (p.p.) in % of Own Resource Spent on Health \n 2000-2010",
                     "Shifts in Infant Mortality Rate \n 2000-2010",
                     "Shifts in Infant Mortality Rate - Amenable to Primary Care \n 2000-2010",
                     "Shifts in Infant Mortality Rate - Non-Amenable to Primary Care \n 2000-2010"),
                   c("shift10_health_finbra",
                     "shift10_health_siops",
                     "shift10_health_siops_pct_own",
                     "shift10_imr",
                     "shift10_imr_icsap",
                     "shift10_imr_nicsap"))

for(i in 1:nrow(variables)){
  bs(df_shift,variables[i,1],variables[i,2],variables[i,3])
}


# Shifts 1998-2000
#------------------------------

variables <- cbind(grep("shift98",names(df_shift), value = T),
                   c("Shifts in Health and Sanitation Spending per capita \n 1998-2000",
                     "Shifts in Health Spending per capita \n 1998-2000",
                     "Shifts (p.p.) in % of Own Resource Spent on Health \n 1998-2000",
                     "Shifts in Infant Mortality Rate \n 1998-2000",
                     "Shifts in Infant Mortality Rate - Amenable to Primary Care \n 1998-2000",
                     "Shifts in Infant Mortality Rate - Non-Amenable to Primary Care \n 1998-2000"),
                   c("shift98_health_finbra",
                     "shift98_health_siops",
                     "shift98_health_siops_pct_own",
                     "shift98_imr",
                     "shift98_imr_icsap",
                     "shift98_imr_nicsap"))

variables <- variables[-c(2,3),]

for(i in 1:nrow(variables)){
  bs(df_shift,variables[i,1],variables[i,2],variables[i,3])
}



# Census Shifts 1991-2000
#------------------------------

variables <- cbind(grep("shift91",names(census_shift), value = T),
                   c("Shifts in Expected Years of Study  \n 1991-2000",
                     "Shifts in Iliteracy Rate (above 18y old) \n 1991-2000",
                     "Shifts in Gini Coefficient \n 1991-2000",
                     "Shifts in Population Below Extreme Poverty Line  \n 1991-2000",
                     "Shifts in Population Below Poverty Line  \n 1991-2000",
                     "Shifts in Income per capita  \n 1991-2000",
                     "Shifts in Access to Water Network  \n 1991-2000",
                     "Shifts in Access to Garbage Collection Service  \n 1991-2000",
                     "Shifts in Access to Electricity  \n 1991-2000",
                     "Shifts in Access to Sewage Network  \n 1991-2000",
                     "Shifts in IDH  \n 1991-2000"))

for(i in 1:nrow(variables)){
  bs(census_shift,variables[i,1],variables[i,2],variables[i,1])
}


# Census Shifts 2000-2010
#------------------------------

variables <- cbind(grep("shift10",names(census_shift), value = T),
                   c("Shifts in Expected Years of Study  \n 2000-2010",
                     "Shifts in Iliteracy Rate (above 18y old) \n 2000-2010",
                     "Shifts in Gini Coefficient \n 2000-2010",
                     "Shifts in Population Below Extreme Poverty Line  \n 2000-2010",
                     "Shifts in Population Below Poverty Line  \n 2000-2010",
                     "Shifts in Income per capita  \n 2000-2010",
                     "Shifts in Access to Water Network  \n 2000-2010",
                     "Shifts in Access to Garbage Collection Service  \n 2000-2010",
                     "Shifts in Access to Electricity  \n 2000-2010",
                     "Shifts in Access to Sewage Network  \n 2000-2010",
                     "Shifts in IDH  \n 2000-2010"))

for(i in 1:nrow(variables)){
  bs(census_shift,variables[i,1],variables[i,2],variables[i,1])
}

