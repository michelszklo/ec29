#######################################################################################################
# Author: Michel Szklo
# April 2020
# 
# This script creates time scatter plots for "% of own resource spending in base line vs 
# change in spending in the followin years" with SIOPS data 
# 
# These scatters are essential for the empirical strategy
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
            'sf',
            'sp',
            'xlsx',
            'httr',
            'mapview',
            'stringdist',
            'gridExtra')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
output <- "C:/Users/Michel/Documents/GitHub/ec29/outputs/scatter_plots/"


load(paste0(dir,"regs.RData"))
# --------------------------------------------------------------------------------------------------

# 1. New variables
# =================================================================

df_plot <- df

# changes in spending
for (i in seq(1,10,1)){
  varname1 <- paste0("change0",i,"_","finbra_desp_saude_san_pcapita")
  varname2 <- paste0("change0",i,"_","siops_despsaude_pcapita")
  varname3 <- paste0("change0",i,"_","siops_desprecpropriosaude_pcapita")

  df_plot <- df_plot %>% 
    group_by(cod_mun) %>% 
    mutate(!!varname1 := dplyr::lead(finbra_desp_saude_san_pcapita,i) - finbra_desp_saude_san_pcapita,
           !!varname2 := dplyr::lead(siops_despsaude_pcapita,i) - siops_despsaude_pcapita,
           !!varname3 := dplyr::lead(siops_desprecpropriosaude_pcapita,i) - siops_desprecpropriosaude_pcapita) %>% 
    ungroup()
  
}

df_plot <- df_plot %>% 
  filter(ano==2000)



# 2. Scatter plots
# =================================================================

corr(df_plot %>% 
       select(c("dist_ec29_baseline","change05_siops_despsaude_pcapita")) %>% 
       rename(ec29 = 1,
              spending = 2) %>% 
       filter(!is.na(spending)) %>% 
       filter(!is.nan(ec29)) %>% 
       filter(spending<1000) %>% 
       filter(ec29>-0.5))


scatter <- ggplot(df_plot %>% 
                      filter(change05_siops_despsaude_pcapita<1000) %>% 
                      filter(dist_ec29_baseline>-0.5)
                    ,
                    aes(x = dist_ec29_baseline, y = change05_siops_despsaude_pcapita)) +
  geom_point(color = "steelblue4", size = 0.7, alpha = 0.5) +
  geom_smooth(method='lm', color = "sienna2",fill = "sienna2", alpha = 0.1, se = F, size = 0.5)+
  scale_y_continuous(limits = c(-400,950), breaks = seq(-400,900,100)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in Health Spending per capita 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=12),
    legend.position="bottom", legend.box = "horizontal",
    legend.title = element_blank())



filePNG <- paste0(output,"scatter_dist_ec29_baseline_change05.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change05.pdf")
ggsave(filePNG,
       plot = scatter,
       device = "png",
       width = 7, height = 6,
       units = "in")

ggsave(filePDF,
       plot = scatter,
       device = "pdf",
       width = 7, height = 6,
       units = "in")







