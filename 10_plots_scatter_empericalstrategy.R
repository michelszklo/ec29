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

setwd("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000")

# USE FOR FULL SAMPLE
# --------------------------------------------------------------------------------------------------
#output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/Plots/scatter_strategy/"
output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/Plots/scatter_strategy_original/"
#output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/Plots/scatter_strategy_alt/"


SIOPS <- readRDS("data/SIOPS/SIOPS.rds")
# --------------------------------------------------------------------------------------------------


# USE FOR REELECTION SAMPLE
# --------------------------------------------------------------------------------------------------
# output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/Plots/scatter_strategy2/"
# 
# SIOPS <- readRDS("data/SIOPS.rds")
# run_reelection <- readRDS("data/run_reelection.rds")
# SIOPS <- SIOPS[SIOPS$cod_mun %in% run_reelection$cod_mun,]
# --------------------------------------------------------------------------------------------------

# cut <- 2.5
# 
# SIOPS <- SIOPS %>% 
#  filter(ano==2000) %>% 
#   mutate(percentile = ntile(dist_ec29, 100)) %>% 
#   filter(percentile>=(cut) & percentile<=(100-cut))


glm(pct_recproprios_ec29 ~ change04_pct_recproprios, data = SIOPS %>% filter(ano==2000))
glm(dist_ec29 ~ change04_pct_recproprios, data = SIOPS %>% filter(ano==2000))

# =================================================================
# 1. Scatter plots
# =================================================================

axis_s <- 0
axis_e <- 45
axis_m <- 5

scatter <- ggplot(SIOPS %>% filter(ano==2000),
                  aes(x = pct_recproprios_ec29, y = change04_pct_recproprios)) +
  geom_point(color = "steelblue4", size = 0.7, alpha = 0.5) +
  scale_x_continuous(breaks = seq(axis_s,axis_e,axis_m), limits = c(axis_s,axis_e)) +
  scale_y_continuous(breaks = seq(-40,40,10), limits = c(-40,40)) +
  labs(x = "Health Spending (% of ORS) in 2000",
       y = "Change in Health Spending (% of ORS) 2000-2004") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())

filePNG <- paste0(output,"scatter_pct.png")
filePDF <- paste0(output,"scatter_pct.pdf")
ggsave(filePNG,
       plot = scatter,
       device = "png",
       width = 7, height = 5,
       units = "in")

ggsave(filePDF,
       plot = scatter,
       device = "pdf",
       width = 7, height = 5,
       units = "in")



axis_s <- -2
axis_e <- 1
axis_m <- 0.5

scatter <- ggplot(SIOPS %>% filter(ano==2000),
                  aes(x = dist_ec29, y = change04_pct_recproprios)) +
  geom_point(color = "steelblue4", size = 0.7, alpha = 0.5) +
  scale_x_continuous(breaks = seq(axis_s,axis_e,axis_m), limits = c(axis_s,axis_e)) +
  scale_y_continuous(breaks = seq(-40,40,10), limits = c(-40,40)) +
  labs(x = "% Distance to EC/29 target",
       y = "Change in Health Spending (% of ORS) 2000-2004") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())

filePNG <- paste0(output,"scatter_dist.png")
filePDF <- paste0(output,"scatter_dist.pdf")
ggsave(filePNG,
       plot = scatter,
       device = "png",
       width = 7, height = 5,
       units = "in")

ggsave(filePDF,
       plot = scatter,
       device = "pdf",
       width = 7, height = 5,
       units = "in")







# =================================================================
# 1. Scatter plots
# =================================================================

# Scatter plots
# -----------------------------------------------------------------

c("change","var")

file1 <- "_scatter_0"
file2 <- "_scatter_"
x_var <- "pct_recproprios_ec29"
axis_s <- 0
axis_e <- 100
axis_m <- 20

# file1 <- "_scatterDist_0"
# file2 <- "_scatterDist_"
# x_var <- "dist_ec29"
# axis_s <- -4.5
# axis_e <- 1
# axis_m <- 0.5

nbins = 50

for (j in "change"){
  
  for(i in seq(1,12,1)){
    if(i<10){
      ano <- as.numeric(paste0("200",i))
      SIOPS_temp <- SIOPS %>% 
        filter(ano==2000)
      
      y_var <- paste0(j,"0",i,"_pct_recproprios")
      y_title <- paste0("Change 2000 - 200",i)
      scatter <- ggplot(SIOPS_temp,
                        aes_string(x = x_var, y = y_var)) +
        geom_point(color = "steelblue4", size = 0.7, alpha = 0.7) +
        geom_smooth(method='lm', color = "sienna2",fill = "sienna2", alpha = 0.7, se = F) + 
        #stat_summary_bin(fun.y = mean, bins = nbins, color = "sienna2", size = 1.5, geom = "point", alpha = 0.5) +
        geom_vline(xintercept = 0, color = "black", size = 0.4, alpha = 0.8, linetype = "dashed") +
        scale_x_continuous(breaks = seq(axis_s,axis_e,axis_m), limits = c(axis_s,axis_e)) +
        scale_y_continuous(breaks = seq(-50,50,25), limits = c(-50,50)) +
        labs(y = y_title) +
        theme_light() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.title = element_text(size=6),
              axis.title.x = element_blank(),
              axis.text = element_text(size = 5, angle = 90))
      
      filePNG <- paste0(output,j,file1,i,".png")
      fileRDS <- paste0(output,j,file1,i,".rds")
      ggsave(filePNG,
             plot = scatter,
             device = "png",
             width = 7, height = 3.5,
             units = "in")
      saveRDS(scatter,file = fileRDS)
      
    }else{
      ano <- as.numeric(paste0("20",i))
      SIOPS_temp <- SIOPS %>% 
        filter(ano==ano)
      
      y_var <- paste0(j,i,"_pct_recproprios")
      y_title <- paste0("Change 2000 - 20",i)
      scatter <- ggplot(SIOPS_temp,
                        aes_string(x = x_var, y = y_var)) +
        geom_point(color = "steelblue4", size = 0.7, alpha = 0.7) +
        geom_smooth(method='lm', color = "sienna2",fill = "sienna2", alpha = 0.7, se = F) + 
        #stat_summary_bin(fun.y = mean, bins = nbins, color = "sienna2", size = 1.5, geom = "point", alpha = 0.5) +
        geom_vline(xintercept = 0, color = "black", size = 0.4, alpha = 0.8, linetype = "dashed") +
        scale_x_continuous(breaks = seq(axis_s,axis_e,axis_m), limits = c(axis_s,axis_e)) +
        scale_y_continuous(breaks = seq(-50,50,25), limits = c(-50,50)) +
        labs(y = y_title) +
        theme_light() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.title = element_text(size=6),
              axis.title.x = element_blank(),
              axis.text = element_text(size = 5, angle = 90))
      
      filePNG <- paste0(output,j,file2,i,".png")
      fileRDS <- paste0(output,j,file2,i,".rds")
      
      ggsave(filePNG,
             plot = scatter,
             device = "png",
             width = 7, height = 3.5,
             units = "in")
      saveRDS(scatter,file = fileRDS)
      
    }
  }
}



# temp <- list.files(path = output, pattern = "*.rds")
# 
# 
# for(i in seq(1,length(temp),1)){
#   name <- paste0("plot",i)
#   plot <- readRDS(paste0(output,temp[i]))
#   print(name)
#   assign(name,plot,envir = .GlobalEnv)
# }
# 
# grid <- grid.arrange(plot1,plot2,plot3,plot3,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12, ncol = 4)
# 
# filePDF <- paste0(output,"scatters.pdf")
# 
# ggsave(filePDF,
#        plot = grid,
#        device = "pdf",
#        width = 8, height = 6,
#        units = "in")
# 




