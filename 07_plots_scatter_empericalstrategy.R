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
            'gridExtra',
            'binsreg')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
output <- "C:/Users/mszklo/Documents/GitHub/ec29/outputs/scatter_plots/"


load(paste0(dir,"regs.RData"))
# --------------------------------------------------------------------------------------------------

# 1. New variables
# =================================================================

df_plot <- df

# changes in spending
for (i in seq(1,10,1)){
  varname1 <- paste0("change0",i,"_","finbra_desp_saude_san_pcapita")
  varname2 <- paste0("change0",i,"_","siops_despsaude_pcapita")
  varname3 <- paste0("change0",i,"_","siops_pct_recproprios_ec29")
  varname4 <- paste0("change0",i,"_","tx_mi")
  varname5 <- paste0("change0",i,"_","tx_mi_icsap")
  varname6 <- paste0("change0",i,"_","tx_mi_nicsap")
  
  df_plot <- df_plot %>% 
    group_by(cod_mun) %>% 
    mutate(!!varname1 := dplyr::lead(finbra_desp_saude_san_pcapita,i) - finbra_desp_saude_san_pcapita,
           !!varname2 := dplyr::lead(siops_despsaude_pcapita,i) - siops_despsaude_pcapita,
           !!varname3 := dplyr::lead(siops_pct_recproprios_ec29,i) - siops_pct_recproprios_ec29,
           !!varname4 := dplyr::lead(tx_mi,i) - tx_mi,
           !!varname5 := dplyr::lead(tx_mi_icsap,i) - tx_mi_icsap,
           !!varname6 := dplyr::lead(tx_mi_nicsap,i) - tx_mi_nicsap) %>% 
    ungroup()
  
}

df_plot <- df_plot %>% 
  filter(ano==2000)

grep("change0",names(df_plot), value = T)


# 2. Scatter plots
# =================================================================


cor(df_plot %>% 
      select(c("dist_ec29_baseline","change05_siops_despsaude_pcapita","pop")) %>% 
      rename(ec29 = 1,
             spending = 2) %>% 
      filter(!is.na(spending)) %>% 
      filter(!is.nan(ec29)) %>% 
      filter(spending<1000) %>% 
      filter(ec29>-0.5))

# full sample

scatter <- ggplot(df_plot %>% 
                    filter(change05_siops_despsaude_pcapita<1000) %>% 
                    filter(dist_ec29_baseline>-0.5)
                  ,
                  aes(x = dist_ec29_baseline, y = change05_siops_despsaude_pcapita)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  scale_y_continuous(limits = c(-400,950), breaks = seq(-400,900,100)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in Health Spending per capita \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")



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



scatter <- ggplot(df_plot %>% 
                    filter(dist_ec29_baseline>-0.5)
                  ,
                  aes(x = dist_ec29_baseline, y = change05_siops_pct_recproprios_ec29)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  scale_y_continuous(limits = c(-0.4,0.4), breaks = seq(-0.4,0.4,0.1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change (p.p.) in % of Own Resource Spent on Health \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")


filePNG <- paste0(output,"scatter_dist_ec29_baseline_change05_share.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change05_share.pdf")
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



# Infant Mortality - Total 2000 - 2005

scatter <- ggplot(df_plot %>% 
                    filter(dist_ec29_baseline>-0.5) %>% 
                    filter(change05_tx_mi>-150)
                  ,
                  aes(x = dist_ec29_baseline, y = change05_tx_mi)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method = "loess", color = "#ef8a62", se = FALSE) +
  scale_y_continuous(limits = c(-150,100), breaks = seq(-150,100,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in IMR \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")


filePNG <- paste0(output,"scatter_dist_ec29_baseline_change05_imr.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change05_imr.pdf")
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

# Infant Mortality - Total 2000 - 2010
scatter <- ggplot(df_plot %>% 
                    filter(dist_ec29_baseline>-0.5) %>% 
                    filter(change010_tx_mi>-150)
                  ,
                  aes(x = dist_ec29_baseline, y = change010_tx_mi)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method = "loess", color = "#ef8a62", se = FALSE) +
  scale_y_continuous(limits = c(-150,100), breaks = seq(-150,100,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in IMR \n 2000-2010",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")


filePNG <- paste0(output,"scatter_dist_ec29_baseline_change10_imr.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change10_imr.pdf")
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





# Infant Mortality - Total 2000 - 2005 - ICSAP

scatter <- ggplot(df_plot %>% 
                    filter(dist_ec29_baseline>-0.5) %>% 
                    filter(change05_tx_mi_icsap>-150)
                  ,
                  aes(x = dist_ec29_baseline, y = change05_tx_mi_icsap)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method = "loess", color = "#ef8a62", se = FALSE) +
  scale_y_continuous(limits = c(-150,100), breaks = seq(-150,100,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in IMR ICSAP \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")


filePNG <- paste0(output,"scatter_dist_ec29_baseline_change05_imr_icsap.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change05_imr_icsap.pdf")
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

# Infant Mortality - Total 2000 - 2010 - ICSAP
scatter <- ggplot(df_plot %>% 
                    filter(dist_ec29_baseline>-0.5) %>% 
                    filter(change010_tx_mi_icsap>-250)
                  ,
                  aes(x = dist_ec29_baseline, y = change010_tx_mi_icsap)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method = "loess", color = "#ef8a62", se = FALSE) +
  scale_y_continuous(limits = c(-150,100), breaks = seq(-150,100,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in IMR ICSAP \n 2000-2010",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")


filePNG <- paste0(output,"scatter_dist_ec29_baseline_change10_imr_icsap.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change10_imr_icsap.pdf")
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




# Infant Mortality - Total 2000 - 2005 - NICSAP

scatter <- ggplot(df_plot %>% 
                    filter(dist_ec29_baseline>-0.5) %>% 
                    filter(change05_tx_mi_nicsap>-150)
                  ,
                  aes(x = dist_ec29_baseline, y = change05_tx_mi_nicsap)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method = "loess", color = "#ef8a62", se = FALSE) +
  scale_y_continuous(limits = c(-150,100), breaks = seq(-150,100,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in IMR NICSAP \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")


filePNG <- paste0(output,"scatter_dist_ec29_baseline_change05_imr_nicsap.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change05_imr_nicsap.pdf")
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

# Infant Mortality - Total 2000 - 2010 - NICSAP
scatter <- ggplot(df_plot %>% 
                    filter(dist_ec29_baseline>-0.5) %>% 
                    filter(change010_tx_mi_nicsap>-150)
                  ,
                  aes(x = dist_ec29_baseline, y = change010_tx_mi_nicsap)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  geom_smooth(method = "loess", color = "#ef8a62", se = FALSE) +
  scale_y_continuous(limits = c(-150,100), breaks = seq(-150,100,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in IMR NICSAP \n 2000-2010",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="none")


filePNG <- paste0(output,"scatter_dist_ec29_baseline_change10_imr_nicsap.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change10_imr_nicsap.pdf")
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





# by electoral term
# ----------------------------------------

colors <-  c("#67a9cf","#ef8a62")

scatter_elect <- ggplot(df_plot %>% 
                          filter(change05_siops_despsaude_pcapita<1000) %>% 
                          filter(dist_ec29_baseline>-0.5) %>%
                          filter(!is.na(second_term)) %>% 
                          mutate(`Electoral Term` = ifelse(second_term==0,"1st Term","2nd Term"))
                        ,
                        aes(x = dist_ec29_baseline, y = change05_siops_despsaude_pcapita, color = `Electoral Term`,group = `Electoral Term`)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(size = 0.7, alpha = 0.5) +
  # geom_smooth(method='lm', color = "sienna2",fill = "sienna2", alpha = 0.1, se = F, size = 0.5)+
  scale_y_continuous(limits = c(-400,950), breaks = seq(-400,900,100)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  scale_color_manual(values = colors) +
  labs(y = "Change in Health Spending per capita \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5)))



filePNG <- paste0(output,"scatter_dist_ec29_baseline_change05_elect.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change05_elect.pdf")
ggsave(filePNG,
       plot = scatter_elect,
       device = "png",
       width = 7, height = 6,
       units = "in")

ggsave(filePDF,
       plot = scatter_elect,
       device = "pdf",
       width = 7, height = 6,
       units = "in")



scatter_elect <- ggplot(df_plot %>% 
                          filter(change05_siops_despsaude_pcapita<1000) %>% 
                          filter(dist_ec29_baseline>-0.5) %>%
                          filter(!is.na(second_term)) %>% 
                          mutate(`Electoral Term` = ifelse(second_term==0,"1st Term","2nd Term"))
                        ,
                        aes(x = dist_ec29_baseline, y = change05_siops_pct_recproprios_ec29, color = `Electoral Term`,group = `Electoral Term`)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(size = 0.7, alpha = 0.5) +
  # geom_smooth(method='lm', color = "sienna2",fill = "sienna2", alpha = 0.1, se = F, size = 0.5)+
  scale_y_continuous(limits = c(-0.4,0.4), breaks = seq(-0.4,0.4,0.1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  scale_color_manual(values = colors) +
  labs(y = "Change (p.p.) in % of Own Resource Spent on Health \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5)))



filePNG <- paste0(output,"scatter_dist_ec29_baseline_change05_share_elect.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_change05_share_elect.pdf")
ggsave(filePNG,
       plot = scatter_elect,
       device = "png",
       width = 7, height = 6,
       units = "in")

ggsave(filePDF,
       plot = scatter_elect,
       device = "pdf",
       width = 7, height = 6,
       units = "in")






# 3. Binscatter
# =================================================================

df_bins <- df_plot %>% 
  filter(change05_siops_despsaude_pcapita<1000) %>% 
  filter(dist_ec29_baseline>-0.5)

binscatter <- binsreg(y = df_bins$change05_siops_despsaude_pcapita,
                      x = df_bins$dist_ec29_baseline,
                      line=c(3,3),
                      ci=c(3,3),
                      cb=c(3,3))

bins_ci <- binscatter$data.plot$`Group Full Sample`$data.ci
bins_dots <- binscatter$data.plot$`Group Full Sample`$data.dots
bins_cb <- binscatter$data.plot$`Group Full Sample`$data.cb
bins_line <- binscatter$data.plot$`Group Full Sample`$data.line

bins_dots <- bins_dots %>% cbind(bins_ci %>% select(ci.l,ci.r))
bins_line <- bins_line %>% cbind(bins_cb %>% select(cb.l,cb.r))




scatter <- ggplot(bins_line,
                  aes(x = x, y = fit)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_line(color = "sienna2", alpha = 1) +
  geom_ribbon(aes(ymin = cb.l, ymax = cb.r),alpha = 0.3, fill = "sienna2") +
  geom_pointrange(size = 0.1, alpha = 1,color = "steelblue4",data = bins_dots,aes(x = x, y = fit, ymin = ci.l, ymax = ci.r)) +
  scale_y_continuous(limits = c(-250,250), breaks = seq(-250,250,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change in Health Spending per capita \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())



filePNG <- paste0(output,"binscatter_dist_ec29_baseline_change05.png")
filePDF <- paste0(output,"binscatter_dist_ec29_baseline_change05.pdf")
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





df_bins <- df_plot %>% 
  filter(dist_ec29_baseline>-0.5)

binscatter <- binsreg(y = df_bins$change05_siops_pct_recproprios_ec29,
                      x = df_bins$dist_ec29_baseline,
                      line=c(3,3),
                      ci=c(3,3),
                      cb=c(3,3))

bins_ci <- binscatter$data.plot$`Group Full Sample`$data.ci
bins_dots <- binscatter$data.plot$`Group Full Sample`$data.dots
bins_cb <- binscatter$data.plot$`Group Full Sample`$data.cb
bins_line <- binscatter$data.plot$`Group Full Sample`$data.line

bins_dots <- bins_dots %>% cbind(bins_ci %>% select(ci.l,ci.r))
bins_line <- bins_line %>% cbind(bins_cb %>% select(cb.l,cb.r))




scatter <- ggplot(bins_line,
                  aes(x = x, y = fit)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_line(color = "sienna2", alpha = 1) +
  geom_ribbon(aes(ymin = cb.l, ymax = cb.r),alpha = 0.3, fill = "sienna2") +
  geom_pointrange(size = 0.1, alpha = 1,color = "steelblue4",data = bins_dots,aes(x = x, y = fit, ymin = ci.l, ymax = ci.r)) +
  scale_y_continuous(limits = c(-0.5,0.3), breaks = seq(-0.5,0.3,0.1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Change (p.p.) in % of Own Resource Spent on Health \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())



filePNG <- paste0(output,"binscatter_dist_ec29_baseline_change05_share.png")
filePDF <- paste0(output,"binscatter_dist_ec29_baseline_change05_share.pdf")
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




# by electoral term
# ------------------------------

# first
df_bins <- df_plot %>% 
  filter(change05_siops_despsaude_pcapita<1000) %>% 
  filter(dist_ec29_baseline>-0.5)

binscatter <- binsreg(y = df_bins$change05_siops_despsaude_pcapita,
                      x = df_bins$dist_ec29_baseline,
                      by = df_bins$second_term,
                      samebinsby = TRUE,
                      line=c(3,3),
                      ci=c(3,3),
                      cb=c(3,3))

bins_ci_1 <- binscatter$data.plot$`Group 0`$data.ci
bins_ci_2 <- binscatter$data.plot$`Group 1`$data.ci
bins_ci <- rbind(bins_ci_1,bins_ci_2)

bins_dots_1 <- binscatter$data.plot$`Group 0`$data.dots
bins_dots_2 <- binscatter$data.plot$`Group 1`$data.dots
bins_dots <- rbind(bins_dots_1,bins_dots_2)

bins_cb_1 <- binscatter$data.plot$`Group 0`$data.cb
bins_cb_2 <- binscatter$data.plot$`Group 1`$data.cb
bins_cb <- rbind(bins_cb_1,bins_cb_2)

bins_line_1 <- binscatter$data.plot$`Group 0`$data.line
bins_line_2 <- binscatter$data.plot$`Group 1`$data.line
bins_line <- rbind(bins_line_1,bins_line_2)

bins_dots <- bins_dots %>% cbind(bins_ci %>% select(ci.l,ci.r)) %>% mutate(term = ifelse(group==0,"1st Term","2nd Term"))
bins_line <- bins_line %>% cbind(bins_cb %>% select(cb.l,cb.r)) %>% mutate(term = ifelse(group==0,"1st Term","2nd Term"))


scatter <- ggplot(bins_line,
                  aes(x = x, y = fit, group = term)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_line(alpha = 0.8, size = 0.8, aes(color = term)) +
  geom_ribbon(aes(ymin = cb.l, ymax = cb.r, fill = term),alpha = 0.3) +
  # geom_point(size = 0.1, alpha = 1,data = bins_dots,aes(x = x, y = fit, ymin = ci.l, ymax = ci.r)) +
  scale_y_continuous(limits = c(-250,250), breaks = seq(-250,250,50)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(y = "Change in Health Spending per capita \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())



filePNG <- paste0(output,"binscatter_dist_ec29_baseline_change05_elect.png")
filePDF <- paste0(output,"binscatter_dist_ec29_baseline_change05_elect.pdf")
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





df_bins <- df_plot %>% 
  filter(dist_ec29_baseline>-0.5)

binscatter <- binsreg(y = df_bins$change05_siops_pct_recproprios_ec29,
                      x = df_bins$dist_ec29_baseline,
                      by = df_bins$second_term,
                      samebinsby = TRUE,
                      line=c(3,3),
                      ci=c(3,3),
                      cb=c(3,3))

bins_ci_1 <- binscatter$data.plot$`Group 0`$data.ci
bins_ci_2 <- binscatter$data.plot$`Group 1`$data.ci
bins_ci <- rbind(bins_ci_1,bins_ci_2)

bins_dots_1 <- binscatter$data.plot$`Group 0`$data.dots
bins_dots_2 <- binscatter$data.plot$`Group 1`$data.dots
bins_dots <- rbind(bins_dots_1,bins_dots_2)

bins_cb_1 <- binscatter$data.plot$`Group 0`$data.cb
bins_cb_2 <- binscatter$data.plot$`Group 1`$data.cb
bins_cb <- rbind(bins_cb_1,bins_cb_2)

bins_line_1 <- binscatter$data.plot$`Group 0`$data.line
bins_line_2 <- binscatter$data.plot$`Group 1`$data.line
bins_line <- rbind(bins_line_1,bins_line_2)

bins_dots <- bins_dots %>% cbind(bins_ci %>% select(ci.l,ci.r)) %>% mutate(term = ifelse(group==0,"1st Term","2nd Term"))
bins_line <- bins_line %>% cbind(bins_cb %>% select(cb.l,cb.r)) %>% mutate(term = ifelse(group==0,"1st Term","2nd Term"))




scatter <- ggplot(bins_line,
                  aes(x = x, y = fit, group = term)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_line(alpha = 0.8, size = 0.8, aes(color = term)) +
  geom_ribbon(aes(ymin = cb.l, ymax = cb.r, fill = term),alpha = 0.3) +
  scale_y_continuous(limits = c(-0.5,0.3), breaks = seq(-0.5,0.3,0.1)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(y = "Change (p.p.) in % of Own Resource Spent on Health \n 2000-2005",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 13),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())



filePNG <- paste0(output,"binscatter_dist_ec29_baseline_change05_share_elect.png")
filePDF <- paste0(output,"binscatter_dist_ec29_baseline_change05_share_elect.pdf")
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














# distance to ec29 vs baseline per capita spending 
# ------------------------------------------------------
# total spending
cor(df_plot %>% 
      select(c("dist_ec29_baseline","siops_despsaude_pcapita","pop")) %>% 
      rename(ec29 = 1,
             spending = 2) %>% 
      filter(!is.na(spending)) %>% 
      filter(!is.nan(ec29)))


scatter <- ggplot(df_plot,
                  aes(x = dist_ec29_baseline, y = siops_despsaude_pcapita)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Health Spending per capita in 2000",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 12),
        legend.position="none")



filePNG <- paste0(output,"scatter_dist_ec29_baseline_spending.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_spending.pdf")
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



# transfers
cor(df_plot %>% 
      select(c("dist_ec29_baseline","siops_despexrecproprio_pcapita","pop")) %>% 
      rename(ec29 = 1,
             spending = 2) %>% 
      filter(!is.na(spending)) %>% 
      filter(!is.nan(ec29)))


scatter <- ggplot(df_plot,
                  aes(x = dist_ec29_baseline, y = siops_despexrecproprio_pcapita)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Health Spending per capita in 2000 - Transfers",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 12),
        legend.position="none")



filePNG <- paste0(output,"scatter_dist_ec29_baseline_transfers.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_transfers.pdf")
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




# own resources
cor(df_plot %>% 
      select(c("dist_ec29_baseline","siops_desprecpropriosaude_pcapita","pop")) %>% 
      rename(ec29 = 1,
             spending = 2) %>% 
      filter(!is.na(spending)) %>% 
      filter(!is.nan(ec29)))


scatter <- ggplot(df_plot,
                  aes(x = dist_ec29_baseline, y = siops_desprecpropriosaude_pcapita)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(y = "Health Spending per capita in 2000 - Own Resource",
       x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 12),
        legend.position="none")



filePNG <- paste0(output,"scatter_dist_ec29_baseline_own.png")
filePDF <- paste0(output,"scatter_dist_ec29_baseline_own.pdf")
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
