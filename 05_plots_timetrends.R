#######################################################################################################
# Author: Michel Szklo
# April 2020
# 
# This script creates time trends plots with SIOPS data 
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
            'sf',
            'sp',
            'xlsx',
            'httr',
            'mapview',
            'stringdist',
            'gtools',
            'gridExtra',
            'ggsci')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

output <- "C:/Users/Michel/Documents/GitHub/ec29/outputs/time_trends/"


SIOPS <- readRDS("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/SIOPS/SIOPS.rds")

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"


# =================================================================
# 1. New variables
# =================================================================

deflator <- read.csv("C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/deflator/deflator.csv")
SIOPS_temp <- left_join(SIOPS, deflator, by = "ano")

SIOPS_temp <- SIOPS_temp %>% 
  group_by(ano) %>% 
  mutate(pct_recproprios_ec29_mean = mean(pct_recproprios_ec29, na.rm = T),
         quantile = ifelse(ano==2000,quantcut(pct_recproprios_ec29,4),NA)) %>% 
  ungroup() %>% 
  group_by(cod_mun) %>% 
  mutate(quantile = mean(quantile, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(quantile = as.character(quantile)) %>% 
  filter(quantile != 'NaN')



for(i in seq(1,4,1)){
  name <- 25*i
  value <- paste0(name,"%")
  i <- as.character(i)
  SIOPS_temp <- SIOPS_temp %>%
    mutate(quantile = ifelse(quantile==i,value,quantile))
}

SIOPS_quantile <- SIOPS_temp %>% 
  filter(ano==2000) %>% 
  select(cod_mun,quantile) %>%
  filter(quantile == "25%" | quantile == "100%" )
mutate(quantile = ifelse(quantile=="25%","Bottom quartile",quantile),
       quantile = ifelse(quantile=="100%","Top quartile",quantile))


SIOPS_temp <- SIOPS_temp %>% 
  mutate(quantile = as.factor(quantile),
         quantile = factor(quantile, levels = c('25%','50%','75%','100%'))) %>% 
  group_by(ano,quantile) %>% 
  summarise(despsaude_pcapita = mean(despsaude_pcapita, na.rm = T),
            despexrecproprio_pcapita = mean(despexrecproprio_pcapita, na.rm = T),
            desprecpropriosaude_pcapita = mean(desprecpropriosaude_pcapita, na.rm = T),
            deflator_saude = mean(deflator_saude)) %>%
  ungroup() %>% 
  mutate(date = as.Date(as.character(ano), format = "%Y"),
         despsaude_pcapita = despsaude_pcapita / deflator_saude,
         desprecpropriosaude_pcapita = desprecpropriosaude_pcapita / deflator_saude,
         despexrecproprio_pcapita = despexrecproprio_pcapita / deflator_saude) %>% 
  group_by(quantile) %>% 
  mutate(var1 = ifelse(ano==2000,1,despsaude_pcapita / dplyr::lag(despsaude_pcapita,1)),
         var2 = ifelse(ano==2000,1,desprecpropriosaude_pcapita / dplyr::lag(desprecpropriosaude_pcapita,1)),
         var3 = ifelse(ano==2000,1,despexrecproprio_pcapita / dplyr::lag(despexrecproprio_pcapita,1))) %>%
  mutate(`Health spending per capita index (2000 = 100)` = 100 * cumprod(var1),
         `Own resources health spending per capita index (2000 = 100)` = 100 * cumprod(var2),
         `Health spending - own resources per capita index (2000 = 100)` = 100 * cumprod(var3)) %>% 
  ungroup() %>% 
  mutate(quantile = as.character(quantile),
         quantile = ifelse(quantile=="25%","Bottom quartile",quantile),
         quantile = ifelse(quantile=="100%","Top quartile",quantile))







# =================================================================
# 2. Time trends
# =================================================================

# total health spending per capita

color_graph <- c("#a50026","#313695")

plot1 <- SIOPS_temp %>%
  filter(ano<=2010) %>%
  filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
  ggplot(aes(x = ano, y = `Health spending per capita index (2000 = 100)`)) +
  geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
  scale_y_continuous(breaks = seq(100,350,50), limits = c(90,350)) +
  theme_light() +
  labs(x = "Year",
       y = "Spending per capita index (2000 = 100)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())


plot2 <- SIOPS_temp %>%
  filter(ano<=2010) %>% 
  filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
  ggplot(aes(x = ano, y = `Own resources health spending per capita index (2000 = 100)`)) +
  geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
  scale_y_continuous(breaks = seq(100,500,50), limits = c(90,500)) +
  labs(x = "Year",
       y = "Spending per capita index (2000 = 100)")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())




plot3 <- SIOPS_temp %>%
  filter(ano<=2010) %>% 
  filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
  ggplot(aes(x = ano, y = `Health spending - own resources per capita index (2000 = 100)`)) +
  geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
  scale_y_continuous(breaks = seq(100,500,50), limits = c(90,500)) +
  labs(x = "Year",
       y = "Spending per capita index (2000 = 100)")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank())


filePNG <- paste0(output,"plot_total.png")
filepdf <- paste0(output,"plot_total.pdf")
ggsave(filePNG,
       plot = plot1,
       device = "png",
       width = 12, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot1,
       device = "pdf",
       width = 12, height = 5,
       units = "in")



filePNG <- paste0(output,"plot_own.png")
filepdf <- paste0(output,"plot_own.pdf")
ggsave(filePNG,
       plot = plot2,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot2,
       device = "pdf",
       width = 6, height = 5,
       units = "in")

filePNG <- paste0(output,"plot_transf.png")
filepdf <- paste0(output,"plot_transf.pdf")
ggsave(filePNG,
       plot = plot3,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot3,
       device = "pdf",
       width = 6, height = 5,
       units = "in")




# 3. Shares of spending time trend
# =================================================================


# data mgmt
# --------------------------------------------------

rm(list= ls()[!(ls() %in% c("output","dir","SIOPS_quantile"))])

load(paste0(dir,"regs.RData"))

var_map <- rbind(cbind('finbra_desp_pessoal_share','Human Resources Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_investimento_share','Investment Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_outros_share','Other Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_saude_san_share','Health and Sanitation Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_transporte_share','Transport Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_educ_cultura_share','Education and Culture Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_hab_urb_share','Housing and Urban Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_assist_prev_share','Social Security Spending per capita (% Total Spending)'),
                 cbind('siops_desprecpropriosaude_share','Health Spending per capita - Own Resources (% Health Spending)'),
                 cbind('siops_despexrecproprio_share','Health Spending per capita - Transfers (% Health Spending)'),
                 cbind('siops_desppessoal_share','Health Spending per capita - Human Resources (% Health Spending)'),
                 cbind('siops_despinvest_share','Health Spending per capita - Investiment (% Health Spending)'),
                 cbind('siops_despservicoster_share','Health Spending per capita - 3rd parties services (% Health Spending)'),
                 cbind('siops_despoutros_share','Health Spending per capita - other expenditures (% Health Spending)'))


share_spending_vars <- var_map[,1]
share_spending_names <- var_map[,2]

share_spending_names_finbra <- grep("% Total Spending",share_spending_names,value = T) 
share_spending_names_siops <- grep("% Health Spending",share_spending_names,value = T) 


# full sample
# --------------------------------------------------
df_plot <- df %>% 
  select(ano,cod_mun,all_of(share_spending_vars)) %>% 
  group_by(ano) %>% 
  summarise_at(share_spending_vars, mean, na.rm = T) %>% 
  filter(ano<=2010)


names(df_plot) <- c("ano",share_spending_names)


df_plot_finbra <- df_plot %>% 
  select(ano,all_of(share_spending_names_finbra)) %>% 
  pivot_longer(cols = all_of(share_spending_names_finbra),
               names_to = "category",
               values_to = "spending")

df_plot_siops <- df_plot %>% 
  select(ano,all_of(share_spending_names_siops)) %>% 
  pivot_longer(cols = all_of(share_spending_names_siops),
               names_to = "category",
               values_to = "spending")


# top and bottom quartile
# --------------------------------------------------

df_plot_quartile <- df %>% 
  select(ano,cod_mun,all_of(share_spending_vars)) %>%
  left_join(SIOPS_quantile %>% mutate(cod_mun = as.numeric(cod_mun)),by = "cod_mun") %>% 
  filter(!is.na(quantile)) %>% 
  group_by(ano,quantile) %>% 
  summarise_at(share_spending_vars, mean, na.rm = T) %>% 
  filter(ano<=2010)

names(df_plot_quartile) <- c("ano","quantile",share_spending_names)


df_plot_finbra_top <- df_plot_quartile %>% 
  filter(quantile=="100%") %>% 
  select(ano,all_of(share_spending_names_finbra)) %>% 
  pivot_longer(cols = all_of(share_spending_names_finbra),
               names_to = "category",
               values_to = "spending")

df_plot_finbra_bottom <- df_plot_quartile %>% 
  filter(quantile=="25%") %>% 
  select(ano,all_of(share_spending_names_finbra)) %>% 
  pivot_longer(cols = all_of(share_spending_names_finbra),
               names_to = "category",
               values_to = "spending")

df_plot_siops_top <- df_plot_quartile %>% 
  filter(quantile=="100%") %>% 
  select(ano,all_of(share_spending_names_siops)) %>% 
  pivot_longer(cols = all_of(share_spending_names_siops),
               names_to = "category",
               values_to = "spending")

df_plot_siops_bottom <- df_plot_quartile %>% 
  filter(quantile=="25%") %>% 
  select(ano,all_of(share_spending_names_siops)) %>% 
  pivot_longer(cols = all_of(share_spending_names_siops),
               names_to = "category",
               values_to = "spending")



# Finbra Type graph
# --------------------------------------------------

color_graph <- pal_lancet("lanonc")(9)


plot4 <- df_plot_finbra %>%
  filter(category == "Human Resources Spending per capita (% Total Spending)" |
           category == "Investment Spending per capita (% Total Spending)" | 
           category == "Other Spending per capita (% Total Spending)") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,.7,0.1), limits = c(0,.7)) +
  labs(x = "Year",
       y = "% of Total Spending")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))

plot5 <- df_plot_finbra_top %>%
  filter(category == "Human Resources Spending per capita (% Total Spending)" |
           category == "Investment Spending per capita (% Total Spending)" | 
           category == "Other Spending per capita (% Total Spending)") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,.7,0.1), limits = c(0,.7)) +
  labs(x = "Year",
       y = "% of Total Spending",
       title = "Top Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


plot6 <- df_plot_finbra_bottom %>%
  filter(category == "Human Resources Spending per capita (% Total Spending)" |
           category == "Investment Spending per capita (% Total Spending)" | 
           category == "Other Spending per capita (% Total Spending)") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,.7,0.1), limits = c(0,.7)) +
  labs(x = "Year",
       y = "% of Total Spending",
       title = "Bottom Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


filePNG <- paste0(output,"plot_finbra_share_type.png")
filepdf <- paste0(output,"plot_finbra_share_type.pdf")
ggsave(filePNG,
       plot = plot4,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot4,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_share_type_top.png")
filepdf <- paste0(output,"plot_finbra_share_type_top.pdf")
ggsave(filePNG,
       plot = plot5,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot5,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_share_type_bottom.png")
filepdf <- paste0(output,"plot_finbra_share_type_bottom.pdf")
ggsave(filePNG,
       plot = plot6,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot6,
       device = "pdf",
       width = 6, height = 5,
       units = "in")





# Finbra Area graph
# --------------------------------------------------

color_graph <- pal_lancet("lanonc")(9)


plot7 <- df_plot_finbra %>%
  filter(category != "Human Resources Spending per capita (% Total Spending)" &
           category != "Investment Spending per capita (% Total Spending)" & 
           category != "Other Spending per capita (% Total Spending)") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,.5,0.1), limits = c(0,.5)) +
  labs(x = "Year",
       y = "% of Total Spending") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))

plot8 <- df_plot_finbra_top %>%
  filter(category != "Human Resources Spending per capita (% Total Spending)" &
           category != "Investment Spending per capita (% Total Spending)" & 
           category != "Other Spending per capita (% Total Spending)") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,.5,0.1), limits = c(0,.5)) +
  labs(x = "Year",
       y = "% of Total Spending",
       title = "Top Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))


plot9 <- df_plot_finbra_bottom %>%
  filter(category != "Human Resources Spending per capita (% Total Spending)" &
           category != "Investment Spending per capita (% Total Spending)" & 
           category != "Other Spending per capita (% Total Spending)") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,.5,0.1), limits = c(0,.5)) +
  labs(x = "Year",
       y = "% of Total Spending",
       title = "Bottom Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))


filePNG <- paste0(output,"plot_finbra_share_area.png")
filepdf <- paste0(output,"plot_finbra_share_area.pdf")
ggsave(filePNG,
       plot = plot7,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot7,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_share_area_top.png")
filepdf <- paste0(output,"plot_finbra_share_area_top.pdf")
ggsave(filePNG,
       plot = plot8,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot8,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_share_area_bottom.png")
filepdf <- paste0(output,"plot_finbra_share_area_bottom.pdf")
ggsave(filePNG,
       plot = plot9,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot9,
       device = "pdf",
       width = 6, height = 5,
       units = "in")





# Siops source graph
# --------------------------------------------------

color_graph <- pal_lancet("lanonc")(9)


plot10 <- df_plot_siops %>%
  filter(category == "Health Spending per capita - Own Resources (% Health Spending)" |
           category == "Health Spending per capita - Transfers (% Health Spending)" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0.2,0.8,0.1), limits = c(0.2,0.8)) +
  labs(x = "Year",
       y = "% of Health Spending") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))

plot11 <- df_plot_siops_top %>%
  filter(category == "Health Spending per capita - Own Resources (% Health Spending)" |
           category == "Health Spending per capita - Transfers (% Health Spending)" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0.2,0.8,0.1), limits = c(0.2,0.8)) +
  labs(x = "Year",
       y = "% of Health Spending",
       title = "Top Quartile of EC29") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


plot12 <- df_plot_siops_bottom %>%
  filter(category == "Health Spending per capita - Own Resources (% Health Spending)" |
           category == "Health Spending per capita - Transfers (% Health Spending)" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0.2,0.8,0.1), limits = c(0.2,0.8)) +
  labs(x = "Year",
       y = "% of Health Spending",
       title = "Bottom Quartile of EC29") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


filePNG <- paste0(output,"plot_siops_share_source.png")
filepdf <- paste0(output,"plot_siops_share_source.pdf")
ggsave(filePNG,
       plot = plot10,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot10,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_share_source_top.png")
filepdf <- paste0(output,"plot_siops_share_source_top.pdf")
ggsave(filePNG,
       plot = plot11,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot11,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_share_source_bottom.png")
filepdf <- paste0(output,"plot_siops_share_source_bottom.pdf")
ggsave(filePNG,
       plot = plot12,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot12,
       device = "pdf",
       width = 6, height = 5,
       units = "in")




# Siops type graph
# --------------------------------------------------

color_graph <- pal_lancet("lanonc")(9)


plot13 <- df_plot_siops %>%
  filter(category != "Health Spending per capita - Own Resources (% Health Spending)" &
           category != "Health Spending per capita - Transfers (% Health Spending)" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,0.7,0.1), limits = c(0,0.7)) +
  labs(x = "Year",
       y = "% of Health Spending") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))

plot14 <- df_plot_siops_top %>%
  filter(category != "Health Spending per capita - Own Resources (% Health Spending)" &
           category != "Health Spending per capita - Transfers (% Health Spending)" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,0.7,0.1), limits = c(0,0.7)) +
  labs(x = "Year",
       y = "% of Health Spending",
       title = "Top Quartile of EC29") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


plot15 <- df_plot_siops_bottom %>%
  filter(category != "Health Spending per capita - Own Resources (% Health Spending)" &
           category != "Health Spending per capita - Transfers (% Health Spending)" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,0.7,0.1), limits = c(0,0.7)) +
  labs(x = "Year",
       y = "% of Health Spending",
       title = "Bottom Quartile of EC29") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


filePNG <- paste0(output,"plot_siops_share_type.png")
filepdf <- paste0(output,"plot_siops_share_type.pdf")
ggsave(filePNG,
       plot = plot13,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot13,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_share_type_top.png")
filepdf <- paste0(output,"plot_siops_share_type_top.pdf")
ggsave(filePNG,
       plot = plot14,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot14,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_share_type_bottom.png")
filepdf <- paste0(output,"plot_siops_share_type_bottom.pdf")
ggsave(filePNG,
       plot = plot15,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot15,
       device = "pdf",
       width = 6, height = 5,
       units = "in")




# 4. Levels of spending time trend
# =================================================================


# data mgmt
# --------------------------------------------------

var_map <- rbind(cbind('finbra_desp_c_pcapita','Total Spending per capita'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita'),
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Security Spending per capita'),
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures'))

spending_vars <- var_map[,1]
spending_names <- var_map[,2]

spending_names_finbra <- spending_names[grep("finbra",spending_vars)]
spending_names_siops <- spending_names[grep("siops",spending_vars)]


# full sample
# --------------------------------------------------
df_plot <- df %>% 
  select(ano,cod_mun,all_of(spending_vars)) %>% 
  group_by(ano) %>% 
  summarise_at(spending_vars, mean, na.rm = T) %>% 
  filter(ano<=2010)


names(df_plot) <- c("ano",spending_names)


df_plot_finbra <- df_plot %>% 
  select(ano,all_of(spending_names_finbra)) %>% 
  pivot_longer(cols = all_of(spending_names_finbra),
               names_to = "category",
               values_to = "spending")

df_plot_siops <- df_plot %>% 
  select(ano,all_of(spending_names_siops)) %>% 
  pivot_longer(cols = all_of(spending_names_siops),
               names_to = "category",
               values_to = "spending")


# top and bottom quartile
# --------------------------------------------------

df_plot_quartile <- df %>% 
  select(ano,cod_mun,all_of(spending_vars)) %>%
  left_join(SIOPS_quantile %>% mutate(cod_mun = as.numeric(cod_mun)),by = "cod_mun") %>% 
  filter(!is.na(quantile)) %>% 
  group_by(ano,quantile) %>% 
  summarise_at(spending_vars, mean, na.rm = T) %>% 
  filter(ano<=2010)

names(df_plot_quartile) <- c("ano","quantile",spending_names)


df_plot_finbra_top <- df_plot_quartile %>% 
  filter(quantile=="100%") %>% 
  select(ano,all_of(spending_names_finbra)) %>% 
  pivot_longer(cols = all_of(spending_names_finbra),
               names_to = "category",
               values_to = "spending")

df_plot_finbra_bottom <- df_plot_quartile %>% 
  filter(quantile=="25%") %>% 
  select(ano,all_of(spending_names_finbra)) %>% 
  pivot_longer(cols = all_of(spending_names_finbra),
               names_to = "category",
               values_to = "spending")

df_plot_siops_top <- df_plot_quartile %>% 
  filter(quantile=="100%") %>% 
  select(ano,all_of(spending_names_siops)) %>% 
  pivot_longer(cols = all_of(spending_names_siops),
               names_to = "category",
               values_to = "spending")

df_plot_siops_bottom <- df_plot_quartile %>% 
  filter(quantile=="25%") %>% 
  select(ano,all_of(spending_names_siops)) %>% 
  pivot_longer(cols = all_of(spending_names_siops),
               names_to = "category",
               values_to = "spending")



# Finbra Type graph
# --------------------------------------------------

color_graph <- pal_lancet("lanonc")(9)


plot16 <- df_plot_finbra %>%
  filter(category == "Human Resources Spending per capita" |
           category == "Investment Spending per capita" | 
           category == "Other Spending per capita") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  labs(x = "Year",
       y = "Spending Per Capita")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))

plot17 <- df_plot_finbra_top %>%
  filter(category == "Human Resources Spending per capita" |
           category == "Investment Spending per capita" | 
           category == "Other Spending per capita") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  labs(x = "Year",
       y = "Spending Per Capita",
       title = "Top Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


plot18 <- df_plot_finbra_bottom %>%
  filter(category == "Human Resources Spending per capita" |
           category == "Investment Spending per capita" | 
           category == "Other Spending per capita") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  labs(x = "Year",
       y = "Spending Per Capita",
       title = "Bottom Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


filePNG <- paste0(output,"plot_finbra_level_type.png")
filepdf <- paste0(output,"plot_finbra_level_type.pdf")
ggsave(filePNG,
       plot = plot16,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot16,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_level_type_top.png")
filepdf <- paste0(output,"plot_finbra_level_type_top.pdf")
ggsave(filePNG,
       plot = plot17,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot17,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_level_type_bottom.png")
filepdf <- paste0(output,"plot_finbra_level_type_bottom.pdf")
ggsave(filePNG,
       plot = plot18,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot18,
       device = "pdf",
       width = 6, height = 5,
       units = "in")





# Finbra Area graph
# --------------------------------------------------

color_graph <- pal_lancet("lanonc")(9)


plot19 <- df_plot_finbra %>%
  filter(category != "Human Resources Spending per capita" &
           category != "Investment Spending per capita" & 
           category != "Other Spending per capita") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  labs(x = "Year",
       y = "Spending Per Capita") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))

plot20 <- df_plot_finbra_top %>%
  filter(category != "Human Resources Spending per capita" &
           category != "Investment Spending per capita" & 
           category != "Other Spending per capita") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  labs(x = "Year",
       y = "Spending Per Capita",
       title = "Top Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))


plot21 <- df_plot_finbra_bottom %>%
  filter(category != "Human Resources Spending per capita" &
           category != "Investment Spending per capita" & 
           category != "Other Spending per capita") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  labs(x = "Year",
       y = "Spending Per Capita",
       title = "Bottom Quartile of EC29")+
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))


filePNG <- paste0(output,"plot_finbra_level_area.png")
filepdf <- paste0(output,"plot_finbra_level_area.pdf")
ggsave(filePNG,
       plot = plot19,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot19,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_level_area_top.png")
filepdf <- paste0(output,"plot_finbra_level_area_top.pdf")
ggsave(filePNG,
       plot = plot20,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot20,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_finbra_level_area_bottom.png")
filepdf <- paste0(output,"plot_finbra_level_area_bottom.pdf")
ggsave(filePNG,
       plot = plot21,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot21,
       device = "pdf",
       width = 6, height = 5,
       units = "in")





# Siops source graph
# --------------------------------------------------

color_graph <- c("#a50026","#313695")

plot22 <- df_plot_siops %>%
  filter(category == "Health Spending per capita - Own Resources" |
           category == "Health Spending per capita - Transfers" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) +
  labs(x = "Year",
       y = "Spending Per Capita") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))

plot23 <- df_plot_siops_top %>%
  filter(category == "Health Spending per capita - Own Resources" |
           category == "Health Spending per capita - Transfers" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) +
  labs(x = "Year",
       y = "Spending Per Capita") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


plot24 <- df_plot_siops_bottom %>%
  filter(category == "Health Spending per capita - Own Resources" |
           category == "Health Spending per capita - Transfers" ) %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) +
  labs(x = "Year",
       y = "Spending Per Capita") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE))


filePNG <- paste0(output,"plot_siops_level_source.png")
filepdf <- paste0(output,"plot_siops_level_source.pdf")
ggsave(filePNG,
       plot = plot22,
       device = "png",
       width = 12, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot22,
       device = "pdf",
       width = 12, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_level_source_top.png")
filepdf <- paste0(output,"plot_siops_level_source_top.pdf")
ggsave(filePNG,
       plot = plot23,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot23,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_level_source_bottom.png")
filepdf <- paste0(output,"plot_siops_level_source_bottom.pdf")
ggsave(filePNG,
       plot = plot24,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot24,
       device = "pdf",
       width = 6, height = 5,
       units = "in")




# Siops type graph
# --------------------------------------------------

color_graph <- pal_lancet("lanonc")(9)


plot25 <- df_plot_siops %>%
  filter(category != "Health Spending per capita - Own Resources" &
           category != "Health Spending per capita - Transfers" &
           category != "Health Spending per capita - Total") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) +
  labs(x = "Year",
       y = "Spending Per Capita") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))

plot26 <- df_plot_siops_top %>%
  filter(category != "Health Spending per capita - Own Resources" &
           category != "Health Spending per capita - Transfers" &
           category != "Health Spending per capita - Total") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) +
  labs(x = "Year",
       y = "Spending Per Capita",
       title = "Top Quartile of EC29") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))


plot27 <- df_plot_siops_bottom %>%
  filter(category != "Health Spending per capita - Own Resources" &
           category != "Health Spending per capita - Transfers" &
           category != "Health Spending per capita - Total") %>% 
  ggplot(aes(x = ano, y = spending, color = category, group = category)) +
  geom_line(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = color_graph) +
  scale_x_continuous(breaks = seq(2000,2010,1), limits = c(1999.5,2010.5)) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) +
  labs(x = "Year",
       y = "Spending Per Capita",
       title = "Bottom Quartile of EC29") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=7)) +
  guides(color=guide_legend(ncol=2,byrow=TRUE))


filePNG <- paste0(output,"plot_siops_level_type.png")
filepdf <- paste0(output,"plot_siops_level_type.pdf")
ggsave(filePNG,
       plot = plot25,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot25,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_level_type_top.png")
filepdf <- paste0(output,"plot_siops_level_type_top.pdf")
ggsave(filePNG,
       plot = plot26,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot26,
       device = "pdf",
       width = 6, height = 5,
       units = "in")


filePNG <- paste0(output,"plot_siops_level_type_bottom.png")
filepdf <- paste0(output,"plot_siops_level_type_bottom.pdf")
ggsave(filePNG,
       plot = plot27,
       device = "png",
       width = 6, height = 5,
       units = "in")
ggsave(filepdf,
       plot = plot27,
       device = "pdf",
       width = 6, height = 5,
       units = "in")











# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot1 <- SIOPS_temp %>%
#   filter(ano<=2010) %>%
#   filter(quantile == "Bottom quartile" | quantile == "Top quartile") %>% 
#   ggplot(aes(x = ano, y = `Health spending per capita index (2000 = 100)`)) +
#   geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
#   scale_color_manual(values = color_graph) +
#   scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
#   scale_y_continuous(breaks = seq(100,500,25), limits = c(100,500)) +
#   labs(title = "Total health spending",
#        subtitle = "by quantiles of the baseline % of own resource spending off total health spending") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=8),
#         legend.position=c(0.1,0.7),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 8),
#         legend.key.size = unit(0.8, "cm"),
#         legend.key.width = unit(0.2, "cm"),
#         plot.title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 7))
# 
# 
# 
# # own resources per capita
# 
# plot2 <- SIOPS_temp %>%
#   filter(ano<=2010) %>% 
#   filter(quantile == "20%" | quantile == "80%") %>% 
#   ggplot(aes(x = ano, y = `Own resources health spending per capita index (2000 = 100)`)) +
#   geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
#   scale_color_manual(values = color_graph) +
#   scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
#   scale_y_continuous(breaks = seq(100,500,25), limits = c(100,500)) +
#   labs(title = "Own resources health spending",
#        subtitle = "by quantiles of the baseline % of own resource spending off total health spending") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=8),
#         legend.position=c(0.1,0.7),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 8),
#         legend.key.size = unit(0.8, "cm"),
#         legend.key.width = unit(0.2, "cm"),
#         plot.title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 7))
# 
# 
# # besides own resources per capita
# 
# plot3 <- SIOPS_temp %>%
#   filter(ano<=2010) %>% 
#   filter(quantile == "20%" | quantile == "80%") %>% 
#   ggplot(aes(x = ano, y = `Health spending - own resources per capita index (2000 = 100)`)) +
#   geom_line(aes(color = quantile), size = 0.8, alpha = 0.6) +
#   scale_color_manual(values = color_graph) +
#   scale_x_continuous(breaks = seq(2000,2010,1), limits = c(2000,2010)) +
#   scale_y_continuous(breaks = seq(100,500,25), limits = c(100,500)) +
#   labs(title = "Health spending from other sources",
#        subtitle = "by quantiles of the baseline % of own resource spending off total health spending") +
#   theme_light() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=8),
#         legend.position=c(0.1,0.7),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 8),
#         legend.key.size = unit(0.8, "cm"),
#         legend.key.width = unit(0.2, "cm"),
#         plot.title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 7))
# 
# 
# 
# grid <- grid.arrange(plot1,plot2,plot3,ncol = 3)
# 
# 
# filePNG <- paste0(output,"plot.png")
# filepdf <- paste0(output,"plot.pdf")
# ggsave(filePNG,
#        plot = grid,
#        device = "png",
#        width = 12, height = 5,
#        units = "in")
# ggsave(filepdf,
#        plot = grid,
#        device = "pdf",
#        width = 12, height = 5,
#        units = "in")
# 
# 









# 
# 
# SIOPS_agg <- SIOPS %>% 
#   group_by(ano) %>% 
#   summarise(target_ec29 = mean(target_ec29, na.rm = T),
#             pct_recproprios_ec29 = mean(pct_recproprios_ec29, na.rm = T),
#             achieved_ec29 = mean(achieved_ec29, na.rm = T),
#             dist_ec29_desp = mean(dist_ec29_desp, na.rm = T),
#             dist_ec29_desp_pc = mean(dist_ec29_desp_pc, na.rm = T))
# 
# plot1 <- SIOPS_agg %>% ggplot(aes(x = ano, y = achieved_ec29)) +
#   geom_line(color = "blue", size = 2) +
#   scale_y_continuous(breaks = seq(0,1,0.05), limits = c(0.8,1), labels = percent) +
#   scale_x_continuous(breaks = seq(2000,2018,1), limits = c(2000,2018)) +
#   theme(axis.text.x = element_text(angle=90))
# 
# plot2 <- SIOPS_agg %>% ggplot(aes(x = ano, y = pct_recproprios_ec29)) +
#   geom_line(color = "blue", size = 2) +
#   geom_line(aes(x = ano, y = target_ec29), color = "red", size = 2, linetype = "dotted") +
#   geom_line(aes(x = ano, y = pct_dist_ec29), color = "green", size = 2, linetype = "dashed") +
#   scale_y_continuous(breaks = seq(0,100,5), limits = c(0,25)) +
#   scale_x_continuous(breaks = seq(2000,2018,1), limits = c(2000,2018)) +
#   theme(axis.text.x = element_text(angle=90))
# 
# filePNG <- paste0(output,"plot1.png")
# filepdf <- paste0(output,"plot1.pdf")
# ggsave(filePNG,
#        plot = plot1,
#        device = "png",
#        width = 7, height = 3.5,
#        units = "in")
# ggsave(filepdf,
#        plot = plot1,
#        device = "pdf",
#        width = 7, height = 3.5,
#        units = "in")
# 
# filePNG <- paste0(output,"plot2.png")
# filepdf <- paste0(output,"plot2.pdf")
# ggsave(filePNG,
#        plot = plot2,
#        device = "png",
#        width = 7, height = 3.5,
#        units = "in")
# ggsave(filepdf,
#        plot = plot2,
#        device = "pdf",
#        width = 7, height = 3.5,
#        units = "in")
# 
# 
# 
# 
# 
