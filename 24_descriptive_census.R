#######################################################################################################
# Author: Michel Szklo
# December 2023
# 
# This script creates scatter plots for control variables shifts between census (1991 - 2000)
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
            'gridExtra',
            'binsreg')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
raw <- paste0(dir,"data/")
output <- "C:/Users/Michel/Documents/GitHub/ec29/outputs/scatter_plots/"


# =================================================================
# 1. Loading data
# =================================================================

# complete dataset
load(paste0(dir,"regs.RData"))

outliers <- df %>% 
  mutate(s = log(finbra_desp_o_pcapita)) %>% 
  select(s,everything())

ndesv <- 5
x <- mean(outliers$s, na.rm = T)
sd <- sd(outliers$s, na.rm = T)
outliers <- outliers %>% 
  mutate(s1 = x - sd * ndesv,
         s2 = x + sd * ndesv) %>% 
  filter(s<=s1 | s>=s2) %>% 
  select(cod_mun) %>% 
  unique()

outliers <- outliers$cod_mun

df2 <- df %>% 
  filter(!(cod_mun %in% outliers))

# creating subdataset with only distance to ec29 and municipality code
df <- df %>% 
  filter(ano==2000) %>% 
  select(cod_mun,dist_ec29_baseline,pop)



# loading control data
atlas <- read.csv(paste0(raw,"Atlas2013/atlas_data.csv"), encoding = "UTF-8", sep = ";") %>%
  rename(ano=1) %>%
  filter(ano!=2010) %>% 
  rename(cod_mun = Codmun6)
colnames(atlas) <- tolower(colnames(atlas))
atlas <- atlas %>% 
  select(cod_mun,ano,e_anosestudo,t_analf18m,gini,pind,pmpob,rdpc,t_agua,t_lixo,t_luz,agua_esgoto,idhm) %>% 
  mutate(pind = pind/100,
         pmpob = pmpob/100)

# analysis df
df <- atlas %>% 
  left_join(df, by = "cod_mun")



# =================================================================
# 2. Calculating shifts in variables  - 1991-2000
# =================================================================

vars <- names(df)[5:ncol(df)-2]

for (v in vars){
  
  varname1 <- paste0(v,"_91_00_shift")
  varname2 <- paste0(v,"_91_00_shift_pc")
  df <- df %>% 
    group_by(cod_mun) %>% 
    mutate(!!varname1 := dplyr::lead(get(v),1) - get(v)) %>% 
    mutate(!!varname2 := (dplyr::lead(get(v),1) - get(v)) / get(v))
}

df <- df %>% 
  filter(ano==1991) %>% 
  select(-ano,-all_of(vars))





map <- rbind(
  
  cbind("e_anosestudo_91_00_shift","Change in Average Years of Study \n 1991-2000"),
  cbind("e_anosestudo_91_00_shift_pc","% Change in Average Years of Study \n 1991-2000"),
  
  cbind("t_analf18m_91_00_shift","Change in Illiteracy Rates \n 1991-2000"),
  cbind("t_analf18m_91_00_shift_pc","% Change in Illiteracy Rates \n 1991-2000"),
  
  cbind("gini_91_00_shift","Change in Gini Coefficient \n 1991-2000"),
  cbind("gini_91_00_shift_pc","% Change in Gini Coefficient \n 1991-2000"),
  
  cbind("pind_91_00_shift","Change in the Share of Pop in Extreme Poverty \n 1991-2000"),
  cbind("pind_91_00_shift_pc","% Change in the Share of Pop in Extreme Poverty \n 1991-2000"),
  
  cbind("pmpob_91_00_shift","Change in the Share of Pop in Poverty \n 1991-2000"),
  cbind("pmpob_91_00_shift_pc","% Change in the Share of Pop in Poverty \n 1991-2000"),
  
  cbind("rdpc_91_00_shift","Change in Income Per Capita \n 1991-2000"),
  cbind("rdpc_91_00_shift_pc","% Change in Income Per Capita \n 1991-2000"),
  
  cbind("t_agua_91_00_shift","Change in the Share of Pop with Access to Water \n 1991-2000"),
  cbind("t_agua_91_00_shift_pc","% Change in the Share of Pop with Access to Water \n 1991-2000"),
  
  cbind("t_lixo_91_00_shift","Change in the Share of Pop with Access to Garbage Collection \n 1991-2000"),
  cbind("t_lixo_91_00_shift_pc","%Change in the Share of Pop with Access to Garbage Collection \n 1991-2000"),
  
  cbind("t_luz_91_00_shift","Change in the Share of Pop with Access to Eletricity \n 1991-2000"),
  cbind("t_luz_91_00_shift_pc","% Change in the Share of Pop with Access to Eletricity \n 1991-2000"),
  
  cbind("agua_esgoto_91_00_shift","Change in the Share of Pop with Access to Adequate Water and Sewage \n 1991-2000"),
  cbind("agua_esgoto_91_00_shift_pc","% Change in the Share of Pop with Access to Adequate Water and Sewage \n 1991-2000"),
  
  cbind("idhm_91_00_shift","Change in HDI \n 1991-2000"),
  cbind("idhm_91_00_shift_pc","% Change in HDI \n 1991-2000")
)



for (i in 1: nrow(map)){
  
  var <- map[i,1]
  var_name <- map[i,2]
  
  pc <- grep("pc",var)
  
  if(is.null(pc)){
    var2 <- substr(var,1,nchar(var)-3)
  } else{
    var2 <- var
  }
  
  
  outliers <- df %>% 
    mutate(s = log(get(var2))) %>% 
    select(s,everything())
  
  ndesv <- 2
  x <- mean(outliers$s, na.rm = T)
  sd <- sd(outliers$s, na.rm = T)
  outliers <- outliers %>% 
    mutate(s1 = x - sd * ndesv,
           s2 = x + sd * ndesv) %>% 
    filter(s<=s1 | s>=s2) %>% 
    select(cod_mun) %>% 
    unique()
  
  outliers <- outliers$cod_mun
  
  df_plot <- df %>% 
    filter(!(cod_mun %in% outliers))
  
  
  scatter <- ggplot(df_plot %>% 
                      filter(dist_ec29_baseline>-0.5)
                    ,
                    aes(x = dist_ec29_baseline, y = get(var))) +
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
    # scale_y_continuous(limits = c(-400,950), breaks = seq(-400,900,100)) +
    scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
    labs(y = var_name,
         x = "Distance to the EC29 target") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=12),
          axis.text = element_text(size = 13),
          legend.position="none")
  
  filePNG <- paste0(output,var,"_scatter_dist_ec29_baseline.png")
  filePDF <- paste0(output,var,"_scatter_dist_ec29_baseline_change05.pdf")
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
  
}




df2 <- df2 %>% 
  select(cod_mun,ano,dist_ec29_baseline,finbra_desp_saude_san_pcapita,pop)

df2_98 <- df2 %>% filter(ano==1998) %>% 
  rename(spending98 = finbra_desp_saude_san_pcapita)

df2_00 <- df2 %>% filter(ano==2000) %>% 
  rename(spending00 = finbra_desp_saude_san_pcapita) %>% 
  left_join(df2_98 %>% select(-ano,-pop), by = c("cod_mun","dist_ec29_baseline")) %>% 
  mutate(`Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000` = spending00 - spending98)


scatter <- ggplot(df2_00 %>% 
                    filter(dist_ec29_baseline>-0.5)
                  ,
                  aes(x = dist_ec29_baseline, y = `Change in Healh and Sanitation Spending per capita (FINBRA) \n 1998-2000`)) +
  geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
  geom_point(aes(size = pop),color = "steelblue4", alpha = 0.2) +
  scale_y_continuous(limits = c(-1000,1000), breaks = seq(-1000,1000,250)) +
  scale_x_continuous(limits = c(-0.35,0.155),breaks = seq(-0.40,0.15,0.05)) +
  labs(x = "Distance to the EC29 target") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 13),
        legend.position="none")

filePNG <- paste0(output,"finbra","_scatter_dist_ec29_baseline.png")
filePDF <- paste0(output,"finbra","_scatter_dist_ec29_baseline.pdf")
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




