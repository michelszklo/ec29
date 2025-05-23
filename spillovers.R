#--------------------------------------------------------------------------------
#--- Author: Damian Clarke
#--- May 2025
#--- 
#--- This script analyses whether effects are sensitive to spillover effects by
#---  focusing on isolated municipalities.
#---  Relies on the following scripts / functions:
#---    - 08_regs_vars_specs.R / reduced_yearly_imr()
#--------------------------------------------------------------------------------

rm(list=ls())

#Set-up path for principal directory
if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/"
} else if (Sys.getenv("USERNAME") == "damian") {
  dir <- "/home/damian/investigacion/2021/decentralization/github/"  
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}
SRC <- paste0(dir,"source/")
DAT <- paste0(dir,"data/processed/")
TAB <- paste0(dir,"results/tables/")
FIG <- paste0(dir,"results/figures/")


#--------------------------------------------------------------------------------
#--- (0) Package load and basic set-up
#--------------------------------------------------------------------------------
# packages
packages<-c('readr',
            'tidyverse',
            'RCurl',
            'tidyr',
            'scales',
            'RColorBrewer',
            'ggplot2',
            'ggExtra',
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
            'ggsci',
            'HonestDiD',
            'sf',
            'plotly',
            'sp',
            'httr',
            'mapview',
            'geomerge',
            'cowplot',
            'dplyr',
            'stringdist',
            'gridExtra')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)
options(digits = 15)

#Set Base year (first year in data)
YEAR <- 1998

#--------------------------------------------------------------------------------
#--- (1) Descriptives on "isolated" municipalities  
#--------------------------------------------------------------------------------
load(paste0(DAT,"regs.RData")) 


#Correlation between inflow and outflow
#plot(df2$tx_sih_in_hosp_total, df2$tx_sih_out_hosp_total)
df$propInHosp  <- df$tx_sih_in_hosp_total/(df$tx_sih_adult+df$tx_sih_infant)
df$propOutHosp <- df$tx_sih_out_hosp_total/(df$tx_sih_adult+df$tx_sih_infant)

#df2 <- df2 %>%
#  filter(ano >= 2000 & ano <= 2010) %>% 
#  group_by(cod_mun) %>%  
#  mutate(
#    avgPropInHosp = mean(propInHosp, na.rm = TRUE),
#    avgPropOutHosp = mean(propOutHosp, na.rm = TRUE)
#  ) %>%
#  ungroup()

# Calculate averages for 2000-2010
averages <- df %>%
  filter(ano >= 2000 & ano <= 2010) %>%
  group_by(cod_mun) %>%
  summarise(
    avgPropInHosp = mean(propInHosp, na.rm = TRUE),
    avgPropOutHosp = mean(propOutHosp, na.rm = TRUE)
  )

# Merge the averages back into the full data frame
df <- df %>%
  left_join(averages, by = "cod_mun")

#plot(df2$propInHosp, df2$propOutHosp)

# classic plot :
p <- ggplot(df, aes(x=propInHosp, y=propOutHosp, color=peso_pop, size=peso_pop)) +
      geom_point() +
      xlim(0, 1) +  # Set x-axis limits
      ylim(0, 1) +  # Set y-axis limits
      labs(x = "Hospitalization inflows", y = "Hospitalization outflows") +  # Add axis labels
      theme(legend.position="none")
 
# with marginal histogram
p1 <- ggMarginal(p, type = "histogram", fill = "lightblue", color = "white", bins=50)
ggsave(filename = paste0(FIG, "spillovers/flows_descriptive.pdf"), plot = p1, width = 8, height = 6, dpi = 300)




#--------------------------------------------------------------------------------
#--- (3) Map  
#--------------------------------------------------------------------------------
munbr_shape <- read_sf(paste0(dir,"data/raw/shapefiles/Brazil_s.shp"))
munbr_shape <- munbr_shape %>%
  mutate(cod_mun = as.numeric(substr(CD_GEOCODM,1,6)))

ufbr_shape <- read_sf(paste0(dir,"data/raw/shapefiles/br_estados/estados_2010.shp")) %>% 
  dplyr::select(sigla,geometry) %>% 
  st_as_sf() %>%
  st_transform(4326)


# 2. Merging with dataframe
# =================================================================

df_map <- df %>% 
  right_join(munbr_shape, by = "cod_mun") %>% 
  filter(ano==2010) %>% 
  dplyr::select(cod_mun,geometry,avgPropInHosp,avgPropOutHosp) %>% 
  st_as_sf() %>%
  st_transform(4326)




# Plot the map
ggplot(data = df_map) +
  geom_sf(aes(fill = avgPropInHosp), color = NA) +  # Fill municipalities by avgPropInHosp
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Inflows", 
                       limits = c(0, 0.2), oob = scales::squish) +  # Squish outliers into the range
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
ggsave(filename = paste0(FIG, "spillovers/map_inflows.pdf"), 
       plot = last_plot(),  # Use the last plot created
       width = 8, height = 6, dpi = 300, device = "pdf")


# Plot the map with state borders
ggplot(data = df_map) +
  geom_sf(aes(fill = avgPropInHosp), color = NA) +  # Fill municipalities by avgPropInHosp
  geom_sf(data = ufbr_shape, fill = NA, color = "white", size = 0.3) +  # Add state borders in white
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Inflows", 
                       limits = c(0, 0.2), oob = scales::squish) +  # Squish outliers into the range
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
ggsave(filename = paste0(FIG, "spillovers/map_inflows_with_white_states.pdf"), 
       plot = last_plot(),  # Use the last plot created
       width = 8, height = 6, dpi = 300, device = "pdf")



ggplot(data = df_map) +
  geom_sf(aes(fill = avgPropOutHosp), color = NA) +  # Fill municipalities by avgPropInHosp
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Inflows") +  # Color scale
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
ggsave(filename = paste0(FIG, "spillovers/map_outflows.pdf"), 
       plot = last_plot(),  # Use the last plot created
       width = 8, height = 6, dpi = 300, device = "pdf")

# Plot the map for avgPropOutHosp with state borders
ggplot(data = df_map) +
  geom_sf(aes(fill = avgPropOutHosp), color = NA) +  # Fill municipalities by avgPropOutHosp
  geom_sf(data = ufbr_shape, fill = NA, color = "white", size = 0.3) +  # Add state borders in white
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Outflows", 
                       limits = c(0, 0.2), oob = scales::squish) +  # Squish outliers into the range
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
ggsave(filename = paste0(FIG, "spillovers/map_outflows_with_white_states.pdf"), 
       plot = last_plot(),  # Use the last plot created
       width = 8, height = 6, dpi = 300, device = "pdf")


# Detach packages to avoid issues with select from raster
detach("package:mapview", unload = TRUE)
detach("package:geomerge", unload = TRUE)


#--------------------------------------------------------------------------------
#--- (2) Models on "isolated" municipalities: mortality
#--------------------------------------------------------------------------------
dfisolated <- df %>% filter(avgPropInHosp < 0.1 & avgPropOutHosp < 0.1)


var_map <- rbind(
  cbind('tx_mi','Infant Mortality Rate'),
  cbind('tx_mi_icsap','Infant Mortality Rate - APC'),
  cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC'),
  cbind('tx_mi_infec','Infant Mortality Rate - Infectious'),
  cbind('tx_mi_resp','Infant Mortality Rate - Respiratory'),
  cbind('tx_mi_perinat','Infant Mortality Rate - Perinatal'),
  cbind('tx_mi_cong','Infant Mortality Rate - Congenital'),
  cbind('tx_mi_ext','Infant Mortality Rate - External'),
  cbind('tx_mi_nut','Infant Mortality Rate - Nutritional'),
  cbind('tx_mi_out','Infant Mortality Rate - Other'),
  cbind('tx_mi_illdef','Infant Mortality Rate - Ill-Defined'),
  cbind('tx_mi_fet','Infant Mortality Rate - Fetal'),
  cbind('tx_mi_24h','Infant Mortality Rate - Within 24h'),
  cbind('tx_mi_27d','Infant Mortality Rate - 1 to 27 days'),
  cbind('tx_mi_ano','Infant Mortality Rate - 27 days to 1 year'),
  cbind('tx_mm',"Maternal Mortality Rate")
)
# continuous

for (i in seq(1, 16, 1)) {
  var <- var_map[i, 1]
  dfisolated[[var]][dfisolated$birth_nasc_vivos == 0] <- NA  
}

yearly_folder <- "spillovers/"

#Single coefficient
for (i in c(1,13,14,15,16)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,dfisolated,3,1998,-25,10,5,
                     paste0("1_cont_level_",i),weight = "peso_pop",
                     year_cap = 2010, cont = 1, ramb_roth=F)

  reduced_yearly_ab_imr(var,var_name,dfisolated,3,1998,-40,30,5,
                     paste0("2_ab_level_",i),weight = "peso_pop",
                     year_cap = 2010, cont = 1)
}

 
#--------------------------------------------------------------------------------
#--- (2) Models on "isolated" municipalities: spending
#--------------------------------------------------------------------------------
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

df_noout <- df %>% 
  filter(!(cod_mun %in% outliers))


# Define variable sets
vars_finbra <- c('finbra_recorc_pcapita','finbra_desp_o_pcapita',
                 'finbra_desp_saude_san_pcapita','finbra_desp_nao_saude_pcapita',
                 'finbra_despsocial_pcapita','finbra_desp_outros_area_pcapita',
                 'gdp_mun_pcapita','pbf_pcapita','t_tx_mi_baseline',
                 'dist_ec29_baseline')

nonzero_finbra <- c('finbra_recorc_pcapita','finbra_desp_o_pcapita',
                    'finbra_desp_saude_san_pcapita','finbra_desp_nao_saude_pcapita',
                    'finbra_despsocial_pcapita','finbra_desp_outros_area_pcapita')

vars_siops <- c('siops_despsaude_pcapita','siops_desprecpropriosaude_pcapita',
                'siops_despexrecproprio_pcapita','siops_desppessoal_pcapita',
                'siops_despinvest_pcapita','siops_despservicoster_pcapita',
                'siops_despoutros_pcapita','gdp_mun_pcapita',
                'pbf_pcapita','t_tx_mi_baseline','dist_ec29_baseline')

nonzero_siops <- c('siops_despsaude_pcapita','siops_desprecpropriosaude_pcapita',
                   'siops_despexrecproprio_pcapita','siops_desppessoal_pcapita',
                   'siops_despinvest_pcapita','siops_despservicoster_pcapita')

# Filter for balanced data
df_balance_finbra <- df_noout[
  complete.cases(df_noout[vars_finbra]) &
    rowSums(df_noout[nonzero_finbra] == 0) == 0,
]

df_balance_siops <- df_noout[
  complete.cases(df_noout[vars_siops]) &
    rowSums(df_noout[nonzero_siops] == 0) == 0 &
    df_noout$siops_despoutros_pcapita > 0,
]

dfisolated_finbra <- df_balance_finbra %>% filter(avgPropInHosp < 0.1 & avgPropOutHosp < 0.1)
dfisolated_siops  <- df_balance_siops  %>% filter(avgPropInHosp < 0.1 & avgPropOutHosp < 0.1)



var_map <- rbind(
  cbind('finbra_recorc_pcapita','Total Revenue per capita (log)'),
  cbind('finbra_desp_o_pcapita','Total Spending per capita (log)'),                 
  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)'),
  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (log)'),
  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (log)'),
  cbind('finbra_impostos_total_pcapita', 'Total Tax Revenue (log)'),
  cbind('finbra_iptu_pcapita', 'Property Tax Revenue (log)'),
  cbind('finbra_iss_pcapita', 'Services Tax Revenue (log)'),
  cbind('finbra_passivo_pcapita','Total Liabilities (log)'),
  cbind('finbra_passivo_pcapita','Financial Liabilities (log)')
)

for (i in seq(1,11,1)){
  vals <- if (i %in% c(1, 2, 4:11)) {
    list(x_min = -1, x_max = 2.5, x_inc = 0.25)
  } else if (i %in% c(3)) {
    list(x_min = -3, x_max = 9.25, x_inc = 1)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }


  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,dfisolated_finbra,1,1998,
                     vals$x_min,vals$x_max,vals$x_inc,
                     paste0("1_cont_level_",i),weight = "peso_pop",
                     year_cap = 2010, cont = 1, ramb_roth=F)

  reduced_yearly_ab_imr(var,var_name,dfisolated_finbra,1,1998,
                        vals$x_min,vals$x_max,vals$x_inc,
                        paste0("2_ab_level_",i),weight = "peso_pop",
                        year_cap = 2010, cont = 1)
}

var_map <- rbind(
  cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
  cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
  cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)'),
  cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (log)'),
  cbind('siops_despinvest_pcapita','Health Spending per capita - Investment (log)'),
  cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (log)'),
  cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (log)')
)
for (i in seq(1,7,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,dfisolated_siops,1,1998,-3,9.25,1,
                     paste0("1_cont_level_",i),weight = "peso_pop",
                     year_cap = 2010, cont = 1, ramb_roth=F)

  reduced_yearly_ab_imr(var,var_name,dfisolated_siops,1,1998,-3,9.25,1,
                     paste0("2_ab_level_",i),weight = "peso_pop",
                     year_cap = 2010, cont = 1)
}
