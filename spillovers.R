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
#--- (2) Models on "isolated" municipalities  
#--------------------------------------------------------------------------------
dfisolated <- df %>% filter(avgPropInHosp < 0.1 & avgPropOutHosp < 0.1)


var_map <-  rbind(cbind('tx_mi','Infant Mortality Rate'),
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
                  cbind('tx_mm',"Maternal Mortality Rate"))
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
  reduced_yearly_imr(var,var_name,dfisolated,3,1998,-25,10,5,paste0("1_cont_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1, ramb_roth=F)
  reduced_yearly_ab_imr(var,var_name,dfisolated,3,1998,-40,30,5,paste0("2_ab_level_",i),weight = "peso_pop",year_cap = 2010, cont = 1)
}

 
