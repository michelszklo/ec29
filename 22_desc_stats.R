#######################################################################################################
# Author: Michel Szklo
# May 2022
# 
# This scripts creates descriptive statistics table
#
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
            'ggsci',
            'gtools')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)



# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/ec29/"
} else if(Sys.getenv("USERNAME")=="damian") {
  dir <- "/home/damian/investigacion/2021/decentralization/github/ec29/"
} else {
  dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}

# ------------------------------------


# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))

# creating missing SIA variable
df <- df %>% 
  mutate(sia_nab_pcapita = sia_pcapita - sia_ab_pcapita)


if(Sys.getenv("USERNAME")=="dcc213") {
  index <- data.frame(read.dta13("/home/dcc213/investigacion/2021/decentralization/github/ec29/indexes.dta"))
} else if(Sys.getenv("USERNAME")=="damian") {
  index <- data.frame(read.dta13("/home/damian/investigacion/2021/decentralization/github/ec29/indexes.dta"))
} else {
  index <- data.frame(read.dta13("C:/Users/Michel/Documents/GitHub/ec29/indexes.dta"))
}


# merge indexes to main df
all_df <- c("df")

imerge <- function(df){
  df <- df %>% 
    left_join(index, by = c("ano","cod_mun","cod_uf"))
}

for(d in all_df){
  df_merge <- get(d)
  df_merge <- df_merge %>% imerge()
  assign(d,df_merge,envir = .GlobalEnv)
}



# 2. Summary Statistic Function
# =================================================================

summary_stat <- function(df,baseline_year,obj){
  
  var_list <- var_map[,1]
  var_lables <- var_map[,2]
  datsource <- var_map[,3]
  covered <- var_map[,4]
  
  var_names_simple <- unlist(lapply(var_list, function(x) gsub("_","",x)))
  var_map<- cbind(var_map,var_names_simple)
  
  df_stat <- df %>%
    filter(ano==baseline_year) %>%
    filter(!is.na(ec29_baseline)) %>%
    select(all_of(var_list))
  
  names(df_stat) <- var_names_simple
  
  
  df_stat <- df_stat %>% 
    summarise_all(list(mean = ~ mean(.,na.rm = T), 
                       sd = ~ sd(.,na.rm = T),
                       obs = ~ sum(!is.na(.)))) %>% 
    pivot_longer(cols = everything(),
                 names_to = c(".value","variable"),
                 names_sep = "_") %>% 
    t() %>% 
    as.data.frame() %>% 
    slice(2:n()) %>% 
    mutate_all(as.numeric) %>% 
    mutate_all(~ round(.,digits = 3)) %>% 
    cbind(var_lables) %>% 
    select(var_lables,everything())  %>% cbind(datsource)%>% cbind(covered)
  
  names(df_stat) <- c("Variable","Mean","Std.Dev","Obs", "Source of Data", "Period")
  df_stat <- df_stat %>%
    mutate(Baseline = baseline_year)
  
  assign(paste0(obj),df_stat,envir = .GlobalEnv)
  
}


# 3. List of variables and baseline year adjustments
# =================================================================

var_map <- rbind(cbind("ec29_baseline","\\textbf{EC 29 Variables}&&&& \\\\ Own Resource Spent in Health", "Datasus/SIOPS", "2000-2010"),
                 cbind("dist_ec29_baseline","Distance to the EC29 Target", "Datasus/SIOPS", "2000-2010"),
                 
                 # fiscal reaction
                 cbind('finbra_recorc_pcapita','&&&&\\\\ \\textbf{Public Revenue}&&&& \\\\ Total Revenue per capita',"FINBRA", "1998-2010"),

                 cbind('finbra_desp_o_pcapita','&&&&\\\\ \\textbf{Public Spending}&&&& \\\\ Total Spending per capita',"FINBRA", "1998-2010"),
                 
                 cbind('finbra_desp_saude_san_pcapita','&&&&\\\\ \\textit{Spending by Category -- per capita} &&&& \\\\ Health and Sanitation',"FINBRA", "1998-2010"),
                 cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending ',"FINBRA", "1998-2010"),
                 cbind('finbra_despsocial_pcapita','Non-Health Social Spending',"FINBRA", "1998-2010"),
                 cbind('finbra_desp_outros_area_pcapita','Non-Social Spending',"FINBRA", "1998-2010"),
                 
                 cbind('siops_despsaude_pcapita','&&&&\\\\ \\textbf{Public Health Spending}&&&& \\\\  Health Spending per capita',"Datasus/SIOPS", "2000-2010"),
                 cbind('siops_desprecpropriosaude_pcapita','&&&&\\\\ \\textit{Health Spending by Source (p.c.)} &&&& \\\\  Own Resources Spending',"Datasus/SIOPS", "2000-2010"),
                 cbind('siops_despexrecproprio_pcapita','Transfers Spending',"Datasus/SIOPS", "2000-2010"),
                 cbind('siops_desppessoal_pcapita','&&&&\\\\ \\textit{Health Spending by Type (p.c.)} &&&& \\\\ Personnel Spending',"Datasus/SIOPS", "2000-2010"),
                 cbind('siops_despinvest_pcapita','Investment Spending',"Datasus/SIOPS", "2000-2010"),
                 cbind('siops_despservicoster_pcapita','3\\textsuperscript{rd} parties services Spending)',"Datasus/SIOPS", "2000-2010"),
                 cbind('siops_despoutros_pcapita','Admin, Management, Other',"Datasus/SIOPS", "2000-2010"),
                 
                 
                 # Access and production
                 
                cbind('ACS_popprop','\\textbf{Primary Care Coverage}&&&&\\\\ \\textit{Extensive Margin (share)}&&&& \\\\ Population covered by ACS',"Datasus/SIAB","1998-2010"),
                cbind('eSF_popprop','Population covered by PSF',"Datasus/SIAB","1998-2010"),
                cbind('siab_accomp_especif_pcapita','\\textit{Intensive Margin (per capita)}&&&& \\\\ N. of People Visited by PCA',"Datasus/SIAB","1998-2010"),
                cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by ACS',"Datasus/SIAB" ,"1998-2010"),
                cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by PSF',"Datasus/SIAB", "1998-2010"),
                cbind('siab_visit_cons_pcapita','N. of Household Visits and Appointments',"Datasus/SIAB", "1998-2010"),
                cbind('siab_visit_cons_pacs_pcapita','N. of Household Visits and Appointments by ACS',"Datasus/SIAB", "1998-2010"),
                cbind('siab_visit_cons_psf_pcapita','N. of Household Visits and Appointments by PSF',"Datasus/SIAB", "1998-2010"),

                
                cbind('ams_hr_all_pcapita',"\\textbf{Health Human Resources (per capita $\\times$ 1,000)}&&&& \\\\ N. of Health Professionals","IBGE/AMS", "`99, `02, `05, `09"),
                cbind('ams_hr_superior_pcapita','N. of Doctors',"IBGE/AMS", "`99, `02, `05, `09"),
                cbind('ams_hr_technician_pcapita','N. of Nurses',"IBGE/AMS", "`99, `02, `05, `09"),
                cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants',"IBGE/AMS", "`99, `02, `05, `09"),
                cbind('ams_hr_admin_pcapita','N. of Administrative Professionals',"IBGE/AMS", "`99, `02, `05, `09"),
                
                
                                
                cbind('sia_ncnes_acs_pcapita','\\textbf{Primary Care Related Infrastructure \\& HR}&&&& \\\\ \\textit{N. of Health Facilities (per capita$\\times$1,000) with:}&&&& \\\\  \\ \\ Ambulatory Service and ACS Teams',"Datasus/SIA", "1998-2007"),
                cbind('sia_ncnes_medcom_pcapita','\\ \\ Ambulatory Service and Community Doctors',"Datasus/SIA", "1998-2007"),
                cbind('sia_ncnes_medpsf_pcapita','\\ \\ Ambulatory Service and PSF Doctors',"Datasus/SIA", "1998-2007"),
                cbind('sia_ncnes_enfacs_pcapita','\\ \\ Ambulatory Service and ACS Nurses',"Datasus/SIA", "1998-2007"),
                cbind('sia_ncnes_enfpsf_pcapita','\\ \\ Ambulatory Service and PSF Nurses',"Datasus/SIA", "1998-2007"),
                cbind('sia_ncnes_outpsf_pcapita','\\ \\ Ambulatory Service and PSF Nursing Assistants',"Datasus/SIA", "1998-2007"),
                
                cbind('sia_pcapita','\\textbf{Ambulatory Production (per capita $\\times$ 1000)}&&&& \\\\ N. Outpatient Procedures',"Datasus/SIA", "1998-2007"),
                cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures',"Datasus/SIA", "1998-2007"),
                cbind('sia_nprod_amb_lc_mun_pcapita','N. Low \\& Mid Complexity Outpatient Procedures',"Datasus/SIA", "1998-2007"),
                cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures',"Datasus/SIA", "1998-2007"),
                
                cbind('birth_prenat_ig','\\textbf{Access to Health Services (share)}&&&& \\\\ Prenatal Visits: Unknown',"Datasus/SINASC", "1998-2010"),
                cbind('birth_prenat_0','Prenatal Visits: None',"Datasus/SINASC", "1998-2010"),
                cbind('birth_prenat_1_6','Prenatal Visits: 1--6',"Datasus/SINASC", "1998-2010"),
                cbind('birth_prenat_7_plus','Prenatal Visits: 7+',"Datasus/SINASC", "1998-2010"),
                 

                cbind('tx_sih_maternal','\\textbf{Hospitalization (per capita $\\times$ 1000)}&&&& \\\\ Maternal Hospitalization Rate',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_infant_icsap','Infant Hospitalization Rate -- APC',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate -- non-APC',"Datasus/SIH", "1998-2010"),
                #cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)',"Datasus/SIH", "1998-2010"),
                #cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)',"Datasus/SIH", "1998-2010"),
                
                # Inputs
                 
                cbind('ams_hospital_mun_pcapita','\\textbf{Health System (per capita $\\times$ 1000)} &&&& \\\\ N. of Municipal Hospitals',"IBGE/AMS", "`99, `02, `05, `09"),
                cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals',"IBGE/AMS", "`99, `02, `05, `09"),
                cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals',"IBGE/AMS", "`99, `02, `05, `09"),
                cbind('cobertura_plano','Private Insurance Coverage',"Datasus/SIOPS", "2000-2010"),
                # cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                
                #sia_ncnes_amb_mun_pcapita
                
                # Adult Mortality and Hospitalization
                cbind('tx_sih_adult','\\textbf{Adult Hospitalization (per capita $\\times$ 1000)} &&&& \\\\ Adult Hospitalization',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_adult_icsap','Adult Hospitalization -- APC',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_adult_nicsap','Adult Hospitalization Rate -- non-APC',"Datasus/SIH", "1998-2010"),
                cbind('tx_ma5','\\textbf{Adult Mortality (per capita $\\times$ 1000)} &&&& \\\\  Adult Mortality',"Datasus/SIM", "1998-2010"),
                cbind('tx_ma5_icsap','Adult Mortality -- APC',"Datasus/SIM", "1998-2010"),
                cbind('tx_ma5_nicsap','Adult Mortality -- non-APC',"Datasus/SIM", "1998-2010"),
                
                # Health care system
                cbind('tx_sih_in_hosp_total','\\textbf{Hospitalization Flows (per capita $\\times$ 1000)} &&&& \\\\ Total Hospitalization Inflow',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_in_hosp_icsap','Inflow Amenable to Primary Care',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_in_hosp_nicsap','Inflow Not Amenable to Primary Care',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_out_hosp_total','Total Hospitalization Outflow',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_out_hosp_icsap','Outflow Amenable to Primary Care',"Datasus/SIH", "1998-2010"),
                cbind('tx_sih_out_hosp_nicsap','Outflow Not Amenable to Primary Care',"Datasus/SIH", "1998-2010"),
                
                 
                 # hosp
                 
                 
                 # Birth & Fertility
                #  
                # cbind('birth_prenat_ig','Prenatal Visits - Ignored'),
                # cbind('birth_prenat_0','Prenatal Visits -  None'),
                # cbind('birth_prenat_1_6','Prenatal Visits - 1-6'),
                # cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
                #cbind('birth_fertility','Fertility (N. of Births per 10-49y women)',"a", "1998-2010"),
                #cbind('birth_apgar1','Apgar 1',"a", "1998-2010"),
                #cbind('birth_apgar5','Apgar 5',"a", "1998-2010"),
                #cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)',"a", "1998-2010"),
                #cbind('birth_premature','Premature Birth',"a", "1998-2010"),
                #cbind('birth_sexratio',"Sex Ratio at Birth","a", "1998-2010"),
                #cbind('birth_c_sections','Share of C-Section',"a", "1998-2010"),
                #cbind('birth_hospital','Birth at Hospital',"a", "1998-2010"),
                 
                 # IMR
                cbind('tx_mi','\\textbf{Infant Mortality Rate (per 1,000)} &&&& \\\\ Infant Mortality Rate (all cause)',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_icsap','Amenable to Primary Care (APC)',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_nicsap','non-APC',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_fet','Fetal',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_24h','Within 24h',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_27d','1 to 27 days',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_ano','27 days to 1 year',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_infec','Infectious',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_resp','Respiratory',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_perinat','Perinatal',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_cong','Congenital',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_ext','External',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_nut','Nutritional',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_out','Other',"Datasus/SIM", "1998-2010"),
                cbind('tx_mi_illdef','Ill-Defined',"Datasus/SIM", "1998-2010"),

                 

                # Index
                #cbind('access_index','Access and Production of Health Services Index',"a", "1998-2010"),
                #cbind('access_pc_index','Primary Care Access and Production Index',"a", "1998-2010"),
                #cbind('access_npc_index','Non-Primary Care Access and Production Index',"a", "1998-2010"),
                #cbind('input_index','Health Inputs Index',"a", "1998-2010"),
                #cbind('hr_index','Human Resources Index',"a", "1998-2010"),
                #cbind('hospital_index','Hospitals Index',"a", "1998-2010"),
                #cbind('birth_index','Birth Outcomes Index',"a", "1998-2010"),
                #cbind('imr_index','Infant Mortality Index',"a", "1998-2010"),
                #cbind('birth_others_index','Other Birth Outcomes Index',"a", "1998-2010"),
                 
                 
                 cbind('pop','\\textbf{Controls} &&&& \\\\ Population (1,000s)',"IBGE/Census", "1998-2010"),
                 cbind('gdp_mun_pcapita','GDP per capita (2010 R\\$)',"IBGE/Census", "1998-2010"),
                 cbind('pbf_pcapita',"Bolsa Familia transfers per capita","IBGE/Census", "1998-2010"),
                 cbind('espvida_baseline','Life Expectancy',"IBGE/Census", "1998-2010"),
                 cbind('e_anosestudo_baseline','Expected Years of Study',"IBGE/Census", "1998-2010"),
                 cbind('t_analf18m_baseline','Iliteracy Rate (above 18y old)',"IBGE/Census", "1998-2010"),
                 cbind('rdpc_baseline','Income per capita',"IBGE/Census", "1998-2010"),
                 cbind('pmpob_baseline','Share of Population Below Poverty Line',"IBGE/Census", "1998-2010"),
                 cbind('gini_baseline','Gini Coefficient',"IBGE/Census", "1998-2010"),
                 cbind('sewage_gen_network_baseline','Access to Sewage Network',"IBGE/Census", "1998-2010"),
                 cbind('garbage_coll_service_baseline','Access to Garbage Collection Service', "IBGE/Census", "1998-2010"),
                 cbind('water_gen_network_baseline','Access to Water Network',"IBGE/Census", "1998-2010"),
                 cbind('elect_access_baseline','Access to Electricity',"IBGE/Census", "1998-2010"),
                 cbind('urb_baseline','Urbanization Rate',"IBGE/Census", "1998-2010"),
                 cbind('finbra_desp_saude_san_pcapita_neighbor','Average Neighbor Health Spending p.c.',"FINBRA", "1998-2010"),
                 cbind('lrf',"Human Resources Spending (/Revenue)","FINBRA", "1998-2010"))
                 
                 
                 



# adding copying AMS data to 2000
ams_vars <- grep("_pcapita",grep("ams",names(df),value = T),value = T)
df <- df %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))


# 4. Summary Statistics
# =================================================================
summary_stat(df %>% mutate(pop = pop/1000),2000,"stats_2000")



# 5. final table and export
# =================================================================

stats <- stats_2000 %>% 
    mutate_at(c("Mean","Std.Dev","Obs"),~ round(.,digits = 3)) %>% 
    select(-Baseline)


output_file <- "regression_tables_raw_28Aug2024.xlsx"
dir1 <- "/home/damian/investigacion/2021/decentralization/github/ec29/regs_outputs/"
write.table(stats, file = paste0(dir1,"sumstats.tex"),row.names = F,sep="&", eol="\\\\ \n", quote=FALSE,col.names=F)
#write.xlsx2(stats, file = paste0(dir,main_folder,output_file),sheetName = "descriptive",row.names = F,append = T)


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

df <- df %>% 
  filter(!(cod_mun %in% outliers))

summary_stat(df %>% mutate(pop = pop/1000),2000,"stats_2000")



# 5. final table and export
# =================================================================

stats <- stats_2000 %>% 
  mutate_at(c("Mean","Std.Dev","Obs"),~ round(.,digits = 3)) %>% 
  select(-Baseline)

dir1 <- "/home/damian/investigacion/2021/decentralization/github/ec29/regs_outputs/"
write.table(stats, file = paste0(dir1,"sumstats_nooutliers.tex"),row.names = F,sep="&", eol="\\\\ \n", quote=FALSE,col.names=F)
