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

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


# 1. Load data
# =================================================================
load(paste0(dir,"regs.RData"))

# creating missing SIA variable
df <- df %>% 
  mutate(sia_nab_pcapita = sia_pcapita - sia_ab_pcapita)


index <- data.frame(read.dta13("C:/Users/Michel/Documents/GitHub/ec29/indexes.dta"))


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
  
  var_names_simple <- unlist(lapply(var_list, function(x) gsub("_","",x)))
  var_map<- cbind(var_map,var_names_simple)
  
  df_stat <- df %>%
    filter(ano==baseline_year) %>%
    select(all_of(var_list))
  
  names(df_stat) <- var_names_simple
  
  
  df_stat <- df_stat %>% 
    summarise_all(list(mean = ~ mean(.,na.rm = T), 
                       sd = ~ sd(.,na.rm = T),
                       min = ~ min(., na.rm = T),
                       max = ~ max(., na.rm = T),
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
    select(var_lables,everything())
  
  names(df_stat) <- c("Variable","Mean","Std.Dev","Min","Max","Obs")
  df_stat <- df_stat %>%
    mutate(Baseline = baseline_year)
  
  assign(paste0(obj),df_stat,envir = .GlobalEnv)
  
}


# 3. List of variables and baseline year adjustments
# =================================================================

var_map <- rbind(cbind("ec29_baseline","Share of Municipality's Own Resource Spent in Health"),
                 cbind("dist_ec29_baseline","Distance to the EC29 Target"),
                 
                 # fiscal reaction
                 cbind('finbra_recorc_pcapita','Total Revenue per capita (log)'),
                 cbind('finbra_reccorr_pcapita','Current Revenue per capita (log)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (log)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
                 cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)'),
                 cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (log)'),
                 cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (log)'),
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (log)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (log)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (log)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (log)'),
                 
                 
                 # Access and production
                 
                cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
                cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
                cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
                cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
                cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
                cbind('siab_visit_cons_pcapita','N. of Household Visits and Appointments (per capita)'),
                cbind('siab_visit_cons_pacs_pcapita','N. of Household Visits and Appointments from Community Health Agents (per capita)'),
                cbind('siab_visit_cons_psf_pcapita','N. of Household Visits and Appointments from Family Health Agents (per capita)'),
                
                cbind('sia_ncnes_amb_mun_pcapita','N. of Health Facilities with Ambulatory Service (per capita*1000)'),
                cbind('sia_ncnes_acs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Teams (per capita*1000)'),
                cbind('sia_ncnes_psf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Teams (per capita*1000)'),
                cbind('sia_ncnes_medcom_pcapita','N. of Health Facilities with Ambulatory Service and Community Doctors (per capita*1000)'),
                cbind('sia_ncnes_medpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Doctors (per capita*1000)'),
                cbind('sia_ncnes_enfacs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Nurses (per capita*1000)'),
                cbind('sia_ncnes_enfpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nurses (per capita*1000)'),
                cbind('sia_ncnes_outpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nursing Assistants (per capita*1000)'),
                
                cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
                cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
                cbind('sia_nab_pcapita','N. Non-Primary Care Outpatient Procedures (per capita)'), # precisa criar
                cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
                cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
                
                cbind('birth_prenat_ig','Proportion of births with unknown prenatal care coverage'),
                cbind('birth_prenat_0','Proportion of births with 0 prenatal visits'),
                cbind('birth_prenat_1_6','Proportion of births with 1-6 prenatal visits'),
                cbind('birth_prenat_7_plus','Proportion of births with 7+ prenatal visits'),
                 
                 # Inputs
                 
                cbind('ams_hospital_mun_pcapita','N. of Municipal Hospitals (per capita*1000)'),
                cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals (per capita*1000)'),
                # cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                
                cbind('ams_hr_all_pcapita',"N. of Health Professionals (per capita*1000)"),
                cbind('ams_hr_superior_pcapita','N. of Doctors (per capita*1000)'),
                cbind('ams_hr_technician_pcapita','N. of Nurses (per capita*1000)'),
                cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants (per capita*1000)'),
                cbind('ams_hr_admin_pcapita','N. of Administrative Professionals (per capita*1000)'),
                 
                 
                 # hosp
                 
                cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)'),
                cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
                cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
                cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
                cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)'),
                 
                 # Birth & Fertility
                #  
                # cbind('birth_prenat_ig','Prenatal Visits - Ignored'),
                # cbind('birth_prenat_0','Prenatal Visits -  None'),
                # cbind('birth_prenat_1_6','Prenatal Visits - 1-6'),
                # cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
                cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
                cbind('birth_apgar1','Apgar 1'),
                cbind('birth_apgar5','Apgar 5'),
                cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                cbind('birth_premature','Premature Birth'),
                cbind('birth_sexratio',"Sex Ratio at Birth"),
                cbind('birth_c_sections','Share of C-Section'),
                cbind('birth_hospital','Birth at Hospital'),
                 
                 # IMR
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
                cbind('tx_mm',"Maternal Mortality Rate"),
                 
                 # Adult Mortality and Hospitalization
                cbind('tx_sih_adult','Adult Hospitalization Rate (pop 40+y * 1000)'),
                cbind('tx_sih_adult_icsap','Adult Hospitalization Rate - APC (pop 40+y * 1000)'),
                cbind('tx_sih_adult_nicsap','Adult Hospitalization Rate - non-APC (pop 40+y * 1000)'),
                cbind('tx_ma5','Adult Mortality Rate (40+ y)'),
                cbind('tx_ma5_icsap','Adult Mortality Rate (40+ y) - APC'),
                cbind('tx_ma5_nicsap','Adult Mortality Rate (40+ y) - non-APC'),
                 
                 
                 # Health care system
                cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                cbind('cobertura_plano','Private Insurance Coverage'),
                cbind('tx_sih_in_hosp_total','Hospitalization Inflow rate (pop * 1000)'),
                cbind('tx_sih_in_hosp_icsap','Hospitalization Inflow rate - APC (pop * 1000)'),
                cbind('tx_sih_in_hosp_nicsap','Hospitalization Inflow rate - non-APC (pop * 1000)'),
                cbind('tx_sih_out_hosp_total','Hospitalization Outflow rate (pop * 1000)'),
                cbind('tx_sih_out_hosp_icsap','Hospitalization Outflow rate - APC (pop * 1000)'),
                cbind('tx_sih_out_hosp_nicsap','Hospitalization Outflow rate - non-APC (pop * 1000)'),

                # Index
                cbind('access_index','Access and Production of Health Services Index'),
                cbind('access_pc_index','Primary Care Access and Production Index'),
                cbind('access_npc_index','Non-Primary Care Access and Production Index'),
                cbind('input_index','Health Inputs Index'),
                cbind('hr_index','Human Resources Index'),
                cbind('hospital_index','Hospitals Index'),
                cbind('birth_index','Birth Outcomes Index'),
                cbind('imr_index','Infant Mortality Index'),
                cbind('birth_others_index','Other Birth Outcomes Index'),
                 
                 
                 cbind('pop','Population'),
                 cbind('gdp_mun_pcapita','GDP per capita (2010 R$)'),
                 cbind('pbf_pcapita',"'Bolsa Familia' transfers per capita (2010 R$)"),
                 cbind('espvida_baseline','Life Expectancy'),
                 cbind('e_anosestudo_baseline','Expected Years of Study'),
                 cbind('t_analf18m_baseline','Iliteracy Rate (above 18y old)'),
                 cbind('rdpc_baseline','Income per capita'),
                 cbind('pmpob_baseline','Share of Population Below Poverty Line'),
                 cbind('gini_baseline','Gini Coefficient'),
                 cbind('sewage_gen_network_baseline','Access to Sewage Network'),
                 cbind('garbage_coll_service_baseline','Access to Garbage Collection Service'),
                 cbind('water_gen_network_baseline','Access to Water Network'),
                 cbind('elect_access_baseline','Access to Electricity'),
                 cbind('urb_baseline','Urbanization Rate'),
                 cbind('finbra_desp_saude_san_pcapita_neighbor','Average Neighbors Spending Health Spending per capita (2010 R$)'),
                 cbind('lrf',"Municipality's Spending in Human Resources (% of Total Revenue)"))
                 
                 
                 



# adding copying AMS data to 2000
ams_vars <- grep("_pcapita",grep("ams",names(df),value = T),value = T)
df <- df %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))


# 4. Summary Statistics
# =================================================================
summary_stat(df %>% mutate(pop = pop/1000),2000,"stats_2000")


# 5. Summary Statistics without spending outliers
# =================================================================

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
summary_stat(df2 %>% mutate(pop = pop/1000),2000,"stats_2000_no")



# 6. final table and export
# =================================================================

stats <- stats_2000 %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Baseline)

stats_no <- stats_2000_no %>% 
  mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
  select(-Baseline)


output_file <- "regression_tables_raw.xlsx"
write.xlsx2(stats, file = paste0(dir,main_folder,output_file),sheetName = "descriptive",row.names = F,append = T)

write.xlsx2(stats_no, file = paste0(dir,main_folder,output_file),sheetName = "descriptive_nooutliers",row.names = F,append = T)


