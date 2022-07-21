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


# 3. Statistic for variables with baseline in 2000
# =================================================================

var_map <- rbind(cbind("ec29_baseline","Share of Municipality's Own Resource Spent in Health"),
                 cbind("dist_ec29_baseline","Distance to the EC29 Target"),
                 
                 # fiscal reaction
                 cbind('finbra_reccorr_pcapita','Total Revenue per capita (2010 R$)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (2010 R$)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita (2010 R$)'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (2010 R$)'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (2010 R$)'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_area_pcapita','Other Areas Spending per capita (2010 R$)'),
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'),
                 
                 
                 # coverage and infra
                 
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
                 cbind('leitos_pc',"N. of Hospital Beds (per capita)"),
                 cbind('hospital','Presence of Hospital'),
                 
                 cbind('ams_hospital_mun_pcapita','N. of Municipal Hospitals (per capita*1000)'),
                 cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals (per capita*1000)'),
                 cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
                 cbind('ams_unity_mun_pcapita','N. of Health Facilities (per capita*1000)'),
                 cbind('ams_hr_all_pcapita',"N. of Health Professionals (per capita*1000)"),
                 cbind('ams_hr_superior_pcapita','N. of Doctors (per capita*1000)'),
                 cbind('ams_hr_technician_pcapita','N. of Nurses (per capita*1000)'),
                 cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants (per capita*1000)'),
                 cbind('ams_hr_admin_pcapita','N. of Administrative Professionals (per capita*1000)'),
                 
                 # access and productivity
                 
                 cbind('birth_prenat_ig','Prenatal Visits - Ignored'),
                 cbind('birth_prenat_0','Prenatal Visits None'),
                 cbind('birth_prenat_1_6','Prenatal Visits 1-6'),
                 cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
                 
                 cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
                 cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_low_skill_mun_pcapita','N. Outpatient Procedures by Low Skilled Workers (per capita)'),
                 cbind('sia_nprod_med_skill_mun_pcapita','N. Outpatient procedures by Mid Skilled Workers (per capita)'),
                 
                 
                 # IMR
                 
                 cbind('tx_mi','Infant Mortality Rate'),
                 cbind('tx_mi_icsap','Infant Mortality Rate - APC'),
                 cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC'),
                 cbind('tx_mi_fet','Infant Mortality Rate - Fetal'),
                 cbind('tx_mi_24h','Infant Mortality Rate - Within 24h'),
                 cbind('tx_mi_27d','Infant Mortality Rate - 1 to 27 days'),
                 cbind('tx_mi_ano','Infant Mortality Rate - 27 days to 1 year'),
                 cbind('tx_mi_infec','Infant Mortality Rate - Infectious'),
                 cbind('tx_mi_resp','Infant Mortality Rate - Respiratory'),
                 cbind('tx_mi_perinat','Infant Mortality Rate - Perinatal'),
                 cbind('tx_mi_cong','Infant Mortality Rate - Congenital'),
                 cbind('tx_mi_ext','Infant Mortality Rate - External'),
                 cbind('tx_mi_nut','Infant Mortality Rate - Nutritional'),
                 cbind('tx_mi_out','Infant Mortality Rate - Other'),
                 cbind('tx_mi_illdef','Infant Mortality Rate - Ill-Defined'),
                 cbind('tx_mm',"Maternal Mortality Rate"),
                 
                 # Birth & Fertility
                 
                 cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
                 cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_premature','Premature Birth'),
                 cbind('birth_sexratio',"Sex Ratio at Birth"),
                 
                 # Maternal and Infant Hospitalizatio
                 cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
                 cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)'),
                 
                 
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
                 cbind('lrf',"Municipality's Spending in Human Resources (% of Total Revenue)"),
                 
                 
                 cbind('finbra_rectribut_pcapita','Tax Revenue per capita (2010 R$)'),
                 cbind('finbra_rectransf_pcapita','Transfers Revenue per capita (2010 R$)'),
                 cbind('finbra_rec_outros_pcapita','Other Revenues per capita (2010 R$)'),
                 
                 cbind('finbra_rectribut_share','Tax Revenue (% Total Revenue)'),
                 cbind('finbra_rectransf_share','Transfers Revenue (% Total Revenue)'),
                 cbind('finbra_rec_outros_share','Other Revenues (% Total Revenue)'),
                 
                 cbind('finbra_desp_saude_san_share','Health and Sanitation Spending (% Total Spending)'),
                 cbind('finbra_desp_transporte_share','Transport Spending (% Total Spending)'),
                 cbind('finbra_desp_educ_cultura_share','Education and Culture Spending (% Total Spending)'),
                 cbind('finbra_desp_hab_urb_share','Housing and Urban Spending (% Total Spending)'),
                 cbind('finbra_desp_assist_prev_share','Social Assistance Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_area_share','Other Areas Spending (% Total Spending)'),
                 
                 cbind('siops_desprecpropriosaude_share','Health Spending - Own Resources (% Health Spending)'),
                 cbind('siops_despexrecproprio_share','Health Spending - Transfers (% Health Spending)'),
                 cbind('siops_desppessoal_share','Health Spending - Human Resources (% Health Spending)'),
                 cbind('siops_despinvest_share','Health Spending - Investiment (% Health Spending)'),
                 cbind('siops_despservicoster_share','Health Spending - 3rd parties services (% Health Spending)'),
                 cbind('siops_despoutros_share','Health Spending - other expenditures (% Health Spending)'),
                 
                 cbind('siab_regist_pers_pcapita','N. of People Register in the Primary Care System (per capita)'),
                 cbind('siab_regist_pers_pacs_pcapita','N. of People Register in the CH Program (per capita)'),
                 cbind('siab_regist_pers_psf_pcapita','N. of People Register in the FH Program (per capita)'),
                 
                 cbind('sia_ncnes_amb_lc_mun_pcapita','N. of Health Facilities with Low & Mid Complexity Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_amb_hc_mun_pcapita','N. of Health Facilities with High Complexity Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_low_skill_mun_pcapita','N. of Health Facilities with Ambulatory Service by Low Skilled Workers (per capita*1000)'),
                 cbind('sia_ncnes_med_skill_mun_pcapita','N. of Health Facilities with Ambulatory Service by Mid Skilled Workers (per capita*1000)'),
                 cbind('sia_ncnes_enf_mun_pcapita','N. of Health Facilities with Ambulatory Service by Nurses (per capita*1000)'),
                 cbind('sia_ncnes_enfobs_mun_pcapita','N. of Health Facilities with Ambulatory Service by Obstetrical Nurses (per capita*1000)'),
                 
                 cbind('sia_ncnes_ginobs_mun_pcapita','N. of Health Facilities with Obstetrical/Gyneco. Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_pediat_mun_pcapita','N. of Health Facilities with Pediatric Ambulatory Service (per capita*1000)')
                 
                 
)


# generating df of top and bottom quartile of the distance to ec29 in the baseline

df_q <- df %>%
  group_by(ano) %>% 
  mutate(quantile = ifelse(ano==2000,quantcut(ec29_baseline,4),NA)) %>% 
  ungroup() %>% 
  group_by(cod_mun) %>% 
  mutate(quantile = mean(quantile, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(quantile = as.character(quantile)) %>% 
  filter(quantile != 'NaN')


# adding copying AMS data to 2000
ams_vars <- grep("_pcapita",grep("ams",names(df),value = T),value = T)
df <- df %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))

df_low_ineq <- df_low_ineq %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))
df_high_ineq <- df_high_ineq %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))

df_low_pov <- df_low_pov %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))
df_high_pov <- df_high_pov %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))

df_low_hi <- df_low_hi %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))
df_high_hi <- df_high_hi %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))

df_first <- df_first %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))

df_second <- df_second %>% 
  mutate_at(ams_vars, function(x) ifelse(.$ano==2000,dplyr::lag(x,1),x))


summary_stat(df %>% mutate(pop = pop/1000),2000,"stats_2000")


summary_stat(df_low_ineq %>%  mutate(pop = pop/1000),2000,"stats_2000_low_ineq")
summary_stat(df_high_ineq %>%  mutate(pop = pop/1000),2000,"stats_2000_high_ineq")

summary_stat(df_low_pov %>%  mutate(pop = pop/1000),2000,"stats_2000_low_pov")
summary_stat(df_high_pov %>%  mutate(pop = pop/1000),2000,"stats_2000_high_pov")

summary_stat(df_low_hi %>%  mutate(pop = pop/1000),2000,"stats_2000_low_hi")
summary_stat(df_high_hi %>%  mutate(pop = pop/1000),2000,"stats_2000_high_hi")

summary_stat(df_first %>% mutate(pop = pop/1000),2000,"stats_2000_first")
summary_stat(df_second %>% mutate(pop = pop/1000),2000,"stats_2000_second")




# 6. final table and export
# =================================================================

stats <- cbind(
  stats_2000 %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Baseline),
  stats_2000_low_ineq %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable,-Baseline),
  stats_2000_high_ineq %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable,-Baseline),
  stats_2000_low_pov %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable,-Baseline),
  stats_2000_high_pov %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable,-Baseline),
  stats_2000_low_hi %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable,-Baseline),
  stats_2000_high_hi %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable,-Baseline),
  stats_2000_first %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable,-Baseline),
  stats_2000_second %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable)  )

sample <- c("","full","full","full","full","full",
            "low ineq","low ineq","low ineq","low ineq","low ineq",
            "high ineq","high ineq","high ineq","high ineq","high ineq",
            "low pov","low pov","low pov","low pov","low pov",
            "high pov","high pov","high pov","high pov","high pov",
            "low hi","low hi","low hi","low hi","low hi",
            "high hi","high hi","high hi","high hi","high hi",
            "first","first","first","first","first",
            "second","second","second","second","second")

stats <- rbind(sample,stats)

write.xlsx2(stats, file = paste0(dir,main_folder,output_file),sheetName = "descriptive",row.names = F,append = T)



