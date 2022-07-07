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


options(digits = 30)


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
                 
                 cbind('finbra_reccorr_pcapita','Total Revenue per capita (2010 R$)'),
                 cbind('finbra_rectribut_pcapita','Tax Revenue per capita (2010 R$)'),
                 cbind('finbra_rectransf_pcapita','Transfers Revenue per capita (2010 R$)'),
                 cbind('finbra_rec_outros_pcapita','Other Revenues per capita (2010 R$)'),
                 
                 cbind('finbra_rectribut_share','Tax Revenue (% Total Revenue)'),
                 cbind('finbra_rectransf_share','Transfers Revenue (% Total Revenue)'),
                 cbind('finbra_rec_outros_share','Other Revenues (% Total Revenue)'),
                 
                 cbind('finbra_desp_o_pcapita','Total Spending per capita (2010 R$)'),
                 cbind('finbra_desp_pessoal_pcapita','Human Resources Spending per capita (2010 R$)'),
                 cbind('finbra_desp_investimento_pcapita','Investment Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_nature_pcapita','Other Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_pessoal_share','Human Resources Spending (% Total Spending)'),
                 cbind('finbra_desp_investimento_share','Investment Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_nature_share','Other Spending per capita (% Total Spending)'),
                 
                 cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (2010 R$)'),
                 cbind('finbra_desp_transporte_pcapita','Transport Spending per capita (2010 R$)'),
                 cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (2010 R$)'),
                 cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (2010 R$)'),
                 cbind('finbra_desp_assist_prev_pcapita','Social Assistance Spending per capita (2010 R$)'),
                 cbind('finbra_desp_outros_area_pcapita','Other Areas Spending per capita (2010 R$)'),
                 
                 cbind('finbra_desp_saude_san_share','Health and Sanitation Spending (% Total Spending)'),
                 cbind('finbra_desp_transporte_share','Transport Spending (% Total Spending)'),
                 cbind('finbra_desp_educ_cultura_share','Education and Culture Spending (% Total Spending)'),
                 cbind('finbra_desp_hab_urb_share','Housing and Urban Spending (% Total Spending)'),
                 cbind('finbra_desp_assist_prev_share','Social Assistance Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_area_share','Other Areas Spending (% Total Spending)'),
                 
                 
                 cbind('siops_despsaude_pcapita','Health Spending per capita - Total (2010 R$)'),
                 cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (2010 R$)'),
                 cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (2010 R$)'),
                 cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (2010 R$)'),
                 cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (2010 R$)'),
                 cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (2010 R$)'),
                 cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (2010 R$)'),
                 
                 
                 cbind('siops_desprecpropriosaude_share','Health Spending - Own Resources (% Health Spending)'),
                 cbind('siops_despexrecproprio_share','Health Spending - Transfers (% Health Spending)'),
                 cbind('siops_desppessoal_share','Health Spending - Human Resources (% Health Spending)'),
                 cbind('siops_despinvest_share','Health Spending - Investiment (% Health Spending)'),
                 cbind('siops_despservicoster_share','Health Spending - 3rd parties services (% Health Spending)'),
                 cbind('siops_despoutros_share','Health Spending - other expenditures (% Health Spending)'),
                 
                 
                 cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
                 cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
                 cbind('siab_regist_pers_pcapita','N. of People Register in the Primary Care System (per capita)'),
                 cbind('siab_regist_pers_pacs_pcapita','N. of People Register in the CH Program (per capita)'),
                 cbind('siab_regist_pers_psf_pcapita','N. of People Register in the FH Program (per capita)'),
                 cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
                 cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
                 cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
                 cbind('siab_visit_cha_pcapita','N. of Household Visits (per capita)'),
                 cbind('siab_visit_cha_pacs_pcapita','N. of Household Visits by Community Health Agents (per capita)'),
                 cbind('siab_visit_cha_psf_pcapita','N. of Household Visits by Family Health Agents (per capita)'),
                 cbind('siab_cons_especif_pcapita','N. of Appointments (per capita)'),
                 cbind('siab_cons_especif_pacs_pcapita','N. of Appointments from Community Health Program (per capita)'),
                 cbind('siab_cons_especif_psf_pcapita','N. of Appointments from Family Health Program (per capita)'),
                 
                 cbind('sia_ncnes_amb_mun_pcapita','N. of Health Facilities with Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_amb_lc_mun_pcapita','N. of Health Facilities with Low & Mid Complexity Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_amb_hc_mun_pcapita','N. of Health Facilities with High Complexity Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_low_skill_mun_pcapita','N. of Health Facilities with Ambulatory Service by Low Skilled Workers (per capita*1000)'),
                 cbind('sia_ncnes_med_skill_mun_pcapita','N. of Health Facilities with Ambulatory Service by Mid Skilled Workers (per capita*1000)'),
                 cbind('sia_ncnes_enf_mun_pcapita','N. of Health Facilities with Ambulatory Service by Nurses (per capita*1000)'),
                 cbind('sia_ncnes_enfobs_mun_pcapita','N. of Health Facilities with Ambulatory Service by Obstetrical Nurses (per capita*1000)'),
                 cbind('sia_ncnes_medcom_pcapita','N. of Health Facilities with Ambulatory Service and Community Doctors (per capita*1000)'),
                 cbind('sia_ncnes_ginobs_mun_pcapita','N. of Health Facilities with Obstetrical/Gyneco. Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_pediat_mun_pcapita','N. of Health Facilities with Pediatric Ambulatory Service (per capita*1000)'),
                 cbind('sia_ncnes_medpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Doctors (per capita*1000)'),
                 cbind('sia_ncnes_enfpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nurses (per capita*1000)'),
                 cbind('sia_ncnes_outpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nursing Assistants (per capita*1000)'),
                 cbind('sia_ncnes_psf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Teams (per capita*1000)'),
                 cbind('sia_ncnes_acs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Teams (per capita*1000)'),
                 cbind('sia_ncnes_enfacs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Nurses (per capita*1000)'),
                 cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
                 cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
                 cbind('sia_nprod_low_skill_mun_pcapita','N. Outpatient Procedures by Low Skilled Workers (per capita)'),
                 cbind('sia_nprod_med_skill_mun_pcapita','N. Outpatient procedures by Mid Skilled Workers (per capita)'),
                 
                 
                 cbind('birth_c_sections','Share of C-Section'),
                 cbind('birth_hospital','Birth at Hospital'),
                 cbind('birth_prenat_0','Prenatal Visits None'),
                 cbind('birth_prenat_1_6','Prenatal Visits 1-6'),
                 cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
                 
                 
                 cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_gest_37plus','Gestation Weeks 37+'),
                 
                 
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
                 
                 
                 cbind('tx_ma3','25-59y Mortality Rate'),
                 cbind('tx_ma3_icsap','25-59y Mortality Rate - APC'),
                 cbind('tx_ma3_nicsap','25-59y Mortality Rate - non-APC'),
                 cbind('tx_ma3_circ','25-59y Mortality Rate - Circulatory'),
                 cbind('tx_ma3_neop','25-59y Mortality Rate - Neoplasm'),
                 cbind('tx_ma3_resp','25-59y Mortality Rate - Respiratory'),
                 cbind('tx_ma3_infec','25-59y Mortality Rate - Infectious'),
                 cbind('tx_ma3_ext','25-59y Mortality Rate - External'),
                 cbind('tx_ma3_dig','25-59y Mortality Rate - Digestive'),
                 cbind('tx_ma3_illdef','25-59y Mortality Rate - Ill-Defined'),
                 cbind('tx_ma3_out','25-59y Mortality Rate - Other'), #
                 cbind('tx_ma3_diab','25-59y Mortality Rate - Diabetes'),
                 cbind('tx_ma3_hyper','25-59y Mortality Rate - Hypertension'),
                 
                 cbind('pop','Population'),
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
                 cbind('finbra_desp_saude_san_pcapita_neighbor','Average Neighbors Spending Health Spending per capita'),
                 cbind('lrf',"Municipality's Spending in Human Resources (% of Total Spending)")
                 
                 
)


# generating df of top and bottom quartile of the distance to ec29 in the baseline

df <- df %>%
  group_by(ano) %>% 
  mutate(quantile = ifelse(ano==2000,quantcut(ec29_baseline,4),NA)) %>% 
  ungroup() %>% 
  group_by(cod_mun) %>% 
  mutate(quantile = mean(quantile, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(quantile = as.character(quantile)) %>% 
  filter(quantile != 'NaN')

# generating low (high) income sample and high (low) inequality sample
df <- df %>%
  group_by(ano) %>%
  mutate(avg_rdpc_baseline = mean(rdpc_baseline,na.rm = T),
         avg_gini_baseline = mean(gini_baseline, na.rm = T)) %>%
  ungroup() %>%
  mutate(low_income = 0,
         high_ineq = 0) %>%
  mutate(low_income = ifelse(rdpc_baseline<avg_rdpc_baseline,1,low_income),
         high_ineq = ifelse(gini_baseline>avg_gini_baseline,1,high_ineq))


df_top <- df %>% filter(quantile=="4") %>% select(quantile,dist_ec29_baseline,everything())
df_bottom <- df %>% filter(quantile=="1") %>% select(quantile,dist_ec29_baseline,everything())


summary_stat(df %>% mutate(pop = pop/1000),2000,"stats_2000")
summary_stat(df_top %>% mutate(pop = pop/1000),2000,"stats_2000_top")
summary_stat(df_bottom %>% mutate(pop = pop/1000),2000,"stats_2000_bottom")
summary_stat(df %>% mutate(pop = pop/1000) %>% filter(second_term==1),2000,"stats_2000_second")
summary_stat(df %>% mutate(pop = pop/1000) %>% filter(second_term==0),2000,"stats_2000_first")

summary_stat(df %>%  mutate(pop = pop/1000) %>% filter(low_income==1),2000,"stats_2000_lowincome")
summary_stat(df %>%  mutate(pop = pop/1000) %>% filter(low_income==0),2000,"stats_2000_highincome")

summary_stat(df %>%  mutate(pop = pop/1000) %>% filter(high_ineq==1),2000,"stats_2000_highineq")
summary_stat(df %>%  mutate(pop = pop/1000) %>% filter(high_ineq==0),2000,"stats_2000_lowineq")


# 4. Statistic for variables with baseline in 2002
# =================================================================

# var_map <- rbind(cbind('ams_cnes_bed_sur_total_pcapita','Surgical Public Beds (per capita * 1000)'),
#                  cbind('ams_cnes_bed_clin_total_pcapita','Clinical Public Beds (per capita * 1000)'),
#                  cbind('ams_cnes_bed_obs_total_pcapita','Obstetric Public Beds (per capita * 1000)'),
#                  cbind('ams_cnes_bed_ped_total_pcapita','Pediatric Public Beds (per capita * 1000)'),
#                  cbind('ams_cnes_bed_sus_priv_pcapita','Total Number of Beds, Public and Private (per capita * 1000)'),
#                  cbind('ams_cnes_bed_total_pcapita','Number of Public of Beds (per capita * 1000)'),
#                  cbind('ams_cnes_itubed_total_pcapita','Number of Public ITU Beds (per capita * 1000)'),
#                  cbind('ams_cnes_ad_itubed_total_pcapita','Number of Public Adult ITU Beds (per capita * 1000)'),
#                  cbind('ams_cnes_hpp_count_pcapita','Number of Small Facilities with Hospital Services (per capita * 1000)'),
#                  cbind('ams_cnes_hpp_bed_count_pcapita','Number of Beds in Small Facilities (per capita * 1000)'),
#                  cbind('ams_cnes_hmp_count_pcapita','Number of Medium Facilities with Hospital Services (per capita * 1000)'),
#                  cbind('ams_cnes_hmp_bed_count_pcapita','Number of Beds in Medium Facilities (per capita * 1000)'),
#                  cbind('ams_cnes_hgp_count_pcapita','Number of Large Facilities with Hospital Services (per capita * 1000)'),
#                  cbind('ams_cnes_hgp_bed_count_pcapita','Number of Beds in Large Facilities (per capita * 1000)'),
#                  cbind('ams_cnes_n_estab_emg_pcapita','Number of Facilities with Emergency Services (per capita * 1000)'),
#                  cbind('ams_cnes_n_estab_amb_pcapita','Number of Facilities with Ambulatorial Services (per capita * 1000)'),
#                  cbind('ams_cnes_n_estab_pcapita','Number of Facilities (per capita * 1000)')
# )
# 
# 
# summary_stat(df,2002,"stats_2002")



# 5. Statistic for variables with baseline in 2005
# =================================================================

# load(paste0(dir,"regs_cs_2005.RData"))


# 
# var_map <- rbind(cbind('cnes_st_all_mun_pcapita','Municipal Health Facilities (per capita * 1mi)'),
#                  cbind('cnes_st_posto_mun_pcapita','Municipal Health Center (per capita * 1mi)'),
#                  cbind('cnes_st_consultorio_mun_pcapita','Municipal Consulting Rooms (per capita * 1mi)'),
#                  cbind('cnes_st_clinicaesp_mun_pcapita','Municipal Specialized Health Clinics (per capita * 1mi)'),
#                  cbind('cnes_st_diagnos_mun_pcapita','Municipal Diagnostic Center (per capita * 1mi)'),
#                  cbind('cnes_st_all_private_pcapita','Private Health Facilities (per capita * 1mi)'),
#                  cbind('cnes_st_posto_private_pcapita','Private Health Center (per capita * 1mi)'),
#                  cbind('cnes_st_consultorio_private_pcapita','Private Consulting Rooms (per capita * 1mi)'),
#                  cbind('cnes_st_clinicaesp_private_pcapita','Private Specialized Health Clinics (per capita * 1mi)'),
#                  cbind('cnes_st_diagnos_private_pcapita','Private Diagnostic Center (per capita * 1mi)'),
#                  cbind('cnes_st_all_public_pcapita','Public Health Facilities (per capita * 1mi)'),
#                  cbind('cnes_st_posto_public_pcapita','Public Health Center (per capita * 1mi)'),
#                  cbind('cnes_st_consultorio_public_pcapita','Public Consulting Rooms (per capita * 1mi)'),
#                  cbind('cnes_st_clinicaesp_public_pcapita','Public Specialized Health Clinics (per capita * 1mi)'),
#                  cbind('cnes_st_diagnos_public_pcapita','Public Diagnostic Center (per capita * 1mi)'),
#                  cbind('cnes_lt_private_sus_funded_pcapita','Private Hospital Beds funded by SUS (per capita * 1mi)'),
#                  cbind('cnes_lt_mun_pcapita','Municipal Hospital Beds (per capita * 1mi)'),
#                  cbind('cnes_lt_public_pcapita','Public Hospital Beds (per capita * 1mi)'),
#                  cbind('cnes_eq_mun_pcapita','Municipal Health Equipments (per capita * 1mi)'),
#                  cbind('cnes_eq_private_pcapita','Private Health Equipments (per capita * 1mi)'),
#                  cbind('cnes_eq_public_pcapita','Public Health Equipments (per capita * 1mi)'))
# 
# 
# summary_stat(df,2005,"stats_2005")




# 6. final table and export
# =================================================================

# stats <- rbind.data.frame(stats_2000,stats_2002,stats_2005) %>% 
#   mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3))


stats <- cbind(
  stats_2000 %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)),
  stats_2000_top %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable),
  stats_2000_bottom %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable))


stats_elect <-cbind(
  stats_2000_first %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)),
  stats_second <-stats_2000_second %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable)
)

stats_income <- cbind(
  stats_2000_lowincome %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)),
  stats_2000_highincome %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable)
  
)

stats_ineq <- cbind(
  stats_2000_highineq %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)),
  stats_2000_lowineq %>% 
    mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3)) %>% 
    select(-Variable)
)



write.xlsx2(stats, file = paste0(dir,main_folder,output_file),sheetName = "descriptive",row.names = F,append = T)

write.xlsx2(stats_elect, file = paste0(dir,main_folder,"results_dist_ec29_baseline_elect.xlsx"),sheetName = "descriptive",row.names = F,append = T)


write.xlsx2(stats_income, file = paste0(dir,main_folder,"income_stats.xlsx"),sheetName = "descriptive",row.names = F,append = T)
write.xlsx2(stats_ineq, file = paste0(dir,main_folder,"ineq_stats.xlsx"),sheetName = "descriptive",row.names = F,append = T)


