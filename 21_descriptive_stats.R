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
            'ggsci')
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
                 cbind('finbra_reccorr_pcapita','Total Revenue per capita (2010 R$)'),
                 cbind('finbra_rectribut_pcapita','Tax Revenue per capita (2010 R$)'),
                 cbind('finbra_rectransf_pcapita','Transfers Revenue per capita (2010 R$)'),
                 cbind('finbra_rec_outros_pcapita','Other Revenues per capita (2010 R$)'),
                 
                 cbind('finbra_desp_pessoal_share','Human Resources Spending (% Total Spending)'),
                 cbind('finbra_desp_investimento_share','Investment Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_nature_share','Other Spending per capita (% Total Spending)'),
                 cbind('finbra_desp_saude_san_share','Health and Sanitation Spending (% Total Spending)'),
                 cbind('finbra_desp_transporte_share','Transport Spending (% Total Spending)'),
                 cbind('finbra_desp_educ_cultura_share','Education and Culture Spending (% Total Spending)'),
                 cbind('finbra_desp_hab_urb_share','Housing and Urban Spending (% Total Spending)'),
                 cbind('finbra_desp_assist_prev_share','Social Assistance Spending (% Total Spending)'),
                 cbind('finbra_desp_outros_area_share','Other Areas Spending (% Total Spending)'),
                 cbind('finbra_rectribut_share','Tax Revenue (% Total Revenue)'),
                 cbind('finbra_rectransf_share','Transfers Revenue (% Total Revenue)'),
                 cbind('finbra_rec_outros_share','Other Revenues (% Total Revenue)'),
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
                 
                 cbind('sia_pcapita','Outpatient procedures (per capita)'),
                 cbind('sia_ab_pcapita','PC Outpatient procedures per capita'),
                 cbind('sia_visita_superior_pcapita','Household visits by college degree personal (per capita)'),
                 cbind('sia_visita_medio_pcapita','Household visits by non college degree personal (per capita)'),
                 cbind('sia_ativ_grupo_pcapita','Educational activities in group (per capita)'),
                 
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
                 
                 cbind('tx_ma','Adult Mortality Rate'),
                 cbind('tx_ma_circ','Adult Mortality Rate - Circulatory'),
                 cbind('tx_ma_neop','Adult Mortality Rate - Neoplasm'),
                 cbind('tx_ma_resp','Adult Mortality Rate - Respiratory'),
                 cbind('tx_ma_endoc','Adult Mortality Rate - Endocrine'),
                 cbind('tx_ma_ext','Adult Mortality Rate - External'),
                 cbind('tx_ma_nut','Adult Mortality Rate - Nutritional'),
                 cbind('tx_ma_illdef','Adult Mortality Rate - Ill-Defined'),
                 cbind('tx_ma_out','Adult Mortality Rate - Other'),
                 cbind('tx_ma_diab','Adult Mortality Rate - Diabetes'),
                 cbind('tx_ma_hyper','Adult Mortality Rate - Hypertension'),
                 cbind('tx_ma_icsap','Adult Mortality Rate - APC'),
                 cbind('tx_ma_nicsap','Adult Mortality Rate - non-APC'),
                 
                 cbind('birth_apgar1','Apgar 1'),
                 cbind('birth_apgar5','Apgar 5'),
                 cbind('birth_c_sections','Share of C-Section'),
                 cbind('birth_gest_37plus','Gestation Weeks 37+'),
                 cbind('birth_hospital','Birth at Hospital'),
                 cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
                 cbind('birth_prenat_0','Prenatal Visits None'),
                 cbind('birth_prenat_1_6','Prenatal Visits 1-6'),
                 cbind('birth_prenat_7_plus','Prenatal Visits 7+')
                 
)

summary_stat(df,2000,"stats_2000")


# 4. Statistic for variables with baseline in 2002
# =================================================================

var_map <- rbind(cbind('ams_cnes_bed_sur_total_pcapita','Surgical Public Beds (per capita * 1000)'),
                 cbind('ams_cnes_bed_clin_total_pcapita','Clinical Public Beds (per capita * 1000)'),
                 cbind('ams_cnes_bed_obs_total_pcapita','Obstetric Public Beds (per capita * 1000)'),
                 cbind('ams_cnes_bed_ped_total_pcapita','Pediatric Public Beds (per capita * 1000)'),
                 cbind('ams_cnes_bed_sus_priv_pcapita','Total Number of Beds, Public and Private (per capita * 1000)'),
                 cbind('ams_cnes_bed_total_pcapita','Number of Public of Beds (per capita * 1000)'),
                 cbind('ams_cnes_itubed_total_pcapita','Number of Public ITU Beds (per capita * 1000)'),
                 cbind('ams_cnes_ad_itubed_total_pcapita','Number of Public Adult ITU Beds (per capita * 1000)'),
                 cbind('ams_cnes_hpp_count_pcapita','Number of Small Facilities with Hospital Services (per capita * 1000)'),
                 cbind('ams_cnes_hpp_bed_count_pcapita','Number of Beds in Small Facilities (per capita * 1000)'),
                 cbind('ams_cnes_hmp_count_pcapita','Number of Medium Facilities with Hospital Services (per capita * 1000)'),
                 cbind('ams_cnes_hmp_bed_count_pcapita','Number of Beds in Medium Facilities (per capita * 1000)'),
                 cbind('ams_cnes_hgp_count_pcapita','Number of Large Facilities with Hospital Services (per capita * 1000)'),
                 cbind('ams_cnes_hgp_bed_count_pcapita','Number of Beds in Large Facilities (per capita * 1000)'),
                 cbind('ams_cnes_n_estab_emg_pcapita','Number of Facilities with Emergency Services (per capita * 1000)'),
                 cbind('ams_cnes_n_estab_amb_pcapita','Number of Facilities with Ambulatorial Services (per capita * 1000)'),
                 cbind('ams_cnes_n_estab_pcapita','Number of Facilities (per capita * 1000)')
)


summary_stat(df,2002,"stats_2002")



# 5. Statistic for variables with baseline in 2005
# =================================================================

load(paste0(dir,"regs_cs_2005.RData"))



var_map <- rbind(cbind('cnes_st_all_mun_pcapita','Municipal Health Facilities (per capita * 1mi)'),
                 cbind('cnes_st_posto_mun_pcapita','Municipal Health Center (per capita * 1mi)'),
                 cbind('cnes_st_consultorio_mun_pcapita','Municipal Consulting Rooms (per capita * 1mi)'),
                 cbind('cnes_st_clinicaesp_mun_pcapita','Municipal Specialized Health Clinics (per capita * 1mi)'),
                 cbind('cnes_st_diagnos_mun_pcapita','Municipal Diagnostic Center (per capita * 1mi)'),
                 cbind('cnes_st_all_private_pcapita','Private Health Facilities (per capita * 1mi)'),
                 cbind('cnes_st_posto_private_pcapita','Private Health Center (per capita * 1mi)'),
                 cbind('cnes_st_consultorio_private_pcapita','Private Consulting Rooms (per capita * 1mi)'),
                 cbind('cnes_st_clinicaesp_private_pcapita','Private Specialized Health Clinics (per capita * 1mi)'),
                 cbind('cnes_st_diagnos_private_pcapita','Private Diagnostic Center (per capita * 1mi)'),
                 cbind('cnes_st_all_public_pcapita','Public Health Facilities (per capita * 1mi)'),
                 cbind('cnes_st_posto_public_pcapita','Public Health Center (per capita * 1mi)'),
                 cbind('cnes_st_consultorio_public_pcapita','Public Consulting Rooms (per capita * 1mi)'),
                 cbind('cnes_st_clinicaesp_public_pcapita','Public Specialized Health Clinics (per capita * 1mi)'),
                 cbind('cnes_st_diagnos_public_pcapita','Public Diagnostic Center (per capita * 1mi)'),
                 cbind('cnes_lt_private_sus_funded_pcapita','Private Hospital Beds funded by SUS (per capita * 1mi)'),
                 cbind('cnes_lt_mun_pcapita','Municipal Hospital Beds (per capita * 1mi)'),
                 cbind('cnes_lt_public_pcapita','Public Hospital Beds (per capita * 1mi)'),
                 cbind('cnes_eq_mun_pcapita','Municipal Health Equipments (per capita * 1mi)'),
                 cbind('cnes_eq_private_pcapita','Private Health Equipments (per capita * 1mi)'),
                 cbind('cnes_eq_public_pcapita','Public Health Equipments (per capita * 1mi)'))


summary_stat(df,2005,"stats_2005")




# 6. final table and export
# =================================================================

stats <- rbind.data.frame(stats_2000,stats_2002,stats_2005) %>% 
  mutate_at(c("Mean","Std.Dev","Min","Max","Obs"),~ round(.,digits = 3))


write.xlsx2(stats, file = paste0(dir,main_folder,output_file),sheetName = "descriptive",row.names = F,append = T)



