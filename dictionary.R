
library("VIM")
dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"


dictionary <- rbind(
      # 11 infra
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
      
      # 12 SIA
      cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
      cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
      cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
      cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
      
      # 13 AMS
      cbind('ams_hospital_mun_pcapita','N. of Municipal Hospitals (per capita*1000)'),
      cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals (per capita*1000)'),
      cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
      
      cbind('ams_hr_all_pcapita',"N. of Health Professionals (per capita*1000)"),
      cbind('ams_hr_superior_pcapita','N. of Doctors (per capita*1000)'),
      cbind('ams_hr_technician_pcapita','N. of Nurses (per capita*1000)'),
      cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants (per capita*1000)'),
      cbind('ams_hr_admin_pcapita','N. of Administrative Professionals (per capita*1000)'),
      
      # 18 Birth
      
      cbind('birth_prenat_ig','Prenatal Visits - Ignored'),
      cbind('birth_prenat_0','Prenatal Visits -  None'),
      cbind('birth_prenat_1_6','Prenatal Visits - 1-6'),
      cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
      cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
      cbind('birth_apgar1','Apgar 1'),
      cbind('birth_apgar5','Apgar 5'),
      cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
      cbind('birth_premature','Premature Birth'),
      cbind('birth_sexratio',"Sex Ratio at Birth"),
      cbind('birth_c_sections','Share of C-Section'),
      cbind('birth_hospital','Birth at Hospital'),
      
      # 19 SIH
      cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)'),
      cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
      cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
      cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
      cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)'),
      cbind('tx_sih_adult','Adult Hospitalization Rate (pop 40+y * 1000)'),
      cbind('tx_sih_adult_icsap','Adult Hospitalization Rate - APC (pop 40+y * 1000)'),
      cbind('tx_sih_adult_nicsap','Adult Hospitalization Rate - non-APC (pop 40+y * 1000)'),
      
      # 14 IMR
      
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
      
      # 15 AMR
      
      cbind('tx_ma5','Adult Mortality Rate (40+ y)'),
      cbind('tx_ma5_icsap','Adult Mortality Rate (40+ y) - APC'),
      cbind('tx_ma5_nicsap','Adult Mortality Rate (40+ y) - non-APC')
      
)

variables <- dictionary[,1]

d <- df %>% filter(!is.na(dist_ec29_baseline)) %>% select(all_of(variables))

missings <- aggr(d, plot = F)  
missings_count <- missings$missings$Count

dictionary <- cbind(dictionary,missings_count)

write.xlsx2(dictionary, file = paste0(dir,"data/","dictionary.xlsx"),row.names = F)  





