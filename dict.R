dict <- rbind(cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
              cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
              cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
              cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
              cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
              cbind('siab_visit_cons_pcapita','N. of Household Visits and Appointments (per capita)'),
              cbind('siab_visit_cons_pacs_pcapita','N. of Household Visits and Appointments from Community Health Agents (per capita)'),
              cbind('siab_visit_cons_psf_pcapita','N. of Household Visits and Appointments from Family Health Agents (per capita)'),
              cbind('sia_ncnes_acs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Teams (per capita*1000)'),
              cbind('sia_ncnes_medcom_pcapita','N. of Health Facilities with Ambulatory Service and Community Doctors (per capita*1000)'),
              cbind('sia_ncnes_enfacs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Nurses (per capita*1000)'),
              cbind('sia_ncnes_psf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Teams (per capita*1000)'),
              cbind('sia_ncnes_medpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Doctors (per capita*1000)'),
              cbind('sia_ncnes_enfpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nurses (per capita*1000)'),
              cbind('sia_ncnes_outpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nursing Assistants (per capita*1000)'),
              cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
              cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
              cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
              cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
              cbind('birth_prenat_ig','Prenatal Visits - Ignored'),
              cbind('birth_prenat_0','Prenatal Visits None'),
              cbind('birth_prenat_1_6','Prenatal Visits 1-6'),
              cbind('birth_prenat_7_plus','Prenatal Visits 7+'),
              cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)'),
              cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
              cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
              
              cbind('ams_hr_superior_pcapita','N. of Doctors (per capita*1000)'),
              cbind('ams_hr_technician_pcapita','N. of Nurses (per capita*1000)'),
              cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants (per capita*1000)'),
              cbind('ams_hr_admin_pcapita','N. of Administrative Professionals (per capita*1000)'),
              cbind('ams_hospital_mun_pcapita','N. of Municipal Hospitals (per capita*1000)'),
              cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals (per capita*1000)'),
              cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
              cbind('birth_apgar1','Apgar 1'),
              cbind('birth_apgar5','Apgar 5'),
              cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
              cbind('birth_premature','Premature Birth'),
              cbind('birth_sexratio',"Sex Ratio at Birth"),
              cbind('tx_mi_icsap','Infant Mortality Rate - APC'),
              cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC')
              )
              
              
              
write.xlsx2(dict, file = paste0(dir,"data/","dict.xlsx"),row.names = F)  
              
              
