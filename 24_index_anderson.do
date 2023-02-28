/* partial_identification.do          DCC                  yyyy-mm-dd:2022-08-10
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
*/


clear all
set more off
cap log close



*-------------------------------------------------------------------------------
*--- (1) Setup and generate indexes
*-------------------------------------------------------------------------------
set maxvar 10000
set matsize 10000

cd "C:\Users\Michel\Documents\GitHub\ec29"

use data, clear

gen pre = ano<2001

**Spending index

** Access and production of health services
#delimit ;
local coverE ACS_popprop eSF_popprop;
local coverI siab_accomp_especif_pcapita siab_accomp_especif_pacs_pcapit
             siab_accomp_especif_psf_pcapita siab_visit_cons_pcapita
             siab_visit_cons_pacs_pcapita siab_visit_cons_psf_pcapita; 
local primI  sia_ncnes_acs_pcapita sia_ncnes_medcom_pcapita sia_ncnes_enfacs_pcapita
             sia_ncnes_psf_pcapita sia_ncnes_medpsf_pcapita sia_ncnes_enfpsf_pcapita
             sia_ncnes_outpsf_pcapita;
local amb sia_pcapita sia_ab_pcapita sia_nprod_amb_lc_mun_pcapita
          sia_nprod_amb_hc_mun_pcapita;
local acc birth_prenat_ig birth_prenat_0 birth_prenat_1_6 birth_prenat_7_plus;
local hosp tx_sih_maternal tx_sih_infant_icsap tx_sih_infant_nicsap;
#delimit cr
local j=1
foreach var of varlist `coverE' `coverI' `primI' `amb' `acc' `hosp' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_index) normby(pre) flip(_ivar20 _ivar21 _ivar25)
sum access_index
drop _ivar*

** Access and production of health services SPLIT primary vs non primary care
* PRIMARY CARE
#delimit ;
local coverE ACS_popprop eSF_popprop;
local coverI siab_accomp_especif_pcapita siab_accomp_especif_pacs_pcapit
             siab_accomp_especif_psf_pcapita siab_visit_cons_pcapita
             siab_visit_cons_pacs_pcapita siab_visit_cons_psf_pcapita; 
local primI  sia_ncnes_acs_pcapita sia_ncnes_medcom_pcapita sia_ncnes_enfacs_pcapita
             sia_ncnes_psf_pcapita sia_ncnes_medpsf_pcapita sia_ncnes_enfpsf_pcapita
             sia_ncnes_outpsf_pcapita;
local amb    sia_ab_pcapita sia_nprod_amb_lc_mun_pcapita;
local acc birth_prenat_ig birth_prenat_0 birth_prenat_1_6 birth_prenat_7_plus;
local hosp tx_sih_maternal tx_sih_infant_icsap;
#delimit cr
local j=1
foreach var of varlist `coverE' `coverI' `primI' `amb' `acc' `hosp' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_pc_index) normby(pre) flip(_ivar18 _ivar19 _ivar23)
sum access_pc_index
drop _ivar*


* NON-PRIMARY CARE
local j=1
foreach var of varlist sia_pcapita sia_nprod_amb_hc_mun_pcapita tx_sih_infant_nicsap {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_npc_index) normby(pre)
sum access_npc_index
drop _ivar*




**Input index
#delimit ;
local humanR ams_hr_superior_pcapita ams_hr_technician_pcapita
             ams_hr_elementary_pcapita ams_hr_admin_pcapita;
local infras ams_hospital_mun_pcapita ams_hospital_nmun_pcapita
             ams_hospital_pvt_pcapita;

#delimit cr
local j=1
foreach var of varlist `humanR' `infras' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(input_index) normby(pre)
sum input_index
drop _ivar*


local j=1
foreach var of varlist `humanR' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(hr_index) normby(pre)
sum hr_index
drop _ivar*

local j=1
foreach var of varlist `infras' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(hospital_index) normby(pre)
sum hospital_index
drop _ivar*

    


**Birth index
#delimit ;
local birth birth_apgar1 birth_apgar5 birth_low_weight_2500g
            birth_premature birth_sexratio tx_mi_icsap tx_mi_nicsap;
#delimit cr
local j=1
foreach var of varlist `birth' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(birth_index) normby(pre) flip(_ivar3 _ivar4 _ivar5 _ivar6 _ivar7)
sum birth_index
drop _ivar*


#delimit ;
local birth tx_mi_icsap tx_mi_nicsap;
#delimit cr
local j=1
foreach var of varlist `birth' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(imr_index) normby(pre) flip(_ivar1 _ivar2)
sum imr_index
drop _ivar*

#delimit ;
local birth birth_apgar1 birth_apgar5 birth_low_weight_2500g
            birth_premature birth_sexratio;
#delimit cr
local j=1
foreach var of varlist `birth' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(birth_others_index) normby(pre) flip(_ivar3 _ivar4 _ivar5)
sum birth_others_index
drop _ivar*



keep  ano cod_mun cod_uf access_index access_pc_index access_npc_index input_index hr_index hospital_index birth_index imr_index birth_others_index
save indexes.dta, replace


*-------------------------------------------------------------------------------
*--- (2) IV models
*-------------------------------------------------------------------------------
gen post = ano >= 2001
gen z    = post*dist_ec29

#delimit ;
local y input_index access_index health_index birth_index;
local x finbra_desp_saude_san_pcapita;
local z z;
local cov t_analf18m_baseline t_espvida_baseline t_e_anosestudo_baseline
          t_t_analf18m_baseline t_pmpob_baseline t_rdpc_baseline t_gini_baseline
          t_sewage_gen_network_baseline t_garbage_coll_service_baseline
          t_water_gen_network_baseline t_elect_access_baseline t_urb_baseline;
local wt [aw=peso_b];
local wt ;
#delimit cr

foreach var of varlist `y' `x' `z' {
    areg `var' i.ano  i.cod_uf#c.ano `wt', abs(cod_mun)
    predict r_`var', resid
}

local X (r_finbra_desp_saude_san_pcapita = r_z)
ivregress 2sls r_input_index  `X' `wt', vce(cluster cod_mun) first
ivregress 2sls r_access_index `X' `wt', vce(cluster cod_mun) first
ivregress 2sls r_health_index `X' [aw=peso_b], vce(cluster cod_mun) first
ivregress 2sls r_birth_index  `X' [aw=peso_b], vce(cluster cod_mun) first

rename r_finbra_desp_saude_san_pcapita W
plausexog ltz r_access_index (W=r_z) `wt', vce(cluster cod_mun) mu(-0.1) omega(0.01)



