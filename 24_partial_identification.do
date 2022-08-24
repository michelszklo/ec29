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
use data

gen pre = ano<2001

**Spending index
**Input index
#delimit ;
local coverE ACS_popprop eSF_popprop;
local coverI siab_accomp_especif_pcapita siab_accomp_especif_pacs_pcapit
             siab_accomp_especif_psf_pcapita siab_visit_cons_pcapita
             siab_visit_cons_pacs_pcapita siab_visit_cons_psf_pcapita; 
local humanR ams_hr_superior_pcapita ams_hr_technician_pcapita
             ams_hr_elementary_pcapita ams_hr_admin_pcapita;
local infras ams_hospital_mun_pcapita ams_hospital_nmun_pcapita
             ams_hospital_pvt_pcapita sia_ncnes_amb_mun_pcapita;
local primI  sia_ncnes_acs_pcapita sia_ncnes_medcom_pcapita sia_ncnes_enfacs_pcapita
             sia_ncnes_psf_pcapita sia_ncnes_medpsf_pcapita sia_ncnes_enfpsf_pcapita
             sia_ncnes_outpsf_pcapita;
#delimit cr
local j=1
foreach var of varlist `coverE' `coverI' `humanR' `infras' `primI' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(input_index) normby(pre)
sum input_index
drop _ivar*

**Access index
#delimit ;
local amb sia_pcapita sia_ab_pcapita sia_nprod_amb_lc_mun_pcapita
          sia_nprod_amb_hc_mun_pcapita;
local acc birth_prenat_ig birth_prenat_0 birth_prenat_1_6 birth_prenat_7_plus;
#delimit cr

local j=1
foreach var of varlist `amb' `acc' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_index) normby(pre) flip(_ivar5 _ivar6)
sum access_index
drop _ivar*

**Output index
local hosp tx_sih_maternal tx_sih_infant tx_sih_infant_icsap tx_sih_infant_nicsap
local inf  tx_mi
local j=1
foreach var of varlist `hosp' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(health_index) normby(pre)
sum health_index
drop _ivar*
    


*-------------------------------------------------------------------------------
*--- (2) IV models
*-------------------------------------------------------------------------------
gen post = ano >= 2001
gen z    = post*dist_ec29

#delimit ;
local y input_index access_index health_index;
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

rename r_finbra_desp_saude_san_pcapita W
plausexog ltz r_access_index (W=r_z) `wt', vce(cluster cod_mun) mu(-0.1) omega(0.01)



