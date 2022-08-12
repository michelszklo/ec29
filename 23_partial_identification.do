/* partial_identification.do          DCC                  yyyy-mm-dd:2022-08-10
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
*/


clear all
set more off
cap log close


*-------------------------------------------------------------------------------
*--- (1) Setup 
*-------------------------------------------------------------------------------
set maxvar 10000
set matsize 10000
use data

*replace dist_ec29 = 0 if dist_ec29<0
gen post = ano >= 2001
gen z    = post*dist_ec29

gen logSpend = log(finbra_desp_saude_san_pcapita)

#delimit ;
local y sia_ncnes_amb_m;
local x finbra_desp_saude_san_pcapita;
local z z;
local cov t_analf18m_baseline t_espvida_baseline t_e_anosestudo_baseline
          t_t_analf18m_baseline t_pmpob_baseline t_rdpc_baseline t_gini_baseline
          t_sewage_gen_network_baseline t_garbage_coll_service_baseline
          t_water_gen_network_baseline t_elect_access_baseline t_urb_baseline;
#delimit cr

foreach var of varlist `y' `x' `z' {
    areg `var' i.ano  i.cod_uf#c.ano [aw=peso_b], abs(cod_mun)
    predict r_`var', resid
}

ivregress 2sls r_`y' (r_finbra_desp_saude_san_pcapita = r_z) [aw=peso_b], vce(cluster cod_mun) first


