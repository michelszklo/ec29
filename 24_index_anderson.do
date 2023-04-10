/* 24_index_anderson.do               DCC                  yyyy-mm-dd:2022-08-10
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

  This file generates indexes using the Anderson procedure, and then conducts a
number of basic models following the longer code in R.  At present, the file is
not well organized.

1: state_year + mun fe (w = pop)
2: 1 + baseline_controls
3: 2 + tvarying_controls
4: 3 + fiscal_controls


baseline_controls: t_analf18m_baseline t_espvida_baseline t_e_anosestudo_baseline
t_t_analf18m_baseline t_pmpob_baseline t_rdpc_baseline t_gini_baseline
t_sewage_gen_network_baseline t_garbage_coll_service_baseline
t_water_gen_network_baseline t_elect_access_baseline t_urb_baseline

tvarying_controls: gdp_mun_pcapita pbf_pcapita

fiscal_controls: finbra_desp_saude_san_pcapita_neighbor lrf

*/


clear all
set more off
cap log close



#delimit ;
local baseline t_analf18m_baseline t_espvida_baseline t_e_anosestudo_baseline
      t_t_analf18m_baseline t_pmpob_baseline t_rdpc_baseline t_gini_baseline
      t_sewage_gen_network_baseline t_garbage_coll_service_baseline
      t_water_gen_network_baseline t_elect_access_baseline t_urb_baseline;
#delimit cr

cap log close
log using output.txt, text replace
*-------------------------------------------------------------------------------
*--- (1) Setup and generate indexes
*-------------------------------------------------------------------------------
set maxvar 10000
set matsize 10000

if c(username)=="damian" {
    cd "/home/damian/investigacion/2021/decentralization/ec29"
}
else {
    cd "C:\Users\Michel\Documents\GitHub\ec29"
}
cap mkdir outputs

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

sum `coverE' `coverI' `primI' `amb' `acc' `hosp'
egen missing=rowmiss(`coverE' `coverI' `primI' `amb' `acc' `hosp')

preserve
drop if missing!=0
bys cod_mun: gen N=_N
keep if N==10
swindex _ivar*, generate(BAL_access_index) normby(pre) flip(_ivar20 _ivar21 _ivar25)
sum BAL_access_index
keep BAL_access_index ano cod_mun
tempfile index
save `index'
restore
drop missing
merge 1:1 ano cod_mun using `index'
sum BAL_access_index access_index
drop _merge


foreach num of numlist 20 21 25 {
    replace _ivar`num' = _ivar`num'*-1
}
foreach var of varlist _ivar* {
    sum `var'
    replace `var'=(`var'-r(mean))/r(sd)
}
egen access_zsum = rowmean(_ivar*)
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
local acc    birth_prenat_ig birth_prenat_0 birth_prenat_1_6 birth_prenat_7_plus;
local hosp   tx_sih_maternal tx_sih_infant_icsap;
#delimit cr
local j=1
foreach var of varlist `coverE' `coverI' `primI' `amb' `acc' `hosp' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_pc_index) normby(pre) flip(_ivar18 _ivar19 _ivar23)
sum access_pc_index


sum `coverE' `coverI' `primI' `amb' `acc' `hosp'
egen missing=rowmiss(`coverE' `coverI' `primI' `amb' `acc' `hosp')

preserve
drop if missing!=0
bys cod_mun: gen N=_N
tab N
keep if N==10
swindex _ivar*, generate(BAL_access_pc_index) normby(pre) flip(_ivar18 _ivar19 _ivar23)
sum BAL_access_pc_index
keep BAL_access_pc_index ano cod_mun
save `index', replace
restore
drop missing
merge 1:1 ano cod_mun using `index'
sum BAL_access_pc_index access_pc_index
drop _merge


foreach num of numlist 18 19 23 {
    replace _ivar`num' = _ivar`num'*-1
}
foreach var of varlist _ivar* {
    sum `var'
    replace `var'=(`var'-r(mean))/r(sd)
}
egen access_pc_zsum = rowmean(_ivar*)
drop _ivar*

    

* NON-PRIMARY CARE
local j=1
foreach var of varlist sia_pcapita sia_nprod_amb_hc_mun_pcapita tx_sih_infant_nicsap {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_npc_index) normby(pre)
sum access_npc_index


egen missing=rowmiss(sia_pcapita sia_nprod_amb_hc_mun_pcapita tx_sih_infant_nicsap)

preserve
drop if missing!=0
bys cod_mun: gen N=_N
tab N
keep if N==10
swindex _ivar*, generate(BAL_access_npc_index) normby(pre) 
sum BAL_access_npc_index
keep BAL_access_npc_index ano cod_mun
save `index', replace
restore
drop missing
merge 1:1 ano cod_mun using `index'
sum BAL_access_npc_index access_npc_index
drop _merge


foreach var of varlist _ivar* {
    sum `var'
    replace `var'=(`var'-r(mean))/r(sd)
}
egen access_npc_zsum = rowmean(_ivar*)
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

foreach var of varlist _ivar* {
    sum `var'
    replace `var'=(`var'-r(mean))/r(sd)
}
egen hospital_zsum = rowmean(_ivar*)
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


egen missing=rowmiss(`birth')

preserve
drop if missing!=0
bys cod_mun: gen N=_N
tab N
keep if N==13
swindex _ivar*, generate(BAL_birth_index) normby(pre) flip(_ivar3 _ivar4 _ivar5 _ivar6 _ivar7)
sum BAL_birth_index
keep BAL_birth_index ano cod_mun
save `index', replace
restore
drop missing
merge 1:1 ano cod_mun using `index'
sum BAL_birth_index birth_index
drop _merge



foreach num of numlist 3 4 5 6 7 {
    replace _ivar`num' = _ivar`num'*-1
}
foreach var of varlist _ivar* {
    sum `var'
    replace `var'=(`var'-r(mean))/r(sd)
}
egen birth_zsum = rowmean(_ivar*)
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
egen missing=rowmiss(tx_mi_icsap tx_mi_nicsap)

preserve
drop if missing!=0
bys cod_mun: gen N=_N
tab N
keep if N==13
swindex _ivar*, generate(BAL_imr_index) normby(pre)  flip(_ivar1 _ivar2)
sum BAL_imr_index
keep BAL_imr_index ano cod_mun
save `index', replace
restore
drop missing
merge 1:1 ano cod_mun using `index'
sum BAL_imr_index imr_index
drop _merge


foreach num of numlist 1 2 {
    replace _ivar`num' = _ivar`num'*-1
}
foreach var of varlist _ivar* {
    sum `var'
    replace `var'=(`var'-r(mean))/r(sd)
}
egen imr_zsum = rowmean(_ivar*)
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

egen missing=rowmiss(`birth')

preserve
drop if missing!=0
bys cod_mun: gen N=_N
tab N
keep if N==13
swindex _ivar*, generate(BAL_birth_others_index) normby(pre)  flip(_ivar3 _ivar4 _ivar5)
sum BAL_birth_others_index
keep BAL_birth_others_index ano cod_mun
save `index', replace
restore
drop missing
merge 1:1 ano cod_mun using `index'
sum BAL_birth_others_index birth_others_index
drop _merge


drop _ivar*



preserve
#delimit ;
keep ano cod_mun cod_uf access_index access_pc_index access_npc_index
input_index hr_index hospital_index birth_index imr_index birth_others_index
BAL_*;
#delimit cr
save indexes.dta, replace
restore

*-------------------------------------------------------------------------------
*--- (1b) Variable generation 
*-------------------------------------------------------------------------------
drop if dist_ec29_baseline==.

sum dist_ec29_baseline if dist_ec29_baseline>=0, d
gen Base_P1 = dist_ec29_baseline>=0 & dist_ec29_baseline<r(p25)
gen Base_P2 = dist_ec29_baseline>=r(p25) & dist_ec29_baseline<r(p50)
gen Base_P3 = dist_ec29_baseline>=r(p50) & dist_ec29_baseline<r(p75)
gen Base_P4 = dist_ec29_baseline>=r(p75) 
sum dist_ec29_baseline if dist_ec29_baseline<0, d
gen Base_N4 = dist_ec29_baseline< r(p25)
gen Base_N3 = dist_ec29_baseline>=r(p25)&dist_ec29_baseline<r(p50)
gen Base_N2 = dist_ec29_baseline>=r(p50)&dist_ec29_baseline<r(p75)
gen Base_N1 = dist_ec29_baseline>=r(p75)&dist_ec29_baseline<0

*-------------------------------------------------------------------------------
*--- (2) Reduced form models
*-------------------------------------------------------------------------------
#delimit ;
local opts  absorb(i.cod_uf#i.ano cod_mun) cluster(cod_mun);
local inds  access_index access_pc_index access_npc_index input_index hr_index
            hospital_index birth_index imr_index birth_others_index;
#delimit cr


gen treat    = post*dist_ec29_baseline

foreach ind of varlist `inds' tx_mi {
    eststo: reghdfe `ind' treat `baseline' [aw=pop], `opts'
}
esttab est1 est4 est7, keep(treat)
esttab est2 est3 est5 est6 est8 est9, keep(treat)
estimates clear


*-------------------------------------------------------------------------------
*--- (3) Time-based models BALANCED
*-------------------------------------------------------------------------------
#delimit ;
local Binds  BAL_access_index BAL_access_pc_index BAL_access_npc_index
             BAL_birth_index BAL_imr_index BAL_birth_others_index;
#delimit cr


local year 1998
foreach lead of numlist 3 2 1 {
    gen intX = ano==`year'
    gen lead`lead' = intX*dist_ec29_baseline    
    local ++year
    drop intX
}
foreach lag of numlist 0(1)9 {
    gen intX = ano==`year'
    gen lag`lag' = intX*dist_ec29_baseline    
    local ++year
    drop intX
}


drop firjan_index

cap mkdir "outputs/events"
foreach index of varlist tx_mi `Binds' {
    preserve
    if inlist(`"`index'"', "BAL_access_index", "BAL_access_pc_index", "BAL_access_npc_index") {
        local xvars lead3 lead1 lag0 lag1 lag2 lag3 lag4 lag5 lag6
        local short = 1                                     
        gen beta = .
        gen LB   = .
        gen UB   = .
        gen LB90 = .
        gen UB90 = .
        gen time = _n+1997 in 1/10
        local yend 2007
    }
    else {
        local xvars lead3 lead2 lag*
        local short = 0
        gen beta = .
        gen LB   = .
        gen UB   = .
        gen LB90 = .
        gen UB90 = .
        gen time = _n+1997 in 1/13
        local yend 2010
    }
    reghdfe `index' `xvars' `baseline' [aw=pop], `opts'
    local tot=string(e(N))
    levelsof cod_mun if e(sample)==1
    local mun = string(r(r))
    
    
    replace beta = 0 if time==2000
    replace LB   = 0 if time==2000
    replace UB   = 0 if time==2000
    replace LB90 = 0 if time==2000
    replace UB90 = 0 if time==2000
    
    local year = 1998
    foreach var of varlist `xvars' {
        replace beta = _b[`var'] if time==`year'
        replace LB   = _b[`var']+invnormal(0.025)*_se[`var'] if time==`year'
        replace UB   = _b[`var']+invnormal(0.975)*_se[`var'] if time==`year'
        replace LB90 = _b[`var']+invnormal(0.050)*_se[`var'] if time==`year'
        replace UB90 = _b[`var']+invnormal(0.950)*_se[`var'] if time==`year'

        if `year'==1999 local ++year
        local ++year    
    }
    #delimit ;
    twoway rarea LB   UB   time, color(gs12%40)
        || rarea LB90 UB90 time, color(gs11%50)
    || scatter beta time, msymbol(Sh) msize(medium) mcolor(black)
    legend(order(3 "Point Estimate" 1 "95% CI" 2 "90% CI") rows(1) position(6))
    xlabel(1998(1)`yend') xline(2000, lcolor(gs14) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");
    #delimit cr
    graph export "outputs/events/Balanced_Anderson_`index'.pdf", replace
    restore
}


*-------------------------------------------------------------------------------
*--- (4) Bounds BALANCED
*-------------------------------------------------------------------------------
gen HBl5 = .
gen HBu5 = .
gen HBl10 = .
gen HBu10 = .


foreach index of varlist `Binds' {
    preserve
    if inlist(`"`index'"', "BAL_access_index", "BAL_access_pc_index", "BAL_access_npc_index") {
        local xvars lead3 lead1 lag0 lag1 lag2 lag3 lag4 lag5 lag6
        local short = 1                                     
        gen beta = .
        gen LB   = .
        gen UB   = .
        gen LB90 = .
        gen UB90 = .
        gen time = _n+1997 in 1/10
        local yend 2007
        local npost 7
        local nvar  9
    }
    else {
        local xvars lead3 lead2 lag*
        local short = 0
        gen beta = .
        gen LB   = .
        gen UB   = .
        gen LB90 = .
        gen UB90 = .
        gen time = _n+1997 in 1/13
        local yend 2010
        local npost 10
        local nvar  12
    }
    reghdfe `index' `xvars' `baseline' [aw=pop], `opts'
    local tot=string(e(N))
    levelsof cod_mun if e(sample)==1
    local mun = string(r(r))
    replace beta = 0 if time==2000
    replace LB   = 0 if time==2000
    replace UB   = 0 if time==2000
    replace LB90 = 0 if time==2000
    replace UB90 = 0 if time==2000
    
    local year = 1998
    foreach var of varlist `xvars' {
        replace beta = _b[`var'] if time==`year'
        replace LB   = _b[`var']+invnormal(0.025)*_se[`var'] if time==`year'
        replace UB   = _b[`var']+invnormal(0.975)*_se[`var'] if time==`year'
        replace LB90 = _b[`var']+invnormal(0.050)*_se[`var'] if time==`year'
        replace UB90 = _b[`var']+invnormal(0.950)*_se[`var'] if time==`year'
        
        if `year'==1999 local ++year
        local ++year    
    }
    local year=2001
    foreach pos of numlist 1(1)`npost' {
        matrix l_vec = J(`npost',1,0)
        matrix l_vec[`pos',1] = 1
        mat list l_vec
        honestdid, pre(1/2) post(3/`nvar') mvec(0 0.01) l_vec(l_vec) delta(sd)
        mata st_numscalar("LB5",`s(HonestEventStudy)'.CI[2,2])
        mata st_numscalar("UB5",`s(HonestEventStudy)'.CI[2,3])
        mata st_numscalar("LB10",`s(HonestEventStudy)'.CI[3,2])
        mata st_numscalar("UB10",`s(HonestEventStudy)'.CI[3,3])

        replace HBl5  = LB5  if time==`year'
        replace HBu5  = UB5  if time==`year'
        replace HBl10 = LB10 if time==`year'
        replace HBu10 = UB10 if time==`year'
        local ++year
    }    
    #delimit ;
    twoway rarea LB   UB   time, color(gs12%40)
        || rarea LB90 UB90 time, color(gs11%50)
        || rarea HBl5  HBu5  time, color(purple%30)
        || rarea HBl10 HBu10 time, color(purple%10)
    || scatter beta time, msymbol(Sh) msize(medium) mcolor(black)
    legend(order(5 "Point Estimate" 1 "95% CI" 2 "90% CI"
                 3 "Honest" 4 "Honest (M=0.01)") rows(1) position(6))
    xlabel(1998(1)`yend') xline(2000, lcolor(gs14) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "outputs/events/Balanced_Honest_`index'.pdf", replace
    restore
}


    
*-------------------------------------------------------------------------------
*--- (3) Time-based models
*-------------------------------------------------------------------------------
gen beta = .
gen LB   = .
gen UB   = .
gen LB90 = .
gen UB90 = .
gen time = _n+1997 in 1/13


foreach index of varlist `inds' {
    preserve
    if inlist(`"`index'"', "input_index", "hr_index", "hospital_index") {
        local xvars lead3 lead1 lag*
        local sparse = 1                                     
    }
    else {
        local xvars lead3 lead2 lag*
        local sparse = 0
    }
    reghdfe `index' `xvars' `baseline' [aw=pop], `opts'
    local tot=string(e(N))
    levelsof cod_mun if e(sample)==1
    local mun = string(r(r))

    replace beta = 0 if time==2000
    replace LB   = 0 if time==2000
    replace UB   = 0 if time==2000
    replace LB90 = 0 if time==2000
    replace UB90 = 0 if time==2000
    
    local year = 1998
    foreach var of varlist `xvars' {
        replace beta = _b[`var'] if time==`year'
        replace LB   = _b[`var']+invnormal(0.025)*_se[`var'] if time==`year'
        replace UB   = _b[`var']+invnormal(0.975)*_se[`var'] if time==`year'
        replace LB90 = _b[`var']+invnormal(0.050)*_se[`var'] if time==`year'
        replace UB90 = _b[`var']+invnormal(0.950)*_se[`var'] if time==`year'

        if `sparse'==1&`year'==1998 local ++year
        if `sparse'==0&`year'==1999 local ++year
        local ++year    
    }
    #delimit ;
    twoway rarea LB   UB   time, color(gs12%40)
        || rarea LB90 UB90 time, color(gs11%50)
    || scatter beta time, msymbol(Sh) msize(medium) mcolor(black)
    legend(order(3 "Point Estimate" 1 "95% CI" 2 "90% CI") rows(1) position(6))
    xlabel(1998(1)2010) xline(2000, lcolor(gs14) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "outputs/events/Anderson_`index'.pdf", replace
    *graph export "outputs/events/Zscore/Zmean_`index'.pdf", replace
    restore
}


*-------------------------------------------------------------------------------
*--- (4) Bounds
*-------------------------------------------------------------------------------
#delimit ;
local boundsind access_index access_pc_index access_npc_index birth_index
                imr_index birth_others_index;
#delimit cr


foreach index of varlist `boundsind' {
    preserve
    reghdfe `index' lead3 lead2 lag* `baseline' [aw=pop], `opts'
    local tot=string(e(N))
    levelsof cod_mun if e(sample)==1
    local mun = string(r(r))
    replace beta = 0 if time==2000
    replace LB   = 0 if time==2000
    replace UB   = 0 if time==2000
    replace LB90 = 0 if time==2000
    replace UB90 = 0 if time==2000
    
    local year = 1998
    foreach var of varlist lead3 lead2 lag0 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 {
        replace beta = _b[`var'] if time==`year'
        replace LB   = _b[`var']+invnormal(0.025)*_se[`var'] if time==`year'
        replace UB   = _b[`var']+invnormal(0.975)*_se[`var'] if time==`year'
        replace LB90 = _b[`var']+invnormal(0.050)*_se[`var'] if time==`year'
        replace UB90 = _b[`var']+invnormal(0.950)*_se[`var'] if time==`year'
        
        if `year'==1999 local ++year
        local ++year    
    }
    local year=2001
    foreach pos of numlist 1(1)10 {
        matrix l_vec = J(10,1,0)
        matrix l_vec[`pos',1] = 1
        mat list l_vec
        honestdid, pre(1/2) post(3/12) mvec(0 0.01) l_vec(l_vec) delta(sd)
        mata st_numscalar("LB5",`s(HonestEventStudy)'.CI[2,2])
        mata st_numscalar("UB5",`s(HonestEventStudy)'.CI[2,3])
        mata st_numscalar("LB10",`s(HonestEventStudy)'.CI[3,2])
        mata st_numscalar("UB10",`s(HonestEventStudy)'.CI[3,3])

        replace HBl5  = LB5  if time==`year'
        replace HBu5  = UB5  if time==`year'
        replace HBl10 = LB10 if time==`year'
        replace HBu10 = UB10 if time==`year'
        local ++year
    }    
    #delimit ;
    twoway rarea LB   UB   time, color(gs12%40)
        || rarea LB90 UB90 time, color(gs11%50)
        || rarea HBl5  HBu5  time, color(purple%30)
        || rarea HBl10 HBu10 time, color(purple%10)
    || scatter beta time, msymbol(Sh) msize(medium) mcolor(black)
    legend(order(5 "Point Estimate" 1 "95% CI" 2 "90% CI"
                 3 "Honest" 4 "Honest (M=0.01)") rows(1) position(6))
    xlabel(1998(1)2010) xline(2000, lcolor(gs14) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "outputs/events/Honest_`index'.pdf", replace
    restore
}



    
/*
*-------------------------------------------------------------------------------
*--- (5) Intensity-based models (dCDH) [TAKES CONSIDERABLE TIME TO RUN]
*-------------------------------------------------------------------------------
local didmopts trends_nonparam(cod_uf) threshold_stable_treatment(0.1) weight(pop) breps(5)
local topts    dynamic(10) robust_dynamic placebo(2) longdiff_placebo

did_multiplegt imr_index cod_mun ano treat, `didmopts' `topts'
graph export "outputs/dcdh/imr_index.eps", replace


*/


*-------------------------------------------------------------------------------
*--- (6) Above/below
*-------------------------------------------------------------------------------
cap mkdir "outputs/aboveBelow"
foreach l in above below {
    gen beta_`l' = .
    gen LB_`l'   = .
    gen UB_`l'   = .
}
gen time  = _n+1997 in 1/13
gen time2 = time+0.22

gen below = dist_ec29_baseline>0
gen above = 1-below

foreach l in above below {
    local year 1998
    foreach lead of numlist 3 2 1 {
        gen intX = ano==`year'
        gen `l'_lead`lead' = intX*dist_ec29_baseline*`l'
        local ++year
        drop intX
    }
    foreach lag of numlist 0(1)9 {
        gen intX = ano==`year'
        gen `l'_lag`lag' = intX*dist_ec29_baseline*`l'
        local ++year
        drop intX
    }
}



foreach index of varlist tx_mm tx_ma5 tx_mi tx_mi_neonat `inds' {
    preserve
    if inlist(`"`index'"', "input_index", "hr_index", "hospital_index") {
        local xvar_above above_lead3 above_lead1 above_lag*
        local xvar_below below_lead3 below_lead1 below_lag*
        local sparse = 1                                     
    }
    else {
        local xvar_above above_lead3 above_lead2 above_lag*
        local xvar_below below_lead3 below_lead2 below_lag*
        local sparse = 0
    }
    reghdfe `index' `xvar_above' `xvar_below' `baseline' [aw=pop], `opts'
    local tot=string(e(N), "%12.0fc")
    levelsof cod_mun if e(sample)==1
    local mun = string(r(r))

    foreach l in above below {
        replace beta_`l' = 0 if time==2000
        replace LB_`l'   = 0 if time==2000
        replace UB_`l'   = 0 if time==2000
    }

    foreach l in above below {
        local year = 1998
        foreach var of varlist `xvar_`l'' {
            replace beta_`l' = _b[`var'] if time==`year'
            replace LB_`l'   = _b[`var']+invnormal(0.025)*_se[`var'] if time==`year'
            replace UB_`l'   = _b[`var']+invnormal(0.975)*_se[`var'] if time==`year'
    
            if `sparse'==1&`year'==1998 local ++year
            if `sparse'==0&`year'==1999 local ++year
            local ++year    
        }
    }
    #delimit ;
    twoway rcap LB_above UB_above time,  color(gs8%40) lwidth(medthick)
        || rcap LB_below UB_below time2, color(gs8%40) lwidth(medthick)
    || scatter beta_above time, msymbol(Sh) msize(medium) mcolor(purple)
    || scatter beta_below time2, msymbol(Dh) msize(medium) mcolor(orange)
    legend(order(3 "Above Threshold" 4 "Below Threshold" 1 "95% CI")
           rows(1) position(12))
    xlabel(1998(1)2010) xline(2000, lcolor(black) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "outputs/aboveBelow/Anderson_`index'.pdf", replace
    restore
}
drop time time2


*-------------------------------------------------------------------------------
*--- (7) Binary increase/decrease
*-------------------------------------------------------------------------------
cap mkdir "outputs/distributional"
gen increases  = dist_ec29_baseline>0
foreach l in increases {
    gen beta_`l' = .
    gen LB_`l'   = .
    gen UB_`l'   = .
    gen LB90_`l' = .
    gen UB90_`l' = .
}
gen time  = _n+1997 in 1/13

foreach l in increases {
    local year 1998
    foreach lead of numlist 3 2 1 {
        gen intX = ano==`year'
        gen `l'_lead`lead' = intX*`l'
        local ++year
        drop intX
    }
    foreach lag of numlist 0(1)9 {
        gen intX = ano==`year'
        gen `l'_lag`lag' = intX*`l'
        local ++year
        drop intX
    }
}



foreach index of varlist tx_mi tx_mi_neonat tx_mm tx_ma5 `inds' {
    preserve
    if inlist(`"`index'"', "input_index", "hr_index", "hospital_index") {
        local x  increases_lead3  increases_lead1  increases_lag*
        local sparse = 1
    }
    else {
        local x increases_lead3  increases_lead2  increases_lag*
        local sparse = 0
    }
    reghdfe `index' `x' `baseline' [aw=pop], `opts'
    local tot=string(e(N), "%12.0fc")
    levelsof cod_mun if e(sample)==1
    local mun = string(r(r))

    foreach l in increases {
        replace beta_`l' = 0 if time==2000
        replace LB_`l'   = 0 if time==2000
        replace UB_`l'   = 0 if time==2000
        replace LB90_`l' = 0 if time==2000
        replace UB90_`l' = 0 if time==2000
    }

    foreach l in increases {
        local year = 1998
        foreach var of varlist `x' {
            replace beta_`l' = _b[`var'] if time==`year'
            replace LB_`l'   = _b[`var']+invnormal(0.025)*_se[`var'] if time==`year'
            replace UB_`l'   = _b[`var']+invnormal(0.975)*_se[`var'] if time==`year'
            replace LB90_`l' = _b[`var']+invnormal(0.05)*_se[`var']  if time==`year'
            replace UB90_`l' = _b[`var']+invnormal(0.95)*_se[`var']  if time==`year'
    
            if `sparse'==1&`year'==1998 local ++year
            if `sparse'==0&`year'==1999 local ++year
            local ++year    
        }
    }
    dis "HERE"
    #delimit ;
    twoway rarea LB_increases   UB_increases   time, color(gs12%40)
      ||   rarea LB90_increases UB90_increases time, color(gs11%50)
      || scatter beta_increases time, msymbol(Sh) msize(medium) mcolor(black)
    legend(order(2 "Shock Increases Spending" 1 "95% CI")
           rows(1) position(12))
    xlabel(1998(1)2010) xline(2000, lcolor(black) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "outputs/distributional/Binary_`index'.pdf", replace
    restore
}
drop time



*-------------------------------------------------------------------------------
*--- (8) Intensity models
*-------------------------------------------------------------------------------
local LOCAL 0.01

gen nonchanger = abs(dist_ec29_baseline)<`LOCAL' 
gen increaser  = dist_ec29_baseline>`LOCAL'
gen decreaser  = dist_ec29_baseline<-`LOCAL'

foreach l in increaser nonchanger decreaser {
    gen beta_`l' = .
    gen LB_`l'   = .
    gen UB_`l'   = .
}
gen time  = _n+1997 in 1/13
gen time2 = time+0.22
gen time3 = time2+0.22

foreach l in increaser nonchanger decreaser {
    local year 1998
    foreach lead of numlist 3 2 1 {
        gen intX = ano==`year'
        gen `l'_lead`lead' = intX*`l'
        local ++year
        drop intX
    }
    foreach lag of numlist 0(1)9 {
        gen intX = ano==`year'
        gen `l'_lag`lag' = intX*`l'
        local ++year
        drop intX
    }
}



foreach index of varlist tx_mi tx_mi_neonat tx_mm tx_ma5 `inds' {
    preserve
    if inlist(`"`index'"', "input_index", "hr_index", "hospital_index") {
        local xvar_increaser  increaser_lead3  increaser_lead1  increaser_lag*
        local xvar_nonchanger nonchanger_lead3 nonchanger_lead1 nonchanger_lag*
        local xvar_decreaser  decreaser_lead3  decreaser_lead1  decreaser_lag*
        local sparse = 1
    }
    else {
        local xvar_increaser  increaser_lead3  increaser_lead2  increaser_lag*
        local xvar_nonchanger nonchanger_lead3 nonchanger_lead2 nonchanger_lag*
        local xvar_decreaser  decreaser_lead3  decreaser_lead2  decreaser_lag*
        local sparse = 0
    }
    local x `xvar_increaser' `xvar_decreaser'
    reghdfe `index' `x' `baseline' [aw=pop], `opts'
    local tot=string(e(N), "%12.0fc")
    levelsof cod_mun if e(sample)==1
    local mun = string(r(r))

    foreach l in increaser decreaser {
        replace beta_`l' = 0 if time==2000
        replace LB_`l'   = 0 if time==2000
        replace UB_`l'   = 0 if time==2000
    }

    foreach l in increaser decreaser {
        local year = 1998
        foreach var of varlist `xvar_`l'' {
            replace beta_`l' = _b[`var'] if time==`year'
            replace LB_`l'   = _b[`var']+invnormal(0.025)*_se[`var'] if time==`year'
            replace UB_`l'   = _b[`var']+invnormal(0.975)*_se[`var'] if time==`year'
    
            if `sparse'==1&`year'==1998 local ++year
            if `sparse'==0&`year'==1999 local ++year
            local ++year    
        }
    }
    dis "HERE"
    #delimit ;
    twoway rcap LB_increaser UB_increaser time,  color(gs8%40) lwidth(medthick)
    || rcap LB_decreaser UB_decreaser time2, color(gs8%40) lwidth(medthick)
    || scatter beta_increaser time, msymbol(Sh) msize(medium) mcolor(purple)
    || scatter beta_decreaser time2, msymbol(Dh) msize(medium) mcolor(emerald)
    legend(order(3 "Shock Increases Spending" 4 "Shock Decreases Spending"  1 "95% CI")
           rows(1) position(12))
    xlabel(1998(1)2010) xline(2000, lcolor(black) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "outputs/distributional/Anderson_`index'.pdf", replace
    restore
}
drop time time2 time3

