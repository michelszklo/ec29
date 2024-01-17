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


vers 17
clear all
set more off
cap log close


*-------------------------------------------------------------------------------
*--- (0) Definitions and set-up
*-------------------------------------------------------------------------------
set maxvar 10000
set matsize 10000

if c(username)=="damian" {
    global DAT "~/investigacion/2021/decentralization/ec29"
    global OUT "~/investigacion/2021/decentralization/ec29/results"
}
else {
    global OUT "C:\Users\Michel\Documents\GitHub\ec29"
}
cap mkdir "$OUT"


#delimit ;
local baseline t_analf18m_baseline t_espvida_baseline t_e_anosestudo_baseline
      t_t_analf18m_baseline t_pmpob_baseline t_rdpc_baseline t_gini_baseline
      t_sewage_gen_network_baseline t_garbage_coll_service_baseline
      t_water_gen_network_baseline t_elect_access_baseline t_urb_baseline;
#delimit cr
** Access and production of health services

* generating missing SIA variable
gen sia_nab_pcapita = sia_pcapita - sia_ab_pcapita

#delimit ;
local coverE ACS_popprop eSF_popprop;
local coverI siab_accomp_especif_pcapita siab_accomp_especif_pacs_pcapit
             siab_accomp_especif_psf_pcapita siab_visit_cons_pcapita
             siab_visit_cons_pacs_pcapita siab_visit_cons_psf_pcapita; 
local primI  sia_ncnes_acs_pcapita sia_ncnes_medcom_pcapita sia_ncnes_enfacs_pcapita
             sia_ncnes_psf_pcapita sia_ncnes_medpsf_pcapita sia_ncnes_enfpsf_pcapita
             sia_ncnes_outpsf_pcapita;
local amb sia_pcapita;
local acc birth_prenat_ig birth_prenat_0 birth_prenat_1_6 birth_prenat_7_plus;
local hosp tx_sih_maternal tx_sih_infant_icsap tx_sih_infant_nicsap;
#delimit cr
local j=1
foreach var of varlist `coverE' `coverI' `primI' `amb' `acc' `hosp' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_index) normby(pre) flip(_ivar17 _ivar18 _ivar22)
sum access_index
drop _ivar*

cap log close
log using "$OUT/24_index_anderson.txt", text replace

*-------------------------------------------------------------------------------
*--- (1) Index definitions
*-------------------------------------------------------------------------------
#delimit ;
local INDEXES access_apc access_non_apc input_infra input_hr infant_mort
              other_birth adult_mort;
**Variables;

local access_apc      ACS_popprop eSF_popprop siab_accomp_especif_pcapita
                      siab_accomp_especif_pacs_pcapit
                      siab_accomp_especif_psf_pcapita siab_visit_cons_pcapita
                      siab_visit_cons_pacs_pcapita siab_visit_cons_psf_pcapita
                      sia_ab_pcapita sia_nprod_amb_lc_mun_pcapita
                      birth_prenat_ig birth_prenat_0 birth_prenat_1_6
                      birth_prenat_7_plus birth_hospital tx_sih_infant_icsap;
local access_non_apc  sia_nprod_amb_hc_mun_pcapita tx_sih_infant_nicsap
                      tx_sih_maternal2 tx_sih_maternal tx_sih_adult_nicsap;
local input_infra     sia_ncnes_amb_mun_pcapita ams_hospital_mun_pcapita;
local input_hr        ams_hr_all_pcapita ams_hr_superior_pcapita
                      ams_hr_technician_pcapita ams_hr_elementary_pcapita
                      ams_hr_admin_pcapita;
local infant_mort     tx_mi tx_mi_icsap tx_mi_nicsap tx_mi_infec tx_mi_resp
                      tx_mi_perinat tx_mi_cong tx_mi_ext tx_mi_nut
                      tx_mi_out tx_mi_illdef tx_mi_fet tx_mi_24h
                      tx_mi_27d tx_mi_ano;
local infant_mort     tx_mi_icsap tx_mi_nicsap;
local other_birth     birth_apgar1 birth_low_weight_2500g birth_premature
                      birth_sexratio birth_c_sections;
local adult_mort      tx_ma5 tx_ma5_icsap tx_ma5_nicsap;


**Variables to rescale by -1 (follows order above, 1 is rescale);
local RE_access_apc      0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0;
local RE_access_non_apc  0 0 0 0 0;
local RE_input_infra     0 0;
local RE_input_hr        0 0 0 0 0;
local RE_infant_mort     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1;
local RE_other_birth     0 1 1 0 1;
local RE_adult_mort      1 1 1; 

**Auxiliary variables;
local AUXILIARY ams_hospital_nmun_pcapita ams_hospital_pvt_pcapita
                birth_fertility tx_sih_infant tx_sih_adult tx_mm;
local labels    `" "Access & Production Index (APC)"
                   "Access & Production Index (non-APC)"
                   "Health Inputs Index (Infrastructure)"
                   "Health Inputs Index (Human Resources)"
                   "Infant Mortality Index" "Other Birth Outcome Index"
                   "Adult Mortality Index" "Federal and State Hospitals p.c"
                   "Private Hospitals p.c." "Live births per woman"
                   "Infant Hospitalization Rate"
                   "Adult Hospitalization Rate" "Maternal Mortality Rate" "';
#delimit cr
local coverE ACS_popprop eSF_popprop;
local coverI siab_accomp_especif_pcapita siab_accomp_especif_pacs_pcapit
             siab_accomp_especif_psf_pcapita siab_visit_cons_pcapita
             siab_visit_cons_pacs_pcapita siab_visit_cons_psf_pcapita; 
local primI  sia_ncnes_acs_pcapita sia_ncnes_medcom_pcapita sia_ncnes_enfacs_pcapita
             sia_ncnes_psf_pcapita sia_ncnes_medpsf_pcapita sia_ncnes_enfpsf_pcapita
             sia_ncnes_outpsf_pcapita;
local amb    sia_ab_pcapita;
local acc birth_prenat_ig birth_prenat_0 birth_prenat_1_6 birth_prenat_7_plus;
local hosp tx_sih_infant_icsap;
#delimit cr
local j=1
foreach var of varlist `coverE' `coverI' `primI' `amb' `acc' `hosp' {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_pc_index) normby(pre) flip(_ivar17 _ivar18 _ivar21 )
sum access_pc_index
drop _ivar*

// Variables to run are `INDEXES' `AUXILIARY'
// Names for these variables can be looped from `labels' 

*-------------------------------------------------------------------------------
*--- (2) Setup and generate indexes
*-------------------------------------------------------------------------------
use "$DAT/data", clear
gen pre = ano<2001

* NON-PRIMARY CARE
local j=1
foreach var of varlist sia_nab_pcapita tx_sih_maternal tx_sih_infant_nicsap {
    gen _ivar`j' = `var'
    local ++j
}
swindex _ivar*, generate(access_npc_index) normby(pre)
sum access_npc_index
drop _ivar*

foreach index of local INDEXES {
    // Reverse variables if lower is good
    tokenize `RE_`index''
    local j=1
    foreach var of varlist ``index'' {
        if `1'==1 {
            dis "Replacing `var' with inverse in Index `index'"
            replace `var'=`var'*-1            
        }
        macro shift
        rename `var' _I`j'
        local ++j
    }

    // Make index
    dis "Making index"
    swindex _I*, generate(`index') normby(pre)
    dis "Done"
    
    // Undo changes from above
    tokenize `RE_`index''
    local j=1
    foreach var in ``index'' {
        rename _I`j' `var'
        if `1'==1 {
            dis "Replacing `var' with inverse in Index `index'"
            replace `var'=`var'*-1            
        }
        macro shift
        local ++j
    }
    sum `index'
}

//CAN USE SOMETHING LIKE BELOW FOR MISSING
//egen missing=rowmiss(`coverE' `coverI' `primI' `amb' `acc' `hosp')


preserve
#delimit ;
keep ano cod_mun cod_uf access_apc access_non_apc input_infra input_hr
         infant_mort other_birth adult_mort;
#delimit cr
save indexes.dta, replace
restore

drop if dist_ec29_baseline==.

*-------------------------------------------------------------------------------
*--- (3) Reduced form models
*-------------------------------------------------------------------------------
#delimit ;
local opts  absorb(i.cod_uf#i.ano cod_mun) cluster(cod_mun);
local inds  access_index access_pc_index access_npc_index input_index hr_index
            hospital_index birth_index imr_index birth_others_index;
#delimit cr


// Variables to run are `INDEXES' `AUXILIARY'
// Names for these variables can be looped from `labels' 
gen treat    = post*dist_ec29_baseline

foreach ind of varlist `INDEXES' `AUXILIARY' {
    eststo: reghdfe `ind' treat `baseline' [aw=pop], `opts'
}
esttab est1 est2 est3 est4 est5 est7, keep(treat)
esttab est6 est9 est10 est11 est12 est13, keep(treat)
estimates clear

*-------------------------------------------------------------------------------
*--- (4) Time-based models
*-------------------------------------------------------------------------------
cap mkdir "$OUT/events"

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

    
gen beta = .
gen LB   = .
gen UB   = .
gen LB90 = .
gen UB90 = .
gen time = _n+1997 in 1/13

tokenize `INDEXES' `AUXILIARY'
foreach yvar in `labels' {
    dis "hello"
    preserve
    if inlist(`"`1'"', "input_hr") {
        local xvars lead3 lead1 lag*
        local sparse = 1                                     
    }
    else {
        local xvars lead3 lead2 lag*
        local sparse = 0
    }
    reghdfe `1' `xvars' `baseline' [aw=pop], `opts'
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
    yline(0, lcolor(red)) ylabel(, format(%04.2f)) ytitle("`yvar'")
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "$OUT/events/Anderson_`1'.pdf", replace
    restore
    macro shift
}


*-------------------------------------------------------------------------------
*--- (5) Bounds
*-------------------------------------------------------------------------------
cap mkdir "$OUT/honest"
gen HBl5 = .
gen HBu5 = .
gen HBl10 = .
gen HBu10 = .

tokenize `INDEXES' `AUXILIARY'
foreach yvar in `labels' {
    if inlist(`"`1'"', "input_hr", "ams_hospital_nmun_pcapita", "ams_hospital_pvt_pcapita", "tx_sih_infant") {
        macro shift
        continue
    }
    preserve
    reghdfe `1' lead3 lead2 lag* `baseline' [aw=pop], `opts'
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
        dis "Variable: `1', lag, `pos'"
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
    yline(0, lcolor(red)) ylabel(, format(%04.2f)) ytitle("`yvar'")
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "$OUT/honest/Honest_`1'.pdf", replace
    restore
    macro shift
}

    
/*
*-------------------------------------------------------------------------------
*--- (6) Intensity-based models (dCDH) [TAKES CONSIDERABLE TIME TO RUN]
*-------------------------------------------------------------------------------
local didmopts trends_nonparam(cod_uf) threshold_stable_treatment(0.1) weight(pop) breps(5)
local topts    dynamic(10) robust_dynamic placebo(2) longdiff_placebo

did_multiplegt imr_index cod_mun ano treat, `didmopts' `topts'
graph export "outputs/dcdh/imr_index.eps", replace


*/


*-------------------------------------------------------------------------------
*--- (7) Above/below
*-------------------------------------------------------------------------------
cap mkdir "$OUT/aboveBelow"
foreach l in above below {
    gen beta_`l' = .
    gen LB_`l'   = .
    gen UB_`l'   = .
}
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



tokenize `INDEXES' `AUXILIARY'
foreach yvar in `labels' {
    preserve
    if inlist(`"`1'"', "input_hr") {
        local xvar_above above_lead3 above_lead1 above_lag*
        local xvar_below below_lead3 below_lead1 below_lag*
        local sparse = 1                                     
    }
    else {
        local xvar_above above_lead3 above_lead2 above_lag*
        local xvar_below below_lead3 below_lead2 below_lag*
        local sparse = 0
    }
    reghdfe `1' `xvar_above' `xvar_below' `baseline' [aw=pop], `opts'
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
           rows(1) position(12)) ytitle("`yvar'")
    xlabel(1998(1)2010) xline(2000, lcolor(black) lpattern(solid)) xtitle("Year")
    yline(0, lcolor(red)) ylabel(, format(%04.2f))
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "$OUT/aboveBelow/Anderson_`1'.pdf", replace
    restore
    macro shift
}
drop time time2


*-------------------------------------------------------------------------------
*--- (8) Binary increase/decrease
*-------------------------------------------------------------------------------
cap mkdir "$OUT/distributional"
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



tokenize `INDEXES' `AUXILIARY'
foreach yvar in `labels' {
    preserve
    if inlist(`"`1'"', "input_hr") {
        local x  increases_lead3  increases_lead1  increases_lag*
        local sparse = 1
    }
    else {
        local x increases_lead3  increases_lead2  increases_lag*
        local sparse = 0
    }
    reghdfe `1' `x' `baseline' [aw=pop], `opts'
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
    yline(0, lcolor(red)) ylabel(, format(%04.2f)) ytitle("`yvar'")
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "$OUT/distributional/Binary_`1'.pdf", replace
    restore
    macro shift
}
drop time



*-------------------------------------------------------------------------------
*--- (9) Intensity models
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



tokenize `INDEXES' `AUXILIARY'
foreach yvar in `labels' {
    preserve
    if inlist(`"`1'"', "input_hr") {
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
    reghdfe `1' `x' `baseline' [aw=pop], `opts'
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
    yline(0, lcolor(red)) ylabel(, format(%04.2f)) ytitle("`yvar'")
    note("Sample consist of `mun' municipalities.  Total estimation sample = `tot'.");    
    #delimit cr
    graph export "$OUT/distributional/Anderson_`1'.pdf", replace
    restore
    macro shift
}
drop time time2 time3



