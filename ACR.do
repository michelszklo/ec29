global outreg "$ROOT/results/ACR"
cap mkdir $outreg



**NOTE THAT IN THIS CODE b_inf is the variable that measures treatement intensity

gen N = 1
preserve
collapse b_inf (sum) N, by(birth_state)
sum b_inf [aw=N]
local max = r(max)
local min = r(min)
local varD = r(sd)^2
local ED   = r(mean)
local range = `max'-`min'
count
local N = r(N)

sort b_inf

gen grid = ((_n-1)/(`N'-1)*`range')+`min'
gen CGSwt = . 
foreach num of numlist 1(1)`N' {
    sum grid in `num'
    local l = r(mean)
    sum b_inf [aw=N] if b_inf>`l'
    local EDl = r(mean)
    sum N  if b_inf>`l'
    local PDl = (r(mean)*r(N))/5259986    
    replace CGSwt = ((`EDl'-`ED')*`PDl')/`varD' in `num'

    dis "EDl `Edl'"
    dis "ED `ED'"
    dis "PDl `PDl'"
    dis "VarD `varD'"
}
sum CGSwt
local denom = r(mean)*r(N)
replace CGSwt = CGSwt/`denom'

replace N = N/5259986
#delimit ;
twoway line N b_inf, xtitle("Dose (d)")                         
ylabel(, format("%04.2f")) xlabel(, format("%03.1f"))
ytitle("Weights on ACR(d)")
text(0.07 1.23 "{it:f}{sub:D|D>0}(d)")
text(0.038 0.92 "TWFE")
|| line CGSwt grid, lpattern(dash) lcolor(black) lwidth(thick)
legend(off);
#delimit cr
graph export "$outreg/estimandWeights.eps", replace

restore
