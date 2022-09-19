clear all
program drop _all
set more off
snapshot erase _all

*** Table 4.do
*** 11/28/2016

cd "$directory"

*************************************************************
*************************************************************
*** Table 4: Comparison of scores on FSI components for WB fragile states and OECD-only fragile states
*************************************************************
*************************************************************

*************************************
*** Read in the data
*************************************

*** Get total CPA flow for each recipient from the pair-level data
use "Input Data/Combined Pair-Level Data.dta", clear
collapse (sum) aid, by(recipientcode year)

*** Merge in recipient-level data on FSI codes
merge 1:1 recipientcode year using "Input Data/Fragile State Regression Data.dta", assert(3) keepusing(recipient* year wbfragile oecdfragile *fsi ) nogen norep

*************************************
*** Mark cases that are OECD fragile but not WB fragile -- that is, cases that are OECD fragile ONLY because of their FSI score
*************************************
gen oecd_only = 1 if wbfragile == 0 & oecdfragile == 1
replace oecd_only =  0 if wbfragile == 1 & oecdfragile == 1

*** Check if this was created correctly
preserve
contract oecd_only wbfragile oecdfragile
list oecd_only wbfragile oecdfragile, ab(20) sepby(oecd_only)
restore

*************************************
*** Check the components of FSI for cases that are OECD fragile but not WB fragile
*************************************

*** Set up blank tempfile to store the results 
preserve
clear
set obs 13
gen fsi = ""
label variable fsi "FSI Component"
gen wb_mean = ""
label variable wb_mean "WB Fragile States Mean"
gen oecd_mean = ""
label variable oecd_mean "OECD Only Fragile States Mean"
gen diff = ""
label variable diff "Difference" 
tempfile results
save `results'.dta, replace
restore

*** Set aid weights
svyset [weight = aid]
*** Loop through all FSI components
local i = 1
foreach var of varlist demographicpressures_fsi-externalintervention_fsi total_fsi {

	*** Store name of FSI component
	local fsi: variable label `var'
	local fsi = subinstr("`fsi'", "FSI - ", "", 1)

	*** Get the means for each subgroup of fragile states
	svy: mean `var', over(oecd_only)
	matrix results = r(table)
	local wb_mean = results[1, 1]
	local oecd_mean = results[1, 2]
	*** Test whether means are significantly different
	test [`var']0 == [`var']1
	return list
	local diff = `wb_mean' - `oecd_mean'
	if `r(p)' <= 0.01 {
		local diff = strofreal(`diff', "%8.2f") + "***"
	}
	else if `r(p)' <= .05 {
		local diff = strofreal(`diff', "%8.2f") + "**"
	}
	else if `r(p)' <= 0.1 {
		local diff = strofreal(`diff', "%8.2f") + "*"
	}
	else {
		local diff = strofreal(`diff', "%8.2f")
	}
	
	*** Format results before storing them
	foreach type in wb oecd {
		local `type'_mean = strofreal(``type'_mean', "%8.2f")
	}
	
	*** Store results
	preserve
	use `results'.dta, clear
	foreach var of varlist fsi wb_mean oecd_mean diff {
		replace `var' = "``var''" if _n == `i'
	}
	tempfile results
	save `results'.dta, replace
	restore
	
	local i = `i' + 1
}

*************************************
*** Export table to excel
*************************************
use `results'.dta, clear
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table 4") sheetreplace firstrow(varlabels)
