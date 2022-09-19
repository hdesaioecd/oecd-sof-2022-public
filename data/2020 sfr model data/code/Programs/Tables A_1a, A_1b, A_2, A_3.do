clear all
program drop _all
set more off
snapshot erase _all

*** Tables A_1a, A_1b, A_2.do
*** 11/28/2016

cd "$directory"

************************************************************
************************************************************
*** Table A.1a: # of recipient countries with data for each indicator, in each year
************************************************************
************************************************************

*************************************
*** Read in the data
*************************************

use "Input Data/Combined Pair-Level Data.dta", clear

*************************************
*** Mark cases with nonmissing data
*************************************
local i = 1
foreach ind in on_budget use_pfm pius prog_other joint_work aid_thru_others multi_pct bi_multi_share avg_pred vol {

	gen aid`i' = aid if `ind' < .
	
	*** Also store indicator labels
	local lbl`i': variable label `ind'
	
	local i = `i' + 1
}
replace aid7 = aid // mutli_pct -- we have complete data for this 

*************************************
*** Collapse to year level, counting cases with nonmissing data 
*************************************

*** Store variable labels 
local i = 1
foreach var in on_budget use_pfm pius prog_other joint_work aid_thru_others multi_pct bi_multi_share avg_pred vol {
	local lbl`i': variable label `var'
	local i = `i' + 1
}

*** Collapse to recipient level, counting cases with nonmissing data
collapse (count) on_budget use_pfm pius prog_other joint_work aid_thru_others multi_pct bi_multi_share avg_pred vol, by(year recipient)

*** Mark cases with any nonmissing data
local i = 1
foreach var in on_budget use_pfm pius prog_other joint_work aid_thru_others multi_pct bi_multi_share avg_pred vol {

	*** Mark recipients with nonmissing data
	gen count`i' = 1 if `var' > 0 & `var' < .
	
	local i = `i' + 1
}

*** Collapse to year level, counting the number of recipients with nonmissing data
collapse (count) count*, by(year)

*************************************
*** Label variables
*************************************
forvalues i = 1/10 {
	label variable count`i' "lbl`i'"
}

*************************************
*** Export tables to excel
*************************************

preserve
keep year count*
xpose, clear
list
gen ind = ""
order ind, first
forvalues i = 1/10 {
	replace ind = "`lbl`i''" if _n == `i' + 1
}
export excel ind v* using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table A_1a") sheetreplace
restore

************************************************************
************************************************************
*** Table A.1b: % of global CPA represented by pairs with data for each indicator, in each year
************************************************************
************************************************************

*************************************
*** Read in the data
*************************************

use "Input Data/Combined Pair-Level Data.dta", clear

*************************************
*** Create variable marking CPA for flows with nonmissing data
*************************************
local i = 1
foreach ind in on_budget use_pfm pius prog_other joint_work aid_thru_others multi_pct bi_multi_share avg_pred vol {

	gen aid`i' = aid if `ind' < .
	
	*** Also store indicator labels
	local lbl`i': variable label `ind'
	
	local i = `i' + 1
}
replace aid7 = aid // mutli_pct -- we have complete data for this

*************************************
*** Calculate percent of total CPA represented by flows with data each indicator
*************************************

*** Collapse to year level
drop aid_thru_others
collapse (sum) aid*, by(year)

forvalues i = 1/10 {
	
	*** Calculate percent of total CPA represented by flows with data each indicator
	replace aid`i' = aid`i'/aid*100

	*** Format as a string with a percent sign (for exporting to excel)
	tostring aid`i', format(%4.1f) replace force
	replace aid`i' = aid`i' + "%"
}

*************************************
*** Label variables
*************************************
forvalues i = 1/10 {
	label variable aid`i' "lbl`i'"
}

*************************************
*** Export to excel
*************************************
preserve
keep year aid1-aid10
tostring year, replace
sxpose, clear
list
gen ind = ""
order ind, first
forvalues i = 1/10 {
	replace ind = "`lbl`i''" if _n == `i' + 1
}
export excel ind _v* using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table A_1b") sheetreplace
restore

************************************************************
************************************************************
*** Table A.2: Summary stats (at recipient-year level)
************************************************************
************************************************************

*************************************
*** Read in the data
*************************************

use "Input Data/Combined Pair-Level Data.dta", clear

*************************************
*** Collapse to the recipient-year level
*************************************
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol (rawsum) multi_pct (count) n=aid_thru_others [weight = aid], by(recipient year)

*** Cap predictability indicator at 50% (definitionally, the ratio of aid given through others to total cannot be more than 50% for donors in the aggregate)
replace aid_thru_others = 50 if aid_thru_others > 50 & aid_thru_others < . // Impacts 1 observation (Solomon Islands, 2010)

*************************************
*** Label indicator variables
*************************************
label variable on_budget "Reported on budget"
label variable use_pfm "Use of PFMs"
label variable pius "Use of PIUs"
label variable prog_other "Use of programmatic aid"
label variable joint_work "Joint activities"
label variable aid_thru_others "Aid through others"
label variable multi_pct "Multilateral aid"
label variable bi_multi_share "Multi-bi aid"
label variable avg_pred "Predictable funding"
label variable vol "Volatility"

*************************************
*** Collapse to get summary stats for each indicator 
*************************************

*** Rename indicator variables so that we can reshape easily later, storing indicator labels
local i = 1
foreach var of varlist on_budget use_pfm pius prog_other joint_work aid_thru_others multi_pct bi_multi_share avg_pred vol {
	local ind`i'_lbl: variable label `var'
	rename `var' ind`i'
	local i = `i' + 1
}

*** Prepare collapse statement
foreach stat in "n" "mean" "sd" "min" "p5" "p95" "max" {
	local collapse_`stat' = ""
	forvalues i = 1/10 {
		local collapse_`stat' = "`collapse_`stat'' `stat'`i'=ind`i'"
	}
}

*** Collapse to get stats
collapse (count) `collapse_n' (mean) `collapse_mean' (sd) `collapse_sd' (min) `collapse_min' (p5) `collapse_p5' (p95) `collapse_p95' (max) `collapse_max'

*** Reshape
gen i = .
reshape long n mean sd min p5 p95 max, i(i) j(ind)
drop i 

*** Label indicators
gen ind_name = ""
forvalues i = 1/10 {
	replace ind_name = "`ind`i'_lbl'" if ind == `i'
}
drop ind
order ind_name, first

export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table A_2") sheetreplace firstrow(varlabels)

************************************************************
************************************************************
*** Table A.3: Percent of CPA from each donor 
************************************************************
************************************************************

*************************************
*** Read in the data
*************************************

use "Input Data/Combined Pair-Level Data.dta", clear

*************************************
*** Collapse to the donor level (across all years, 2007-2014)
*************************************

*** Mark donor as "other" if they are not part of the sample that provides >= 1% of aid to fragile states
replace donor = "Other" if over_1pct == 0

*** Collapse, adding up total CPA to fragile and stable states separately for each donor
collapse (sum) aid, by(donor wbfragile)

*** Reshape
reshape wide aid, i(donor) j(wbfragile)

*************************************
*** Calculate each donor's percent of total aid, aid to fragile states, and aid to stable states
*************************************

*** Aid to fragile and stable states
forvalues fragile = 0/1 {
	egen total`fragile' = total(aid`fragile')
	gen donor_pct`fragile' = aid`fragile'/total`fragile' * 100
}

*** Total aid
gen a = aid0
egen total = total(aid0 + aid1)
gen donor_pct_total = (aid0 + aid1)/total * 100

*************************************
*** Export Table A.3 to excel
*************************************

*** Label the values
keep donor donor_pct_total donor_pct1 donor_pct0
order donor donor_pct_total donor_pct1 donor_pct0
label variable donor "Donor"
label variable donor_pct_total "% of Total CPA"
label variable donor_pct1 "% of CPA to Fragile States"
label variable donor_pct0 "% of CPA to Stable States"

*** Format the values
foreach var of varlist donor_pct_total donor_pct1 donor_pct0 {
	assert `var' < .
	tostring `var', replace force format(%10.1f)
	replace `var' = `var' + "%"
}

*** Sort for the table (with "Other" at the bottom)
sort donor
gen sort_order = _n
replace sort_order = 100 if donor == "Other"
sort sort_order
drop sort_order

*** Export to excel
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table A_3") sheetreplace firstrow(varlabels)
