clear all
program drop _all
set more off
snapshot erase _all

*** Table 6, G_1.do
*** 11/28/2016

cd "$directory"

*************************************************************
*************************************************************
*** Define a program to re-label the indicator variables 
*************************************************************
*************************************************************
program define LABEL_IND
	label variable on_budget "Reported on budget"
	label variable use_pfm "Use of PFM systems"
	label variable pius "Use of PIUs"
	label variable prog_other "Use of programmatic aid"
	label variable joint_work "Joint activities"
	label variable aid_thru_others "Aid through others"
	*label variable multi_pct "Multilateral aid"
	*label variable bi_multi_share "Multi-bi aid"
	label variable avg_pred "Predictable funding"
	label variable vol "Volatility"
end

*************************************************************
*************************************************************
*** Table 6: Scores for all bilaterals and multilaterals combined across all indicators
*** Table G.1: Scores for all bilaterals and multilaterals combined on each indicator
*************************************************************
*************************************************************

*************************************
*** Read in the data
*************************************
use "Input Data/Combined Pair-Level Data.dta", clear

*** Label multi variable
label define multi 0 "Bilateral" 1 "Multilateral"
label values multi multi

*** Keep only donors with >1% of aid to fragile states
keep if over_1pct == 1
codebook donor

*************************************
*** Drop data for donors that have less than 20% of their aid to either fragile or stable states represented by flows for which we have data
*************************************

*** Calculate aid represented by flows with data on each indicator 
local inds on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol
forvalues i = 1/9 {
	local ind: word `i' of `inds'
	gen aid`i' = aid if `ind' < .
	local lbl`i': variable label `ind'
}

*** Mark years when we have any data for each indicator 
*** (We do not want to drop donors because they don't have any data in years in which no survey was conducted, for example)
forvalues i = 1/9 {
	local ind: word `i' of `inds'
	quietly bys year: egen any_data = max(`ind')
	quietly gen aid_any_data`i' = aid if any_data < .
	**** Confirm that this was done correctly
	preserve
	quietly keep if any_data <. 
	contract year
	disp "Years with `lbl`i'' data:"
	list year, clean noobs
	restore
	drop any_data
}

*** Calculate the % of total CPA represented by flows with data, separately in stable in stable and fragile states -- for years when we expect to have data
forvalues i = 1/9 {
	quietly bys donor wbfragile: egen aid_with_data`i' = total(aid`i')
	quietly bys donor wbfragile: egen total_aid`i' = total(aid_any_data`i')
	quietly gen pct_with_data`i' = aid_with_data`i'/total_aid`i' * 100 
	quietly bys donor:egen min_pct_with_data`i' = min(pct_with_data`i')
	*** Show donors that do not have at least 20% of their CPA represented by flows with data on this indicator
	disp "Donors without 20% of aid represented by flows with data on `lbl`i'' in either fragile or stable states:"
	preserve
	quietly keep if min_pct_with_data`i' < 20
	if _N > 0 {
		contract donor 
		list donor, clean 
	}
	else {
		disp "No donors"
	}
	restore
	
	*** Drop data on this indicator for donors that do not have at least 20% of their CPA represented by flows with data on this indicator
	local ind: word `i' of `inds'
	quietly replace `ind' = . if min_pct_with_data`i' < 20
}

drop aid? aid_any_data? aid_with_data? total_aid? pct_with_data? 

*************************************
*** Drop donors that have data for fewer than four indicators in either fragile or stable states
*************************************
preserve
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol, by(wbfragile donor)
egen num_ind_temp = rownonmiss(on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol)
bys donor: egen num_ind = min(num_ind_temp)
tab donor if num_ind <= 4
keep if num_ind > 4
keep donor
duplicates drop
tempfile to_keep 
save `to_keep'.dta, replace
restore
merge m:1 donor using `to_keep'.dta, keep(3) nogen norep

*************************************
*** Calculate scaled scores in fragile and stable states for each donor by finding the aid-weighted mean of standardized pair-level scores  
*************************************

*** Calculate the standard deviation above the mean for each pair on each indicator
foreach ind in on_budget use_pfm pius prog_other joint_work aid_thru_others vol avg_pred {
	quietly sum `ind' [aweight=aid]
	replace `ind' = (`ind'-`r(mean)')/`r(sd)'
}

*** Reverse sign of the standard deviations for indicators on which lower is better
foreach var in pius avg_pred vol {
	replace `var' = -1 * `var'
}

*************************************
*** Get mean scaled score and the standard deviation of that score for each type of donor (bilateral/multilateral) on each indicator, separately in fragile and stable states
*** and also get mean score and sd for bilaterals and multilaterals across all indicators
*************************************
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others  vol avg_pred ///
	(sd) on_budget_sd=on_budget use_pfm_sd=use_pfm pius_sd=pius prog_other_sd=prog_other joint_work_sd=joint_work aid_thru_others_sd=aid_thru_others vol_sd=vol avg_pred_sd=avg_pred ///
	(count) on_budget_num=on_budget use_pfm_num=use_pfm pius_num=pius prog_other_num=prog_other joint_work_num=joint_work aid_thru_others_num=aid_thru_others vol_num=vol avg_pred_num=avg_pred ///
	[aweight = aid], by(multi wbfragile)

*** Re-label the indicators
LABEL_IND

*** Get mean overall score and pooled sd for that score (based on the sd of each indicator) across all indicators
egen overall_mean = rowmean(on_budget use_pfm pius prog_other joint_work aid_thru_others vol avg_pred)
foreach var of varlist on_budget use_pfm pius prog_other joint_work aid_thru_others  vol avg_pred {
	gen wgtd_var_`var' = (`var'_num-1) * `var'_sd^2 //weighted variance
}
egen total_weighted_variances = rowtotal(wgtd_var_*)
egen total_num = rowtotal(*_num)
gen pooled_variance = total_weighted_variances/total_num
gen pooled_sd = sqrt(pooled_variance)
rename (pooled_sd total_num) (overall_mean_sd overall_mean_num)
label variable overall_mean "All Indicators"

*************************************
*** Set up blank tempfile to store results
*************************************
preserve
clear all
set obs 9
label define wbfragile 0 "Stable" 1 "Fragile"
gen ind = ""
label variable ind "Indicator"
forvalues i = 0/1 {
	local type: label wbfragile `i'
	gen multi_mean1_`i' = .
	label variable multi_mean1_`i' "Multilateral Score (`type')"
	gen multi_mean2_`i' = .
	label variable multi_mean2_`i' "Multilateral SD (`type')"
	gen bi_mean1_`i' = .
	label variable bi_mean1_`i' "Bilateral Score (`type')"
	gen bi_mean2_`i' = .
	label variable bi_mean2_`i' "Bilateral SD (`type')"
	gen diff1_`i' = ""
	label variable diff1_`i' "Multilateral Score - Bilateral Score (`type')"
	gen diff2_`i' = ""
	label variable diff2_`i' "Multilateral Score - Bilateral Score SD (`type')"
}
tempfile results
save `results'.dta, replace
restore

*************************************
*** t-test the difference between bilaterals and multilaterals in stable and fragile states for each indicator
*************************************
local i = 1
foreach ind in on_budget use_pfm pius prog_other joint_work aid_thru_others vol avg_pred overall_mean {
	local lbl: variable label `ind'
	forvalues recip_type = 0/1 { 				//Loop through stable and fragile
		
		local recip_type_lbl: label wbfragile `recip_type'
		
		preserve
		
		keep if wbfragile == `recip_type'


		*** Get mean, standard deviations, and number of observations for bilateral and multilateral scores on this indicator
		assert _N == 2
		sort multi
		local j = 1
		foreach type in "bi" "multi" {
			local `type'_mean = `ind'[`j']
			local `type'_sd = `ind'_sd[`j']
			local `type'_num = `ind'_num[`j']
			local j = `j' + 1
		}

		*** t-test the difference between bilaterals and multilaterals on this indicator
		ttesti `bi_num' `bi_mean' `bi_sd' `multi_num' `multi_mean' `multi_sd'

		*** Store the results
		use `results'.dta, clear
		replace ind = "`lbl'" if _n == `i'
		replace multi_mean1_`recip_type' = `r(mu_2)' if _n == `i'
		replace bi_mean1_`recip_type' = `r(mu_1)' if _n == `i'
		replace multi_mean2_`recip_type' = `r(sd_2)'/sqrt(`r(N_2)') if _n == `i'
		replace bi_mean2_`recip_type' = `r(sd_1)'/sqrt(`r(N_1)') if _n == `i'
		replace diff1_`recip_type' = strofreal(multi_mean1_`recip_type' - bi_mean1_`recip_type', "%8.2f") if _n == `i'
		replace diff2_`recip_type' = strofreal(`r(se)', "%8.2f") if _n == `i'
		if `r(p)' <= 0.01 {
			replace diff1_`recip_type' = diff1_`recip_type'+ "***" if _n == `i'
		}
		else if `r(p)' <= .05 {
			replace diff1_`recip_type' = diff1_`recip_type'+ "**" if _n == `i'
		}
		else if `r(p)' <= 0.1 {
			replace diff1_`recip_type' = diff1_`recip_type'+ "*" if _n == `i'
		}
		tempfile results
		save `results'.dta, replace

		restore

	}

	local i = `i' + 1

}


*************************************
*** Export Table G.1: Scores for all bilaterals and multilaterals combined on each indicator
*************************************

use `results'.dta, clear

*** Reshape so that standard deviations show up beneath the means
rename (*1_0 *2_0 *1_1 *2_1) (*_stable1 *_stable2 *_frag1 *_frag2)
reshape long multi_mean_stable bi_mean_stable diff_stable multi_mean_frag bi_mean_frag diff_frag, i(ind) j(type)
label define type 1 "Mean" 2 "SD"
label values type type

*** Sort in the correct order, with the total row at the bottom
local i = 1
gen sort_order = .
foreach ind in "Reported on budget" "Use of PFM systems" "Use of PIUs" "Use of programmatic aid" "Joint activities" "Aid through others" "Predictable funding" "Volatility" {
	replace sort_order = `i' if ind == "`ind'"
	local i = `i' + 1
}
sort sort_order type
replace ind = "" if type == 2

*** Format the results
foreach var of varlist multi_mean_stable bi_mean_stable multi_mean_frag bi_mean_frag diff_stable diff_frag {
	tostring `var', replace force format(%10.2f)
	replace `var' = "(" + `var' + ")" if type == 2
}
label variable multi_mean_stable "Multilateral Score (Stable States)"
label variable bi_mean_stable "Bilateral Score (Stable States)"
label variable diff_stable "Multilateral Score - Bilateral Score (Stable States)"
label variable multi_mean_frag "Multilateral Score (Fragile States)"
label variable bi_mean_frag "Bilateral Score (Fragile States)"
label variable diff_frag "Multilateral Score - Bilateral Score (Fragile States)"

*** Export table
export excel ind *stable *frag using "Tables and Figures/Tables for Fragile States Report.xlsx", sheetreplace sheet("Table G_1") firstrow(varlabels)

*************************************
*** Export Table 6: Scores for all bilaterals and multilaterals combined on each indicator
*************************************

*** Keep only the scores for all indicators combined
keep if ind == "All Indicators" & type == 1

*** Reshape
rename *_stable *0
rename *_frag *1
reshape long multi_mean bi_mean diff, i(ind) j(fragile)

*** Label variables
label define fragile 0 "Stable States" 1 "Fragile States"
label values fragile fragile
label variable multi_mean "Multilateral Score" 
label variable bi_mean "Bilateral Score"
label variable diff "Difference"

*** Export table
export excel fragile multi_mean bi_mean diff using "Tables and Figures/Tables for Fragile States Report.xlsx", sheetreplace sheet("Table 6") firstrow(varlabels)

