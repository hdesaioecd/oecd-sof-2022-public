clear all
program drop _all
set more off
snapshot erase _all

*** Table 5, F_1, F_2a, F_2b, F_3a, F_3b & Figure 1.do
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
	label variable bi_multi_share "Multi-bi aid"
	label variable avg_pred "Predictable funding"
	label variable vol "Volatility"
end

*************************************************************
*************************************************************
*** Table F.2a: Raw values for all big donors on each indicator in fragile states
*** Table F.2b: Raw values for all big donors on each indicator in stable states
*************************************************************
*************************************************************

*************************************
*** Read in the data
*************************************
use "Input Data/Combined Pair-Level Data.dta", clear

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
*** (We do not want to drop data because donors don't have any data in years in which no survey was conducted, for example)
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
*** Get the mean raw values for all donors, and export Table F.2a (fragile states) amd Table F.2b (stable states) to excel
*************************************

*** Fragile states
preserve
keep if wbfragile == 1
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol [aweight=aid], by(donor)
LABEL_IND
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table F_2a") sheetreplace firstrow(varlabels)
restore

*** Stable states
preserve
keep if wbfragile == 0
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol [aweight=aid], by(donor)
LABEL_IND
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table F_2b") sheetreplace firstrow(varlabels)
restore

*************************************************************
*************************************************************
*** Table F.3a: Scaled scores for all big donors on each indicator in fragile states
*** Table F.3b: Scaled scores for all big donors on each indicator in stable states
*************************************************************
*************************************************************

*************************************
*** Calculate scaled scores in fragile and stable states for each donor by finding the aid-weighted mean of standardized pair-level scores  
*************************************

*** Calculate the standard deviation above the mean for each pair on each indicator
foreach ind in on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share vol avg_pred {
	quietly sum `ind' [aweight=aid]
	replace `ind' = (`ind'-`r(mean)')/`r(sd)'
}

*** Reverse sign of the standard deviations for indicators on which lower is better
foreach var in pius avg_pred vol {
	replace `var' = -1 * `var'
}

*************************************
*** Get the mean scaled values for all donors, and export Table F.3a (fragile states) and F.3b (stable states) to excel
*************************************

*** Fragile states
preserve
keep if wbfragile == 1
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol [aweight=aid], by(donor)
LABEL_IND
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table F_3a") sheetreplace firstrow(varlabels)
restore

*** Stable states
preserve
keep if wbfragile == 0
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol [aweight=aid], by(donor)
LABEL_IND
sleep 1000
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table F_3b") sheetreplace firstrow(varlabels)
restore

*************************************************************
*************************************************************
*** Table 5: Donor's scaled scores in stable and fragile states (no standard deviations)
*** Table F.1: Donor's scaled scores in stable and fragile states (with standard deviations)
*************************************************************
*************************************************************

*************************************
*** Set up blank tempfile to store results 
*************************************
quietly tab donor
local num_obs = `r(r)' + 1
preserve
clear all
set obs `num_obs'
gen donor = ""
label variable donor "Donor"
gen fragile_mean1 = .
label variable fragile_mean1 "Fragile Score"
gen fragile_mean2 = .
label variable fragile_mean2 "Fragile Standard Deviation"
gen stable_mean1 = .
label variable stable_mean1 "Stable Score"
gen stable_mean2 = .
label variable stable_mean2 "Stable Standard Deviation"
gen diff1 = ""
label variable diff1 "Fragile Score - Stable Score"
gen diff2 = .
label variable diff2 "Fragile Score - Stable Score (Standard Deviation)"
tempfile results
save `results'.dta, replace
restore

*************************************
*** Get means scaled score and the standard deviation of that score for each donor on each indicator, separately in fragile and stable states
*************************************

*** For all donors together
preserve
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share vol avg_pred ///
	(sd) on_budget_sd=on_budget use_pfm_sd=use_pfm pius_sd=pius prog_other_sd=prog_other joint_work_sd=joint_work aid_thru_others_sd=aid_thru_others bi_multi_share_sd=bi_multi_share vol_sd=vol avg_pred_sd=avg_pred ///
	(count) on_budget_num=on_budget use_pfm_num=use_pfm pius_num=pius prog_other_num=prog_other joint_work_num=joint_work aid_thru_others_num=aid_thru_others bi_multi_share_num=bi_multi_share vol_num=vol avg_pred_num=avg_pred ///
	[aweight = aid], by(wbfragile)
gen donor = "Total"
tempfile all_donors
save `all_donors'.dta, replace
restore

*** For each donor separately
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share vol avg_pred ///
	(sd) on_budget_sd=on_budget use_pfm_sd=use_pfm pius_sd=pius prog_other_sd=prog_other joint_work_sd=joint_work aid_thru_others_sd=aid_thru_others bi_multi_share_sd=bi_multi_share vol_sd=vol avg_pred_sd=avg_pred ///
	(count) on_budget_num=on_budget use_pfm_num=use_pfm pius_num=pius prog_other_num=prog_other joint_work_num=joint_work aid_thru_others_num=aid_thru_others bi_multi_share_num=bi_multi_share vol_num=vol avg_pred_num=avg_pred ///
	[aweight = aid], by(donor donorcode wbfragile)

*** Add in a row for all donors together
append using `all_donors'.dta

	
*************************************
*** Get mean overall score and pooled standard deviation of that score for each donor across ALL indicators, seaprately in fragile and stable states
*************************************

*** Mean
egen overall_mean = rowmean(on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share vol avg_pred)

*** Pooled standard deviation
foreach var of varlist on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share vol avg_pred {
	gen wgtd_var_`var' = (`var'_num-1) * `var'_sd^2 //weighted variance
}
egen total_weighted_variances = rowtotal(wgtd_var_*)
egen total_num = rowtotal(*_num)
gen pooled_variance = total_weighted_variances/total_num
gen pooled_sd = sqrt(pooled_variance)

*************************************
*** t-test the difference between each donor's mean score in stable and fragile states
*************************************

*** Loop through each donor
levelsof donor, local(donors)
local i = 1
foreach donor of local donors {
	
	*** Keep only observations for each donor
	preserve
	keep if donor == "`donor'"
	assert _N == 2
	sort wbfragile
	
	*** Pull n, mean, and sd for this donor's scores in fragile and stable states
	local j = 1
	foreach type in "stable" "fragile" {
		local `type'_mean = overall_mean[`j']
		local `type'_sd = pooled_sd[`j']
		local `type'_num = total_num[`j']
		local j = `j' + 1
	}
	
	*** t-test the difference between the donor's score in fragile and stable states
	ttesti `stable_num' `stable_mean' `stable_sd' `fragile_num' `fragile_mean' `fragile_sd'
	
	*** Store the results
	use `results'.dta, clear
	replace donor = "`donor'" if _n == `i'
	replace fragile_mean1 = `r(mu_2)' if _n == `i'
	replace stable_mean1 = `r(mu_1)' if _n == `i'
	replace fragile_mean2 = `r(sd_2)'/sqrt(`r(N_2)') if _n == `i'
	replace stable_mean2 = `r(sd_1)'/sqrt(`r(N_1)') if _n == `i'
	replace diff1 = strofreal(fragile_mean1 - stable_mean1, "%8.2f") if _n == `i'
	replace diff2 = `r(se)' if _n == `i'
	if `r(p)' <= 0.01 {
		replace diff1 = diff1 + "***" if _n == `i'
	}
	else if `r(p)' <= .05 {
		replace diff1 = diff1 +  "**" if _n == `i'
	}
	else if `r(p)' <= 0.1 {
		replace diff1 = diff1 +  "*" if _n == `i'
	}
	tempfile results
	save `results'.dta, replace
	restore
			
	local i = `i' + 1
}


*************************************
*** Export Table 5: Donor scores in stable and fragile states without standard deviations
*************************************
use `results'.dta, clear

*** Sort in order of performance in fragile states, with the total row at the bottom
gen sort_order = (donor == "Total")
gsort sort_order -fragile_mean1
export excel donor *_mean1 diff1 using "Tables and Figures/Tables for Fragile States Report.xlsx", sheet("Table 5") sheetreplace firstrow(varlabels)

*************************************
*** Export Table F.1: Donor scores in stable and fragile states with standard deviations for every statistic
*************************************

*** Reshape so that standard deviations show up beneath the means
keep donor fragile_mean* stable_mean* diff*
tostring diff2, replace format(%8.2f) force
reshape long fragile_mean stable_mean diff, i(donor) j(type)
label define type 1 "Mean" 2 "SD"
label values type type

*** Sort in order of performance in fragile states, with the total row at the bottom
gen for_sorting_temp = fragile_mean if type == 1
bys donor: egen for_sorting = max(for_sorting_temp)
replace for_sorting = -100 if (donor == "Total")
gsort -for_sorting type
replace donor = "" if type == 2

*** Format the results
foreach var of varlist fragile_mean stable_mean diff {
	tostring `var', replace force format(%10.2f)
	replace `var' = "(" + `var' + ")" if type == 2
}
label variable fragile_mean "Fragile Score"
label variable stable_mean "Stable Score"
label variable diff "Fragile Premium"

export excel donor fragile_mean stable_mean diff using "Tables and Figures/Tables for Fragile States Report.xlsx", sheetreplace sheet("Table F_1") firstrow(varlabels)

*************************************
*** Export the input for Figure 1: Donor performance in fragile and stable states
*************************************

use `results'.dta, clear

*** Drop total row
keep if donor != "Total"

*** Get a "rank" for each donor's performance in stable and fragile states
sort fragile_mean1
gen fragile_rank = _n
sort stable_mean1
gen stable_rank = _n
gsort -stable_rank

*** Export the results 
export excel donor stable_rank fragile_rank using "Tables and Figures/Figures for Fragile States Report.xlsm", sheet("Figure 1") sheetreplace firstrow(varlabels)
