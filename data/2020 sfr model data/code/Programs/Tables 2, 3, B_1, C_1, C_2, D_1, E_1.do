clear all
program drop _all
set more off
snapshot erase _all

*** Tables 2, 3, B_1, C_1, C_2, D_1, E_1.do
*** 11/28/2016

cd "$directory"

************************************************************
************************************************************
*** Prepare data for all regressions
************************************************************
************************************************************

*************************************
*** Read in the data
*************************************

*** Read in pair-level data
use "Input Data/Combined Pair-Level Data.dta", clear

*** Collapse pair-level data to the recipient level,  weighted by pair-level CPA
collapse (mean) on_budget use_pfm pius prog_other joint_work aid_thru_others bi_multi_share avg_pred vol (rawsum) multi_pct aid [aweight=aid], by(recipient recipientcode year)

*** Cap predictability indicator at 50% (definitionally, the ratio of aid given through others to total aid cannot be more than 50% for donors in the aggregate)
replace aid_thru_others = 50 if aid_thru_others > 50 & aid_thru_others < . // This only impacts 1 observation: the Solomon Islands in 2010

*** Merge in relevant recipient-level variables
merge 1:1 recipientcode year using "Input Data/Fragile State Regression Data.dta", assert(3) nogen norep

*************************************
*** Re-label indicator variables 
*************************************
label variable on_budget "Reported on budget"
label variable use_pfm "Use of PFM systems"
label variable pius "Use of PIUs"
label variable prog_other "Use of programmatic aid"
label variable joint_work "Joint activities"
label variable aid_thru_others "Aid through others"
label variable multi_pct "Multilateral aid"
label variable bi_multi_share "Multi-bi aid"
label variable avg_pred "Predictable funding"
label variable vol "Volatility"

*************************************
*** Save snapshot
*************************************
snapshot save 

************************************************************
************************************************************
*** Define a program to run regressions on all ten indicators and save the output
*** Arguments
***		fragile 		= definition of fragility
*** 	table_name 		= name of table (to use in naming the output file)
***		controls 		= list of any additional regressors beyond GDP, population, and donor controls
************************************************************
************************************************************

program define INDICATOR_REGS 

	args fragile table_name controls
	
	*************************************
	*** Set fragility indicator
	*************************************
	gen fragile = `fragile'
	label variable fragile "Fragile Dummy"
		
	local i = 1
	
	*************************************
	*** Loop through all indicators
	*************************************
	foreach ind in on_budget use_pfm pius prog_other joint_work aid_thru_others multi_pct bi_multi_share avg_pred vol {
		
		local lbl: variable label `ind'
			
		*************************************
		*** Run regressions with GDP, population, donor controls, and additional control variables stored in the `controls' argument
		*** (but, for the indicator measuring aid from multilaterals (multi_pct), exclude the donor control variables)
		*************************************
		if "`ind'" != "multi_pct" {
			reg `ind' fragile gdp pop donor_pct* `controls' [aweight=aid]
			
		}
		else {
			reg `ind' fragile gdp pop `controls' [aweight=aid]
		}

		*** Get the number of recipients included in this regression 
		quietly tab recipient if e(sample)
		local num_recips = `r(r)'

		*************************************
		*** Outreg the results to .dta files that we will later combine into an excel spreadsheet
		*************************************
		
		if `i' == 1 {
			outreg2 using "Tables and Figures/Outreg Output/`table_name'", dta replace label ctitle("`lbl'") keep(fragile gdp pop `controls')  addstat(# of Recipients, `num_recips') dec(1) 
		}
		else {
			outreg2 using "Tables and Figures/Outreg Output/`table_name'", dta append label ctitle("`lbl'") keep(fragile gdp pop `controls') addstat(# of Recipients, `num_recips') dec(1) 
		}
	
		local i = `i' + 1
		
	}
	
	*************************************
	*** Export the results that we combined in a .dta file to the Microsoft Excel file that containes all tables combined
	*** (Note: This is necessary because outreg2 does not allow you to specify sheetnames when exporting to excel.)
	*************************************
	preserve
	use "Tables and Figures/Outreg Output/`table_name'_dta.dta", clear
	export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheetreplace sheet("`table_name'")
	restore 
	
	drop fragile 
	
end

************************************************************
************************************************************
*** Table 2: Regression results using WB fragiltiy indicator (fragility coefficients only, in text form)
*** Table B.1: Regression results using WB fragiltiy indicator (full results)
************************************************************
************************************************************

*************************************
*** Run regressions and save output for table with full regression results (Table C.1)
*************************************
INDICATOR_REGS wbfragile "Table B_1" "i.year"

*************************************
*** Save a table with just the appropriate coefficients for the table of fragility coefficients only, in text form (Table 2)
*************************************

*** Read in regression results saved in .dta format
use "Tables and Figures/Outreg Output/Table B_1_dta.dta", clear

*** Pull indicator names and coeffients 
forvalues i = 1/10 {
	local j = `i' + 1
	local ind`i' = v`j'[2]
	local coeff`i' = v`j'[4]
}

*** Save indicator names and coefficients in table form
clear
set obs 10
gen ind = ""
label variable ind "Indicator"
gen coeff = ""
label variable coeff "Coefficient of Fragility"
forvalues i = 1/10 {
	quietly replace ind = "`ind`i''" if _n == `i'
	quietly replace coeff = "`coeff`i''" if _n == `i'
}

*** Export table to excel 
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheetreplace sheet("Table 2") firstrow(varlabels)

************************************************************
************************************************************
*** Table 3: Regression results using OECD fragiltiy indicator (fragility coefficients only, in text form)
*** Table E.1: Regression results using OECD fragiltiy indicator (full results)
************************************************************
************************************************************

snapshot restore 1

*************************************
*** Run regressions and save output for table with full regression results (Table F.1)
*************************************
INDICATOR_REGS oecdfragile "Table E_1" "i.year"

*************************************
*** Save a table with just the appropriate coefficients for the table of fragility coefficients only, in text form (Table 3)
*************************************

*** Read in regression results saved in .dta format
use "Tables and Figures/Outreg Output/Table E_1_dta.dta", clear

*** Pull indicator names and coeffients 
forvalues i = 1/10 {
	local j = `i' + 1
	local ind`i' = v`j'[2]
	local coeff`i' = v`j'[4]
}

*** Save indicator names and coefficients in table form
clear
set obs 10
gen ind = ""
label variable ind "Indicator"
gen coeff = ""
label variable coeff "Coefficient of Fragility"
forvalues i = 1/10 {
	quietly replace ind = "`ind`i''" if _n == `i'
	quietly replace coeff = "`coeff`i''" if _n == `i'
}

*** Export table to excel 
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheetreplace sheet("Table 3") firstrow(varlabels)

************************************************************
************************************************************
*** Table C.1: Regression results using WB fragiltiy indicator interacted with year
************************************************************
************************************************************

snapshot restore 1

*************************************
*** Create and label interaction variables
*************************************
levelsof year, local(years)
foreach year of local years {
	gen year`year'xfragile = (year == `year' & wbfragile == 1)
	label variable year`year'xfragile "Year `year' x Fragile"
}


*************************************
*** Run regressions and save output
*************************************
INDICATOR_REGS wbfragile "Table C_1" "i.year *xfragile"

************************************************************
************************************************************
*** Table C.2: Regression results using WB fragiltiy indicator and continuous year variable interacted with fragility
************************************************************
************************************************************

snapshot restore 1

*************************************
*** Create a continous variable marking the # of years since the earliest year (2007)
*************************************
quietly sum year, det
gen years_since_`r(min)' = year - `r(min)'
label variable years_since_`r(min)' "Years Since `r(min)'"

*************************************
*** Create interaction of continuous year variable and fragility
*************************************
gen yearxfragile = years_since_`r(min)' * wbfragile
label variable yearxfragile "Years x Fragile"

*************************************
*** Run regressions and save output
*************************************
INDICATOR_REGS wbfragile "Table C_2" "years_since_`r(min)' yearxfragile"

************************************************************
************************************************************
*** Table D.1: Regression results using WB fragiltiy indicator and sector variables interacted with fragility
************************************************************
************************************************************

snapshot restore 1

*************************************
*** Create sector interaction variables
*************************************
foreach var of varlist sector* {
	local lbl: variable label `var'
	gen `var'xfragile = `var' * wbfragile
	label variable `var'xfragile "`lbl' x Fragile"
}
*************************************
*** Run regressions and save output
*************************************
INDICATOR_REGS wbfragile "Table D_1" "i.year sector*"


