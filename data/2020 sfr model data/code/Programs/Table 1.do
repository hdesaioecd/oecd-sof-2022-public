clear all
program drop _all
set more off
snapshot erase _all

*** Table 1.do
*** 11/28/2016

cd "$directory"

*************************************************************
*************************************************************
*** Table 1: WB and OECD definition of fragile states
*************************************************************
*************************************************************

*************************************
*** Read in the data
*************************************
use "Input Data/Fragile State Regression Data.dta", clear

*** Keep only necessary variables
keep recipient year wbfragile oecdfragile

*************************************
*** Reshape for table
*************************************
reshape wide *fragile, i(recipient) j(year)

*************************************
*** Keep only states that are fragile at some point
*************************************
egen any_fragile = rowtotal(*fragile*)
drop if any_fragile == 0
drop any_fragile

*************************************
*** Export table
*************************************

*** Label the variables
forvalues i = 2007/2014 {
	label variable oecdfragile`i' "`i'"
	label variable wbfragile`i' "`i'"
}

*** Order the variables
order recipient wb* oecd*

*** Export table
export excel using "Tables and Figures/Tables for Fragile States Report.xlsx", sheetreplace sheet("Table 1") firstrow(varlabels)

