clear all
program drop _all
set more off
snapshot erase _all

*** Aid effectiveness in fragile states: How bad is it and how can it improve? 
*** Laurence Chandy, Brina Seidel, and Christine Zhang

*** File name: Fragile States Analysis Master Do File.do
*** Last Updated: 12/1/2016
*** Description: This program calls all subsequent programs necessary to produce the tables and figures for this report. 

*** Assign your working directory here
global directory = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\Final Fragile States Analysis"

cd "$directory"

*************************************
*** Table 1
*************************************
do "Programs/Table 1.do"

*************************************
*** Tables 2, 3, B_1, C_1, C_2, D_1, E_1.do
*************************************
do "Programs/Tables 2, 3, B_1, C_1, C_2, D_1, E_1.do"

*************************************
*** Table 4
*************************************
do "Programs/Table 4.do"

*************************************
*** Tables 5, F_1, F_2a, F_2b, F_3a, F_3b & Figure 1
*************************************
do "Programs/Tables 5, F_1, F_2a, F_2b, F_3a, F_3b & Figure 1.do"

*************************************
*** Table 6, G_1
*************************************
do "Programs/Table 6, G_1.do"

*************************************
*** Tables A_1a, A_1b, A_2, A_3
*************************************
do "Programs/Tables A_1a, A_1b, A_2, A_3.do"

