************ TAKING OVER FROM R ****************
clear
cls 


use "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/Master thesis/destat data/destat2020.dta"

* a little cleaning
encode product_category, gen(product__category)
encode partner, gen(partner_)
gen level_lksg1 = level*lksg1
gen level_lksg2 = level*lksg2
gen cpi_lksg1 = cpi*lksg1
gen cpi_lksg2 = cpi*lksg2
gen ldc_lksg1 = ldc_dummy*lksg1
gen ldc_lksg2 = ldc_dummy*lksg2
gen lksg_level = lksg*level
gen lksg1_level = lksg1*level
gen lksg2_level = lksg2*level
gen hi_ldc = high_impact * ldc_dummy
gen lksg1_ldc_hi = lksg1* ldc_dummy * high_impact
gen lksg2_ldc_hi = lksg2* ldc_dummy * high_impact
gen lksg1hi = lksg1 * high_impact
gen lksg2hi = lksg2 * high_impact

ssc install moremata, replace
ssc install ftools, replace
ssc install reghdfe, replace
ssc install ppmlhdfe, replace

save "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/Master thesis/destat data/destat2020.dta", replace


*****  COUNTRY LEVEL
cls
clear 

use "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/Master thesis/destat data/destatsmall.dta"

* Equation 4
ppmlhdfe importsvalue gdp_de gdp lksg, vce(cluster partner_iso3)


* Equation 5
ppmlhdfe importsvalue gdp_de gdp lksg, absorb (partner_iso3) vce(cluster partner_iso3)


* Equation 6
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2, vce(cluster partner_iso3)


* Equation 7
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2, absorb (partner_iso3) vce(cluster partner_iso3)


* Equation 8
ppmlhdfe importsvalue gdp_de gdp ldc_dummy lksg1 lksg2 ldc_lksg1 ldc_lksg2, vce(cluster partner_iso3)


* Equation 9
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 ldc_lksg1 ldc_lksg2, absorb (partner_iso3) vce(cluster partner_iso3)


* SAVE WORK

eststo clear
eststo: 
esttab using "/Users/hastings/Desktop/stata/eq ?.csv", se star(* 0.10 ** 0.05 *** 0.01) replace



***** SECTORAL LEVEL
clear
cls

use "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/Master thesis/destat data/destat2020.dta"

* Equation 10
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 ldc_lksg1 ldc_lksg2, absorb (partner_#product__category) vce(cluster partner_#product__category)



* Equation 11
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##high_impact, absorb (partner_) vce(cluster partner_#product__category)


* Equation 12
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##high_impact, absorb (partner_#period) vce(cluster partner_#product__category)


* Equation 13
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##high_impact, absorb (partner_#product__category) vce(cluster partner_#product__category)


* Equation 14
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 high_impact ldc_dummy lksg1hi lksg2hi ldc_lksg1 ldc_lksg2 ldc_hi lksg1_ldc_hi lksg2_ldc_hi, vce(cluster partner_#product__category)


* Equation 15
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 lksg1hi lksg2hi ldc_lksg1 ldc_lksg2 lksg1_ldc_hi lksg2_ldc_hi, absorb (partner_#product__category) vce(cluster partner_#product__category)


* Equation 16
ppmlhdfe importsvalue high_impact lksg1hi lksg2hi ldc_hi lksg1_ldc_hi lksg2_ldc_hi, absorb (partner_#period) vce(cluster partner_#product__category)


* Equation 17
ppmlhdfe importsvalue gdp_de gdp i.product__category##i.lksg, absorb(partner_#product__category) vce(cluster partner_#product__category)



*** EXTENSION
* Equation 18
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 level ib1.lksg1_level ib1.lksg2_level, absorb (partner_) vce(cluster partner_)

* Equation 19
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 cpi cpi_lksg1 cpi_lksg2, absorb (partner_) vce(cluster partner_)


*SAVE WORK
	

eststo clear
eststo: 
esttab using "/Users/hastings/Desktop/stata/eq…csv", se star(* 0.10 ** 0.05 *** 0.01) replace

save "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/Master thesis/destat data/destat2020.dta", replace

clear
cls


