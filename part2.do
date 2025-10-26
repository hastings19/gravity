************************** TAKING OVER FROM R **********************************

ssc install moremata, replace
ssc install ftools, replace
ssc install reghdfe, replace
ssc install ppmlhdfe, replace
ssc install estout, replace



************************* COUNTRY LEVEL ****************************************
cls
clear 

use "C:\Users\hasti\OneDrive\Documents\MSc Econ\Sem 5 SS\Master thesis\destat data\destatsmall.dta"

save "C:\Users\hasti\OneDrive\Documents\MSc Econ\Sem 5 SS\Master thesis\destat data\destatsmall.dta", replace

* Save work

eststo clear
eststo: 
esttab using "C:\Users\hasti\OneDrive\Desktop\stata\eq .csv", se star(* 0.10 ** 0.05 *** 0.01) replace

* gen
encode product_category, gen(product__category)
encode partner, gen(partner_)
gen ldc_lksg1 = ldc_dummy*lksg1
gen ldc_lksg2 = ldc_dummy*lksg2
gen lngdpde = ln(gdp_de)
gen lngdp = ln(gdp)
gen ln_impvalue = ln(importsvalue)

* Equation 4
reg ln_impvalue lngdpde lngdp lksg, vce(cluster partner_iso3)

* Equation 5
ppmlhdfe importsvalue gdp_de gdp lksg, vce(cluster partner_iso3)

* Equation 6
ppmlhdfe importsvalue gdp_de gdp lksg, absorb (partner_iso3) vce(cluster partner_iso3)

* Equation 7
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2, vce(cluster partner_iso3)

* Equation 8
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2, absorb (partner_iso3) vce(cluster partner_iso3)

* Equation 9
ppmlhdfe importsvalue gdp_de gdp ldc_dummy lksg1 lksg2 ldc_lksg1 ldc_lksg2, vce(cluster partner_iso3)

* Equation 10
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 ldc_lksg1 ldc_lksg2, absorb (partner_iso3) vce(cluster partner_iso3)

* Equation 11
ppmlhdfe importsvalue ldc_lksg1 ldc_lksg2, absorb (partner_iso3 period) vce(cluster partner_iso3)



************************* SECTORAL LEVEL ***************************************
clear
cls

use "C:\Users\hasti\OneDrive\Documents\MSc Econ\Sem 5 SS\Master thesis\destat data\destat2020.dta"
save "C:\Users\hasti\OneDrive\Documents\MSc Econ\Sem 5 SS\Master thesis\destat data\destat2020.dta", replace


* Save work

eststo clear
eststo: 
esttab using "C:\Users\hasti\OneDrive\Desktop\stata\eq.csv", se star(* 0.10 ** 0.05 *** 0.01) replace

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
gen hi_ldc = high_impact * ldc_dummy
gen lksg1_ldc_hi = lksg1* ldc_dummy * high_impact
gen lksg2_ldc_hi = lksg2* ldc_dummy * high_impact
gen lksg1hi = lksg1 * high_impact
gen lksg2hi = lksg2 * high_impact


* Equation 12
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 ldc_lksg1 ldc_lksg2, absorb (partner_#product__category) vce(cluster partner_#product__category)

* Equation 13
ppmlhdfe importsvalue ldc_lksg1 ldc_lksg2, absorb (partner_#product__category period) vce(cluster partner_#product__category)

* Equation 14
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##high_impact, absorb (partner_) vce(cluster partner_#product__category)

* Equation 15
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##high_impact, absorb (partner_#product__category) vce(cluster partner_#product__category)

* Equation 16
ppmlhdfe importsvalue high_impact lksg1hi lksg2hi, absorb (partner_#period) vce(cluster partner_#product__category)

* Equation 17
ppmlhdfe importsvalue lksg1hi lksg2hi, absorb (partner_#product__category period) vce(cluster partner_#product__category)

* Equation 18
reg ln_impvalue lngdpde lngdp lksg1 lksg2 high_impact ldc_dummy lksg1hi ldc_lksg1 lksg2hi ldc_lksg2 ldc_hi lksg1_ldc_hi lksg2_ldc_hi

* Equation 19
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 high_impact ldc_dummy lksg1hi ldc_lksg1 lksg2hi ldc_lksg2 ldc_hi lksg1_ldc_hi lksg2_ldc_hi, vce(cluster partner_#product__category)

* Equation 20
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 high_impact lksg1hi ldc_lksg1 lksg2hi ldc_lksg2 ldc_hi lksg1_ldc_hi lksg2_ldc_hi, absorb (partner_) vce (cluster partner_#product__category)

* Equation 21
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 lksg1hi ldc_lksg1 lksg2hi ldc_lksg2  lksg1_ldc_hi lksg2_ldc_hi, absorb (partner_#product__category) vce(cluster partner_#product__category)

* Equation 22
ppmlhdfe importsvalue high_impact lksg1hi lksg2hi ldc_hi lksg1_ldc_hi lksg2_ldc_hi, absorb (partner_#period) vce (cluster partner_#product__category)

* Equation 23
ppmlhdfe importsvalue lksg1hi ldc_lksg1 lksg2hi ldc_lksg2  lksg1_ldc_hi lksg2_ldc_hi, absorb (partner_#product__category period) vce(cluster partner_#product__category)

* Equation 24
ppmlhdfe importsvalue i.product__category##i.lksg, absorb(partner_#product__category period) vce(cluster partner_#product__category)

* Equation 25
ppmlhdfe importsvalue ib1.level_lksg1 ib1.level_lksg2, absorb (partner_ period) vce(cluster partner_)

* Equation 26
ppmlhdfe importsvalue cpi_lksg1 cpi_lksg2, absorb (partner_ period) vce(cluster partner_)



