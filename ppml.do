******** TAKING OVER FROM R *********

** prep
encode partner, gen(partner_)
gen level_lksg1 = level*lksg1
gen level_lksg2 = level*lksg2
gen cpi_lksg1 = cpi*lksg1
gen cpi_lksg2 = cpi*lksg2
gen ldc_lksg1 = ldc_dummy*lksg1
gen ldc_lksg2 = ldc_dummy*lksg2


****** PPML REGRESSIONS *******

import delimited "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data/destatsmall.csv

save "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data/destatsmall.dta"

** COUNTRY LEVEL **

* model i (with fe)
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 ldc_dummy ldc_lksg1 ldc_lksg2, absorb (partner_) vce(robust)

* model i (without fe)
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 ldc_dummy ldc_lksg1 ldc_lksg2, vce(robust)

** PRODUCT CATEGORY LEVEL **

import delimited "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data/destat2020.csv

save "/Users/hastings/Library/CloudStorage/OneDrive-Personal/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data/destat2020.dta"

* model ii
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##ldc_dummy##high_impact, absorb (product__category#partner_) vce(robust)

* model iii
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##high_impact, absorb (partner_#product__category) vce(robust)

* model iv
ppmlhdfe importsvalue gdp_de gdp lksg1##lksg2##level, absorb (partner_#product__category) vce(robust)

* model v
ppmlhdfe importsvalue gdp_de gdp lksg1 lksg2 lksg1_cpi lksg2_cpi, absorb (partner_#product__category) vce(robust)

* model vi (value per kilo)
* check for systematicly missing variables
gen vpk = importsvalue / importsnetmass
gen vpk_missing = missing(vpk)
tabulate product_category vpk_missing, missing
tabulate partner_iso3 vpk_missing, missing
tabulate level vpk_missing, missing
* clear to reg
ppmlhdfe vpk gdp_de gdp lksg1 lksg2, absorb (partner_#product__category) vce(robust)


*save
eststo clear
eststo:
esttab using "output.csv", se star(* 0.10 ** 0.05 *** 0.01) replace
