* Basic HLM model to compare with Openbugs
* Data cleaned in R and output for Openbugs and for Stata
* Descriptives and data definitions in relevant output files

*@@@@@@@@@@@@@
*Preliminaries
*@@@@@@@@@@@@@@

* Load Clean Data
 use "/misc/utopia3/bjb40/lanhome/Projects/hlm_final/output/basic_hlm.dta"


capture log close
log using "/misc/utopia3/bjb40/lanhome/Projects/hlm_final/output/stata_basic_hlm_log.txt", replace text


*@@@@@@@@@@@@@@@@@@@
*Prepare interactions with age for mixed models
*@@@@@@@@@@@@@@@@@@@

*linear
gen ageXz1 = age*z1
gen ageXz2 = age*z2
gen ageXz3 = age*z3
 
*quadratic 
gen age2 = age*age
gen age2Xz1 = age2*z1
gen age2Xz2 = age2*z2
gen age2Xz3 =age2*z2
 
*@@@@@@@@@@@@@@@@@@@
*Unconditional Random Models (general shape of outcome)
*@@@@@@@@@@@@@@@@@@@
 
* unconditional intercept only
mixed y ||id: , reml cov(un) var

* unconditional linear
mixed y age ||id: age, reml cov(un) var

* unconditional quadratic
mixed y age age2 ||id: age age2, reml cov(un) var


*@@@@@@@@@@@@@@@@@@@
*Models to Report
*@@@@@@@@@@@@@@@@@@@

	*----------------------------
	*1. Fixed Effects Model (2 equivalent ways)
	*----------------------------
	xi: regress y age x1-x3 x5 x6 i.id 
	estat ic 
	esttab using "/misc/utopia3/bjb40/lanhome/Projects/hlm_final/output/st-fixed.csv", replace wide plain b se

	*==>produces demeaned (absorb in stata) estimates -- equivalent
	*needed to get the appropriate fixed effect
	*xtset id
	*xtreg y age x1-x3 x5 x6, fe

	*save estimates for hausman test
	estimates store fixed

	*----------------------------
	*2. Random Intercept (2 models)
	*----------------------------

		*A. no random coefficients
		*mixed y age x1-x3 x5 x6 || id: , mle cov(un) var
		*estimates close, but will not produce results for hausman test
		xtset id
		xtreg y age x1-x3 x5 x6, re
		esttab using "/misc/utopia3/bjb40/lanhome/Projects/hlm_final/output/st-random.csv", replace wide plain b se
 

		*Hausman test for A
		hausman fixed ., sigmamore

		*B. random coefficients model
		xtreg y z1-z3 age x1-x3 x5 x6, re 

	*----------------------------
	*3. Random Coefficients Growth (Intercpet and Slope; 2 estimators)
	*----------------------------
		*A. ML estimator
		mixed y z1-z3 age x1-x3 x5 x6 ageXz1-ageXz3 || id: age, mle cov(un) var
		estat ic
		
		*B. REML estimator
		mixed y z1-z3 age x1-x3 x5 x6 ageXz1-ageXz3 || id: age, reml cov(un) var
		estat ic
		esttab using "/misc/utopia3/bjb40/lanhome/Projects/hlm_final/output/st-rcoeff.csv", replace wide plain b se

