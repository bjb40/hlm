---
Author: Bryce Bartlett
---

This is code and selected output for comparing individual fixed effect models and a hierarchical growth curve. Data utilized was the HRS file compiled by RAND (randhrsN; dated 3/31/2015). Data was cleaned in R, duplicate models were estimated in Stata and Openbugs with significant data cleaning and post-processing in R.

- code/config.R: provides global variables for all of the R code (primarily local pathways).

#Cleaning HRS Data

- code/prep-data.R: loads HRS data, subsets, cleans, and outputs into a file for Stata (basic_hlm.dta) and Openbugs (bugs.dat). Openbugs data required further processing to clean the lists conducted manually and saved as bugsdat.dat. Data is not available in this repository. This code also outputs text files to check data cleaning.

#Output to check Data Cleaning

- output/output-mean.txt: (1) reports basic statistics of full data; cross-checked with HRS codebook to confirm accurate data loading; (2) reports tables to confirm dummy variable recoding.
-output/transform-dat.txt: (1) confirms data appropriately recoded from "wide" format to "long" (2) identifies variable renames (3) summarizes results of transforming age, income, education, and wealth; (4) outputs descriptive statistics of transformed data.

#Stata

- code/basic_hlm.do: contains all Stata code; detailed descriptions for estimated models and minor recoding within file. Log of results output to output/stata_basic_hlm_log.txt.

#Openbugs

- code/fixed_eff.h: individual fixed effects model.
- code/random_intercept.h: comparable hierarchical model with random intercept and time-varying covariates only.
- code/basic_hlm.h: random coefficients growth curve with random intercepts and linear slopes.

Note that filename containing "inits" (in some cases two chains for convergence testing). Results from models saved manually to file output/bugs.

#Post-processing Analysis in R

- code/ppd.R: loads posterior samples from Openbugs output; samples PPD; and calculates DIC.
- code/analyze-ppd.R: contains data analyzing posterior from Openbugs and PPD calculated in R. Includes several figures some of which were saved manually for write-up.

#Miscellaneous

- code/inv_wish_prior.R: used to test development of a covariance matrix for a multivariate normal prior described by Gelman (called a scaled inverse wishart).