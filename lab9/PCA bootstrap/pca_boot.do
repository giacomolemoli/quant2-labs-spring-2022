
** Set directory
cd "C:/Users/giaco/Dropbox/NYU/TA Work/Quant 2 Spring 2022/Lab material/lab9"

** Import data
use FinalDatasetForReplication, clear

** Prepare data
keep if MainSample == 1

** Global: control variables to use
global C "share_allende70 share_alessandri70 lnDistStgo lnDistRegCapital Pop70_pthousands sh_rural_70"	

** Global: weights to use in the regression
global W "Pop70"



set matsize 3000

** Vector where to store the results
mat def ests = J(1000,1,.)

** Loop
set seed 123

forval i=1/1000{
** Maintain the original dataset
preserve

** Size of each new random sample (N)
qui count
local n = r(N)

** Resample with replacement
bsample `n'

** PCA
qui pca Share_reg70_w2 VoteShareNo VoteShareNo_pop70 

** Scores
predict pc1, score

** Regression on the score
qui reghdfe pc1 DMilitaryPresence $C [aw=$W], absorb(IDProv) vce(robust)

** Save the estimate
mat ests[`i',1] = _b["DMilitaryPresence"]

** Restore the original dataset
restore
}

** Clear workspace
clear

** Turn the estimates in variable format
svmat ests

** Compute mean and CI quantiles
sum ests1, de
local mean = r(mean)

_pctile ests1, p(2.5 97.5)
return list
local ll = r(r1)
local ul = r(r2)

di "Mean= `mean'" 
di "2.5%:`ll'" 
di "97.5%: `ul'"

