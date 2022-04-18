***** Examples of the Different Multiple Hypothesis Test Commands in Stata  ************
*** David McKenzie, July 15, 2021

cd "C:\Users\wb200090\Dropbox\WorldImpactBlog\Blogposts\StataMHT\"

use "MHTplaydata.dta", clear

******************************************************
*** A regression with 5 outcomes and 4 treatments ****
******************************************************

eststo clear
areg Y1 treat1 treat2 treat3 treat4, r a(strata)
eststo table1_1
areg Y2 treat1 treat2 treat3 treat4 b_Y2, r a(strata)
eststo table1_2
areg  Y3 treat1 treat2 treat3 treat4 b_Y3, r a(strata)
eststo table1_3
areg Y4 treat1 treat2 treat3 treat4 b_Y4, r a(strata)
eststo table1_4
areg Y5 treat1 treat2 treat3 treat4 b_Y5,  r a(strata)
eststo table1_5
#delimit ;
esttab table1_*  using "output/MHT_1.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons b_*)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr

*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(20,3,.)
* Populate Outcome and Treatment 
* Outcome
forvalues j=1(1)4 {
mat y[`j',1]=1
mat y[`j'+4,1]=2
mat y[`j'+8,1]=3
mat y[`j'+12,1]=4
mat y[`j'+16,1]=5
}
* Treatment
forvalues j=1(4)17 {
mat y[`j',2]=1
mat y[`j'+1,2]=2
mat y[`j'+2,2]=3
mat y[`j'+3,2]=4
}
local i=1
foreach var of varlist Y1 Y2 Y3 Y4 Y5 {
areg `var' treat1 treat2 treat3 treat4 b_`var', r a(strata)
test treat1=0
mat y[4*`i'-3,3]=r(p)
test treat2=0
mat y[4*`i'-2,3]=r(p)
test treat3=0
mat y[4*`i'-1,3]=r(p)
test treat4=0
mat y[4*`i',3]=r(p)
local i=`i'+1
}
mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("output/Tablepvals.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "output/Tablepvals.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "output/Tablepvals.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "output/sharpenedqvals.dta", replace

restore

**************************************************************************************************
**** MHTEXP **************************************************************************************
**************************************************************************************************
cap ssc install mhtexp
set seed 123
mhtexp Y1 Y2 Y3 Y4 Y5, treatment(treatment) bootstrap(3000)

** mhtexp2 also does not allow for different controls in different equations, nor for clustering.

**************************************************************************************************
**** MHTREG **************************************************************************************
**************************************************************************************************

cap ssc install mhtreg
#delimit ;
mhtreg (Y1 treat1 treat2 treat3 treat4 i.strata)
 (Y1 treat2 treat3 treat4 treat1 i.strata)
 (Y1 treat3 treat4 treat1 treat2  i.strata) 
 (Y1 treat4 treat1 treat2 treat3  i.strata) 
(Y2 treat1 treat2 treat3 treat4 b_Y2 i.strata) 
(Y2 treat2 treat3 treat4 treat1 b_Y2 i.strata) 
(Y2 treat3 treat4 treat1  treat2 b_Y2 i.strata) 
(Y2 treat4 treat1  treat2  treat3 b_Y2 i.strata) 
(Y3 treat1 treat2 treat3 treat4 b_Y3 i.strata)
(Y3 treat2 treat3 treat4 treat1 b_Y3 i.strata)
(Y3 treat3 treat4 treat1 treat2  b_Y3 i.strata)
(Y3 treat4  treat1 treat2 treat3 b_Y3 i.strata)
(Y4 treat1 treat2 treat3 treat4 b_Y4 i.strata)
(Y4 treat2 treat3 treat4 treat1 b_Y4 i.strata)
(Y4 treat3 treat4 treat1 treat2 b_Y4 i.strata)
(Y4 treat4 treat1 treat2 treat3 b_Y4 i.strata)
(Y5 treat1 treat2 treat3 treat4 b_Y5 i.strata)
(Y5 treat2 treat3 treat4 treat1 b_Y5 i.strata)
(Y5 treat3 treat4 treat1 treat2 b_Y5 i.strata)
(Y5 treat4 treat1 treat2 treat3 b_Y5 i.strata), 
seed(789) bootstrap(3000);
#delimit cr

**************************************************************************************************************
**** WYOUNG **************************************************************************************************
**************************************************************************************************************

* Install latest version
net install wyoung, from("https://raw.githubusercontent.com/reifjulian/wyoung/master") replace

*** New controls command when controls differ across equations
gen constant=1
preserve 
#delimit ;
wyoung Y1 Y2 Y3 Y4 Y5, cmd(areg OUTCOMEVAR treat1 treat2 treat3 treat4 CONTROLVARS, r a(strata)) familyp(treat1 treat2 treat3 treat4) controls("constant" "b_Y2" "b_Y3" "b_Y4" "b_Y5") bootstraps(3000) seed(123);
#delimit cr
restore


****************************************************************************************************
**** RWOLF *****************************************************************************************
****************************************************************************************************

cap ssc install rwolf2


rwolf2 (areg Y1 treat1 treat2 treat3 treat4, r a(strata)) ///
(areg Y2 treat1 treat2 treat3 treat4, r a(strata)) ///
(areg  Y3 treat1 treat2 treat3 treat4 b_Y3, r a(strata)) ///
(areg Y4 treat1 treat2 treat3 treat4 b_Y4, r a(strata)) ///
(areg Y5 treat1 treat2 treat3 treat4 b_Y5,  r a(strata)), ///
indepvars(treat1 treat2 treat3 treat4, treat1 treat2 treat3 treat4, ///
treat1 treat2 treat3 treat4, treat1 treat2 treat3 treat4, ////
treat1 treat2 treat3 treat4) usevalid seed(123) reps(3000)


********************************************************************************************
*** randcmd *******************************************************************************
****************************************************************************************
#delimit ;
randcmd 
((treat1 treat2 treat3 treat4) areg Y1 treat1 treat2 treat3 treat4, r a(strata))
((treat1 treat2 treat3 treat4) areg Y2 treat1 treat2 treat3 treat4 b_Y2, r a(strata)) 
((treat1 treat2 treat3 treat4) areg  Y3 treat1 treat2 treat3 treat4 b_Y3, r a(strata))
((treat1 treat2 treat3 treat4) areg Y4 treat1 treat2 treat3 treat4 b_Y4, r a(strata))
((treat1 treat2 treat3 treat4) areg Y5 treat1 treat2 treat3 treat4 b_Y5, r a(strata))
, strata(strata) sample treatvars(treat1 treat2 treat3 treat4) seed(123) reps(2000);






