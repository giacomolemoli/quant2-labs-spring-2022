********************************************************************************
*          Quantile regression and Indexes
*                  Giacomo Lemoli
*               Quant II Spring 2022
********************************************************************************

/* 
Let's use the data by Bautista, González, Martínez, Muñoz, Prem (AJPS 2021)
To apply quantile regression, we focus on results in Table 3, Panel A
(reduced form), and on the binary instrument/treatment case (presence of military base)
*/

** Set directory
cd "C:/Users/giaco/Dropbox/NYU/TA Work/Quant 2 Spring 2022/Lab material/lab9"

** Packages
ssc install outreg2, replace

** Import data
use FinalDatasetForReplication, clear

** Prepare data
keep if MainSample == 1

** Global: control variables to use
global C "share_allende70 share_alessandri70 lnDistStgo lnDistRegCapital Pop70_pthousands sh_rural_70"	

** Global: weights to use in the regression
global W "Pop70"


***** Original regression *****
reghdfe VoteShareNo DMilitaryPresence $C [aw=$W], absorb(IDProv) vce(robust)


***** Quantile regression *****
* Note that differently from the original paper we don't use weights 
* (sqreg/bsqreg don't accept them)

** qreg
forval q = 0.25(0.25)0.75 {
local x = `q'*100
qreg VoteShareNo DMilitaryPresence $C i.IDProv, quantile(`q')
est sto est_`x'
}

esttab est_25 est_50 est_75, keep(DMilitaryPresence) se 

** Bootstrapped standard errors: sqreg
set seed 10
sqreg VoteShareNo DMilitaryPresence $C i.IDProv, quantiles(0.25 0.5 0.75) reps(100)
outreg2 using sqreg.txt, bdec(5) keep(DMilitaryPresence) nocons replace



***** Principal Component Analysis *****
/*
Outcomes: 
- Share_reg70_w2: Share of registered voters
- VoteShareNo: Vote share for No (over total votes cast)
- VoteShareNo_pop70: Vote share for No (over population)
*/

pca Share_reg70_w2 VoteShareNo VoteShareNo_pop70 

** Screeplot
screeplot, ci 

** Generate factor scores for the first 2 principal components
predict pc1 pc2, score

** Check they are orthogonal (i.e. 0 correlation)
corr pc1 pc2

** These are now columns of the dataset so we can use them as we want
reghdfe pc1 DMilitaryPresence $C [aw=$W], absorb(IDProv) vce(robust)

