cap cd "C:\Users\giaco\Dropbox\NYU\TA Work\Quant 2 Spring 2022\Lab material\lab7"

** Import the data
import delim trade.csv, clear

** Generate the variables for the analysis
gen log_euro = log(euros)
gen log_dist = log(dist_km)
encode origin, gen(origin2)

** regress
reg log_euro log_dist i.origin2, cluster(origin2)
est sto reg_est

** areg
areg log_euro log_dist, a(origin2) cluster(origin2)
est sto areg_est

** xtreg
xtset origin2
xtreg log_euro log_dist, fe cluster(origin2)
est sto xtreg_est

** reghdfe
reghdfe log_euro log_dist, a(origin2) cluster(origin2)
est sto hdfe_est

est tab reg_est areg_est xtreg_est hdfe_est, keep(log_dist) se
