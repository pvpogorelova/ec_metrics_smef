clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23
log using sem15.log

use "http://www.princeton.edu/~otorres/WDI.dta", clear
gen after = (year >= 2009) if !missing(year)

merge m:1 country using "http://www.princeton.edu/~otorres/Treated.dta", gen(merge1)
replace treated = 0 if treated == .
gen did = after * treated
encode country, gen(country1)
reg gdppc after treated  did, robust cluster(country1)

ssc install diff
diff gdppc, period(after) treated(treated) cluster(country1)

bysort year treated: egen mean_gdppc = mean(gdppc)
twoway line mean_gdppc year if treated == 0, sort || line mean_gdppc year if treated == 1, sort lpattern(dash) legend(label(1 "Control") label(2 "Treated")) xline(2009)
reg gdppc treated##ibn.year if after == 0, vce(cluster country1) hascons

log close
