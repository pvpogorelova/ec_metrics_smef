clear

set obs 100

set seed 777
gen n = _n
gen z1 = runiform(0,1)
gen z2 = 0.1*runiform(0,1) + 0.9*z1
gen eps = rnormal()
gen y = 1 + z1 + z2 + eps

corr y z1 z2

forvalues i = 1(1)10 {
    generate x`i' = (`i'+100)/100 * z1 + rnormal()
}

gen dtest = 1
replace dtest = 2 if n > 90

* Оценим модель с помощью МНК, используя все регрессоры
reg y z1 z2 x* if dtest==1
predict res_ols, res
gen res_ols2 = res_ols^2

* LASSO
lasso2 y z1 z2 x* if dtest == 1, lic(bic) 
lasso2, lic(bic) 
predict res_lasso, r lic(bic)
gen res_lasso2 = res_lasso^2

* LASSO+CV
cvlasso y z1 z2 x* if dtest == 1, nfolds(10)
cvlasso, lopt
predict res2_cv, residuals lopt
gen res2_cv2 = res2_cv^2


reg y z2 x5 if dtest == 1
predict res1, res
gen res21 = res1^2

bysort dtest: sum res_ols2 res21 res2_cv2 res_lasso2




