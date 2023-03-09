clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика
log using sem8.log

* Multinomial logistic regression
use "/Users/polinapogorelova/Downloads/PENSION.DTA", clear

sum
gen pctstck1 = 1
replace pctstck1 = 2 if pctstck == 50
replace pctstck1 = 3 if pctstck == 100


mlogit pctstck1 i.choice age educ i.female i.married wealth89 prftshr, base(1)
mlogit, rrr // relative-risk ratios (показывает, чему равно отношение вероятностей принадлежности к разным группам при увеличении объясняющей переменной на 1 единицу)
test [2]age = [3]age

margins i.female, pr(out(1))

margins, dydx(female) predict(out(1)) // предельный эффект, рассчитанный для переменной female (как изменится вероятность выбрать облигации в зависимости от значения female)
margins, dydx(female) predict(out(2))
margins, dydx(female) predict(out(3))


margins i.female, atmeans pr(out(1)) // прогноз вероятности выбрать облигации (y=1) в зависимости от пола при условии, что все остальные переменные выбраны на их среднем уровне
marginsplot, name(Bonds1)
graph export oblig, as(png) width(600) height(450) replace


margins i.female, atmeans pr(out(2)) // прогноз вероятности выбрать смешанный тип (y=2) в зависимости от пола при условии, что все остальные переменные выбраны на их среднем уровне
marginsplot, name(Mixed2)
margins i.female, atmeans pr(out(3)) // прогноз вероятности выбрать акции (y=3) в зависимости от пола при условии, что все остальные переменные выбраны на их среднем уровне
marginsplot, name(Stocks3)

graph combine Bonds1 Mixed2 Stocks3, ycommon

margins, at(educ = (8(2)18)) predict(outcome(1)) vsquish // средняя вероятность того, что y=1 на каждом уровне количественной переменной educ (от 8 до 18 с шагом 2 года)
margins, at(educ = (8(2)18)) predict(outcome(2)) vsquish
margins, at(educ = (8(2)18)) predict(outcome(3)) vsquish

predict p_1 p_2 p_3, pr // прогноз вероятности принадлежности индивидуума к каждой категории
sum p_1 p_2 p_3
tab pctstck1 

fitstat // проверка качества модели


* Tobit I. Цензурированная регрессия. Пример 1 (данные о дополнительных выплатах сотрудникам)
use http://fmwww.bc.edu/ec-p/data/wooldridge/fringe.dta, clear

sum annbens exper age educ tenure married male // описательные статистики

reg annbens exper expersq age annearn tenure // линейная модель регрессии, оцененная с помощью МНК
predict yhat_lm // прогноз доп. выплат согласно линейной модели регрессии

tobit annbens exper age annearn tenure, ll(0) // тобит-модель для данных, цензурированных слева нулем
predict yhat_tobit // прогноз доп. выплат согласно tobit-модели

corr annbens yhat_lm yhat_tobit

margins, dydx(*) predict(ystar(0,.))
margins, dydx(*) predict(e(0,.)) // предельный эффект для латентной переменной в усеченной совокупности (hrbens>0)
margins, dydx(*) predict(pr(1,2)) // вычисляет Pr(a < yj < b) - вероятность того, что yj|xj будет наблюдаемо в интервале (a,b)


* Tobit II (Heckman model)
use http://www.stata.com/data/jwooldridge/eacsap/mroz,clear

tab inlf // таблица частот для переменной inlf
sum lwage if inlf == 1 // описательные статистики зарплаты работающих женщин

* Обычная модель линейной регрессии для логарифма зарплаты
reg lwage educ exper c.exper#c.exper, vce(robust)

* Оценивание модели Хекмана с помощью ММП
heckman lwage educ exper c.exper#c.exper, select(inlf = educ exper age nwifeinc c.exper#c.exper kidslt6 kidsge6) 

* Sigma - стандартная ошибка остатков в уравнении зарплаты
* Rho - коэффициент корреляции между ошибками в уравнении для участия и для интенсивности участия
* Lambda = rho*sigma - эффект самоотбора
* Ожидаемая вероятность участия/отбора (вероятность того, что заработная плата будет наблюдаема)

predict prob_model, psel // прогноз вероятности "участия"
summarize prob_model

* Оценивание модели Хекмана с помощью двухшаговой процедуры
* Шаг 1: probit-модель для inlf
probit inlf educ exper age nwifeinc c.exper#c.exper kidslt6 kidsge6
predict inlf_hat
gen imr = normalden(inlf_hat)/normal(inlf_hat)
label variable imr "inverse Mills ratio"

* Шаг 2: модель регрессии для зарплаты
reg lwage educ exper c.exper#c.exper imr, vce(robust)

* Двухшаговая процедура может быть воспроизведена автоматически с помощью опции twostep
heckman lwage educ exper c.exper#c.exper, select(inlf = educ exper c.exper#c.exper age nwifeinc kidslt6 kidsge6) twostep

* Predictions
* ожидаемая заработная плата среди всех женщин, независимо от того, работали они на момент опроса или нет, т.е. E(y)
predict allwage
summarize allwage

* ycond вычисляет ожидаемое значение зависимой переменной при условии, что зависимая переменная наблюдаема, т.е. E(y|y was observed)
predict dwage, ycond
sum dwage
sum lwage dwage if lwage != .
 
* yexpected calculates the expected value of the dependent variable (y*) for all women,
* where that value is taken to be 0 when it is expected to be unobserved;
* y* = P(y observed) * E(y|y was observed). 
predict hcndwage1, yexpected

gen lwage0 = lwage
replace lwage0 = 0 if lwage == .
summarize hcndwage1 lwage0


* Margins effect
* предельный эффект только для уравнения интенсивности участия (зарплаты)
* нет четкой интерпретации, если независимая переменная присутствует в обоих уравнениях
margins, dydx(exper educ) atmean

* предельный эффект для уравнения отбора - влияние на вероятность того, что inlf = 1
margins, dydx(exper educ kidslt6) predict(psel) atmean

* предельный эффект для ожидаемого среднего значения зависимой переменной (зарплаты), если женщина действительно работала на момент опроса
margins, dydx(exper educ) predict(ycond)

* предельный эффект для ожидаемой зарплаты для всех женщин, учитывая их вероятность работать,
* предполагается, что lwage=0, если ожидается, что женщина не будет работать
margins, dydx(exper educ) predict(yexpected)
